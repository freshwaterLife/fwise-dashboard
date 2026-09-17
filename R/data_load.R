# data_load.R
# The data contract. Every module reads through the functions in this file and
# no module reads a CSV directly. When the data layout changes, this is the only
# file that should need to change.
#
# THE DATA IS ONE WIDE TABLE AND TWO LOOKUPS. attempts.csv holds one row per
# eradication attempt with every column the client's export has, plus the few
# the app owns (id, status, dates). Species and contacts are referenced by id
# into species.csv and contacts.csv, so a name is corrected in one place and a
# typo cannot mint a phantom species. Multi-value cells are FW_MULTI_SEP lists.
#
# THE STAR SCHEMA STILL EXISTS - IN MEMORY. fw_unpack() below splits the wide
# table into the six tables every module was written against (attempt, species,
# attempt_species, method, attempt_method, contact), so nothing downstream
# changed when the files did. At 2,000 rows the split takes well under a tenth
# of a second, measured, and it runs once at startup rather than per session.
#
# The tables are loaded once at app startup, not once per session, because
# they are read-only and identical for every visitor.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# ---- The columns -------------------------------------------------------------

# THE COLUMN CONTRACT for attempts.csv, in file order. A file with a column
# missing or an extra one stops the load (fw_validate_files), which is the guard
# that an earlier hand-written column list did not have: a source column that
# never got named simply vanished, silently, and took its 325 values with it.
#
# The submissions inbox uses the SAME vector, so a submission is one row in the
# shape of the database and a reviewer compares like with like.
#
# Values are stored as the source's text, verbatim. "0.010-0.020" stays a
# range, "≥0.5" keeps its sign, a year is the four characters the client typed.
# Casting is done here, in memory, for the columns in FW_ATTEMPT_NUMERIC only.
FW_ATTEMPT_COLUMNS <- c(
  # App-owned
  "attempt_id", "status", "submitted_at", "last_updated", "consent_data_use",
  # Site
  "site_name", "country", "region", "iso3", "continent", "latitude", "longitude",
  # Waterbody
  "waterbody_type", "water_regime", "area_treated", "area_unit", "area_notes",
  "depth_m", "depth_notes", "volume_m3", "volume_notes", "max_flow_m3s",
  "water_temp_c", "water_temp_notes",
  # Invasives: FW_MULTI_SEP list of species_id
  "invasive_species",
  # Timeline
  "invasion_year", "start_year", "end_year", "duration_days",
  "eradication_or_control", "driver",
  # Beneficiaries: FW_MULTI_SEP list of species_id
  "beneficiary_species",
  # Outcome
  "outcome",
  # Methods: FW_MULTI_SEP list of names; notes as "Name: text" joined by
  # FW_NOTES_SEP, one entry per method in the same order
  "methods", "method_notes", "method_description", "labour_person_days",
  "cost_estimate", "cost_notes",
  # Chemical detail
  "target_ingredient_basis", "toxin_conc_target_mg_l", "conc_target_notes",
  "toxin_conc_measured_mg_l", "conc_measured_notes",
  "neutralising_agent", "neutralising_notes",
  # Evidence
  "verification_method", "verification_notes",
  # People: ids into contacts.csv
  "primary_contact_id", "secondary_contact_id",
  # Provenance
  "source", "sent", "reference", "reference_link", "notes_for_fwise"
)

FW_SPECIES_COLUMNS <- c(
  "species_id", "common_name", "scientific_name", "taxa", "family", "iucn_status",
  "image_url", "image_credit", "image_licence", "image_licence_url", "image_page_url"
)

FW_CONTACT_COLUMNS <- c(
  "contact_id", "contact_name", "contact_email", "organisation", "email_public"
)

# The attempt columns that are numbers in memory. Every one of them is numeric
# on every row of the source. The two that LOOK numeric but are not -
# toxin_conc_target_mg_l (120 ranges and inequalities) and labour_person_days
# (189 sentences) - stay as text, because casting them threw those values away.
FW_ATTEMPT_NUMERIC <- c(
  "latitude", "longitude", "area_treated", "depth_m", "volume_m3", "max_flow_m3s",
  "water_temp_c", "invasion_year", "start_year", "end_year", "duration_days",
  "cost_estimate"
)

# The wide-table cells that fw_unpack() turns into bridge tables. They are
# dropped from the in-memory attempt table so they cannot collide with the
# columns fw_export_frame() builds under the same names. method_notes stays: nine
# attempts have no method and a note saying why, which has no bridge row to
# live in, and the raw cell is the only place it survives in memory.
FW_ATTEMPT_PACKED <- c("invasive_species", "beneficiary_species", "methods")

# ---- Where the data comes from -----------------------------------------------

# ONE RESOLVER. Every read in the application goes through fw_data_path(), so
# there is exactly one place that knows whether the data is on disk, on the far
# end of an authenticated API call, or at a plain URL.

#' Which of the three ways the data is reached
#'
#' THE TOKEN IS THE SWITCH, and it is the only thing a deployment sets. The
#' repository and branch are constants in config.R because neither is a secret
#' and neither varies.
#'
#'   local  ../fwise-data/, or whatever path FWISE_DATA_SOURCE names. No network
#'          calls whatsoever, which is what local development runs on.
#'   api    the private repository over the GitHub API, using FWISE_DATA_TOKEN.
#'          This is production. It is also the only mode that can WRITE.
#'   url    an https:// base read with no credential, for a public mirror.
#'
#' FWISE_DATA_SOURCE wins when it is set, so a test deploy can be pointed at a
#' fork or a local path without a code change. It is not needed in production.
fw_data_mode <- function() {
  src <- FWISE_DATA_SOURCE
  if (!is.null(src)) return(if (grepl("^https?://", src)) "url" else "local")
  if (!is.null(FWISE_DATA_TOKEN)) return("api")
  "local"
}

#' The root of a local or url source
#'
#' Meaningless in api mode, where the root is the repository itself.
fw_data_source <- function() {
  src <- FWISE_DATA_SOURCE %||% FW_DATA_DIR
  # A trailing slash is the easiest thing in the world to leave on the end of a
  # pasted URL, and it produces a "//" that some hosts 404 on.
  sub("/+$", "", src)
}

#' Resolve one file below the data root, fetching it if it is remote
#'
#' Returns something read_csv() and fromJSON() can open: a path on disk, a URL,
#' or - in api mode - the path of the temporary file the response was streamed
#' to. Every reader below therefore just opens a path and none of them contains
#' a second copy of this decision.
#'
#' NULL means the file is not there. Some callers treat that as fatal and others
#' as simply absent, which is why it is a return value rather than an error.
#'
#' @param ... path segments, e.g. "inbox", "FW-20261003-7K3QX9.csv"
fw_data_path <- function(...) {
  parts <- c(...)
  switch(
    fw_data_mode(),
    api = fw_gh_download(paste(parts, collapse = "/")),
    url = paste(c(fw_data_source(), parts), collapse = "/"),
    local = {
      path <- file.path(fw_data_source(), ...)
      if (file.exists(path)) path else NULL
    }
  )
}

#' Why a data file could not be read, in terms of how this app is configured
#'
#' Worth the words. A deployment that has not been given a token falls back to
#' the sibling checkout, which does not exist on the server, and the resulting
#' "no such file" names a path that means nothing to whoever is reading the log.
#' This says which mode produced it and what to do about it.
fw_data_missing <- function(rel) {
  switch(
    fw_data_mode(),
    local = paste0(
      "Could not read ", rel, " from ", fw_data_source(), ".\n",
      "The app is in LOCAL mode, reading the sibling checkout. If this is a ",
      "deployment, that directory does not exist there: set FWISE_DATA_TOKEN so ",
      "the app reads ", FW_DATA_REPO, " over the GitHub API instead."),
    api = paste0(
      "Could not read ", rel, " from ", FW_DATA_REPO, "@", FW_DATA_REF,
      " over the GitHub API.\n",
      "The token is working - an unreadable repository would have failed with a ",
      "clearer message - so the file itself is missing at that ref."),
    url = paste0("Could not read ", rel, " from ", fw_data_source(), ".")
  )
}

# ---- Reading -----------------------------------------------------------------

#' Read one of the three data files, every column as text
#'
#' Text on purpose. readr guessing from the first 1000 rows has bitten this kind
#' of file before - a mostly-empty numeric column reads as logical and every
#' join downstream breaks in a confusing way - and the file's contract is that
#' it holds the source's text verbatim. The casts happen in fw_unpack().
fw_read_table <- function(name) {
  path <- fw_data_path(name)
  if (is.null(path)) stop(fw_data_missing(name), call. = FALSE)
  read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
}

#' Load the data and unpack it into the six in-memory tables
#'
#' READ ONCE, AT STARTUP. Not per session and not on a poll. The data changes
#' when the review team folds submissions in, and visibility comes from a
#' deliberate restart, so re-reading it would spend a request per visitor to
#' discover that nothing had changed.
#'
#' @return A named list of six tibbles, filtered to approved rows.
fw_load_data <- function() {
  attempts <- fw_read_table(FW_ATTEMPTS_FILE)
  species  <- fw_read_table(FW_SPECIES_FILE)
  contacts <- fw_read_table(FW_CONTACTS_FILE)

  fw_validate_files(attempts, species, contacts)
  fw_validate_geography(attempts)
  tables <- fw_unpack(attempts, species, contacts)
  fw_filter_approved(tables)
}

#' Stop the load when a row's iso3 or continent disagrees with the ISO lookup
#'
#' THE RULE FOR PLACE, in one sentence: `country` is the ISO 3166-1 name of the
#' country or territory the site is in, and `iso3` and `continent` are
#' properties of that country and of nothing else.
#'
#' Two consequences, both of which the data used to get wrong:
#'   - a territory with its own ISO entry (Guam, Bermuda, Puerto Rico) is its
#'     own country, not a region of the state that administers it;
#'   - a region never moves its country between continents. Hawaii is a state
#'     of the United States, so it is USA and North America wherever it sits.
#' `region` is free text within the country - a state, a province, an island
#' group - and says nothing about the two derived columns.
#'
#' dev/qa.R writes iso3 and continent from the lookup at fold time; this is
#' the check that a hand edit has not undone that. A country the lookup does
#' not know - one typed in under "Other (specify)" and accepted by the
#' reviewer - is not checked here; qa.R reports it when the row is folded.
#'
#' The lookup is optional (see fw_load_iso), so a checkout without it loads
#' unchecked rather than failing.
fw_validate_geography <- function(attempts) {
  iso <- tryCatch(fw_read_lookup("lookup_iso3166.csv"), error = function(e) NULL)
  if (is.null(iso)) return(invisible(TRUE))

  m <- match(attempts$country, iso$country)
  known <- !is.na(m)
  want_iso3 <- iso$iso3[m]
  want_cont <- iso$continent[m]
  differs <- function(have, want) known & !is.na(want) & (is.na(have) | have != want)
  bad <- differs(attempts$iso3, want_iso3) | differs(attempts$continent, want_cont)
  # A region that is itself a country in the standard is a territory filed
  # under its administering state, which is the Guam mistake.
  bad <- bad | (!is.na(attempts$region) & attempts$region %in% iso$country)
  if (!any(bad)) return(invisible(TRUE))

  lines <- paste0(
    attempts$attempt_id[bad], ": ", attempts$country[bad],
    ifelse(is.na(attempts$region[bad]), "", paste0(" (", attempts$region[bad], ")")),
    " stored as ", attempts$iso3[bad], " / ", attempts$continent[bad],
    ", lookup says ", want_iso3[bad], " / ", want_cont[bad]
  )
  stop(sum(bad), " row(s) in ", FW_ATTEMPTS_FILE,
       " disagree with lookup_iso3166.csv on iso3, continent or territory:\n  ",
       paste(head(lines, 10), collapse = "\n  "),
       if (sum(bad) > 10) "\n  ...",
       "\niso3 and continent are properties of the country; a territory with its ",
       "own ISO entry is its own country. Correct the row, or the lookup.",
       call. = FALSE)
}

#' Fail loudly on a file whose columns are not the contract
#'
#' EXACT, in both directions. A missing column would make a page fail three
#' clicks later with a message about a variable nobody remembers; an extra one
#' is a value with nowhere to go, which is precisely how the ingredient-basis
#' column was lost from the old build. Order is not checked - dev/qa.R writes
#' the canonical order and a hand edit that reorders columns does no harm.
fw_validate_files <- function(attempts, species, contacts) {
  check <- function(name, have, want) {
    missing <- setdiff(want, have)
    extra   <- setdiff(have, want)
    if (length(missing) || length(extra)) {
      stop(name, " does not match the column contract in data_load.R.",
           if (length(missing)) paste0("\n  missing: ", paste(missing, collapse = ", ")),
           if (length(extra))   paste0("\n  unexpected: ", paste(extra, collapse = ", ")),
           "\nA column with nowhere to go is how data goes missing, so the load ",
           "stops here.", call. = FALSE)
    }
  }
  check(FW_ATTEMPTS_FILE, names(attempts), FW_ATTEMPT_COLUMNS)
  check(FW_SPECIES_FILE,  names(species),  FW_SPECIES_COLUMNS)
  check(FW_CONTACTS_FILE, names(contacts), FW_CONTACT_COLUMNS)
  if (any(duplicated(attempts$attempt_id))) {
    stop(FW_ATTEMPTS_FILE, " holds a duplicated attempt_id: ",
         paste(unique(attempts$attempt_id[duplicated(attempts$attempt_id)]),
               collapse = ", "), call. = FALSE)
  }
  if (any(duplicated(species$species_id))) {
    stop(FW_SPECIES_FILE, " holds a duplicated species_id.", call. = FALSE)
  }
  if (any(duplicated(contacts$contact_id))) {
    stop(FW_CONTACTS_FILE, " holds a duplicated contact_id.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Unpacking ---------------------------------------------------------------

#' Split one FW_MULTI_SEP cell into its items
#'
#' @return a character vector, empty for a blank cell. Items are trimmed so a
#'   hand edit with an extra space does not become a new id.
fw_split_multi <- function(x, sep = FW_MULTI_SEP) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  out <- trimws(strsplit(x, sep, fixed = TRUE)[[1]])
  out[nzchar(out)]
}

#' One row per (attempt, item) from a multi-value column
fw_unpack_multi <- function(attempts, column, value_name, sep = FW_MULTI_SEP) {
  items <- lapply(attempts[[column]], fw_split_multi, sep = sep)
  n <- lengths(items)
  out <- tibble(
    attempt_id = rep(attempts$attempt_id, n),
    value      = unlist(items, use.names = FALSE),
    position   = unlist(lapply(n, seq_len), use.names = FALSE)
  )
  names(out)[names(out) == "value"] <- value_name
  out
}

#' The method dimension for whatever names the data holds
#'
#' FW_METHODS supplies the seven known methods with their fixed ids and classes.
#' Anything else - a method a contributor typed in and the reviewer accepted -
#' gets an id derived from its name and the class "other", so it loads and
#' charts rather than stopping the app. dev/qa.R points the reviewer at these.
fw_method_table <- function(names_in_data) {
  known <- FW_METHODS
  extra <- setdiff(unique(names_in_data[!is.na(names_in_data)]), known$method_name)
  if (length(extra)) {
    known <- bind_rows(known, tibble(
      method_id    = paste0("ME-", toupper(substr(vapply(extra, rlang::hash, ""), 1, 6))),
      method_name  = extra,
      method_class = "other"
    ))
  }
  as_tibble(known)
}

#' Turn the wide table and the two lookups into the six tables the app reads
#'
#' Every id in attempts.csv must resolve to a row in its lookup, and the load
#' stops if one does not, naming the attempts. This is the integrity check the
#' old bridge tables could not give a spreadsheet: a reviewer who mistypes a
#' species id finds out at the next restart rather than never.
fw_unpack <- function(attempts, species, contacts) {
  a <- attempts
  for (col in FW_ATTEMPT_NUMERIC) a[[col]] <- suppressWarnings(as.numeric(a[[col]]))
  a$last_updated <- suppressWarnings(as.Date(a$last_updated))

  species  <- species[, FW_SPECIES_COLUMNS]
  contacts <- contacts[, FW_CONTACT_COLUMNS]
  contacts$email_public <- toupper(trimws(contacts$email_public)) %in% c("TRUE", "YES", "1")

  # ---- Species -----------------------------------------------------------------
  attempt_species <- bind_rows(
    mutate(fw_unpack_multi(a, "invasive_species",    "species_id"), role = "invasive"),
    mutate(fw_unpack_multi(a, "beneficiary_species", "species_id"), role = "beneficiary")
  ) |>
    select(attempt_id, species_id, role, position)

  fw_stop_unresolved(attempt_species, "species_id", species$species_id,
                     FW_ATTEMPTS_FILE, FW_SPECIES_FILE)

  # ---- Methods -----------------------------------------------------------------
  am <- fw_unpack_multi(a, "methods", "method_name")
  notes <- fw_unpack_multi(a, "method_notes", "note", sep = FW_NOTES_SEP)
  am <- left_join(am, notes, by = c("attempt_id", "position"))
  # Each note entry is "Method name: text". The prefix is stripped by the name
  # in the same position rather than by a regex on any name, so a note that
  # happens to start with a method's name is left intact.
  prefix <- paste0(am$method_name, ":")
  has_prefix <- !is.na(am$note) & startsWith(am$note, prefix)
  am$note[has_prefix] <- trimws(substring(am$note[has_prefix], nchar(prefix[has_prefix]) + 1))
  am$note[!is.na(am$note) & !nzchar(am$note)] <- NA_character_

  method <- fw_method_table(am$method_name)
  attempt_method <- am |>
    left_join(select(method, method_id, method_name), by = "method_name") |>
    transmute(attempt_id, method_id, method_order = as.integer(position),
              method_notes = note)

  # ---- Contacts ----------------------------------------------------------------
  refs <- bind_rows(
    tibble(attempt_id = a$attempt_id, contact_id = a$primary_contact_id),
    tibble(attempt_id = a$attempt_id, contact_id = a$secondary_contact_id)
  ) |> filter(!is.na(contact_id))
  fw_stop_unresolved(refs, "contact_id", contacts$contact_id,
                     FW_ATTEMPTS_FILE, FW_CONTACTS_FILE)

  list(
    attempt         = a[, setdiff(names(a), FW_ATTEMPT_PACKED)],
    species         = species,
    attempt_species = select(attempt_species, attempt_id, species_id, role),
    method          = method,
    attempt_method  = attempt_method,
    contact         = contacts
  )
}

#' Stop the load when a referenced id has no row
fw_stop_unresolved <- function(refs, id_col, known, from_file, to_file) {
  bad <- refs[!refs[[id_col]] %in% known, ]
  if (nrow(bad) == 0) return(invisible(TRUE))
  stop(nrow(bad), " ", id_col, " value(s) in ", from_file,
       " have no row in ", to_file, ":\n  ",
       paste(head(unique(paste0(bad$attempt_id, " -> ", bad[[id_col]])), 10),
             collapse = "\n  "),
       if (nrow(bad) > 10) "\n  ...",
       "\nAdd the missing row to ", to_file, " or correct the id.", call. = FALSE)
}

# ---- The approval gate -------------------------------------------------------

# The three permitted values. Anything else in the column is a data error and
# stops the load, because the alternative is a row with a typo in its status
# quietly becoming invisible - or worse, quietly becoming visible.
FW_STATUS <- c("pending", "approved", "rejected")

#' Keep only approved data, and everything reachable from it
#'
#' THE SUBTLE PART, AND THE EASY ONE TO GET WRONG. It is not enough to filter the
#' attempt table. A pending submission can introduce a new species, a new method
#' or a new contact, and those dimension rows sit in the same tables as the
#' approved ones. Filtering only the fact table leaves them in place, where every
#' dropdown, every lookup and the whole networking directory would pick them up -
#' publishing an unreviewed person's name and email on a public site.
#'
#' So the filtering is STRUCTURAL rather than a rule each consumer has to
#' remember. Everything downstream - fw_startup_choices(), fw_species_by_taxa(),
#' fw_choices(), fw_contacts_summary(), the report builder - is handed a set of
#' tables that contains nothing unapproved, so it cannot leak even if the person
#' writing it never thinks about approval at all. A new page added next year
#' inherits the guarantee for free.
#'
#' Foreign keys still resolve, because a dimension row is dropped only when
#' nothing approved refers to it any more.
#'
#' The pre-filter counts are attached as an attribute, for the in-review
#' indicator. See fw_review_count().
fw_filter_approved <- function(tables) {
  bad <- setdiff(unique(tables$attempt$status), c(FW_STATUS, NA))
  if (length(bad) > 0) {
    stop("attempts.status holds values that are not ",
         paste(sQuote(FW_STATUS), collapse = ", "), ": ",
         paste(sQuote(bad), collapse = ", "),
         "\nA row with an unrecognised status would be silently included or ",
         "excluded, so the load stops here.", call. = FALSE)
  }

  counts <- table(factor(tables$attempt$status, levels = FW_STATUS))

  keep <- tables$attempt |> filter(!is.na(status), status == "approved")
  ids  <- keep$attempt_id

  attempt_species <- filter(tables$attempt_species, attempt_id %in% ids)
  attempt_method  <- filter(tables$attempt_method,  attempt_id %in% ids)

  live_contacts <- unique(na.omit(c(keep$primary_contact_id,
                                    keep$secondary_contact_id)))

  out <- list(
    attempt         = keep,
    species         = filter(tables$species, species_id %in% attempt_species$species_id),
    attempt_species = attempt_species,
    method          = filter(tables$method,  method_id  %in% attempt_method$method_id),
    attempt_method  = attempt_method,
    contact         = filter(tables$contact, contact_id %in% live_contacts)
  )
  attr(out, "status_counts") <- counts
  out
}

#' How many records are waiting on review
#'
#' Two sources, because a submission is in review from the moment it is sent, not
#' from the moment someone folds it into the data: rows already carried into
#' attempts.csv as `pending`, plus whatever is sitting in the submissions inbox
#' and has not been processed at all yet.
fw_review_count <- function(data) {
  counts <- attr(data, "status_counts")
  in_schema <- if (is.null(counts)) 0L else as.integer(counts[["pending"]])

  # Only submissions NOT yet folded in. A folded submission is already counted
  # by in_schema above, and counting it here as well would double it and leave
  # the indicator permanently inflated.
  inbox <- tryCatch(fw_pending_submissions(), error = function(e) data.frame())
  in_inbox <- if (nrow(inbox) == 0) 0L
    else if ("status" %in% names(inbox)) sum(inbox$status == "pending", na.rm = TRUE)
    else nrow(inbox)

  as.integer(in_schema + in_inbox)
}

#' The release metadata that travels with the data
#'
#' Carries the release date and the row counts as published, so the footer's
#' "last updated" line states when the data was released rather than inferring it
#' from a column inside the data. Written by dev/qa.R on every fold, so the
#' counts cannot drift away from the tables they describe.
#'
#' A missing or unreadable file is not fatal - the app is still perfectly usable
#' without a date in the footer, and failing to boot over it would be a poor
#' trade.
fw_load_metadata <- function() {
  out <- tryCatch({
    path <- fw_data_path("metadata.json")
    if (is.null(path)) NULL else jsonlite::fromJSON(path)
  },
    error = function(e) {
      warning("Could not read metadata.json: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )
  if (is.null(out)) return(list(release = NA, row_counts = NULL))
  out
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Derived views -----------------------------------------------------------

#' A display label for a species, searchable by either name
#'
#' Some entries in the source have no parenthetical scientific name, e.g.
#' "Amphipoda" or "Barbus sp.". For those the whole string sits in common_name,
#' so coalescing here keeps every species findable in a dropdown.
# TODO(alex): the client's QA cleaning should eventually split these properly.
# Until it does, a search for "Amphipoda" matches on the common_name field.
fw_species_label <- function(species) {
  species |>
    mutate(
      label = case_when(
        !is.na(common_name) & !is.na(scientific_name) ~
          paste0(common_name, " (", scientific_name, ")"),
        !is.na(scientific_name) ~ scientific_name,
        TRUE ~ common_name
      )
    )
}

#' One row per contact, with countries and continents derived from their attempts
#'
#' A contact attached to attempts in more than one country belongs to all of
#' them, so country and continent are list columns rather than single values.
#'
#' REDACTION: where email_public is FALSE the address is replaced with NA here,
#' before the data reaches any session. The Networking page reads only from this
#' function, so a redacted address never enters the browser and cannot be
#' recovered from anything served to it.
fw_contacts_summary <- function(data) {
  # Both contact slots on an attempt count towards that contact's totals.
  links <- bind_rows(
    data$attempt |>
      select(attempt_id, contact_id = primary_contact_id, country, continent),
    data$attempt |>
      select(attempt_id, contact_id = secondary_contact_id, country, continent)
  ) |>
    filter(!is.na(contact_id))

  derived <- links |>
    group_by(contact_id) |>
    summarise(
      attempt_count = n_distinct(attempt_id),
      countries     = list(sort(unique(country))),
      continents    = list(sort(unique(continent))),
      attempt_ids   = list(sort(unique(attempt_id))),
      .groups = "drop"
    )

  data$contact |>
    left_join(derived, by = "contact_id") |>
    mutate(
      attempt_count = coalesce(attempt_count, 0L),
      countries  = map(countries,  ~ if (is.null(.x)) character(0) else .x),
      continents = map(continents, ~ if (is.null(.x)) character(0) else .x),
      attempt_ids = map(attempt_ids, ~ if (is.null(.x)) character(0) else .x),
      # The redaction itself. Do not move this downstream.
      contact_email = if_else(email_public, contact_email, NA_character_),
      country_label   = map_chr(countries,  ~ paste(.x, collapse = ", ")),
      continent_label = map_chr(continents, ~ paste(.x, collapse = ", "))
    ) |>
    # An older contact table carried an always-empty country column; drop it if
    # present so nothing downstream mistakes it for the derived value.
    select(-any_of("country")) |>
    arrange(desc(attempt_count), contact_name)
}

#' Sorted unique values from any loaded table, for populating dropdowns
#'
#' Choices come from the data rather than a hardcoded list, so the client's
#' ongoing cleaning flows through without a code change.
fw_choices <- function(data, table, column) {
  values <- data[[table]][[column]]
  if (is.null(values)) {
    stop("No column '", column, "' in table '", table, "'", call. = FALSE)
  }
  values <- values[!is.na(values) & values != ""]
  sort(unique(values))
}

#' The date shown in the footer
#'
#' The RELEASE date from metadata.json, not the maximum of a column inside the
#' data. Those are different things: a release can republish unchanged rows, and
#' last_updated is a property of a record rather than of the publication. Falls
#' back to the column if metadata is missing, so the footer degrades to the old
#' behaviour rather than to nothing.
fw_last_updated <- function(data, meta = NULL) {
  release <- meta$release %||% NA
  if (!is.null(release) && !all(is.na(release)) && nzchar(release[1])) {
    d <- suppressWarnings(as.Date(release[1]))
    if (!is.na(d)) return(d)
  }
  d <- suppressWarnings(max(data$attempt$last_updated, na.rm = TRUE))
  if (is.infinite(d) || is.na(d)) NA else d
}

#' Headline counts for the whole database
#'
#' Computed at runtime from the loaded data, never hardcoded. Read by the
#' Explore page's database panel, the About page's scale sentence and citation,
#' and the Welcome page's species-protected figure. Counts only, never a rate.
fw_headline_stats <- function(data) {
  role_ids <- function(role_name) {
    unique(data$attempt_species$species_id[data$attempt_species$role == role_name])
  }

  list(
    attempts       = nrow(data$attempt),
    countries      = n_distinct(data$attempt$country),
    earliest_year  = suppressWarnings(min(data$attempt$start_year, na.rm = TRUE)),
    latest_year    = suppressWarnings(max(data$attempt$start_year, na.rm = TRUE)),
    species        = length(role_ids("invasive")),
    beneficiaries  = length(role_ids("beneficiary")),
    successful     = sum(data$attempt$outcome == "Successful", na.rm = TRUE),
    # The Welcome page's one figure: beneficiaries of SUCCESSFUL attempts only.
    protected      = length(unique(data$attempt_species$species_id[
      data$attempt_species$role == "beneficiary" &
        data$attempt_species$attempt_id %in%
          data$attempt$attempt_id[data$attempt$outcome %in% "Successful"]
    ]))
  )
}

#' Choropleth source for the landing and explore maps
#'
#' Isolated behind one function so swapping a real per-country invasive fish
#' count for the current placeholder is a single change.
#'
# [PLACEHOLDER] No per-country invasive fish species file is present in the
# repository, so this returns the count of invasive species RECORDED IN FWISE
# per country. That is the response, not the burden, so it does not yet tell the
# mismatch story the landing page needs. Replace the body when the real burden
# file arrives.
fw_country_burden <- function(data) {
  data$attempt |>
    select(attempt_id, country, iso3, continent) |>
    inner_join(filter(data$attempt_species, role == "invasive"), by = "attempt_id") |>
    group_by(country, iso3, continent) |>
    summarise(species_count = n_distinct(species_id), .groups = "drop") |>
    arrange(desc(species_count))
}

# ---- Dropdown choices, built once at startup ---------------------------------

# The text appended to a dropdown when the user needs to supply their own value.
# Selecting it reveals a free-text box.
FW_OTHER <- "Other (specify)"

# The key under which a choice list keeps its unfiltered version. A real name
# rather than "", because R's [[ ]] does not retrieve an empty name.
FW_ALL <- ".all"

# ---- ISO 3166 ----------------------------------------------------------------

#' The ISO country and subdivision lookups
#'
#' Read from disk like every other table - the app makes no network call for
#' these. Built by dev/build_iso_lookups.R and committed alongside the data.
#'
#' DEGRADES RATHER THAN FAILS. If the files are absent - an older data checkout,
#' say - the form falls back to the countries present in the attempts and offers
#' a free-text region, which is exactly what it did before these existed. A
#' missing convenience must not stop someone submitting a record.
fw_load_iso <- function() {
  empty <- list(countries = NULL, subdivisions = list())

  countries <- try(fw_read_lookup("lookup_iso3166.csv"), silent = TRUE)
  if (inherits(countries, "try-error") || is.null(countries)) return(empty)

  subs <- try(fw_read_lookup("lookup_iso3166_2.csv"), silent = TRUE)
  if (inherits(subs, "try-error") || is.null(subs)) {
    return(list(countries = countries, subdivisions = list()))
  }

  named <- subs |>
    left_join(select(countries, country, iso3), by = "iso3") |>
    filter(!is.na(country)) |>
    arrange(country, subdivision_name)

  list(
    countries = countries,
    subdivisions = split(named$subdivision_name, named$country)
  )
}

#' Read one lookup file from the data source, or NULL if it is not there
#'
#' These sit at the ROOT of the data source, beside metadata.json and the three
#' data files - they describe the standard, not this dataset.
fw_read_lookup <- function(name) {
  path <- fw_data_path(name)
  if (is.null(path)) return(NULL)
  suppressWarnings(readr::read_csv(path, show_col_types = FALSE,
                                   progress = FALSE))
}

#' Countries for the contribute form: recorded ones first, then the rest
fw_country_choices <- function(data, iso_countries) {
  recorded <- sort(unique(data$attempt$country))
  if (is.null(iso_countries)) return(c(recorded, FW_OTHER))

  rest <- setdiff(sort(iso_countries$country), recorded)
  c(recorded, rest, FW_OTHER)
}

#' The subdivisions of one country, for the region picker
fw_subdivisions_for <- function(choices, country) {
  if (is.null(country) || !nzchar(country)) return(character(0))
  subs <- choices$subdivision[[country]]
  if (is.null(subs)) character(0) else subs
}

#' Build every dropdown's options once, at startup
#'
#' Options are derived from the loaded data so the client's ongoing cleaning
#' flows through without a code change. A handful of values appear in the form
#' specification but not yet in the data, so the two are unioned rather than the
#' data alone being used; otherwise a contributor could not pick an answer the
#' client has explicitly asked for.
fw_startup_choices <- function(data) {
  species_labelled <- fw_species_label(data$species)

  # Sorted by how often a species actually appears, so the common answers are
  # near the top of a 390-entry list, then alphabetical within that.
  species_freq <- data$attempt_species |>
    count(species_id, name = "n")

  species_choices <- species_labelled |>
    left_join(species_freq, by = "species_id") |>
    mutate(n = coalesce(n, 0L)) |>
    arrange(desc(n), label) |>
    pull(label)

  union_sorted <- function(from_data, from_spec) {
    sort(union(from_data, from_spec))
  }

  iso <- fw_load_iso()

  list(
    # THE FULL ISO 3166-1 LIST, not the 29 countries that happen to have a
    # record already. Offering only what is in the database is circular: the
    # form exists to add the countries that are missing from it. Countries that
    # DO have attempts float to the top, because they are the likely answers,
    # and "Other (specify)" catches anything the standard does not cover.
    country = fw_country_choices(data, iso$countries),

    # Keyed by country name, so the region field can narrow to that country's
    # subdivisions once it is answered. See fw_subdivisions_for().
    subdivision = iso$subdivisions,

    waterbody_type = c(fw_choices(data, "attempt", "waterbody_type"), FW_OTHER),
    water_regime   = fw_regime_choices(fw_choices(data, "attempt", "water_regime")),
    area_unit      = fw_choices(data, "attempt", "area_unit"),

    invasive_taxa = c(
      union_sorted(
        unique(data$species$taxa[!is.na(data$species$taxa)]),
        c("Fish", "Mussel", "Crayfish", "Frog", "Turtle", "Salamander")
      ),
      FW_OTHER
    ),

    beneficiary_taxa = c(
      union_sorted(
        unique(data$species$taxa[!is.na(data$species$taxa)]),
        c("Fish", "Frog", "Invertebrate", "Mussel", "Salamander", "Amphibian",
          "Crayfish", "Bird", "Zooplankton", "Shrimp", "Insect", "Amphipod",
          "Snail", "Plant", "Notostracan", "Lizard", "Turtle", "Mammal",
          "Unknown")
      ),
      FW_OTHER
    ),

    species = species_choices,
    family  = fw_choices(data, "species", "family"),

    driver = c(
      union_sorted(
        fw_choices(data, "attempt", "driver"),
        c("Native/endangered species", "Fisheries",
          "Ecosystem/habitat restoration", "Prevent spread/establishment",
          "Research/experimental", "Water supply/infrastructure", "Unknown")
      ),
      FW_OTHER
    ),

    method = c(
      union_sorted(
        fw_choices(data, "method", "method_name"),
        c("Rotenone", "Antimycin-A", "Netting / Trapping", "Electrofishing",
          "Draining", "Other chemical", "Other mechanical")
      ),
      FW_OTHER
    ),

    # Drives the conditional chemical-detail step.
    chemical_methods = data$method$method_name[data$method$method_class == "chemical"],

    neutralising_agent = c(
      union_sorted(
        fw_choices(data, "attempt", "neutralising_agent"),
        c("Potassium permanganate", "Natural degradation", "Methylene blue",
          "Malonic acid", "Sodium hypochlorite")
      ),
      FW_OTHER
    ),

    # Fixed by the specification, not derived: these four are the analysis
    # categories the paper uses and must not drift with the data.
    outcome = c("Successful", "Failed", "Ongoing", "Unknown"),

    # Waterbody types split by regime, so the contribute form can narrow the
    # list once still or flowing has been answered. See fw_waterbody_by_regime().
    waterbody_by_regime = fw_waterbody_by_regime(data),

    # Species split by the group they belong to, so choosing "Fish" narrows the
    # species picker to fish. The FW_ALL element is every species, for a target
    # whose group is not yet answered or is one with nothing recorded under it.
    species_by_taxa = fw_species_by_taxa(species_labelled, species_choices),

    # label -> species_id, so the form writes the id the database already
    # holds for a species the contributor picked. A species they typed in has
    # no entry here and travels as text for the reviewer - see submit.R.
    species_ids = stats::setNames(species_labelled$species_id, species_labelled$label)
  )
}

# THE ONE PLACE THAT SAYS WHICH WATERBODIES FLOW.
#
# Read by TWO consumers that used to disagree:
#   - the migration that built attempts.csv, which corrected water_regime once
#   - fw_waterbody_by_regime() below, which narrows the contribute form's
#     waterbody list once "still or flowing" has been answered
#
# Before this was shared, the override existed only for the form, so the form
# knew a tributary flows while the data still filed three of them under Lentic.
# Anything not named here keeps whatever the source says; anything genuinely
# missing is covered by "Other (specify)", which is always appended.
#
# NOT A REGIME AT ALL. "Multiple" means more than one system was treated, so
# claiming either regime for it is a statement the record does not support.
FW_REGIME_BY_TYPE <- c(
  Canal = "Lotic", Channel = "Lotic", Creek = "Lotic", River = "Lotic",
  Stream = "Lotic", Tributary = "Lotic",
  # A spring is a discharge point and a drain is an engineered channel; the
  # water in both is moving. Both arrived from the source marked Lentic.
  Spring = "Lotic", Drain = "Lotic",
  Multiple = NA_character_
)

FW_LOTIC_ALWAYS <- names(FW_REGIME_BY_TYPE)[
  !is.na(FW_REGIME_BY_TYPE) & FW_REGIME_BY_TYPE == "Lotic"
]

# ---- Regime wording ----------------------------------------------------------

# LABELS ONLY. Lentic and Lotic are the STORED vocabulary: they are what sits in
# attempt.csv, what the data dictionary defines, what the submissions inbox
# carries and what FW_REGIME_BY_TYPE above is keyed on. A contributor should not
# have to know the words, so the pickers show plain English and hand back the
# stored value unchanged. Recoding the column instead would have meant rebuilding
# the schema and chasing every "Lotic" comparison in the app.
FW_REGIME_LABELS <- c(Lentic = "Still water", Lotic = "Flowing water")

#' The plain-English wording for a stored regime value
#'
#' Anything the lookup has not been told about passes through as itself, so a
#' new value appears in the picker rather than silently becoming NA.
fw_regime_label <- function(x) {
  out <- unname(FW_REGIME_LABELS[x])
  ifelse(is.na(out), x, out)
}

#' Stored regime values, named by what the user should see
#'
#' selectizeInput() and radioButtons() both display the NAMES and submit the
#' VALUES, so this is the whole of the translation.
fw_regime_choices <- function(values) {
  stats::setNames(values, fw_regime_label(values))
}

#' Waterbody types that belong to each water regime

fw_waterbody_by_regime <- function(data) {
  seen <- data$attempt |>
    filter(!is.na(waterbody_type), !is.na(water_regime)) |>
    distinct(waterbody_type, water_regime)

  regimes <- sort(unique(seen$water_regime))
  out <- lapply(regimes, function(rg) {
    types <- sort(unique(seen$waterbody_type[seen$water_regime == rg]))
    if (identical(rg, "Lotic")) {
      types <- sort(union(types, intersect(FW_LOTIC_ALWAYS, seen$waterbody_type)))
    } else {
      types <- setdiff(types, FW_LOTIC_ALWAYS)
    }
    c(types, FW_OTHER)
  })
  names(out) <- regimes

  # The unfiltered list, for before the regime question is answered. Keyed by
  # FW_ALL rather than "": R's [[ ]] does not retrieve an empty name reliably.
  out[[FW_ALL]] <- c(sort(unique(seen$waterbody_type)), FW_OTHER)
  out
}

#' Species labels grouped by the taxa they belong to
#'
#' @param species_labelled the species table with a `label` column
#' @param all_labels every label, already ordered by how often it is recorded
fw_species_by_taxa <- function(species_labelled, all_labels) {
  by_taxa <- split(species_labelled$label,
                   species_labelled$taxa %||% NA_character_)
  by_taxa <- by_taxa[!is.na(names(by_taxa)) & nzchar(names(by_taxa))]
  # Keep each group in the same frequency order as the full list.
  by_taxa <- lapply(by_taxa, function(x) all_labels[all_labels %in% x])
  by_taxa[[FW_ALL]] <- all_labels
  by_taxa
}
