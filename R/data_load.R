# data_load.R
# The data contract. Every module reads data through this file; none reads a
# CSV directly.
#
# One wide table (attempts.csv, one row per attempt) and two lookups referenced
# by id (species.csv, contacts.csv). Multi-value cells are FW_MULTI_SEP lists.
# fw_unpack() splits them into six in-memory tables (attempt, species,
# attempt_species, method, attempt_method, contact). Loaded once at startup.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# ---- The columns -------------------------------------------------------------

# Column contract for attempts.csv, in file order. A missing or extra column
# stops the load (fw_validate_files()). The inbox sheet's columns derive from
# this too (FW_INBOX_COLUMNS in submit.R).
#
# Values are stored as the source text ("0.010-0.020", "≥0.5"); only
# FW_ATTEMPT_NUMERIC columns are cast, in memory.
FW_ATTEMPT_COLUMNS = c(
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
  "cost_estimate", "cost_currency", "cost_notes",
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

FW_SPECIES_COLUMNS = c(
  "species_id", "common_name", "scientific_name", "taxa", "family",
  # IUCN Red List assessment (dev/qa.R fills these for new species)
  "iucn_status", "iucn_category", "iucn_url", "iucn_year", "iucn_note", "iucn_matched_name",
  "image_url", "image_credit", "image_licence", "image_licence_url", "image_page_url"
)

FW_CONTACT_COLUMNS = c(
  "contact_id", "contact_name", "contact_email", "organisation", "contact_public"
)

# Columns cast to numbers in memory. toxin_conc_target_mg_l and
# labour_person_days look numeric but hold ranges and sentences, so stay text.
FW_ATTEMPT_NUMERIC = c(
  "latitude", "longitude", "area_treated", "depth_m", "volume_m3", "max_flow_m3s",
  "water_temp_c", "invasion_year", "start_year", "end_year", "duration_days",
  "cost_estimate"
)

# Packed cells that become bridge tables, dropped from the in-memory attempt
# table so they don't clash with fw_export_frame()'s columns. method_notes stays:
# attempts with no method can still have a note.
FW_ATTEMPT_PACKED = c("invasive_species", "beneficiary_species", "methods")

# ---- Where the data comes from -----------------------------------------------

# Every read goes through fw_data_path(), the one place that knows where the
# data lives.

#' How the data is reached
#'
#'   local  ../fwise-data/ or a FWISE_DATA_SOURCE path. No network.
#'   api    the private repo over the GitHub API (FWISE_DATA_TOKEN). Production.
#'   url    an https:// FWISE_DATA_SOURCE, no credential.
#'
#' FWISE_DATA_SOURCE wins when set.
fw_data_mode = function() {
  src = FWISE_DATA_SOURCE
  if (!is.null(src)) return(if (grepl("^https?://", src)) "url" else "local")
  if (!is.null(FWISE_DATA_TOKEN)) return("api")
  "local"
}

#' The root of a local or url source
#'
#' Meaningless in api mode, where the root is the repository itself.
fw_data_source = function() {
  src = FWISE_DATA_SOURCE %||% FW_DATA_DIR
  # Drop trailing slashes: "//" 404s on some hosts.
  sub("/+$", "", src)
}

#' Resolve one file below the data root, fetching it if it is remote
#'
#' Returns a path, URL or (api mode) temp file that read_csv() can open, or
#' NULL if the file isn't there (callers decide whether that's fatal).
#'
#' @param ... path segments, e.g. "attempts.csv"
fw_data_path = function(...) {
  parts = c(...)
  switch(
    fw_data_mode(),
    api = fw_gh_download(paste(parts, collapse = "/")),
    url = paste(c(fw_data_source(), parts), collapse = "/"),
    local = {
      path = file.path(fw_data_source(), ...)
      if (file.exists(path)) path else NULL
    }
  )
}

#' Why a data file couldn't be read, naming the mode and the fix
fw_data_missing = function(rel) {
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
#' Text so readr can't mis-guess types; fw_unpack() casts.
fw_read_table = function(name) {
  path = fw_data_path(name)
  if (is.null(path)) stop(fw_data_missing(name), call. = FALSE)
  read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
}

#' Load the data and unpack it into the six in-memory tables
#'
#' Read once at startup. New data appears after a republish.
#'
#' @return A named list of six tibbles, filtered to approved rows.
fw_load_data = function() {
  attempts = fw_read_table(FW_ATTEMPTS_FILE)
  species  = fw_read_table(FW_SPECIES_FILE)
  contacts = fw_read_table(FW_CONTACTS_FILE)

  fw_validate_files(attempts, species, contacts)
  fw_validate_geography(attempts)
  tables = fw_unpack(attempts, species, contacts)
  fw_filter_approved(tables)
}

#' Stop the load when a row's iso3 or continent disagrees with the ISO lookup
#'
#' `country` is the ISO 3166-1 country or territory; `iso3` and `continent`
#' follow from it alone. A territory with its own ISO entry (Guam) is its own
#' country; a region (Hawaii) never changes the country or continent. `region`
#' is free text.
#'
#' Catches hand edits that undo dev/qa.R's import. Skipped if the lookup is
#' missing.
fw_validate_geography = function(attempts) {
  iso = tryCatch(fw_read_lookup("lookup_iso3166.csv"), error = function(e) NULL)
  if (is.null(iso)) return(invisible(TRUE))

  m = match(attempts$country, iso$country)
  known = !is.na(m)
  want_iso3 = iso$iso3[m]
  want_cont = iso$continent[m]
  differs = function(have, want) known & !is.na(want) & (is.na(have) | have != want)
  bad = differs(attempts$iso3, want_iso3) | differs(attempts$continent, want_cont)
  # A region that is an ISO country is a territory filed under its state.
  bad = bad | (!is.na(attempts$region) & attempts$region %in% iso$country)
  if (!any(bad)) return(invisible(TRUE))

  lines = paste0(
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
#' Exact in both directions (an extra column is data with nowhere to go).
#' Order isn't checked.
fw_validate_files = function(attempts, species, contacts) {
  check = function(name, have, want) {
    missing = setdiff(want, have)
    extra   = setdiff(have, want)
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
#' @return a character vector, empty for a blank cell. Items are trimmed.
fw_split_multi = function(x, sep = FW_MULTI_SEP) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  out = trimws(strsplit(x, sep, fixed = TRUE)[[1]])
  out[nzchar(out)]
}

#' One row per (attempt, item) from a multi-value column
fw_unpack_multi = function(attempts, column, value_name, sep = FW_MULTI_SEP) {
  items = lapply(attempts[[column]], fw_split_multi, sep = sep)
  n = lengths(items)
  out = tibble(
    attempt_id = rep(attempts$attempt_id, n),
    value      = unlist(items, use.names = FALSE),
    position   = unlist(lapply(n, seq_len), use.names = FALSE)
  )
  names(out)[names(out) == "value"] = value_name
  out
}

#' The method dimension for whatever names the data holds
#'
#' FW_METHODS plus any other name in the data, which gets a hashed id and
#' class "other" (dev/qa.R warns about these).
fw_method_table = function(names_in_data) {
  known = FW_METHODS
  extra = setdiff(unique(names_in_data[!is.na(names_in_data)]), known$method_name)
  if (length(extra)) {
    known = bind_rows(known, tibble(
      method_id    = paste0("ME-", toupper(substr(vapply(extra, rlang::hash, ""), 1, 6))),
      method_name  = extra,
      method_class = "other"
    ))
  }
  as_tibble(known)
}

#' Turn the wide table and the two lookups into the six tables the app reads
#'
#' Every id in attempts.csv must resolve to its lookup, or the load stops
#' naming the attempts.
fw_unpack = function(attempts, species, contacts) {
  a = attempts
  for (col in FW_ATTEMPT_NUMERIC) a[[col]] = suppressWarnings(as.numeric(a[[col]]))
  a$last_updated = suppressWarnings(as.Date(a$last_updated))

  species  = species[, FW_SPECIES_COLUMNS]
  contacts = contacts[, FW_CONTACT_COLUMNS]
  contacts$contact_public = toupper(trimws(contacts$contact_public)) %in% c("TRUE", "YES", "1")

  # ---- Species -----------------------------------------------------------------
  attempt_species = bind_rows(
    mutate(fw_unpack_multi(a, "invasive_species",    "species_id"), role = "invasive"),
    mutate(fw_unpack_multi(a, "beneficiary_species", "species_id"), role = "beneficiary")
  ) |>
    select(attempt_id, species_id, role, position)

  fw_stop_unresolved(attempt_species, "species_id", species$species_id,
                     FW_ATTEMPTS_FILE, FW_SPECIES_FILE)

  # ---- Methods -----------------------------------------------------------------
  am = fw_unpack_multi(a, "methods", "method_name")
  notes = fw_unpack_multi(a, "method_notes", "note", sep = FW_NOTES_SEP)
  am = left_join(am, notes, by = c("attempt_id", "position"))
  # Notes are "Method name: text". Strip the prefix of the method in the same
  # position only.
  prefix = paste0(am$method_name, ":")
  has_prefix = !is.na(am$note) & startsWith(am$note, prefix)
  am$note[has_prefix] = trimws(substring(am$note[has_prefix], nchar(prefix[has_prefix]) + 1))
  am$note[!is.na(am$note) & !nzchar(am$note)] = NA_character_

  method = fw_method_table(am$method_name)
  attempt_method = am |>
    left_join(select(method, method_id, method_name), by = "method_name") |>
    transmute(attempt_id, method_id, method_order = as.integer(position),
              method_notes = note)

  # ---- Contacts ----------------------------------------------------------------
  refs = bind_rows(
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
fw_stop_unresolved = function(refs, id_col, known, from_file, to_file) {
  bad = refs[!refs[[id_col]] %in% known, ]
  if (nrow(bad) == 0) return(invisible(TRUE))
  stop(nrow(bad), " ", id_col, " value(s) in ", from_file,
       " have no row in ", to_file, ":\n  ",
       paste(head(unique(paste0(bad$attempt_id, " -> ", bad[[id_col]])), 10),
             collapse = "\n  "),
       if (nrow(bad) > 10) "\n  ...",
       "\nAdd the missing row to ", to_file, " or correct the id.", call. = FALSE)
}

# ---- The approval gate -------------------------------------------------------

# Permitted status values. Anything else stops the load.
FW_STATUS = c("pending", "approved", "rejected")

#' Keep only approved data, and everything reachable from it
#'
#' Blocker so unreviewed data never reaches a page. Drops unapproved attempts
#' and every species, method and contact only they reference, so a pending
#' person's name can't leak through a dropdown or the directory. Don't reduce
#' this to filtering the attempt table.
#'
#' Pre-filter status counts are attached as an attribute for fw_review_count().
fw_filter_approved = function(tables) {
  bad = setdiff(unique(tables$attempt$status), c(FW_STATUS, NA))
  if (length(bad) > 0) {
    stop("attempts.status holds values that are not ",
         paste(sQuote(FW_STATUS), collapse = ", "), ": ",
         paste(sQuote(bad), collapse = ", "),
         "\nA row with an unrecognised status would be silently included or ",
         "excluded, so the load stops here.", call. = FALSE)
  }

  counts = table(factor(tables$attempt$status, levels = FW_STATUS))

  keep = tables$attempt |> filter(!is.na(status), status == "approved")
  ids  = keep$attempt_id

  attempt_species = filter(tables$attempt_species, attempt_id %in% ids)
  attempt_method  = filter(tables$attempt_method,  attempt_id %in% ids)

  live_contacts = unique(na.omit(c(keep$primary_contact_id,
                                    keep$secondary_contact_id)))

  out = list(
    attempt         = keep,
    species         = filter(tables$species, species_id %in% attempt_species$species_id),
    attempt_species = attempt_species,
    method          = filter(tables$method,  method_id  %in% attempt_method$method_id),
    attempt_method  = attempt_method,
    contact         = filter(tables$contact, contact_id %in% live_contacts)
  )
  attr(out, "status_counts") = counts
  out
}

#' How many records are waiting on review
#'
#' Pending rows in attempts.csv plus inbox rows neither imported nor rejected.
#'
#' @param strict TRUE lets a failed inbox read raise, so fw_review_count_live()
#'   can keep its last good count; FALSE counts the inbox as empty.
fw_review_count = function(data, strict = FALSE) {
  counts = attr(data, "status_counts")
  in_schema = if (is.null(counts)) 0L else as.integer(counts[["pending"]])

  # Not-yet-imported only; imported ones are pending (counted above) or live.
  inbox = if (strict) fw_pending_submissions() else
    tryCatch(fw_pending_submissions(), error = function(e) data.frame())

  as.integer(in_schema + nrow(inbox))
}

#' The release metadata that travels with the data
#'
#' Release date and row counts, written by dev/qa.R on import. Not fatal if
#' missing.
fw_load_metadata = function() {
  out = tryCatch({
    path = fw_data_path("metadata.json")
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

`%||%` = function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Derived views -----------------------------------------------------------

#' A display label for a species, searchable by either name
#'
#' Some entries ("Amphipoda", "Barbus sp.") have no scientific name and sit
#' whole in common_name; coalescing keeps them findable.
# TODO(alex): the client's QA cleaning should split these properly.
fw_species_label = function(species) {
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

#' A protected species' label as it is shown, with its Red List code
#'
#' Adds `shown`, e.g. "Little grebe (Tachybaptus ruficollis) (IUCN: LC)".
#' Display only, protected species only; `label` stays the matching key.
#'
#' @param species rows of fw_species_label()
fw_species_shown = function(species) {
  # Read once: fw_t() merges the whole copy deck on every call.
  iucn_suffix = fw_t("species", "iucn_suffix")
  species$shown = species$label
  coded = !is.na(species$label) & !is.na(species$iucn_status)
  species$shown[coded] = paste0(species$label[coded], vapply(
    species$iucn_status[coded], function(code) fw_fill(iucn_suffix, code = code), ""))
  species
}

#' One row per contact, with countries and continents derived from their attempts
#'
#' country and continent are list columns (a contact can span several).
#'
#' Blocker to ensure no privacy leaks: contacts with contact_public FALSE are
#' dropped here, name and all, before any session sees them. The directory and
#' Plan contacts read only from this. Attempts are untouched.
fw_contacts_summary = function(data) {
  # Both contact slots on an attempt count towards that contact's totals.
  links = bind_rows(
    data$attempt |>
      select(attempt_id, contact_id = primary_contact_id, country, continent),
    data$attempt |>
      select(attempt_id, contact_id = secondary_contact_id, country, continent)
  ) |>
    filter(!is.na(contact_id))

  derived = links |>
    group_by(contact_id) |>
    summarise(
      attempt_count = n_distinct(attempt_id),
      countries     = list(sort(unique(country))),
      continents    = list(sort(unique(continent))),
      attempt_ids   = list(sort(unique(attempt_id))),
      .groups = "drop"
    )

  data$contact |>
    # The redaction. Drop the row; don't blank or move it downstream.
    filter(contact_public) |>
    left_join(derived, by = "contact_id") |>
    mutate(
      attempt_count = coalesce(attempt_count, 0L),
      countries  = map(countries,  ~ if (is.null(.x)) character(0) else .x),
      continents = map(continents, ~ if (is.null(.x)) character(0) else .x),
      attempt_ids = map(attempt_ids, ~ if (is.null(.x)) character(0) else .x),
      country_label   = map_chr(countries,  ~ paste(.x, collapse = ", ")),
      continent_label = map_chr(continents, ~ paste(.x, collapse = ", "))
    ) |>
    # Drop any legacy empty country column.
    select(-any_of("country")) |>
    arrange(desc(attempt_count), contact_name)
}

#' Sorted unique values from any loaded table, for populating dropdowns
#'
fw_choices = function(data, table, column) {
  values = data[[table]][[column]]
  if (is.null(values)) {
    stop("No column '", column, "' in table '", table, "'", call. = FALSE)
  }
  values = values[!is.na(values) & values != ""]
  sort(unique(values))
}

#' The date shown in the footer
#'
#' The release date from metadata.json; falls back to the latest last_updated.
fw_last_updated = function(data, meta = NULL) {
  release = meta$release %||% NA
  if (!is.null(release) && !all(is.na(release)) && nzchar(release[1])) {
    d = suppressWarnings(as.Date(release[1]))
    if (!is.na(d)) return(d)
  }
  d = suppressWarnings(max(data$attempt$last_updated, na.rm = TRUE))
  if (is.infinite(d) || is.na(d)) NA else d
}

#' Headline counts for the whole database
#'
#' For the Explore panel and the Welcome figure. Counts only, never a rate.
fw_headline_stats = function(data) {
  role_ids = function(role_name) {
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

# ---- Dropdown choices, built once at startup ---------------------------------

# Appended to dropdowns; selecting it reveals a free-text box.
FW_OTHER = "Other (specify)"

# Key for a choice list's unfiltered version (R's [[ ]] can't use "").
FW_ALL = ".all"

# ---- ISO 3166 ----------------------------------------------------------------

#' The ISO country and subdivision lookups
#'
#' From the data repo. If missing, the form falls back to recorded countries
#' and a free-text region.
fw_load_iso = function() {
  empty = list(countries = NULL, subdivisions = list())

  countries = try(fw_read_lookup("lookup_iso3166.csv"), silent = TRUE)
  if (inherits(countries, "try-error") || is.null(countries)) return(empty)

  subs = try(fw_read_lookup("lookup_iso3166_2.csv"), silent = TRUE)
  if (inherits(subs, "try-error") || is.null(subs)) {
    return(list(countries = countries, subdivisions = list()))
  }

  named = subs |>
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
fw_read_lookup = function(name) {
  path = fw_data_path(name)
  if (is.null(path)) return(NULL)
  suppressWarnings(readr::read_csv(path, show_col_types = FALSE,
                                   progress = FALSE))
}

#' Countries for the contribute form: recorded ones first, then the rest
fw_country_choices = function(data, iso_countries) {
  recorded = sort(unique(data$attempt$country))
  if (is.null(iso_countries)) return(c(recorded, FW_OTHER))

  rest = setdiff(sort(iso_countries$country), recorded)
  c(recorded, rest, FW_OTHER)
}

#' The subdivisions of one country, for the region picker
fw_subdivisions_for = function(choices, country) {
  if (is.null(country) || !nzchar(country)) return(character(0))
  subs = choices$subdivision[[country]]
  if (is.null(subs)) character(0) else subs
}

#' Build every dropdown's options once, at startup
#'
#' From the data, unioned with the form specification's values so every
#' specified answer is pickable.
fw_startup_choices = function(data) {
  species_labelled = fw_species_label(data$species)

  # Most-recorded species first, then alphabetical.
  species_freq = data$attempt_species |>
    count(species_id, name = "n")

  species_choices = species_labelled |>
    left_join(species_freq, by = "species_id") |>
    mutate(n = coalesce(n, 0L)) |>
    arrange(desc(n), label) |>
    pull(label)

  union_sorted = function(from_data, from_spec) {
    sort(union(from_data, from_spec))
  }

  iso = fw_load_iso()

  list(
    # Full ISO 3166-1 list, recorded countries first, then "Other (specify)".
    country = fw_country_choices(data, iso$countries),

    # Keyed by country, for the region picker. See fw_subdivisions_for().
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

    # Fixed: the paper's four outcome categories.
    outcome = c("Successful", "Failed", "Ongoing", "Unknown"),

    # Waterbody types by regime, for the form. See fw_waterbody_by_regime().
    waterbody_by_regime = fw_waterbody_by_regime(data),

    # Species by taxa, so picking "Fish" narrows the picker. FW_ALL is all.
    species_by_taxa = fw_species_by_taxa(species_labelled, species_choices),

    # label -> species_id for picked species. Typed-in species have no entry
    # and travel as text (see submit.R).
    species_ids = stats::setNames(species_labelled$species_id, species_labelled$label)
  )
}

# Which waterbody types flow. Used by fw_waterbody_by_regime() and
# dev/reconcile_source.R. Unlisted types keep the source's regime. "Multiple"
# has no regime.
FW_REGIME_BY_TYPE = c(
  Canal = "Lotic", Channel = "Lotic", Creek = "Lotic", River = "Lotic",
  Stream = "Lotic", Tributary = "Lotic",
  # Springs and drains move water, though the source marked them Lentic.
  Spring = "Lotic", Drain = "Lotic",
  Multiple = NA_character_
)

FW_LOTIC_ALWAYS = names(FW_REGIME_BY_TYPE)[
  !is.na(FW_REGIME_BY_TYPE) & FW_REGIME_BY_TYPE == "Lotic"
]

# ---- Regime wording ----------------------------------------------------------

# Display labels only. Lentic/Lotic stay the stored values.
FW_REGIME_LABELS = c(Lentic = "Still water", Lotic = "Flowing water")

#' The plain-English wording for a stored regime value
#'
#' Unknown values pass through unchanged.
fw_regime_label = function(x) {
  out = unname(FW_REGIME_LABELS[x])
  ifelse(is.na(out), x, out)
}

#' Stored regime values, named by what the user should see
#'
#' Pickers show the names and submit the values.
fw_regime_choices = function(values) {
  stats::setNames(values, fw_regime_label(values))
}

#' Waterbody types that belong to each water regime

fw_waterbody_by_regime = function(data) {
  seen = data$attempt |>
    filter(!is.na(waterbody_type), !is.na(water_regime)) |>
    distinct(waterbody_type, water_regime)

  regimes = sort(unique(seen$water_regime))
  out = lapply(regimes, function(rg) {
    types = sort(unique(seen$waterbody_type[seen$water_regime == rg]))
    if (identical(rg, "Lotic")) {
      types = sort(union(types, intersect(FW_LOTIC_ALWAYS, seen$waterbody_type)))
    } else {
      types = setdiff(types, FW_LOTIC_ALWAYS)
    }
    c(types, FW_OTHER)
  })
  names(out) = regimes

  # The unfiltered list, before regime is answered.
  out[[FW_ALL]] = c(sort(unique(seen$waterbody_type)), FW_OTHER)
  out
}

#' Species labels grouped by the taxa they belong to
#'
#' @param species_labelled the species table with a `label` column
#' @param all_labels every label, already ordered by how often it is recorded
fw_species_by_taxa = function(species_labelled, all_labels) {
  by_taxa = split(species_labelled$label,
                   species_labelled$taxa %||% NA_character_)
  by_taxa = by_taxa[!is.na(names(by_taxa)) & nzchar(names(by_taxa))]
  # Keep each group in the same frequency order as the full list.
  by_taxa = lapply(by_taxa, function(x) all_labels[all_labels %in% x])
  by_taxa[[FW_ALL]] = all_labels
  by_taxa
}
