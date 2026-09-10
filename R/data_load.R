# data_load.R
# The data contract. Every module reads through the functions in this file and
# no module reads a CSV directly. When the real schema changes, this is the only
# file that should need to change.
#
# The six tables are loaded once at app startup, not once per session, because
# they are read-only and identical for every visitor.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

FW_TABLES <- c("attempt", "species", "attempt_species",
               "method", "attempt_method", "contact")

# Column types are declared rather than guessed. readr guessing from the first
# 1000 rows has bitten this kind of file before: a mostly-empty numeric column
# reads as logical and every downstream join breaks in a confusing way.
fw_col_types <- list(
  attempt = cols(
    .default = col_character(),
    latitude = col_double(), longitude = col_double(),
    area_treated = col_double(), depth_m = col_double(),
    volume_m3 = col_double(), max_flow_m3s = col_double(),
    water_temp_c = col_double(), invasion_year = col_double(),
    start_year = col_double(), end_year = col_double(),
    duration_days = col_double(), toxin_conc_mg_l = col_double(),
    labour_person_days = col_double(), cost_estimate = col_double(),
    last_updated = col_date(format = "")
  ),
  species         = cols(.default = col_character()),
  attempt_species = cols(.default = col_character()),
  method          = cols(.default = col_character()),
  attempt_method  = cols(.default = col_character(), method_order = col_integer()),
  contact         = cols(.default = col_character(), email_public = col_logical())
)

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

fw_source_is_remote <- function() fw_data_mode() != "local"

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
#' @param ... path segments, e.g. "schema", "attempt.csv"
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

#' Load the six star-schema tables
#'
#' READ ONCE, AT STARTUP. Not per session and not on a poll. The data changes
#' quarterly and visibility comes from a deliberate republish, so re-reading it
#' would spend a request per visitor to discover that nothing had changed.
#'
#' @return A named list of six tibbles.
fw_load_data <- function() {
  tables <- set_names(FW_TABLES) |>
    map(function(nm) {
      rel  <- paste0("schema/", nm, ".csv")
      path <- fw_data_path("schema", paste0(nm, ".csv"))
      if (is.null(path)) stop(fw_data_missing(rel), call. = FALSE)
      read_csv(path, col_types = fw_col_types[[nm]], progress = FALSE)
    })

  fw_validate_data(tables)
  fw_filter_approved(tables)
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
    stop("attempt.status holds values that are not ",
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
#' from the moment someone merges it into the schema: rows already carried into
#' the database as `pending`, plus whatever is sitting in the submissions inbox
#' and has not been processed at all yet.
fw_review_count <- function(data) {
  counts <- attr(data, "status_counts")
  in_schema <- if (is.null(counts)) 0L else as.integer(counts[["pending"]])

  # Only submissions NOT yet carried into the schema. A merged submission is
  # already counted by in_schema above, and counting it here as well would
  # double it and leave the indicator permanently inflated.
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
#' from a column inside the data. Written by R/data_prep.R on every build, so the
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

# Fail loudly at startup rather than producing a page with silently missing
# columns. A scientist re-running data_prep.R with a changed export should get a
# clear message here, not an empty table three pages later.
fw_validate_data <- function(tables) {
  required <- list(
    attempt = c("attempt_id", "country", "continent", "latitude", "longitude",
                "outcome", "start_year", "primary_contact_id", "status",
                "last_updated"),
    species = c("species_id", "scientific_name", "common_name", "taxa", "family"),
    attempt_species = c("attempt_id", "species_id", "role"),
    method = c("method_id", "method_name", "method_class"),
    attempt_method = c("attempt_id", "method_id", "method_order"),
    contact = c("contact_id", "contact_name", "contact_email",
                "organisation", "email_public")
  )
  for (nm in names(required)) {
    missing <- setdiff(required[[nm]], names(tables[[nm]]))
    if (length(missing) > 0) {
      stop("Table '", nm, "' is missing required columns: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
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

#' One row per attempt, with species, methods and contacts collapsed
#'
#' For tables and exports. List columns keep the many-to-one relationships intact
#' rather than flattening them into numbered columns again.
fw_attempts_wide <- function(data) {
  species_labelled <- fw_species_label(data$species)

  sp <- data$attempt_species |>
    left_join(select(species_labelled, species_id, label, taxa, family),
              by = "species_id") |>
    group_by(attempt_id, role) |>
    summarise(names = list(label), .groups = "drop") |>
    pivot_wider(names_from = role, values_from = names,
                names_prefix = "species_")

  # Guard against an export with no beneficiary rows at all.
  for (nm in c("species_invasive", "species_beneficiary")) {
    if (!nm %in% names(sp)) sp[[nm]] <- vector("list", nrow(sp))
  }

  me <- data$attempt_method |>
    left_join(select(data$method, method_id, method_name, method_class),
              by = "method_id") |>
    arrange(attempt_id, method_order) |>
    group_by(attempt_id) |>
    summarise(
      methods       = list(method_name),
      method_classes = list(unique(method_class)),
      .groups = "drop"
    )

  contacts <- select(data$contact, contact_id, contact_name, organisation)

  data$attempt |>
    left_join(sp, by = "attempt_id") |>
    left_join(me, by = "attempt_id") |>
    left_join(
      rename(contacts, primary_contact_id = contact_id,
             primary_contact_name = contact_name,
             primary_contact_org = organisation),
      by = "primary_contact_id"
    ) |>
    left_join(
      rename(contacts, secondary_contact_id = contact_id,
             secondary_contact_name = contact_name,
             secondary_contact_org = organisation),
      by = "secondary_contact_id"
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
    # Drop the source country column, which is intentionally empty, so nothing
    # downstream mistakes it for the derived value.
    select(-country) |>
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

#' Headline counts for the landing page KPI strip
#'
#' Computed at runtime from the loaded data, never hardcoded.
fw_headline_stats <- function(data) {
  invasive_ids <- data$attempt_species |>
    filter(role == "invasive") |>
    pull(species_id) |>
    unique()

  list(
    attempts       = nrow(data$attempt),
    countries      = n_distinct(data$attempt$country),
    earliest_year  = suppressWarnings(min(data$attempt$start_year, na.rm = TRUE)),
    species        = length(invasive_ids),
    successful     = sum(data$attempt$outcome == "Successful", na.rm = TRUE)
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
#' These sit at the ROOT of the data source, beside metadata.json, rather than
#' in schema/ - they describe the standard, not this dataset.
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

    # label -> family, so the family can be filled in from the species the
    # contributor picks rather than asked for. See fw_family_for_species().
    species_family = fw_species_family_lookup(species_labelled)
  )
}

# THE ONE PLACE THAT SAYS WHICH WATERBODIES FLOW.
#
# Read by TWO consumers that used to disagree:
#   - data_prep.R, which corrects water_regime as the schema is built
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

#' A species label to family lookup
#'
#' The form no longer asks for the fish family. It is filled in from the species
#' the contributor picks and falls back to "Unknown" for anything we do not hold,
#' including a species they type in themselves, which is a QA task rather than a
#' question worth putting to a contributor.
fw_species_family_lookup <- function(species_labelled) {
  fam <- species_labelled$family
  names(fam) <- species_labelled$label
  fam[!is.na(fam) & nzchar(fam)]
}

#' The family for a species label, or "Unknown"
#'
#' Single-bracket lookup on purpose: [[ ]] throws on a name that is not there,
#' and a species the contributor typed in themselves is exactly that case.
fw_family_for_species <- function(label, lookup) {
  if (is.null(label) || length(label) != 1 || is.na(label) || !nzchar(label)) {
    return("")
  }
  fam <- unname(lookup[label])
  if (is.na(fam) || !nzchar(fam)) "Unknown" else fam
}
