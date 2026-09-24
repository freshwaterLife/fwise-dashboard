# submit.R
# The submission write path.
#
# There is ONE public function, fw_submit_attempt(record). Where it writes is
# decided by fw_data_mode(): with a token it commits to the data repository over
# the GitHub API, without one it writes a file beside the local checkout. Both
# produce the SAME THING - one CSV per submission, named for its attempt id -
# so QA and dev/qa.R do not care which one made it.
#
# A SUBMISSION IS ONE ROW IN THE SHAPE OF THE DATABASE. The inbox file has
# exactly the columns of attempts.csv (FW_ATTEMPT_COLUMNS), in the same order,
# so the reviewer compares like with like and dev/qa.R fold is an append. The
# two things a submission cannot know are ids for species and contacts the
# database does not hold yet; those travel in the same cell with a `new:`
# prefix and the reviewer resolves them - see fw_new_ref().
#
# ONE FILE PER SUBMISSION, not an append to a shared inbox. The reason is the
# GitHub API: it has no append. Adding a row to a shared file means reading it,
# decoding it, appending, and PUTting the whole thing back quoting the blob SHA
# it was read at - and two contributors pressing Send in the same moment make the
# second one 409 and need retry logic. A file per submission has no
# read-modify-write at all, and the attempt id already makes it idempotent.

# The column order of a submission IS the column order of the database.
FW_SUBMISSION_COLUMNS <- FW_ATTEMPT_COLUMNS

# ---- Identifiers -------------------------------------------------------------

# Ambiguous characters are left out - no I, O, 0 or 1 - because these ids get
# read aloud, retyped and pasted into spreadsheets by people.
FW_ID_ALPHABET <- c(as.character(2:9), setdiff(LETTERS, c("I", "O")))

#' Mint n ids that collide with nothing already in use
#'
#' Format: PREFIX-YYYYMMDD-XXXXXX, e.g. FW-20260908-7K3QX9. The date is when the
#' id was first assigned, so ids sort roughly by when a record entered FWISE.
#' An id, once assigned, is NEVER reassigned: it lives in the row (attempts) or
#' the lookup (species, contacts) that owns it from the moment it is minted.
#'
#' @param prefix "FW", "SP" or "CO"
#' @param exclude ids already in use, so a collision is impossible rather than
#'   merely unlikely (32^6 is about a billion, so the loop effectively never
#'   runs twice).
fw_mint_ids <- function(prefix, n, exclude = character(0), date = Sys.Date()) {
  if (n == 0) return(character(0))
  stamp <- format(date, "%Y%m%d")
  out <- character(0)
  while (length(out) < n) {
    cand <- replicate(n - length(out), paste0(
      prefix, "-", stamp, "-",
      paste(sample(FW_ID_ALPHABET, 6, replace = TRUE), collapse = "")
    ))
    out <- setdiff(unique(c(out, cand)), exclude)
  }
  out[seq_len(n)]
}

# ---- Unresolved references ---------------------------------------------------

# A species or contact the database does not hold yet travels in the id cell as
# `new:` followed by what the reviewer needs to create the row, with FW_NEW_SEP
# between the parts. Neither character occurs in any species name, contact
# name, organisation or email in the data.
FW_NEW_PREFIX <- "new:"
FW_NEW_SEP    <- "|"

#' Mark an unresolved reference
fw_new_ref <- function(...) {
  parts <- vapply(list(...), function(v) {
    v <- as.character(v %||% "")
    if (!length(v) || is.na(v)) "" else gsub(FW_NEW_SEP, "/", v, fixed = TRUE)
  }, character(1))
  paste0(FW_NEW_PREFIX, paste(parts, collapse = FW_NEW_SEP))
}

fw_is_new_ref <- function(x) !is.na(x) & startsWith(x, FW_NEW_PREFIX)

#' The parts of an unresolved reference, as a character vector
fw_parse_new_ref <- function(x, n) {
  body <- substring(x, nchar(FW_NEW_PREFIX) + 1)
  parts <- strsplit(body, FW_NEW_SEP, fixed = TRUE)[[1]]
  length(parts) <- n
  parts[is.na(parts)] <- ""
  trimws(parts)
}

# ---- Writing a submission ----------------------------------------------------

#' Turn the assembled record into one flat row in column order
#'
#' Every column of FW_SUBMISSION_COLUMNS is present, blank where the form has
#' nothing for it, so the inbox file always has the database's shape.
fw_flatten_record <- function(record) {
  row <- lapply(FW_SUBMISSION_COLUMNS, function(col) {
    v <- record[[col]]
    if (is.null(v) || length(v) == 0) return("")
    if (length(v) > 1) return(paste(v, collapse = FW_MULTI_SEP))
    if (is.na(v)) return("")
    as.character(v)
  })
  names(row) <- FW_SUBMISSION_COLUMNS
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Write one submission
#'
#' @param record a named list of the form answers, from fw_collect_submission()
#' @param data   the loaded tables, for the confirmation counts
#' @return list(success, message, attempt_id, submission_id, total_attempts,
#'              country_attempts, country)
fw_submit_attempt <- function(record, data = NULL) {

  # THE ATTEMPT ID IS MINTED HERE and travels with the row from the inbox into
  # attempts.csv unchanged, so a reviewer, a contributor's confirmation screen
  # and the published data all name the same record. Collisions are excluded
  # against the ids the app is holding; the fold step checks again against
  # the file.
  in_use <- if (is.null(data)) character(0) else data$attempt$attempt_id
  record$attempt_id   <- fw_mint_ids("FW", 1, exclude = in_use)
  record$status       <- "pending"
  record$submitted_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  record$last_updated <- format(Sys.Date())

  flat <- fw_flatten_record(record)

  # NO FALLBACK TO THE LOCAL FILE IF THE GITHUB WRITE FAILS. On Connect Cloud
  # the container filesystem is thrown away on restart, so a fallback would
  # report success for a submission that is already gone. Telling the
  # contributor to try again is worth more than a receipt for nothing.
  written <- tryCatch({
    where <- fw_write_submission(flat)
    message("Submission ", record$attempt_id, " written to ", where)
    TRUE
  }, error = function(e) {
    warning("Submission write failed: ", conditionMessage(e))
    FALSE
  })

  if (!written) {
    return(list(
      success = FALSE,
      message = fw_t("contribute", "error", "write_failed"),
      attempt_id = record$attempt_id,
      submission_id = record$attempt_id
    ))
  }

  counts <- fw_submission_counts(data, record$country)

  list(
    success = TRUE,
    message = "",
    attempt_id = record$attempt_id,
    # The confirmation screen calls it a reference; same string.
    submission_id = record$attempt_id,
    total_attempts   = counts$total,
    country_attempts = counts$country,
    country = record$country
  )
}

#' Live counts for the confirmation screen
#'
#' The published database plus everything pending, so a contributor who submits
#' twice in a row sees the number move.
fw_submission_counts <- function(data, country) {
  total <- if (is.null(data)) 0L else nrow(data$attempt)
  in_country <- if (is.null(data) || is.null(country)) {
    0L
  } else {
    sum(data$attempt$country == country, na.rm = TRUE)
  }

  pending <- fw_pending_submissions()
  pending_total <- nrow(pending)
  pending_country <- if (pending_total > 0 && !is.null(country) &&
                         "country" %in% names(pending)) {
    sum(pending$country == country, na.rm = TRUE)
  } else {
    0L
  }

  list(
    total   = total + pending_total,
    country = in_country + pending_country
  )
}

#' Everything sitting in the inbox that nobody has processed
#'
#' Feeds the in-review count and the confirmation screen's totals.
#'
#' THE TWO MODES RETURN DIFFERENT SHAPES, deliberately. Locally the files are on
#' disk and reading all of them is free, so full rows come back. Over the API,
#' reading each file would be one request per submission just to render a
#' counter, so only the id and the status come back - the name of the file is
#' enough to count it. fw_submission_counts() already guards on whether a
#' country column is present, so the per-country figure simply falls to zero in
#' production rather than costing a request per row.
#'
#' Folded submissions are not counted either way, because folding MOVES them to
#' inbox/merged/ and neither listing recurses.
fw_pending_submissions <- function() {
  if (fw_data_mode() == "api") {
    files <- tryCatch(fw_gh_list("inbox"), error = function(e) character(0))
    files <- files[grepl("\\.csv$", files)]
    if (length(files) == 0) return(data.frame())
    return(data.frame(
      attempt_id = sub("\\.csv$", "", files),
      status = "pending",
      stringsAsFactors = FALSE
    ))
  }

  read_one <- function(f) {
    tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character"),
             error = function(e) NULL)
  }

  dir   <- fw_inbox_dir()
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.csv$", full.names = TRUE)
           else character(0)
  rows  <- Filter(Negate(is.null), lapply(files, read_one))
  if (length(rows) == 0) return(data.frame())
  as.data.frame(dplyr::bind_rows(rows))
}

#' Where submissions land when there is no token
#'
#' Beside the data checkout rather than in dev/, so the local loop is the same
#' shape as production: one directory of one-file-per-submission that dev/qa.R
#' reads. Falls back to dev/ only when the sibling checkout is not there at
#' all, which means someone has cloned the app on its own.
fw_inbox_dir <- function() {
  if (dir.exists(FW_DATA_DIR)) file.path(FW_DATA_DIR, "inbox")
  else file.path(FW_DEV_DIR, "inbox")
}

# ---- The write path ----------------------------------------------------------

#' Write one submission, wherever this deployment writes
#'
#' @return a human-readable description of where it went, for the log.
fw_write_submission <- function(flat) {
  name <- paste0(flat$attempt_id[1], ".csv")

  if (fw_data_mode() == "api") {
    fw_gh_put(
      paste0("inbox/", name),
      readr::format_csv(flat),
      paste0("Submission ", flat$attempt_id[1])
    )
    return(paste0(FW_DATA_REPO, "/inbox/", name))
  }

  fw_write_local(flat, name)
}

#' Write one submission to the local inbox, creating the directory
#'
#' The no-credential path. Works on a clean machine, which is what makes the app
#' runnable straight after renv::restore().
fw_write_local <- function(flat, name = paste0(flat$attempt_id[1], ".csv")) {
  dir <- fw_inbox_dir()
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, name)
  readr::write_csv(flat, path, na = "")
  path
}

# ---- Assembling a record from the form ---------------------------------------

#' Collect the contribute form's inputs into one record
#'
#' Lives here rather than in the module so the column set in
#' FW_ATTEMPT_COLUMNS and the code that fills it sit side by side. Adding a
#' field to the form means adding it to the column contract in data_load.R and
#' filling it here.
#'
#' "OTHER (SPECIFY)" GOES INTO THE MAIN COLUMN. The typed value replaces the
#' placeholder, so the reviewer sees an off-list country, waterbody, driver,
#' method or agent in the column it belongs to and dev/qa.R stage flags it.
#'
#' @param input   the module's input object
#' @param rows    the reactiveValues holding which repeatable rows are on screen
#' @param choices the startup choices, for the species id lookup
fw_collect_submission <- function(input, rows, choices = NULL) {

  get_in <- function(nm) {
    v <- input[[nm]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) return("")
    as.character(v)
  }
  # A picker's value, or the typed value when "Other (specify)" was chosen.
  pick <- function(nm, other_nm) {
    v <- get_in(nm)
    if (identical(v, FW_OTHER)) get_in(other_nm) else v
  }

  species_ids <- choices$species_ids %||% character(0)

  # ONE ENTRY PER ROW, resolved to the id the database holds for that species
  # or left as text for the reviewer. IDENTICAL FOR BOTH ROLES: invasive
  # targets and beneficiaries are the same question asked about two sides of
  # the same eradication.
  collect_species <- function(kind, indices) {
    out <- character(0)
    for (i in indices) {
      species <- get_in(paste0(kind, "_species_", i))
      if (!nzchar(species)) next
      taxa <- pick(paste0(kind, "_taxa_", i), paste0(kind, "_taxa_other_", i))
      id <- unname(species_ids[species])
      out <- c(out, if (!is.na(id) && nzchar(id)) id else fw_new_ref(species, taxa))
    }
    paste(out, collapse = FW_MULTI_SEP)
  }

  # Methods travel by NAME; the notes cell pairs each note with its method in
  # the same order, so the map popup can still say which note belongs where.
  method_names <- character(0); method_notes <- character(0)
  for (i in rows$method) {
    nm <- pick(paste0("method_", i), paste0("method_other_", i))
    if (!nzchar(nm)) next
    method_names <- c(method_names, nm)
    method_notes <- c(method_notes, paste0(nm, ": ", get_in(paste0("method_notes_", i))))
  }
  any_notes <- any(nzchar(trimws(sub("^[^:]*:", "", method_notes))))

  # The contributor is always a `new:` reference: the form has no contact
  # picker, and dev/qa.R stage matches the name and organisation against
  # contacts.csv so a known person needs no decision from the reviewer.
  # OPT-IN (Sept 2026): the address is shown only when the contributor ticked
  # "I give permission for my email to be displayed in the app".
  contact_public <- if (isTRUE(input$contact_public)) "public" else "private"
  contact_ref <- function(prefix) {
    name <- get_in(paste0(prefix, "_contact_name"))
    if (!nzchar(name)) return("")
    fw_new_ref(name, get_in(paste0(prefix, "_contact_org")),
               get_in(paste0(prefix, "_contact_email")), contact_public)
  }

  list(
    consent_data_use = if (isTRUE(input$consent_data_use)) "yes" else "no",

    site_name = get_in("site_name"),
    country   = pick("country", "country_other"),
    region    = get_in("region"),
    # Filled by dev/qa.R fold from the ISO lookup once the country is settled.
    iso3      = "",
    continent = "",
    latitude  = get_in("latitude"),
    longitude = get_in("longitude"),

    waterbody_type = pick("waterbody_type", "waterbody_type_other"),
    water_regime   = get_in("water_regime"),
    area_treated   = get_in("area_treated"),
    area_unit      = get_in("area_unit"),
    area_notes     = get_in("area_notes"),
    depth_m        = get_in("depth_m"),
    depth_notes    = get_in("depth_notes"),
    volume_m3      = get_in("volume_m3"),
    volume_notes   = get_in("volume_notes"),
    max_flow_m3s   = get_in("max_flow_m3s"),
    water_temp_c   = get_in("water_temp_c"),
    water_temp_notes = get_in("water_temp_notes"),

    invasive_species = collect_species("target", rows$target),

    invasion_year = get_in("invasion_year"),
    start_year    = get_in("start_year"),
    end_year      = get_in("end_year"),
    duration_days = get_in("duration_days"),
    # The form collects eradication attempts; control programmes are out of
    # scope, so this is a constant rather than a question.
    eradication_or_control = "Eradication",
    driver = pick("driver", "driver_other"),

    beneficiary_species = collect_species("benefit", rows$benefit),

    outcome = get_in("outcome"),

    methods      = paste(method_names, collapse = FW_MULTI_SEP),
    method_notes = if (any_notes) paste(method_notes, collapse = FW_NOTES_SEP) else "",
    method_description = get_in("method_description"),
    labour_person_days = get_in("labour_person_days"),
    cost_estimate = get_in("cost_estimate"),
    cost_notes    = get_in("cost_notes"),

    target_ingredient_basis  = get_in("target_ingredient_basis"),
    toxin_conc_target_mg_l   = get_in("toxin_conc_target"),
    conc_target_notes        = get_in("conc_target_notes"),
    toxin_conc_measured_mg_l = get_in("toxin_conc_measured"),
    conc_measured_notes      = get_in("conc_measured_notes"),
    neutralising_agent = pick("neutralising_agent", "neutralising_agent_other"),
    neutralising_notes = get_in("neutralising_notes"),

    # The specification keeps verification as one box on the form and splits
    # it downstream if needed, so it lands in the method column.
    verification_method = get_in("verification"),
    verification_notes  = "",

    primary_contact_id   = contact_ref("primary"),
    # The form no longer asks for a second contact.
    secondary_contact_id = NA_character_,

    source = get_in("source"),
    sent   = "",
    reference      = get_in("reference"),
    reference_link = "",
    notes_for_fwise = get_in("notes_for_fwise")
  )
}
