# submit.R
# The submission write path.
#
# There is ONE public function, fw_submit_attempt(record). Where it writes is
# decided by fw_data_mode(): with a token it commits to the data repository over
# the GitHub API, without one it writes a file beside the local checkout. Both
# produce the SAME THING - one CSV per submission, named for its submission id -
# so QA and dev/merge_submissions.R do not care which one made it.
#
# ONE FILE PER SUBMISSION, not an append to a shared inbox. The reason is the
# GitHub API: it has no append. Adding a row to a shared file means reading it,
# decoding it, appending, and PUTting the whole thing back quoting the blob SHA
# it was read at - and two contributors pressing Send in the same moment make the
# second one 409 and need retry logic. A file per submission has no
# read-modify-write at all, and submission_id already makes it idempotent.
# The column order of the submissions inbox. Grouped the way a reviewer reads
# them rather than the way the app collects them. Adding a field means adding it
# here AND in fw_flatten_record().
FW_SUBMISSION_COLUMNS <- c(
  # Auto-populated
  "submission_id", "status", "submitted_at",
  # Consent
  "consent_data_use", "email_public",
  # Site
  "site_name", "country", "country_other", "region", "latitude", "longitude",
  # Waterbody
  "waterbody_type", "waterbody_type_other", "water_regime",
  "area_treated", "area_unit", "area_notes",
  "depth_m", "depth_notes", "volume_m3", "volume_notes",
  "max_flow_m3s", "water_temp_c", "water_temp_notes",
  # Invasive targets. invasive_species carries one "group ~ species ~ family"
  # entry per target; the two roll-up columns beside it are there so a reviewer
  # can filter the inbox without unpacking that cell.
  "invasive_taxa", "invasive_taxa_other", "invasive_species", "invasive_family",
  # Timeline
  "invasion_year", "start_year", "end_year", "duration_days",
  "driver", "driver_other",
  # Beneficiaries
  "beneficiary_taxa", "beneficiary_taxa_other", "beneficiary_species",
  # Methods
  "methods", "method_description", "labour_person_days",
  "cost_estimate", "cost_notes",
  # Chemical detail
  "toxin_conc_target", "conc_target_notes", "conc_measured_notes",
  "neutralising_agent", "neutralising_agent_other", "neutralising_notes",
  # Outcome
  "outcome", "verification", "reference", "source",
  # Contributor
  "primary_contact_name", "primary_contact_email", "primary_contact_org",
  "secondary_contact_name", "secondary_contact_email", "secondary_contact_org",
  # Other
  "notes_for_fwise"
)

#' Serialise a repeatable block into one human-readable cell
#'
#' Pipe-delimited rather than JSON: the reviewer reads these in a spreadsheet
#' cell during QA, and JSON in a cell is unreadable at a glance.
fw_serialise_rows <- function(rows, fields) {
  if (length(rows) == 0) return("")
  parts <- vapply(rows, function(r) {
    vals <- vapply(fields, function(f) {
      v <- r[[f]]
      if (is.null(v) || length(v) == 0 || is.na(v) || !nzchar(as.character(v))) {
        ""
      } else {
        # A literal pipe in free text would corrupt the delimiter.
        gsub("|", "/", as.character(v), fixed = TRUE)
      }
    }, character(1))
    paste(vals[nzchar(vals)], collapse = " ~ ")
  }, character(1))
  paste(parts[nzchar(parts)], collapse = " | ")
}

#' A submission identifier
#'
#' Time-ordered so a reviewer sorting by id gets chronological order, with a
#' short random tail so two submissions in the same second cannot collide.
fw_new_submission_id <- function() {
  paste0(
    "SUB-",
    format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"), "-",
    paste(sample(c(LETTERS, 0:9), 4, replace = TRUE), collapse = "")
  )
}

#' Turn the assembled record into one flat row in column order
fw_flatten_record <- function(record) {
  row <- lapply(FW_SUBMISSION_COLUMNS, function(col) {
    v <- record[[col]]
    if (is.null(v) || length(v) == 0) return("")
    if (length(v) > 1) return(paste(v, collapse = "; "))
    if (is.na(v)) return("")
    as.character(v)
  })
  names(row) <- FW_SUBMISSION_COLUMNS
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Write one submission
#'
#' @param record a named list of the form answers
#' @param data   the loaded star-schema tables, for the confirmation counts
#' @return list(success, message, submission_id, total_attempts,
#'              country_attempts, country)
fw_submit_attempt <- function(record, data = NULL) {

  record$submission_id <- fw_new_submission_id()
  # Auto-populated so the QA and publishing pipeline can work without
  # duplicates. QA moves this on to "ready" and then "in database".
  record$status <- "pending"
  record$submitted_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  flat <- fw_flatten_record(record)

  # NO FALLBACK TO THE LOCAL FILE IF THE GITHUB WRITE FAILS. On Connect Cloud
  # the container filesystem is thrown away on restart, so a fallback would
  # report success for a submission that is already gone. Telling the
  # contributor to try again is worth more than a receipt for nothing.
  written <- tryCatch({
    where <- fw_write_submission(flat)
    message("Submission ", record$submission_id, " written to ", where)
    TRUE
  }, error = function(e) {
    warning("Submission write failed: ", conditionMessage(e))
    FALSE
  })

  if (!written) {
    return(list(
      success = FALSE,
      message = fw_t("contribute", "error", "write_failed"),
      submission_id = record$submission_id
    ))
  }

  counts <- fw_submission_counts(data, record$country)

  list(
    success = TRUE,
    message = "",
    submission_id = record$submission_id,
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
#' Merged submissions are not counted either way, because merging MOVES them to
#' inbox/merged/ and neither listing recurses.
fw_pending_submissions <- function() {
  if (fw_data_mode() == "api") {
    files <- tryCatch(fw_gh_list("inbox"), error = function(e) character(0))
    files <- files[grepl("\\.csv$", files)]
    if (length(files) == 0) return(data.frame())
    return(data.frame(
      submission_id = sub("\\.csv$", "", files),
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
  rows  <- lapply(files, read_one)

  # The pre-inbox format, kept readable so submissions made before the GitHub
  # write path landed are not stranded. dev/merge_submissions.R reads it too.
  legacy <- fw_legacy_inbox_path()
  if (file.exists(legacy)) rows <- c(rows, list(read_one(legacy)))

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  as.data.frame(dplyr::bind_rows(rows))
}

#' Where submissions land when there is no token
#'
#' Beside the data checkout rather than in dev/, so the local loop is the same
#' shape as production: one directory of one-file-per-submission that
#' merge_submissions.R reads. Falls back to dev/ only when the sibling checkout
#' is not there at all, which means someone has cloned the app on its own.
fw_inbox_dir <- function() {
  if (dir.exists(FW_DATA_DIR)) file.path(FW_DATA_DIR, "inbox")
  else file.path(FW_DEV_DIR, "inbox")
}

fw_legacy_inbox_path <- function() {
  file.path(FW_DEV_DIR, "submissions_local.csv")
}

# ---- The write path ----------------------------------------------------------

#' Write one submission, wherever this deployment writes
#'
#' @return a human-readable description of where it went, for the log.
fw_write_submission <- function(flat) {
  name <- paste0(flat$submission_id[1], ".csv")

  if (fw_data_mode() == "api") {
    fw_gh_put(
      paste0("inbox/", name),
      readr::format_csv(flat),
      paste0("Submission ", flat$submission_id[1])
    )
    return(paste0(FW_DATA_REPO, "/inbox/", name))
  }

  fw_write_local(flat, name)
}

#' Write one submission to the local inbox, creating the directory
#'
#' The no-credential path. Works on a clean machine, which is what makes the app
#' runnable straight after renv::restore().
fw_write_local <- function(flat, name = paste0(flat$submission_id[1], ".csv")) {
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
#' FW_SUBMISSION_COLUMNS and the code that fills it sit side by side. Adding a
#' field means touching both, and both are in this file.
#'
#' @param input the module's input object
#' @param rows  the reactiveValues holding which repeatable rows are on screen
fw_collect_submission <- function(input, rows, family_lookup = NULL) {

      get_in <- function(nm) {
        v <- input[[nm]]
        if (is.null(v) || (length(v) == 1 && is.na(v))) return("")
        v
      }

      # One entry per row: the group, the species, and the family we looked up
      # for that species. The family is NOT asked for. Contributors were being
      # made to answer a taxonomy question to which the database already holds
      # the answer for every species it knows, and "Unknown" is a better record
      # of the rest than a guess.
      #
      # IDENTICAL FOR BOTH ROLES. Invasive targets and beneficiaries are the
      # same question asked about two sides of the same eradication, so they are
      # collected, serialised and rolled up the same way. Beneficiaries used to
      # carry a bare species name with the group recorded separately for the
      # whole submission, which lost the pairing between them.
      collect_pairs <- function(kind, indices) {
        out <- lapply(indices, function(i) {
          taxa <- get_in(paste0(kind, "_taxa_", i))
          if (identical(taxa, FW_OTHER)) {
            taxa <- get_in(paste0(kind, "_taxa_other_", i))
          }
          species <- get_in(paste0(kind, "_species_", i))
          list(
            taxa = taxa,
            species = species,
            family = if (nzchar(species)) {
              fw_family_for_species(species, family_lookup)
            } else ""
          )
        })
        Filter(function(t) nzchar(t$taxa) || nzchar(t$species), out)
      }

      targets       <- collect_pairs("target",  rows$target)
      beneficiaries <- collect_pairs("benefit", rows$benefit)

      # The roll-up columns beside the serialised cell, so a reviewer can filter
      # the inbox without unpacking it.
      uniq_field <- function(pairs, field) {
        v <- unique(vapply(pairs, function(t) t[[field]], character(1)))
        paste(v[nzchar(v)], collapse = "; ")
      }

      invasive_species <- fw_serialise_rows(targets, c("taxa", "species", "family"))
      benefit_species  <- fw_serialise_rows(beneficiaries, c("taxa", "species", "family"))
      methods <- fw_serialise_rows(
        lapply(rows$method, function(i) {
          nm <- get_in(paste0("method_", i))
          if (identical(nm, FW_OTHER)) nm <- get_in(paste0("method_other_", i))
          list(name = nm, notes = get_in(paste0("method_notes_", i)))
        }),
        c("name", "notes")
      )

      list(
        consent_data_use = if (isTRUE(input$consent_data_use)) "yes" else "no",
        # The specification's control is "keep my email private", so the stored
        # column is its inverse. Both consent answers are persisted as columns on
        # the submitted row.
        email_public = if (isTRUE(input$email_private)) "no" else "yes",

        site_name = get_in("site_name"),
        # country_other travels BESIDE country rather than replacing it, the
        # same as waterbody_type and driver. Resolving "Other (specify)" is a
        # review decision - the reviewer has to decide whether the typed answer
        # is a country, a territory or a mistake - and doing it here would hide
        # that the contributor went off-list.
        country   = get_in("country"),
        country_other = get_in("country_other"),
        region    = get_in("region"),
        latitude  = get_in("latitude"),
        longitude = get_in("longitude"),

        waterbody_type = get_in("waterbody_type"),
        waterbody_type_other = get_in("waterbody_type_other"),
        water_regime = get_in("water_regime"),
        area_treated = get_in("area_treated"),
        area_unit = get_in("area_unit"),
        area_notes = get_in("area_notes"),
        depth_m = get_in("depth_m"),
        depth_notes = get_in("depth_notes"),
        volume_m3 = get_in("volume_m3"),
        volume_notes = get_in("volume_notes"),
        max_flow_m3s = get_in("max_flow_m3s"),
        water_temp_c = get_in("water_temp_c"),
        water_temp_notes = get_in("water_temp_notes"),

        invasive_taxa = uniq_field(targets, "taxa"),
        # Kept as a column because the inbox layout is stable, but the free-text
        # group now travels inside invasive_taxa itself: it belongs to one
        # target, not to the submission.
        invasive_taxa_other = "",
        invasive_species = invasive_species,
        invasive_family = uniq_field(targets, "family"),

        invasion_year = get_in("invasion_year"),
        start_year = get_in("start_year"),
        end_year = get_in("end_year"),
        duration_days = get_in("duration_days"),
        driver = get_in("driver"),
        driver_other = get_in("driver_other"),

        # Derived from the beneficiary rows, exactly as invasive_taxa is derived
        # from the target rows. It is no longer a question of its own.
        beneficiary_taxa = uniq_field(beneficiaries, "taxa"),
        beneficiary_taxa_other = "",
        beneficiary_species = benefit_species,

        methods = methods,
        method_description = get_in("method_description"),
        labour_person_days = get_in("labour_person_days"),
        cost_estimate = get_in("cost_estimate"),
        cost_notes = get_in("cost_notes"),

        toxin_conc_target = get_in("toxin_conc_target"),
        conc_target_notes = get_in("conc_target_notes"),
        conc_measured_notes = get_in("conc_measured_notes"),
        neutralising_agent = get_in("neutralising_agent"),
        neutralising_agent_other = get_in("neutralising_agent_other"),
        neutralising_notes = get_in("neutralising_notes"),

        outcome = get_in("outcome"),
        verification = get_in("verification"),
        reference = get_in("reference"),
        source = get_in("source"),

        primary_contact_name = get_in("primary_contact_name"),
        primary_contact_email = get_in("primary_contact_email"),
        primary_contact_org = get_in("primary_contact_org"),
        secondary_contact_name = get_in("secondary_contact_name"),
        secondary_contact_email = get_in("secondary_contact_email"),
        secondary_contact_org = get_in("secondary_contact_org"),

        notes_for_fwise = get_in("notes_for_fwise")
      )
    }
