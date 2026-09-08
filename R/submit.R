# submit.R
# The submission write path.
#
# There is ONE public function, fw_submit_attempt(record), and it writes to
# dev/submissions_local.csv. It works on a clean machine with no credentials.
#
# THE GOOGLE SHEETS BACKEND HAS BEEN REMOVED. Submissions are moving to GitHub,
# so the Sheets path was deleted rather than left sitting here as untested code
# with credential handling in its documentation. It had never been run against a
# real Sheet.
#
# The GitHub write path is NOT built. It needs a token that does not exist yet
# and is a separate job. Until it lands, the local file is the whole story.
#
# THE INBOX IS A RAW SUBMISSIONS LIST, NOT THE SCHEMA. One flat row per
# submission. The repeatable species, methods and beneficiaries are serialised
# into single pipe-delimited cells because a human reviewer reads them in a
# spreadsheet during QA. Normalisation into the star schema happens manually in
# that review, so the column layout is optimised for the reviewer, not for a
# machine.


# The column order of the submissions inbox. Grouped the way a reviewer reads
# them rather than the way the app collects them. Adding a field means adding it
# here AND in fw_flatten_record().
FW_SUBMISSION_COLUMNS <- c(
  # Auto-populated
  "submission_id", "status", "submitted_at",
  # Consent
  "consent_data_use", "email_public",
  # Site
  "site_name", "country", "region", "latitude", "longitude",
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

  written <- tryCatch({
    fw_write_local(flat)
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

#' Read back the local pending submissions, if any
fw_pending_submissions <- function() {
  path <- fw_local_submission_path()
  if (!file.exists(path)) {
    return(data.frame())
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(e) data.frame()
  )
}

fw_local_submission_path <- function() {
  file.path(FW_DEV_DIR, "submissions_local.csv")
}

# ---- The write path ----------------------------------------------------------

#' Append one row to dev/submissions_local.csv, creating it with headers
#'
#' The only backend. Works with no credentials on a clean machine, which is what
#' makes the app runnable straight after renv::restore(). Replaced by the GitHub
#' write path once a token exists.
fw_write_local <- function(flat) {
  dir.create(FW_DEV_DIR, showWarnings = FALSE, recursive = TRUE)
  path <- fw_local_submission_path()
  new_file <- !file.exists(path)

  utils::write.table(
    flat, path,
    sep = ",", row.names = FALSE,
    col.names = new_file,
    append = !new_file,
    qmethod = "double",
    fileEncoding = "UTF-8"
  )
  invisible(path)
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

      # One entry per target: the group, the species, and the family we looked
      # up for that species. The family is NOT asked for. Contributors were
      # being made to answer a taxonomy question to which the database already
      # holds the answer for every species it knows, and "Unknown" is a better
      # record of the rest than a guess.
      targets <- lapply(rows$target, function(i) {
        taxa <- get_in(paste0("target_taxa_", i))
        if (identical(taxa, FW_OTHER)) {
          taxa <- get_in(paste0("target_taxa_other_", i))
        }
        species <- get_in(paste0("target_species_", i))
        list(
          taxa = taxa,
          species = species,
          family = if (nzchar(species)) {
            fw_family_for_species(species, family_lookup)
          } else ""
        )
      })
      targets <- Filter(function(t) nzchar(t$taxa) || nzchar(t$species), targets)

      uniq_field <- function(field) {
        v <- unique(vapply(targets, function(t) t[[field]], character(1)))
        paste(v[nzchar(v)], collapse = "; ")
      }

      invasive_species <- fw_serialise_rows(targets, c("taxa", "species", "family"))
      benefit_species <- fw_serialise_rows(
        lapply(rows$benefit, function(i) list(name = get_in(paste0("benefit_species_", i)))),
        "name"
      )
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
        country   = get_in("country"),
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

        invasive_taxa = uniq_field("taxa"),
        # Kept as a column because the inbox layout is stable, but the free-text
        # group now travels inside invasive_taxa itself: it belongs to one
        # target, not to the submission.
        invasive_taxa_other = "",
        invasive_species = invasive_species,
        invasive_family = uniq_field("family"),

        invasion_year = get_in("invasion_year"),
        start_year = get_in("start_year"),
        end_year = get_in("end_year"),
        duration_days = get_in("duration_days"),
        driver = get_in("driver"),
        driver_other = get_in("driver_other"),

        beneficiary_taxa = get_in("beneficiary_taxa"),
        beneficiary_taxa_other = get_in("beneficiary_taxa_other"),
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
