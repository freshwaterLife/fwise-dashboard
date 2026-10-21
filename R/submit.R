# submit.R
# The submission write path.
#
# There is ONE public function, fw_submit_attempt(record). It appends one row
# to the `submissions` tab of the inbox sheet - or, with no sheet configured,
# to dev/forms/submissions.csv - through fw_inbox_append() in forms_store.R.
# The reviewer QAs the row in the sheet and dev/qa.R imports it.
#
# A SUBMISSION IS ONE ROW A PERSON CAN READ (Oct 2026). The sheet holds the
# columns of attempts.csv with three differences, all so the reviewer can QA it
# without looking anything up:
#   - species are their LABELS, "Common carp (Cyprinus carpio)", not SP- ids;
#   - the contact is split into name, organisation, email and public, with a
#     blank contact_id the reviewer fills only to point at a contact we hold;
#   - the columns the import derives (iso3, continent, last_updated) and the
#     second contact the form never asks for are not there at all.
# Two columns lead the row for the reviewer: qa_status and imported.
#
# A species the contributor typed in, rather than picked, also gets a row of
# the `new_species` tab, with its names split and the taxa they chose, for the
# reviewer to complete. dev/qa.R mints its id at import.

# The submissions tab, in order. Built from FW_ATTEMPT_COLUMNS so a column
# added to the database arrives here without a second list to keep in step.
FW_INBOX_STATE_COLUMNS = c("qa_status", "imported")
FW_INBOX_CONTACT_COLUMNS = c("contact_id", "contact_name", "contact_organisation",
                              "contact_email", "contact_public")
FW_INBOX_DERIVED = c("iso3", "continent", "last_updated", "secondary_contact_id")
FW_INBOX_COLUMNS = local({
  cols = setdiff(FW_ATTEMPT_COLUMNS, FW_INBOX_DERIVED)
  i = match("primary_contact_id", cols)
  c(FW_INBOX_STATE_COLUMNS, append(cols[-i], FW_INBOX_CONTACT_COLUMNS, after = i - 1L))
})

FW_NEW_SPECIES_COLUMNS = c("label", "common_name", "scientific_name", "taxa",
                            "family", "from_attempt")

# The values of the two reviewer columns. The app writes the first of each.
FW_QA_STATUS = c("not done", "done")
FW_IMPORTED  = c("no", "yes", "rejected")

# ---- Identifiers -------------------------------------------------------------

# Ambiguous characters are left out - no I, O, 0 or 1 - because these ids get
# read aloud, retyped and pasted into spreadsheets by people.
FW_ID_ALPHABET = c(as.character(2:9), setdiff(LETTERS, c("I", "O")))

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
fw_mint_ids = function(prefix, n, exclude = character(0), date = Sys.Date()) {
  if (n == 0) return(character(0))
  stamp = format(date, "%Y%m%d")
  out = character(0)
  while (length(out) < n) {
    cand = replicate(n - length(out), paste0(
      prefix, "-", stamp, "-",
      paste(sample(FW_ID_ALPHABET, 6, replace = TRUE), collapse = "")
    ))
    out = setdiff(unique(c(out, cand)), exclude)
  }
  out[seq_len(n)]
}

#' A species label split into its names
#'
#' "Arctic charr (Salvelinus alpinus)" -> common "Arctic charr", scientific
#' "Salvelinus alpinus". A label with no parenthesis is all common name, as
#' fw_species_label() treats one.
fw_split_species_label = function(label) {
  sci = stringr::str_match(label, "\\(([^()]*)\\)\\s*$")[, 2]
  com = trimws(sub("\\s*\\(([^()]*)\\)\\s*$", "", label))
  list(common = ifelse(is.na(com) | !nzchar(com), NA_character_, com),
       scientific = ifelse(is.na(sci) | !nzchar(trimws(sci)), NA_character_, trimws(sci)))
}

# ---- Writing a submission ----------------------------------------------------

#' One record as a character vector in the given column order, blank where absent
fw_flatten_record = function(record, columns = FW_INBOX_COLUMNS) {
  vapply(columns, function(col) {
    v = record[[col]]
    if (is.null(v) || length(v) == 0) return("")
    if (length(v) > 1) return(paste(v, collapse = FW_MULTI_SEP))
    if (is.na(v)) return("")
    as.character(v)
  }, character(1), USE.NAMES = TRUE)
}

#' The new_species rows for the species a contributor typed in
#'
#' @param typed a data frame of label and taxa, from fw_collect_submission()
fw_new_species_rows = function(typed, attempt_id) {
  if (is.null(typed) || !nrow(typed)) return(list())
  typed = typed[!duplicated(typed$label), , drop = FALSE]
  lapply(seq_len(nrow(typed)), function(i) {
    nm = fw_split_species_label(typed$label[i])
    fw_flatten_record(list(label = typed$label[i], common_name = nm$common,
                           scientific_name = nm$scientific, taxa = typed$taxa[i],
                           family = "", from_attempt = attempt_id),
                      FW_NEW_SPECIES_COLUMNS)
  })
}

#' Write one submission
#'
#' @param record a named list of the form answers, from fw_collect_submission()
#' @param data   the loaded tables, for the confirmation counts
#' @return list(success, message, attempt_id, submission_id, total_attempts,
#'              country_attempts, country)
fw_submit_attempt = function(record, data = NULL) {

  # THE ATTEMPT ID IS MINTED HERE and travels with the row from the sheet into
  # attempts.csv unchanged, so a reviewer, a contributor's confirmation screen
  # and the published data all name the same record. Collisions are excluded
  # against the ids the app is holding; dev/qa.R checks again against the file.
  in_use = if (is.null(data)) character(0) else data$attempt$attempt_id
  record$attempt_id   = fw_mint_ids("FW", 1, exclude = in_use)
  record$status       = "pending"
  record$submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  record$qa_status    = FW_QA_STATUS[1]
  record$imported     = FW_IMPORTED[1]

  # NO FALLBACK TO THE LOCAL FILE IF THE SHEET WRITE FAILS. On Connect Cloud
  # the container filesystem is thrown away on restart, so a fallback would
  # report success for a submission that is already gone. Telling the
  # contributor to try again is worth more than a receipt for nothing.
  written = tryCatch({
    fw_inbox_append(FW_INBOX_TABS$submissions, list(fw_flatten_record(record)),
                    FW_INBOX_COLUMNS)
    message("Submission ", record$attempt_id, " written to ", fw_inbox_mode(), " inbox")
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

  # THE RECORD IS SAFE ONCE THE ROW ABOVE HAS LANDED. A failed new_species
  # write is logged, not shown to the contributor: dev/qa.R reports the typed
  # species as unknown and the reviewer adds its row by hand.
  tryCatch(
    fw_inbox_append(FW_INBOX_TABS$new_species,
                    fw_new_species_rows(record$new_species, record$attempt_id),
                    FW_NEW_SPECIES_COLUMNS),
    error = function(e) warning("new_species write for ", record$attempt_id,
                                " failed: ", conditionMessage(e))
  )

  # The in-review count re-reads the inbox on its next ask, so the
  # contributor sees their own record counted straight away.
  fw_review_count_invalidate()

  counts = fw_submission_counts(data, record$country)

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
#' The published database plus everything waiting in the inbox, so a
#' contributor who submits twice in a row sees the number move.
fw_submission_counts = function(data, country) {
  total = if (is.null(data)) 0L else nrow(data$attempt)
  in_country = if (is.null(data) || is.null(country)) {
    0L
  } else {
    sum(data$attempt$country == country, na.rm = TRUE)
  }

  pending = tryCatch(fw_pending_submissions(), error = function(e) data.frame())
  pending_total = nrow(pending)
  pending_country = if (pending_total > 0 && !is.null(country) &&
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

#' Every submission still waiting: not imported, not rejected
#'
#' Feeds the in-review count and the confirmation screen's totals. One read of
#' the submissions tab.
fw_pending_submissions = function() {
  inbox = fw_inbox_read(FW_INBOX_TABS$submissions)
  if (!nrow(inbox) || !all(c("imported", "status") %in% names(inbox))) return(data.frame())
  waiting = inbox$imported %in% "no" & !inbox$status %in% "rejected"
  inbox[waiting, , drop = FALSE]
}

# ---- The in-review count, kept current ---------------------------------------
#
# ONE CACHE FOR THE WHOLE R PROCESS, not one per visitor. It used to be read
# once at startup (FW_IN_REVIEW), so it moved only when Connect Cloud started a
# new process (client, 14 Oct 2026: a submission did not show). Every open page
# now asks every FW_REVIEW_REFRESH_S, and the sheet is read at most that often
# between them all.
FW_REVIEW_CACHE = new.env(parent = emptyenv())

#' The in-review count, at most FW_REVIEW_REFRESH_S old
#'
#' A failed read keeps the last good count rather than showing 0, which would
#' say the queue had emptied. The fw.now option pins the clock for the tests.
#'
#' @param data the loaded data, for the pending rows already in attempts.csv
fw_review_count_live = function(data, now = getOption("fw.now", Sys.time())) {
  fresh = !is.null(FW_REVIEW_CACHE$at) &&
    as.numeric(difftime(now, FW_REVIEW_CACHE$at, units = "secs")) < FW_REVIEW_REFRESH_S
  if (fresh) return(FW_REVIEW_CACHE$n)
  n = tryCatch(fw_review_count(data, strict = TRUE), error = function(e) {
    warning("In-review count not refreshed: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(n)) return(FW_REVIEW_CACHE$n %||% 0L)
  FW_REVIEW_CACHE$n  = n
  FW_REVIEW_CACHE$at = now
  n
}

#' Make the next fw_review_count_live() read the inbox again
fw_review_count_invalidate = function() {
  FW_REVIEW_CACHE$at = NULL
  invisible(NULL)
}

# ---- Assembling a record from the form ---------------------------------------

#' Collect the contribute form's inputs into one record
#'
#' Lives here rather than in the module so the column set in
#' FW_INBOX_COLUMNS and the code that fills it sit side by side. Adding a
#' field to the form means adding it to the column contract in data_load.R and
#' filling it here.
#'
#' "OTHER (SPECIFY)" GOES INTO THE MAIN COLUMN. The typed value replaces the
#' placeholder, so the reviewer sees an off-list country, waterbody, driver,
#' method or agent in the column it belongs to and dev/qa.R flags it.
#'
#' @param input   the module's input object
#' @param rows    the reactiveValues holding which repeatable rows are on screen
#' @param choices the startup choices, for the species we already hold
fw_collect_submission = function(input, rows, choices = NULL) {

  get_in = function(nm) {
    v = input[[nm]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) return("")
    as.character(v)
  }
  # A picker's value, or the typed value when "Other (specify)" was chosen.
  pick = function(nm, other_nm) {
    v = get_in(nm)
    if (identical(v, FW_OTHER)) get_in(other_nm) else v
  }

  held = names(choices$species_ids %||% character(0))

  # ONE ENTRY PER ROW, as the species' LABEL, which is what the reviewer reads
  # and what dev/qa.R resolves to an id. A label we do not hold is one the
  # contributor typed; it is also collected, with its taxa, for the
  # new_species tab. IDENTICAL FOR BOTH ROLES: invasive targets and
  # beneficiaries are the same question asked about two sides of the same
  # eradication.
  typed = data.frame(label = character(0), taxa = character(0))
  collect_species = function(kind, indices) {
    out = character(0)
    for (i in indices) {
      species = trimws(get_in(paste0(kind, "_species_", i)))
      if (!nzchar(species)) next
      if (!species %in% held) {
        taxa = pick(paste0(kind, "_taxa_", i), paste0(kind, "_taxa_other_", i))
        typed <<- rbind(typed, data.frame(label = species, taxa = taxa))
      }
      out = c(out, species)
    }
    paste(out, collapse = FW_MULTI_SEP)
  }

  # Methods travel by NAME; the notes cell pairs each note with its method in
  # the same order, so the map popup can still say which note belongs where.
  method_names = character(0); method_notes = character(0)
  for (i in rows$method) {
    nm = pick(paste0("method_", i), paste0("method_other_", i))
    if (!nzchar(nm)) next
    method_names = c(method_names, nm)
    method_notes = c(method_notes, paste0(nm, ": ", get_in(paste0("method_notes_", i))))
  }
  any_notes = any(nzchar(trimws(sub("^[^:]*:", "", method_notes))))

  invasive    = collect_species("target", rows$target)
  beneficiary = collect_species("benefit", rows$benefit)

  list(
    consent_data_use = if (isTRUE(input$consent_data_use)) "yes" else "no",

    site_name = get_in("site_name"),
    country   = pick("country", "country_other"),
    region    = get_in("region"),
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

    invasive_species = invasive,

    invasion_year = get_in("invasion_year"),
    start_year    = get_in("start_year"),
    end_year      = get_in("end_year"),
    duration_days = get_in("duration_days"),
    # The form collects eradication attempts; control programmes are out of
    # scope, so this is a constant rather than a question.
    eradication_or_control = "Eradication",
    driver = pick("driver", "driver_other"),

    beneficiary_species = beneficiary,

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

    # The form has no contact picker, so the contributor is always given by
    # name; dev/qa.R matches them against contacts.csv. OPT-IN (Sept 2026):
    # the address is shown only when the contributor ticked "I give permission
    # for my email to be displayed in the app".
    contact_id           = "",
    contact_name         = get_in("primary_contact_name"),
    contact_organisation = get_in("primary_contact_org"),
    contact_email        = get_in("primary_contact_email"),
    contact_public       = if (isTRUE(input$contact_public)) "yes" else "no",

    source = get_in("source"),
    sent   = "",
    reference      = get_in("reference"),
    reference_link = "",
    notes_for_fwise = get_in("notes_for_fwise"),

    # Not a column: the new_species rows fw_submit_attempt() writes.
    new_species = typed
  )
}
