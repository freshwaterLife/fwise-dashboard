# export.R
# The export contract: one flat row per attempt, the workbook, and the download
# bundle.
#
# fw_export_frame() builds both the filtered download and the full Zenodo
# dataset (with and without ids), so the two can't disagree.
# Multi-value fields flatten to one FW_MULTI_SEP-delimited column each.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# Export columns in reading order: where, what, when, how, outcome, who.
FW_EXPORT_COLUMNS = c(
  "attempt_id",
  "site_name", "country", "region", "continent", "iso3", "latitude", "longitude",
  "waterbody_type", "water_regime", "area_treated", "area_unit", "area_notes",
  "depth_m", "depth_notes", "volume_m3", "volume_notes", "max_flow_m3s",
  "water_temp_c", "water_temp_notes",
  "invasive_species", "invasive_taxa",
  "invasion_year", "start_year", "end_year", "duration_days", "reason",
  "beneficiary_species", "beneficiary_taxa",
  "methods", "method_classes", "method_notes", "method_description",
  "labour_person_days", "cost_estimate", "cost_currency", "cost_notes",
  "target_ingredient_basis", "toxin_conc_target_mg_l", "conc_target_notes",
  "toxin_conc_measured_mg_l", "conc_measured_notes",
  "neutralising_agent", "neutralising_notes",
  "outcome", "verification_method", "verification_notes",
  "reference", "reference_link", "source",
  "primary_contact_name", "primary_contact_org", "primary_contact_email",
  "secondary_contact_name", "secondary_contact_org", "secondary_contact_email"
)

# Deliberately not exported (FW_EXPORT_COLUMNS is an allowlist):
#   status            always "approved" here
#   notes_for_fwise   a private message to the review team. Never export.
#   last_updated, submitted_at, consent_data_use, sent, eradication_or_control
#                     row maintenance, not facts about the eradication

#' Collapse a bridge table into one semicolon-delimited value per attempt
#'
#' @param bridge rows carrying attempt_id and the value column
#' @param value  the column to collapse
#' @param ids    every attempt id to return (NA where nothing is recorded)
fw_collapse = function(bridge, value, ids) {
  out = bridge |>
    filter(!is.na(.data[[value]]), .data[[value]] != "") |>
    group_by(attempt_id) |>
    summarise(v = paste(unique(.data[[value]]), collapse = FW_MULTI_SEP),
              .groups = "drop")
  unname(setNames(out$v, out$attempt_id)[ids])
}

#' The export frame: one flat row per attempt
#'
#' @param data the loaded tables, already filtered to approved.
#' @param attempt_ids optional subset. NULL = every attempt (the Zenodo release).
#' @return a data frame, one row per attempt, no list columns, no numbered slots.
fw_export_frame = function(data, attempt_ids = NULL) {
  a = data$attempt
  if (!is.null(attempt_ids)) a = filter(a, attempt_id %in% attempt_ids)
  # A stable, readable order.
  a = arrange(a, country, site_name, start_year)
  ids = a$attempt_id

  species = fw_species_label(data$species)
  sp = data$attempt_species |>
    filter(attempt_id %in% ids) |>
    left_join(select(species, species_id, label, taxa), by = "species_id")

  me = data$attempt_method |>
    filter(attempt_id %in% ids) |>
    left_join(select(data$method, method_id, method_name, method_class),
              by = "method_id") |>
    arrange(attempt_id, method_order)

  # Contacts joined by id, once per slot. Blocker to ensure no privacy leaks:
  # a private contact's name, organisation and address are all blanked here,
  # then checked by fw_assert_export_safe().
  contacts = data$contact |>
    mutate(
      contact_name  = if_else(contact_public, contact_name,  NA_character_),
      organisation  = if_else(contact_public, organisation,  NA_character_),
      contact_email = if_else(contact_public, contact_email, NA_character_)
    ) |>
    select(contact_id, contact_name, organisation, contact_email)
  slot = function(id_col, prefix) {
    m = match(a[[id_col]], contacts$contact_id)
    setNames(
      list(contacts$contact_name[m], contacts$organisation[m],
           contacts$contact_email[m]),
      paste0(prefix, c("_name", "_org", "_email"))
    )
  }

  out = a |>
    transmute(
      attempt_id, site_name, country, region, continent, iso3,
      latitude, longitude,
      waterbody_type, water_regime, area_treated, area_unit, area_notes,
      depth_m, depth_notes, volume_m3, volume_notes, max_flow_m3s,
      water_temp_c, water_temp_notes,
      invasion_year, start_year, end_year, duration_days, reason = driver,
      method_description, labour_person_days, cost_estimate, cost_currency, cost_notes,
      target_ingredient_basis, toxin_conc_target_mg_l, conc_target_notes,
      toxin_conc_measured_mg_l, conc_measured_notes,
      neutralising_agent, neutralising_notes,
      outcome, verification_method, verification_notes,
      reference, reference_link, source
    ) |>
    mutate(
      invasive_species    = fw_collapse(filter(sp, role == "invasive"),    "label", ids),
      invasive_taxa       = fw_collapse(filter(sp, role == "invasive"),    "taxa",  ids),
      beneficiary_species = fw_collapse(filter(sp, role == "beneficiary"), "label", ids),
      beneficiary_taxa    = fw_collapse(filter(sp, role == "beneficiary"), "taxa",  ids),
      methods             = fw_collapse(me, "method_name",  ids),
      method_classes      = fw_collapse(me, "method_class", ids),
      method_notes        = fw_collapse(me, "method_notes", ids)
    )

  # Attempts with a method note but no method have no bridge row: export the
  # raw note.
  no_method = !out$attempt_id %in% me$attempt_id
  raw_notes = a$method_notes[match(out$attempt_id[no_method], a$attempt_id)]
  out$method_notes[no_method] = gsub(FW_NOTES_SEP, FW_MULTI_SEP, raw_notes, fixed = TRUE)

  out = bind_cols(out, as_tibble(c(slot("primary_contact_id",   "primary_contact"),
                                    slot("secondary_contact_id", "secondary_contact"))))

  out = as.data.frame(out[, FW_EXPORT_COLUMNS], stringsAsFactors = FALSE)
  rownames(out) = NULL
  fw_assert_export_safe(out, data)
  out
}

#' Blocker: refuse any export carrying a private contact's name or address
#'
#' Checks the redaction in fw_export_frame() actually held. An export can't be
#' recalled.
fw_assert_export_safe = function(x, data) {
  hidden = !data$contact$contact_public
  clean = function(v) v[!is.na(v) & nzchar(v)]

  check = function(values, pattern, what) {
    values = clean(values)
    if (length(values) == 0) return(invisible(TRUE))
    cols = grep(pattern, names(x), value = TRUE)
    leaked = unique(unlist(lapply(cols, function(c) intersect(x[[c]], values))))
    if (length(leaked) > 0) {
      stop("Refusing to export: ", length(leaked), " ", what,
           " belonging to contacts who asked not to be listed.", call. = FALSE)
    }
    invisible(TRUE)
  }

  check(data$contact$contact_email[hidden], "_email$", "address(es)")
  # _contact_name$, not _name$, so site_name isn't checked.
  check(data$contact$contact_name[hidden], "_contact_name$", "name(s)")
  invisible(TRUE)
}

# ---- Caveats -----------------------------------------------------------------

#' The caveats, with their numbers computed from the data
#'
#' Wording is in copy_export.R; counts are filled from the data here. Used by
#' About and every export.
#'
#' @return a list of list(heading =, body =), in document order
fw_caveat_blocks = function(data) {
  a = data$attempt
  n = nrow(a)
  num = function(x) format(x, big.mark = ",")
  pct = function(x) paste0(round(100 * x / n), "%")

  successful = sum(a$outcome == "Successful", na.rm = TRUE)
  unverified = sum(a$outcome == "Successful" &
                      (is.na(a$verification_notes) | a$verification_notes == ""),
                    na.rm = TRUE)
  no_size  = sum(is.na(a$area_treated))
  no_start = sum(is.na(a$start_year))
  no_end   = sum(is.na(a$end_year))

  lapply(fw_t("export", "caveats"), function(block) {
    list(
      heading = block$heading,
      body = fw_fill(
        block$body,
        successful = num(successful), unverified = num(unverified),
        no_size = num(no_size),   pct_size  = pct(no_size),
        no_start = num(no_start), pct_start = pct(no_start),
        no_end = num(no_end),     pct_end   = pct(no_end)
      )
    )
  })
}

#' The methods statement, in the same shape as a caveat block
#'
fw_methods_blocks = function() {
  list(list(heading = fw_t("export", "methods_heading"),
            body = fw_t("export", "methods")))
}

#' The database citation
#'
#' Shared by About and every export. The citation is literal, so `meta` and
#' `n` are unused.
#'
#' @param meta the release metadata (unused)
#' @param n the number of attempts in the database (unused)
fw_citation_text = function(meta = NULL, n = NULL) {
  fw_t("about", "citation_db")
}

#' The whole closing section: methods, then caveats, then the citation
#'
#' The last section of every export (workbook, PDF, records HTML). "Caveats"
#' is a title with no body; its blocks are `sub = TRUE` (smaller headings).
#'
#' @return a list of list(heading =, body =, sub =)
fw_closing_blocks = function(data, meta = NULL) {
  part = function(heading, body) list(heading = heading, body = body, sub = FALSE)
  caveats = lapply(fw_caveat_blocks(data), function(b) {
    b$sub = TRUE
    b
  })
  c(lapply(fw_methods_blocks(), function(b) part(b$heading, b$body)),
    list(part(fw_t("export", "caveats_title"), character(0))),
    caveats,
    list(part(fw_t("export", "citation_heading"),
              fw_citation_text(meta, nrow(data$attempt)))))
}

#' The closing section as flat lines, for the workbook's last sheet
#'
#' fw_closing_blocks() flattened to one line per row.
fw_methods_caveats_text = function(data, meta = NULL) {
  blocks = fw_closing_blocks(data, meta)
  out = character(0)
  for (i in seq_along(blocks)) {
    b = blocks[[i]]
    h = b$heading
    # Part titles in capitals; caveat headings as written.
    if (length(h) && nzchar(h)) out = c(out, if (isTRUE(b$sub)) h else toupper(h))
    out = c(out, b$body)
    # A title with no body runs straight into its first block.
    if (i < length(blocks) && length(b$body)) out = c(out, "")
  }
  out
}

# ---- Field definitions -------------------------------------------------------

#' The data dictionary that travels with the export
#'
#' From export$dictionary in copy_export.R.
fw_field_definitions = function() {
  d = fw_t("export", "dictionary")
  out = data.frame(names(d), unname(d), stringsAsFactors = FALSE)
  names(out) = c(fw_t("export", "col_field"), fw_t("export", "col_definition"))
  out
}

# ---- Sheets ------------------------------------------------------------------

#' Text as a one-column sheet
#'
#' One row per line; a single newline-joined cell is unreadable in Excel.
fw_text_sheet = function(lines, heading) {
  stats::setNames(data.frame(lines, stringsAsFactors = FALSE), heading)
}

#' A readable description of every filter that produced this extract
#'
#' Provenance rows, then one row per filter from fw_filter_summary() (the
#' same registry that draws the controls).
#' @param applied_only drop untouched filters. FALSE for the workbook (a full
#'   record); TRUE for the PDF, as on screen.
fw_filters_sheet = function(filters, n_rows, n_total, meta = NULL,
                             applied_only = FALSE) {
  provenance = list(
    list(setting = fw_t("export", "generated_at"),
         value = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    list(setting = fw_t("export", "in_extract"),
         value = format(n_rows, big.mark = ",")),
    list(setting = fw_t("export", "in_database"),
         value = format(n_total, big.mark = ",")),
    list(setting = fw_t("export", "release"),
         value = meta$release %||% fw_t("export", "release_unknown"))
  )
  set = fw_filter_summary(filters)
  if (applied_only) {
    # "All" and a default "Yes" both mean untouched.
    skip = c(fw_t("export", "filter_all"), fw_t("export", "filter_yes"))
    set = Filter(function(r) {
      if (isTRUE(r$untouched)) return(FALSE)
      !identical(as.character(r$value), skip[1]) &&
        !identical(as.character(r$value), skip[2])
    }, set)
  }
  rows = c(provenance, set)

  out = data.frame(
    vapply(rows, function(r) r$setting, character(1)),
    vapply(rows, function(r) as.character(r$value), character(1)),
    stringsAsFactors = FALSE
  )
  names(out) = c(fw_t("export", "col_setting"), fw_t("export", "col_value"))
  out
}

#' Write the workbook
#'
#' Always four sheets: attempts, definitions, filters, then methods and
#' caveats last. Contacts are columns on the attempts sheet.
fw_write_workbook = function(path, data, export, filters, meta = NULL) {
  wb = openxlsx::createWorkbook()
  header = openxlsx::createStyle(
    fgFill = FW_COLOURS$teal_text, fontColour = FW_COLOURS$surface, textDecoration = "bold",
    halign = "left", border = "bottom", borderColour = FW_COLOURS$teal_text
  )
  add = function(name, x, widths = "auto") {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, x, headerStyle = header)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(max(1, ncol(x))), widths = widths)
  }

  # Add order is tab order; keep it in step with `sheets` in copy_export.R.
  sheets = fw_t("export", "sheets")
  add(sheets$attempts, export)
  add(sheets$definitions, fw_field_definitions(), widths = c(26, 100))
  add(sheets$filters,
      fw_filters_sheet(filters, nrow(export), nrow(data$attempt), meta),
      widths = c(30, 60))
  add(sheets$caveats,
      fw_text_sheet(fw_methods_caveats_text(data, meta), fw_t("export", "closing_heading")),
      widths = 110)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' When a download was made, for its filename
#'
#' UTC date and time, so two downloads a day get distinct names. Chart PNGs
#' match (fw_png_button()). The fw.now option pins the clock for tests.
fw_file_stamp = function(time = getOption("fw.now", Sys.time())) {
  format(time, "%Y%m%d-%H%M%S", tz = "UTC")
}

#' Filename for a download
fw_export_filename = function(stamp = fw_file_stamp()) {
  paste0(fw_t("export", "filename_stem"), stamp, ".xlsx")
}

# ---- The bundle --------------------------------------------------------------
#
# One download built from what the reader ticks on Plan (fw_plan_download_ui()).
# One part arrives as itself; more arrive as a zip.

# The parts, in write order and picker order.
FW_BUNDLE_PARTS = c("pdf", "records", "xlsx")

#' Which files a selection actually produces
#'
#' Drops unknown parts, and the PDF when there's no Quarto.
fw_bundle_parts = function(parts = character(0)) {
  parts = intersect(FW_BUNDLE_PARTS, parts %||% character(0))
  if (!fw_pdf_available()) parts = setdiff(parts, "pdf")
  # Fallback for an empty request (the button is disabled, but e.g. PDF-only
  # with no Quarto): a spreadsheet, not a zero-byte file.
  if (!length(parts)) parts = "xlsx"
  parts
}

#' What one part is called on its own
#'
#' Used for a lone file and for names inside a zip.
FW_BUNDLE_FILENAME = list(
  pdf     = function(stamp) fw_pdf_filename(stamp),
  records = function(stamp) fw_records_filename(stamp),
  xlsx    = function(stamp) fw_export_filename(stamp)
)

#' What a download of this selection will be called
#'
#' A zip for several parts, the file itself for one.
fw_bundle_filename = function(parts = character(0), stamp = fw_file_stamp()) {
  parts = fw_bundle_parts(parts)
  if (length(parts) == 1) return(FW_BUNDLE_FILENAME[[parts]](stamp))
  paste0(fw_t("export", "bundle_stem"), stamp, ".zip")
}

#' Write the download
#'
#' @param path where to write - the download handler's temp file
#' @param parts what the reader ticked. Anything not recognised is ignored.
#' @param progress called as progress(value, detail) before each step, value
#'   running 0 to 1 across the whole bundle. The handler passes Shiny's
#'   setProgress(); the default does nothing, so the tests need no session.
fw_write_bundle = function(path, parts, data, sel, export, filters, meta = NULL,
                            progress = function(value, detail) NULL) {
  parts = fw_bundle_parts(parts)

  # One stamp for every name: the PDF takes seconds to make.
  stamp = fw_file_stamp()
  names = lapply(FW_BUNDLE_FILENAME, function(f) f(stamp))

  # Progress weighted by time taken (the PDF dominates). The zip step only
  # exists for more than one file.
  steps = c(xlsx = 2, pdf = 12, records = 3, zip = 1)
  steps = steps[c(parts, if (length(parts) > 1) "zip")]
  ends = cumsum(steps) / sum(steps)
  starts = stats::setNames(c(0, utils::head(ends, -1)), names(steps))
  step = function(id, key) progress(starts[[id]], fw_t("plan", key))

  dir = tempfile("fw-bundle-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  files = character(0)

  # Write in FW_BUNDLE_PARTS order, or the progress bar jumps backwards.
  if ("pdf" %in% parts) {
    fw_write_pdf_report(
      path = file.path(dir, names$pdf), data = data, sel = sel,
      filters = filters, meta = meta,
      # The report's own sub-steps, mapped into the PDF's stretch of the bar.
      progress = function(fraction, detail)
        progress(starts[["pdf"]] + fraction * steps[["pdf"]] / sum(steps), detail)
    )
    files = c(files, names$pdf)
  }
  if ("records" %in% parts) {
    step("records", "progress_records")
    fw_write_records_html(file.path(dir, names$records), data, sel,
                          export, filters, meta)
    files = c(files, names$records)
  }

  if ("xlsx" %in% parts) {
    step("xlsx", "progress_xlsx")
    fw_write_workbook(file.path(dir, names$xlsx), data, export,
                      filters, meta)
    files = c(files, names$xlsx)
  }
  # One file arrives as itself (fw_bundle_filename() agrees).
  if (length(files) == 1) {
    file.copy(file.path(dir, files), path, overwrite = TRUE)
    progress(1, fw_t("plan", "progress_done"))
    return(invisible(path))
  }
  step("zip", "progress_zip")

  # Zip to a .zip temp name, then copy: zip appends ".zip" to an extensionless
  # path. utils::zip() signals failure by status only, so check it. Relative
  # paths so the archive holds no absolute paths.
  archive = tempfile(fileext = ".zip")
  wd = setwd(dir); on.exit(setwd(wd), add = TRUE, after = FALSE)
  status = utils::zip(archive, files, flags = "-q")
  if (!identical(as.integer(status), 0L) || !file.exists(archive)) {
    stop("Could not write the download archive (zip exit status ", status, ").",
         call. = FALSE)
  }
  file.copy(archive, path, overwrite = TRUE)
  unlink(archive)
  progress(1, fw_t("plan", "progress_done"))
  invisible(path)
}
