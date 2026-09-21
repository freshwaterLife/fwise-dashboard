# export.R
# The export contract: one flat row per attempt, and the workbook that carries it.
#
# ONE FLATTENING FUNCTION, fw_export_frame(). The filtered download on the report
# builder and the full public dataset published to Zenodo are the SAME function
# called with and without a set of attempt ids. That is deliberate and it is the
# whole reason this file exists separately from the module: if the two were built
# by different code they could disagree, and a reader comparing a download
# against the citable dataset would find different numbers with no way to tell
# which was right.
#
# MULTI-VALUE FIELDS FLATTEN TO SEMICOLON-DELIMITED SINGLE COLUMNS. Neither of
# the source's own conventions survives into an export:
#
#   - eight numbered species columns   ->  one `invasive_species` column
#   - underscore-nested taxa strings   ->  one `invasive_taxa` column
#
# The underscore is a legacy convention of the client's spreadsheet. The
# semicolon is the published convention. The data dictionary sheet says so, so
# nobody splits on the wrong character.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# FW_MULTI_SEP, the delimiter for every multi-value column, lives in config.R
# because attempts.csv uses the same one.

# Column order of the export, and the label each column carries. Order is the
# order a person reads a record in: where, what, when, how, what happened, who.
FW_EXPORT_COLUMNS <- c(
  "attempt_id",
  "site_name", "country", "region", "continent", "iso3", "latitude", "longitude",
  "waterbody_type", "water_regime", "area_treated", "area_unit", "area_notes",
  "depth_m", "depth_notes", "volume_m3", "volume_notes", "max_flow_m3s",
  "water_temp_c", "water_temp_notes",
  "invasive_species", "invasive_taxa",
  "invasion_year", "start_year", "end_year", "duration_days", "driver",
  "beneficiary_species", "beneficiary_taxa",
  "methods", "method_classes", "method_notes", "method_description",
  "labour_person_days", "cost_estimate", "cost_notes",
  "target_ingredient_basis", "toxin_conc_target_mg_l", "conc_target_notes",
  "toxin_conc_measured_mg_l", "conc_measured_notes",
  "neutralising_agent", "neutralising_notes",
  "outcome", "verification_method", "verification_notes",
  "reference", "reference_link", "source",
  "primary_contact_name", "primary_contact_org", "primary_contact_email",
  "secondary_contact_name", "secondary_contact_org", "secondary_contact_email"
)

# Columns deliberately NOT exported. FW_EXPORT_COLUMNS is an allowlist, so
# these stay out by being absent from it; they are named here so nobody adds
# one back without reading why.
#
# `status` - every exported row is approved by construction, so the column would
#   be a constant that invites the reader to wonder what else there is.
# `notes_for_fwise` - a contributor's private message TO the review team. It is
#   collected so the team can read it, not so it can be republished, and it may
#   carry asides the contributor would not put their name to publicly. Stored in
#   the database and shown in record detail; never exported.
# `last_updated`, `submitted_at`, `consent_data_use`, `sent`,
#   `eradication_or_control` - properties of the row's maintenance, not of the
#   eradication (the last is the constant "Eradication" on every row).

#' Collapse a bridge table into one semicolon-delimited value per attempt
#'
#' @param bridge rows carrying attempt_id and the value column
#' @param value  the column to collapse
#' @param ids    every attempt id that must appear in the result, so an attempt
#'   with nothing recorded comes back as NA rather than dropping out of the join
fw_collapse <- function(bridge, value, ids) {
  out <- bridge |>
    filter(!is.na(.data[[value]]), .data[[value]] != "") |>
    group_by(attempt_id) |>
    summarise(v = paste(unique(.data[[value]]), collapse = FW_MULTI_SEP),
              .groups = "drop")
  unname(setNames(out$v, out$attempt_id)[ids])
}

#' THE export frame: one flat row per attempt
#'
#' @param data the loaded star-schema tables, already filtered to approved.
#' @param attempt_ids optional subset. NULL means every approved attempt, which
#'   is what the Zenodo release uses. Pass ids for a filtered download.
#' @return a data frame, one row per attempt, no list columns, no numbered slots.
fw_export_frame <- function(data, attempt_ids = NULL) {
  a <- data$attempt
  if (!is.null(attempt_ids)) a <- filter(a, attempt_id %in% attempt_ids)
  # Stable, meaningful order. Not row order, which carries no meaning now that
  # ids are minted rather than positional.
  a <- arrange(a, country, site_name, start_year)
  ids <- a$attempt_id

  species <- fw_species_label(data$species)
  sp <- data$attempt_species |>
    filter(attempt_id %in% ids) |>
    left_join(select(species, species_id, label, taxa), by = "species_id")

  me <- data$attempt_method |>
    filter(attempt_id %in% ids) |>
    left_join(select(data$method, method_id, method_name, method_class),
              by = "method_id") |>
    arrange(attempt_id, method_order)

  # Contacts are joined by id twice, once per slot. Redaction is applied here
  # and asserted afterwards - see fw_assert_export_safe().
  contacts <- data$contact |>
    mutate(contact_email = if_else(email_public, contact_email, NA_character_)) |>
    select(contact_id, contact_name, organisation, contact_email)
  slot <- function(id_col, prefix) {
    m <- match(a[[id_col]], contacts$contact_id)
    setNames(
      list(contacts$contact_name[m], contacts$organisation[m],
           contacts$contact_email[m]),
      paste0(prefix, c("_name", "_org", "_email"))
    )
  }

  out <- a |>
    transmute(
      attempt_id, site_name, country, region, continent, iso3,
      latitude, longitude,
      waterbody_type, water_regime, area_treated, area_unit, area_notes,
      depth_m, depth_notes, volume_m3, volume_notes, max_flow_m3s,
      water_temp_c, water_temp_notes,
      invasion_year, start_year, end_year, duration_days, driver,
      method_description, labour_person_days, cost_estimate, cost_notes,
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

  # A NOTE WITH NO METHOD STILL TRAVELS. Nine attempts in the source record
  # something about the method - "Piscicide (unspecified)", "Chemical" - and no
  # method at all. They have no bridge row, so the collapse above leaves them
  # blank and the only copy of the note is the raw cell on the attempt. It goes
  # out as it stands, with `methods` left NA beside it, because a blank here
  # would be a value the client's export has and ours does not.
  no_method <- !out$attempt_id %in% me$attempt_id
  raw_notes <- a$method_notes[match(out$attempt_id[no_method], a$attempt_id)]
  out$method_notes[no_method] <- gsub(FW_NOTES_SEP, FW_MULTI_SEP, raw_notes, fixed = TRUE)

  out <- bind_cols(out, as_tibble(c(slot("primary_contact_id",   "primary_contact"),
                                    slot("secondary_contact_id", "secondary_contact"))))

  out <- as.data.frame(out[, FW_EXPORT_COLUMNS], stringsAsFactors = FALSE)
  rownames(out) <- NULL
  fw_assert_export_safe(out, data)
  out
}

#' Refuse to export an address the contact asked to keep private
#'
#' The redaction happens above, in fw_export_frame(). This is the control that
#' makes it a control rather than something we remembered to do: "we applied the
#' filter" is not a guarantee, and an export is the one place a mistake travels
#' outside the building and cannot be recalled.
fw_assert_export_safe <- function(x, data) {
  private <- data$contact$contact_email[!data$contact$email_public]
  private <- private[!is.na(private) & nzchar(private)]
  if (length(private) == 0) return(invisible(TRUE))

  cols <- grep("_email$", names(x), value = TRUE)
  leaked <- unique(unlist(lapply(cols, function(c) intersect(x[[c]], private))))
  if (length(leaked) > 0) {
    stop("Refusing to export: ", length(leaked),
         " address(es) belonging to contacts who asked not to be listed.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Caveats -----------------------------------------------------------------

#' The caveats, with their numbers computed from the data
#'
#' THE SAME TEXT travels into every export and sits beside every result on the
#' report builder. That is a requirement, not a nicety: a spreadsheet that turns
#' up in somebody's inbox six months later has to carry its own qualifications,
#' because by then nobody remembers what was on screen when it was downloaded.
#'
#' The wording lives in R/copy_export.R as templates; the counts are derived
#' here and filled in, so they cannot drift away from the data they describe.
#' A caveat with a stale number in it is worse than no caveat, because it reads
#' as precision.
#'
#' @return a list of list(heading =, body =), in document order. ADD OR REMOVE
#'   A BLOCK IN THE COPY AND NOTHING ELSE HAS TO CHANGE: the panel loops, the
#'   grid reflows, and the tests count what they find.
fw_caveat_blocks <- function(data) {
  a <- data$attempt
  n <- nrow(a)
  num <- function(x) format(x, big.mark = ",")
  pct <- function(x) paste0(round(100 * x / n), "%")

  successful <- sum(a$outcome == "Successful", na.rm = TRUE)
  unverified <- sum(a$outcome == "Successful" &
                      (is.na(a$verification_notes) | a$verification_notes == ""),
                    na.rm = TRUE)
  no_size  <- sum(is.na(a$area_treated))
  no_start <- sum(is.na(a$start_year))
  no_end   <- sum(is.na(a$end_year))

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

#' The caveats as one flat vector, for the workbook sheet
#'
#' Heading in capitals, body, blank line, repeated. Derived from
#' fw_caveat_blocks() so the two can never say different things.
fw_caveats <- function(data) {
  blocks <- fw_caveat_blocks(data)
  out <- character(0)
  for (i in seq_along(blocks)) {
    out <- c(out, toupper(blocks[[i]]$heading), blocks[[i]]$body)
    if (i < length(blocks)) out <- c(out, "")
  }
  out
}

#' The methods-and-caveats text file that travels with every download
#'
#' NEVER OPTIONAL, and that is the point of it. The download picker on the
#' report builder lets a reader choose the spreadsheet, the CSV, the report or
#' any combination; this goes in the bundle whatever they choose, for the same
#' reason the workbook has always carried a caveats sheet. A reader who did not
#' ask for the qualifications is exactly the reader who needs them, and by the
#' time a file reaches somebody else nobody remembers what was on screen.
#'
#' Two sections: how the database was built, then what to watch for in it. The
#' second half is fw_caveats(), the same vector the workbook sheet uses, so the
#' text file and the spreadsheet cannot say different things.
fw_methods_caveats_text <- function(data) {
  c(
    toupper(fw_t("export", "methods_heading")),
    fw_t("export", "methods"),
    "",
    fw_caveats(data)
  )
}

#' Filename for the methods-and-caveats text
fw_methods_filename <- function() fw_t("export", "methods_filename")

# ---- Field definitions -------------------------------------------------------

#' The data dictionary that travels with the export
#'
#' The definitions live in R/copy_export.R, one per exported column. This only
#' shapes them into the sheet.
fw_field_definitions <- function() {
  d <- fw_t("export", "dictionary")
  out <- data.frame(names(d), unname(d), stringsAsFactors = FALSE)
  names(out) <- c(fw_t("export", "col_field"), fw_t("export", "col_definition"))
  out
}

# ---- Sheets ------------------------------------------------------------------

#' Text as a one-column sheet
#'
#' One row per line, NOT one cell holding the whole document. Collapsing to a
#' single newline-joined string renders as one unreadable cell in Excel.
fw_text_sheet <- function(lines, heading) {
  stats::setNames(data.frame(lines, stringsAsFactors = FALSE), heading)
}

#' A readable description of every filter that produced this extract
#'
#' Goes into the workbook as its own sheet, so a file that turns up in somebody's
#' inbox six months later still says what it is a slice of.
#'
#' The filter rows are built by fw_filter_summary() from the SAME registry that
#' draws the controls, so a filter cannot be offered on the page and then be
#' missing from the spreadsheet that is supposed to record what was selected.
#' Only the provenance rows above them are written out by hand.
fw_filters_sheet <- function(filters, n_rows, n_total, meta = NULL) {
  provenance <- list(
    list(setting = fw_t("export", "generated_at"),
         value = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    list(setting = fw_t("export", "in_extract"),
         value = format(n_rows, big.mark = ",")),
    list(setting = fw_t("export", "in_database"),
         value = format(n_total, big.mark = ",")),
    list(setting = fw_t("export", "release"),
         value = meta$release %||% fw_t("export", "release_unknown"))
  )
  rows <- c(provenance, fw_filter_summary(filters))

  out <- data.frame(
    vapply(rows, function(r) r$setting, character(1)),
    vapply(rows, function(r) as.character(r$value), character(1)),
    stringsAsFactors = FALSE
  )
  names(out) <- c(fw_t("export", "col_setting"), fw_t("export", "col_value"))
  out
}

#' Write the workbook
#'
#' FOUR SHEETS, ALWAYS. The data is useless to a careful reader without the other
#' three, and a reader who did not ask for the caveats is exactly the reader who
#' needs them. There is no contacts sheet: the attempts sheet already carries
#' primary_contact_* and secondary_contact_* on every row (client, 21 Sept 2026).
fw_write_workbook <- function(path, data, export, filters, meta = NULL) {
  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(
    fgFill = FW_COLOURS$teal_text, fontColour = FW_COLOURS$surface, textDecoration = "bold",
    halign = "left", border = "bottom", borderColour = FW_COLOURS$teal_text
  )
  add <- function(name, x, widths = "auto") {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, x, headerStyle = header)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(max(1, ncol(x))), widths = widths)
  }

  sheets <- fw_t("export", "sheets")
  add(sheets$attempts, export)
  add(sheets$caveats,
      fw_text_sheet(fw_caveats(data), fw_t("export", "caveats_heading")),
      widths = 110)
  add(sheets$definitions, fw_field_definitions(), widths = c(26, 100))
  add(sheets$filters,
      fw_filters_sheet(filters, nrow(export), nrow(data$attempt), meta),
      widths = c(30, 60))

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Filename for a download
fw_export_filename <- function() {
  paste0(fw_t("export", "filename_stem"), format(Sys.Date(), "%Y%m%d"), ".xlsx")
}

# ---- The bundle --------------------------------------------------------------
#
# ONE DOWNLOAD, ASSEMBLED FROM WHAT THE READER TICKED. The report builder offers
# a picker and one button, and the methods-and-caveats text rides along whatever
# else is in there. See fw_plan_download_ui() in mod_plan.R.
#
# FOUR PARTS, at the client's request (21 Sept 2026): the spreadsheet, the CSV,
# the PDF report and the attempts file. The interactive HTML report they
# replace - live plotly charts and a leaflet map, printed to PDF through the
# browser - is gone; the PDF is now a real one, made on the server.

# The parts a reader can choose, in the order they are written.
FW_BUNDLE_PARTS <- c("xlsx", "csv", "pdf", "records")

#' Which files a selection actually produces
#'
#' Anything not recognised is dropped. The PDF is dropped too when this server
#' cannot make one (no Quarto): the picker has already told the reader so, and
#' the rest of their download should not fail with it.
fw_bundle_parts <- function(parts = character(0)) {
  parts <- intersect(FW_BUNDLE_PARTS, parts %||% character(0))
  if (!fw_pdf_available()) parts <- setdiff(parts, "pdf")
  parts
}

#' What a download of this selection will be called
#'
#' A zip when there is more than one file, and the file itself when there is
#' exactly one. With the text file always travelling, the single-file case is a
#' reader who ticked nothing - which downloads the methods and caveats alone,
#' and is a reasonable thing to want rather than an error to refuse.
fw_bundle_filename <- function(parts = character(0)) {
  if (!length(fw_bundle_parts(parts))) return(fw_methods_filename())
  paste0(fw_t("export", "bundle_stem"), format(Sys.Date(), "%Y%m%d"), ".zip")
}

#' The flattened export as a CSV on disk
#'
#' UTF-8 with no byte-order mark. Species names carry accents and a BOM would
#' make Excel read them correctly while breaking a good number of the data tools
#' this CSV is actually for; the .xlsx alongside it is the answer for Excel.
fw_write_csv <- function(export, dir) {
  path <- file.path(dir, "fwise-attempts.csv")
  utils::write.csv(export, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}

#' Write the download
#'
#' @param path where to write - the download handler's temp file
#' @param parts what the reader ticked. Anything not recognised is ignored.
#' @param method_mode,method_wb_mode,waterbody_mode the chart toggles, passed to
#'   fw_write_pdf_report() so the document matches the screen it came from.
#' @param progress called as progress(value, detail) before each step, value
#'   running 0 to 1 across the whole bundle. The handler passes Shiny's
#'   setProgress(); the default does nothing, so the tests need no session.
fw_write_bundle <- function(path, parts, data, sel, export, filters, meta = NULL,
                            method_mode = "count", method_wb_mode = "count",
                            waterbody_mode = "count",
                            progress = function(value, detail) NULL) {
  parts <- fw_bundle_parts(parts)

  # THE BAR IS WEIGHTED BY WHAT TAKES THE TIME, not by the number of steps: the
  # PDF (Quarto on the server) is most of any bundle that has one, and a bar
  # that spent a sixth of itself on a text file would stall at the end.
  steps <- c(txt = 1, xlsx = 2, csv = 1, pdf = 12, records = 3, zip = 1)
  steps <- steps[c("txt", parts, if (length(parts)) "zip")]
  ends <- cumsum(steps) / sum(steps)
  starts <- stats::setNames(c(0, utils::head(ends, -1)), names(steps))
  step <- function(id, key) progress(starts[[id]], fw_t("plan", key))

  dir <- tempfile("fw-bundle-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # Always, and first, so it is the first thing in the archive listing.
  step("txt", "progress_txt")
  txt <- file.path(dir, fw_methods_filename())
  writeLines(fw_methods_caveats_text(data), txt, useBytes = TRUE)
  files <- fw_methods_filename()

  if ("xlsx" %in% parts) {
    step("xlsx", "progress_xlsx")
    fw_write_workbook(file.path(dir, fw_export_filename()), data, export,
                      filters, meta)
    files <- c(files, fw_export_filename())
  }
  if ("csv" %in% parts) {
    step("csv", "progress_csv")
    files <-c(files, basename(fw_write_csv(export, dir)))
  }
  if ("pdf" %in% parts) {
    fw_write_pdf_report(
      path = file.path(dir, fw_pdf_filename()), data = data, sel = sel,
      filters = filters, meta = meta, method_mode = method_mode,
      method_wb_mode = method_wb_mode, waterbody_mode = waterbody_mode,
      # The report's own sub-steps, mapped into the PDF's stretch of the bar.
      progress = function(fraction, detail)
        progress(starts[["pdf"]] + fraction * steps[["pdf"]] / sum(steps), detail)
    )
    files <- c(files, fw_pdf_filename())
  }
  if ("records" %in% parts) {
    step("records", "progress_records")
    fw_write_records_html(file.path(dir, fw_records_filename()), data, export,
                          filters, meta)
    files <- c(files, fw_records_filename())
  }

  # One file arrives as itself. Zipping a lone text file to save nothing would
  # make the reader unpack an archive to read two paragraphs.
  if (length(files) == 1) {
    file.copy(file.path(dir, files), path, overwrite = TRUE)
    progress(1, fw_t("plan", "progress_done"))
    return(invisible(path))
  }
  step("zip", "progress_zip")

  # WRITE TO A NAME ENDING IN .zip, THEN MOVE IT. The zip binary appends ".zip"
  # to an output name that has no extension, and a downloadHandler's temp file
  # never has one - so zipping straight to `path` wrote the archive beside it
  # and left `path` empty. The handler renames it on the way out anyway, using
  # fw_bundle_filename().
  #
  # utils::zip() shells out and reports failure through a status code rather
  # than a condition, so the status is checked: a download that silently hands
  # back a zero-byte file is worse than one that errors. Relative paths, from
  # inside the directory, so the archive carries no absolute path.
  archive <- tempfile(fileext = ".zip")
  wd <- setwd(dir); on.exit(setwd(wd), add = TRUE, after = FALSE)
  status <- utils::zip(archive, files, flags = "-q")
  if (!identical(as.integer(status), 0L) || !file.exists(archive)) {
    stop("Could not write the download archive (zip exit status ", status, ").",
         call. = FALSE)
  }
  file.copy(archive, path, overwrite = TRUE)
  unlink(archive)
  progress(1, fw_t("plan", "progress_done"))
  invisible(path)
}
