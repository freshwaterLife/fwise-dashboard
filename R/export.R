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

# Columns deliberately NOT exported.
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
FW_EXPORT_EXCLUDE <- c("status", "notes_for_fwise", "last_updated", "submitted_at",
                       "consent_data_use", "sent", "eradication_or_control")

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

# ---- Contacts ----------------------------------------------------------------

#' The contacts attached to an extract, one row per person
#'
#' WHY THIS EXISTS BESIDE THE PER-ROW CONTACT COLUMNS. The attempts sheet
#' already carries primary_contact_* and secondary_contact_* on every row, which
#' answers "who recorded this attempt". This answers the different question the
#' report builder's contacts block asks - "who should I talk to" - by
#' deduplicating those people and counting how many of the attempts in front of
#' the reader each is attached to. The data dictionary says so, rather than
#' leaving a reader to wonder which of the two to trust.
#'
#' REDACTION happens in fw_contacts_summary() and nowhere else. This function
#' reads only from there, and fw_write_workbook() asserts the result before it
#' writes - see fw_assert_export_safe().
#'
#' @param attempt_ids the extract. NULL means every approved attempt.
fw_contacts_export <- function(data, attempt_ids = NULL) {
  contacts <- fw_contacts_summary(data)
  ids <- attempt_ids %||% data$attempt$attempt_id

  here <- vapply(contacts$attempt_ids, function(x) length(intersect(x, ids)),
                 integer(1))
  keep <- contacts[here > 0, , drop = FALSE]
  keep$attempts_in_extract <- here[here > 0]
  keep <- keep[order(-keep$attempts_in_extract, keep$contact_name), , drop = FALSE]

  out <- data.frame(
    contact_id         = keep$contact_id,
    contact_name       = keep$contact_name,
    organisation       = keep$organisation,
    contact_email      = keep$contact_email,
    continents         = keep$continent_label,
    countries          = keep$country_label,
    attempts_in_extract = keep$attempts_in_extract,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  fw_assert_export_safe(out, data)
  out
}

# ---- Field definitions -------------------------------------------------------

#' The data dictionary that travels with the export
#'
#' The definitions live in R/copy_export.R, one per exported column. This only
#' shapes them into the sheet.
#'
#' TWO FRAMES, ONE SHEET. The attempts columns come first and are exactly
#' FW_EXPORT_COLUMNS, in order - the tests check that and nothing should be
#' appended to `dictionary` that is not an exported column. The contacts sheet's
#' own columns follow under their own heading row, because a reader looking up a
#' column name does not know or care which of the two lists it is in.
fw_field_definitions <- function() {
  sheets <- fw_t("export", "sheets")
  # Column names are set HERE rather than at the end: rbind() on data frames
  # matches by name, not by position, so two frames built with auto-generated
  # names would not stack.
  row <- function(field, definition) {
    data.frame(field = field, definition = definition, stringsAsFactors = FALSE)
  }
  frame <- function(d) row(names(d), unname(d))
  group <- function(sheet) row(fw_fill(fw_t("export", "dict_group"), sheet = sheet), "")

  out <- rbind(
    frame(fw_t("export", "dictionary")),
    group(sheets$contacts),
    frame(fw_t("export", "dictionary_contacts"))
  )
  names(out) <- c(fw_t("export", "col_field"), fw_t("export", "col_definition"))
  rownames(out) <- NULL
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
#' FIVE SHEETS, ALWAYS. The data is useless to a careful reader without the other
#' four, and a reader who did not ask for the caveats is exactly the reader who
#' needs them. The contacts sheet is the newest: it is the one-row-per-person,
#' deduplicated view of the people behind the extract, which the per-row contact
#' columns on the attempts sheet cannot answer - see fw_contacts_export().
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
  # Asserted inside fw_contacts_export() before it gets here, and built from
  # fw_contacts_summary(), which redacts. Two controls, on purpose: an export is
  # the one place a mistake travels outside the building and cannot be recalled.
  add(sheets$contacts, fw_contacts_export(data, export$attempt_id),
      widths = c(20, 28, 34, 30, 22, 34, 18))
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
# ONE DOWNLOAD, ASSEMBLED FROM WHAT THE READER TICKED. The report builder used
# to offer a spreadsheet button and a report button; it now offers a picker and
# one button, and the methods-and-caveats text rides along whatever else is in
# there. See fw_plan_download_ui() in mod_plan.R.
#
# The writers themselves are UNCHANGED and are not duplicated here: this decides
# what goes in and calls fw_write_workbook(), fw_html_write_csv() and
# fw_write_html_report() exactly as the two buttons did.

# The parts a reader can choose, and the order they are written in. "pdf" is not
# a file of its own - it resolves to the HTML report, which carries the print
# stylesheet and a Save as PDF button. Ticking both is therefore not an error
# and does not produce two copies; see fw_bundle_parts().
FW_BUNDLE_PARTS <- c("xlsx", "csv", "html", "pdf")

#' Which files a selection actually produces
#'
#' "pdf" and "html" are the same file, so a reader who ticks both gets one copy
#' of it rather than a duplicate under a second name.
fw_bundle_parts <- function(parts = character(0)) {
  parts <- intersect(FW_BUNDLE_PARTS, parts %||% character(0))
  if ("pdf" %in% parts) parts <- unique(c(setdiff(parts, "pdf"), "html"))
  intersect(c("xlsx", "csv", "html"), parts)
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

#' Write the download
#'
#' @param path where to write - the download handler's temp file
#' @param parts what the reader ticked. Anything not recognised is ignored.
#' @param ... the report's own arguments, passed through to
#'   fw_write_html_report() so the document matches the screen it came from.
fw_write_bundle <- function(path, parts, data, sel, export, filters, meta = NULL,
                            method_mode = "count", method_wb_mode = "count") {
  parts <- fw_bundle_parts(parts)

  dir <- tempfile("fw-bundle-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # Always, and first, so it is the first thing in the archive listing.
  txt <- file.path(dir, fw_methods_filename())
  writeLines(fw_methods_caveats_text(data), txt, useBytes = TRUE)
  files <- fw_methods_filename()

  if ("xlsx" %in% parts) {
    fw_write_workbook(file.path(dir, fw_export_filename()), data, export,
                      filters, meta)
    files <- c(files, fw_export_filename())
  }
  if ("csv" %in% parts) {
    files <- c(files, basename(fw_html_write_csv(export, dir)))
  }
  if ("html" %in% parts) {
    fw_write_html_report(
      path = file.path(dir, fw_html_filename()), data = data, sel = sel,
      export = export, filters = filters, meta = meta,
      method_mode = method_mode, method_wb_mode = method_wb_mode
    )
    files <- c(files, fw_html_filename())
  }

  # One file arrives as itself. Zipping a lone text file to save nothing would
  # make the reader unpack an archive to read two paragraphs.
  if (length(files) == 1) {
    file.copy(file.path(dir, files), path, overwrite = TRUE)
    return(invisible(path))
  }

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
  invisible(path)
}
