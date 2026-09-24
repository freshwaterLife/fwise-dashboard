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
  "invasion_year", "start_year", "end_year", "duration_days", "reason",
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
  #
  # THE NAME GOES WITH THE ADDRESS (client, 23 Sept 2026). A contact who did
  # not agree to their address being published has not agreed to being named
  # either, so a private contact exports as a blank name, a blank organisation
  # and a blank address rather than as a named person we decline to put an
  # address beside. The attempt's own row still travels in full; only who to
  # ask about it is withheld.
  contacts <- data$contact |>
    mutate(
      contact_name  = if_else(email_public, contact_name,  NA_character_),
      organisation  = if_else(email_public, organisation,  NA_character_),
      contact_email = if_else(email_public, contact_email, NA_character_)
    ) |>
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
      invasion_year, start_year, end_year, duration_days, reason = driver,
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

#' Refuse to export the name or address of a contact who asked to stay private
#'
#' The redaction happens above, in fw_export_frame(). This is the control that
#' makes it a control rather than something we remembered to do: "we applied the
#' filter" is not a guarantee, and an export is the one place a mistake travels
#' outside the building and cannot be recalled.
#'
#' BOTH FIELDS, since 23 Sept 2026. The flag used to gate the address alone, so
#' a private contact still went out named; the client's rule is that no
#' personally identifying detail of theirs appears anywhere in the app or its
#' downloads. A name is identifying on its own, so it is checked here too.
fw_assert_export_safe <- function(x, data) {
  hidden <- !data$contact$email_public
  clean <- function(v) v[!is.na(v) & nzchar(v)]

  check <- function(values, pattern, what) {
    values <- clean(values)
    if (length(values) == 0) return(invisible(TRUE))
    cols <- grep(pattern, names(x), value = TRUE)
    leaked <- unique(unlist(lapply(cols, function(c) intersect(x[[c]], values))))
    if (length(leaked) > 0) {
      stop("Refusing to export: ", length(leaked), " ", what,
           " belonging to contacts who asked not to be listed.", call. = FALSE)
    }
    invisible(TRUE)
  }

  check(data$contact$contact_email[hidden], "_email$", "address(es)")
  # _contact_name$, not _name$: site_name is also a name column, and a site
  # that happened to share a string with a private contact would halt every
  # download rather than protect anybody.
  check(data$contact$contact_name[hidden], "_contact_name$", "name(s)")
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
#'
#' A BLOCK MAY HAVE NO HEADING and then prints none, rather than a blank line
#' where a heading would be. The placeholder standing in for the client's
#' caveats is one such block - see [PLACEHOLDER] in R/copy_export.R.
fw_caveats <- function(data) {
  blocks <- fw_caveat_blocks(data)
  out <- character(0)
  for (i in seq_along(blocks)) {
    h <- blocks[[i]]$heading
    if (length(h) && nzchar(h)) out <- c(out, toupper(h))
    out <- c(out, blocks[[i]]$body)
    if (i < length(blocks)) out <- c(out, "")
  }
  out
}

#' The methods statement, in the same shape as a caveat block
#'
#' So the closing section of every export is ONE LIST TO LOOP OVER: the methods
#' first, then whatever caveats there are. Kept as its own block rather than
#' folded into the caveats because the two are being written by different
#' people - see the two [PLACEHOLDER] notes in R/copy_export.R.
fw_methods_blocks <- function() {
  list(list(heading = fw_t("export", "methods_heading"),
            body = fw_t("export", "methods")))
}

#' The whole closing section: how it was built, then what to watch for
#'
#' THE LAST THING IN EVERY EXPORT - the workbook's last sheet, the PDF's last
#' section, the records HTML's last section. It used to be a text file that
#' travelled alongside them whatever the reader ticked; the client removed that
#' download on 24 Sept 2026, so each document now carries the section itself,
#' which is what the text file was for in the first place.
fw_closing_blocks <- function(data) {
  c(fw_methods_blocks(), fw_caveat_blocks(data))
}

#' The closing section as flat lines, for the workbook's last sheet
#'
#' The same two halves fw_closing_blocks() gives the PDF and the records HTML -
#' how the database was built, then what to watch for in it - flattened one
#' line per row because that is what a sheet can hold. Derived from the same
#' fw_caveats(), so the spreadsheet and the two documents cannot say different
#' things.
#'
#' THIS USED TO BE A .txt IN EVERY DOWNLOAD. The client removed that file on
#' 24 Sept 2026: a reader who ticks the PDF should get a PDF, not a zip holding
#' a PDF and a text file, and the three documents can each carry the section.
fw_methods_caveats_text <- function(data) {
  c(
    toupper(fw_t("export", "methods_heading")),
    fw_t("export", "methods"),
    "",
    fw_caveats(data)
  )
}

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
#' @param applied_only drop the filters the reader left alone. FALSE for the
#'   workbook, where the sheet is a record of the whole settings panel and a row
#'   reading "All" is a fact about the extract worth keeping. TRUE for the PDF
#'   report (client, 23 Sept 2026), where seventeen rows of mostly "All" pushed
#'   the reader's actual selection off the top of the page. The rule is the one
#'   the report builder's on-screen summary already applies - see
#'   output$filters_summary in mod_plan.R.
fw_filters_sheet <- function(filters, n_rows, n_total, meta = NULL,
                             applied_only = FALSE) {
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
  set <- fw_filter_summary(filters)
  if (applied_only) {
    # "All" is an untouched multi-select; "Yes" is an include-the-unrecorded
    # switch left at its default. Both mean the reader did not narrow on that
    # field. The provenance rows above are never dropped.
    skip <- c(fw_t("export", "filter_all"), fw_t("export", "filter_yes"))
    set <- Filter(function(r) {
      if (isTRUE(r$untouched)) return(FALSE)
      !identical(as.character(r$value), skip[1]) &&
        !identical(as.character(r$value), skip[2])
    }, set)
  }
  rows <- c(provenance, set)

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
#'
#' METHODS AND CAVEATS IS THE LAST TAB (client, 24 Sept 2026), where the PDF and
#' the records HTML also close. It carries what the retired
#' fwise-methods-and-caveats.txt used to carry out of the building on its own.
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

  # THE ADD ORDER IS THE TAB ORDER, and it matches `sheets` in R/copy_export.R.
  # Move one there and move it here.
  sheets <- fw_t("export", "sheets")
  add(sheets$attempts, export)
  add(sheets$definitions, fw_field_definitions(), widths = c(26, 100))
  add(sheets$filters,
      fw_filters_sheet(filters, nrow(export), nrow(data$attempt), meta),
      widths = c(30, 60))
  add(sheets$caveats,
      fw_text_sheet(fw_methods_caveats_text(data), fw_t("export", "closing_heading")),
      widths = 110)

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
# a picker and one button. See fw_plan_download_ui() in mod_plan.R.
#
# NOTHING RIDES ALONG ANY MORE (client, 24 Sept 2026). A methods-and-caveats
# .txt used to go into every download whatever was ticked, which meant a reader
# who wanted the PDF got a zip holding a PDF and a text file. The section it
# carried is now the last tab of the workbook and the last section of the PDF
# and the records HTML, so ticking one thing downloads that one thing.
#
# THREE PARTS, at the client's request (23 Sept 2026): the PDF report, the
# attempts file and the spreadsheet. The interactive HTML report they replace -
# live plotly charts and a leaflet map, printed to PDF through the browser - is
# gone; the PDF is now a real one, made on the server.
#
# THE CSV WENT WITH THAT ROUND. It was the .xlsx's rows a second time in a
# plainer wrapper, and offering the same data twice made the picker read as
# four decisions when it is three. A reader who wants delimited text opens the
# workbook and saves it as one.

# The parts a reader can choose, in the order they are written. The download
# picker lists them in this order too - see fw_plan_download_ui() in mod_plan.R.
FW_BUNDLE_PARTS <- c("pdf", "records", "xlsx")

#' Which files a selection actually produces
#'
#' Anything not recognised is dropped. The PDF is dropped too when this server
#' cannot make one (no Quarto): the picker has already told the reader so, and
#' the rest of their download should not fail with it.
fw_bundle_parts <- function(parts = character(0)) {
  parts <- intersect(FW_BUNDLE_PARTS, parts %||% character(0))
  if (!fw_pdf_available()) parts <- setdiff(parts, "pdf")
  # A DEFENSIVE FLOOR, NOT A FEATURE. The download button is disabled while
  # nothing is ticked (see fw_plan_download_ui() in mod_plan.R), so an empty
  # selection cannot be asked for from the page. This is here so that a request
  # that arrives empty anyway - a stale browser, the PDF dropped above as the
  # only tick - hands back a real spreadsheet rather than a zero-byte file.
  # It also keeps fw_bundle_filename() and fw_write_bundle() answering the same
  # question, which they must: the handler names the file before it writes it.
  if (!length(parts)) parts <- "xlsx"
  parts
}

#' What one part is called on its own
#'
#' The single-file case below, and the name each part is written under inside a
#' zip. One place, so the handler's filename() and fw_write_bundle() cannot
#' disagree about what a lone PDF is called.
FW_BUNDLE_FILENAME <- list(
  pdf     = function() fw_pdf_filename(),
  records = function() fw_records_filename(),
  xlsx    = function() fw_export_filename()
)

#' What a download of this selection will be called
#'
#' A zip when more than one thing was ticked, THE FILE ITSELF WHEN ONE WAS
#' (client, 24 Sept 2026). Ticking the PDF and being handed a zip was the whole
#' complaint, and it was the methods-and-caveats text - which always travelled -
#' that made every download a zip of two things.
fw_bundle_filename <- function(parts = character(0)) {
  parts <- fw_bundle_parts(parts)
  if (length(parts) == 1) return(FW_BUNDLE_FILENAME[[parts]]())
  paste0(fw_t("export", "bundle_stem"), format(Sys.Date(), "%Y%m%d"), ".zip")
}

#' Write the download
#'
#' @param path where to write - the download handler's temp file
#' @param parts what the reader ticked. Anything not recognised is ignored.
#' @param method_mode,waterbody_mode the chart toggles, passed to
#'   fw_write_pdf_report() so the document matches the screen it came from.
#' @param progress called as progress(value, detail) before each step, value
#'   running 0 to 1 across the whole bundle. The handler passes Shiny's
#'   setProgress(); the default does nothing, so the tests need no session.
fw_write_bundle <- function(path, parts, data, sel, export, filters, meta = NULL,
                            method_mode = "count",
                            waterbody_mode = "count",
                            progress = function(value, detail) NULL) {
  parts <- fw_bundle_parts(parts)

  # THE BAR IS WEIGHTED BY WHAT TAKES THE TIME, not by the number of steps: the
  # PDF (Quarto on the server) is most of any bundle that has one, and a bar
  # that spent a third of itself on the spreadsheet would stall at the end.
  #
  # THE ZIP STEP ONLY EXISTS WHEN THERE IS MORE THAN ONE FILE. It used to be
  # claimed whenever anything was ticked, which was right only because the
  # methods text guaranteed a second file; with that gone, a lone PDF never
  # reaches the zip and a bar holding a twelfth back for it would stop short.
  steps <- c(xlsx = 2, pdf = 12, records = 3, zip = 1)
  steps <- steps[c(parts, if (length(parts) > 1) "zip")]
  ends <- cumsum(steps) / sum(steps)
  starts <- stats::setNames(c(0, utils::head(ends, -1)), names(steps))
  step <- function(id, key) progress(starts[[id]], fw_t("plan", key))

  dir <- tempfile("fw-bundle-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  files <- character(0)

  # THE WRITE ORDER IS FW_BUNDLE_PARTS' ORDER, and that is load-bearing rather
  # than tidy. fw_bundle_parts() above has already sorted `parts` into the
  # canonical order, and the weighted bar computes each step's start from that
  # same sequence - so a block written out of sequence reports a fraction it
  # has already passed and the progress bar jumps backwards. If a part moves in
  # FW_BUNDLE_PARTS, move its block here to match.
  if ("pdf" %in% parts) {
    fw_write_pdf_report(
      path = file.path(dir, fw_pdf_filename()), data = data, sel = sel,
      filters = filters, meta = meta, method_mode = method_mode,
      waterbody_mode = waterbody_mode,
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

  if ("xlsx" %in% parts) {
    step("xlsx", "progress_xlsx")
    fw_write_workbook(file.path(dir, fw_export_filename()), data, export,
                      filters, meta)
    files <- c(files, fw_export_filename())
  }
  # ONE FILE ARRIVES AS ITSELF (client, 24 Sept 2026). fw_bundle_filename() has
  # already named it, and it must reach the same conclusion as this branch.
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
