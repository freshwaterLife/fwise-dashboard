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

# The delimiter for every multi-value column. Chosen over a comma because
# references, site names and notes are full of commas, and over the source's
# underscore because species names contain them far less predictably than they
# contain spaces.
FW_MULTI_SEP <- "; "

# Column order of the export, and the label each column carries. Order is the
# order a person reads a record in: where, what, when, how, what happened, who.
FW_EXPORT_COLUMNS <- c(
  "attempt_id",
  "site_name", "country", "region", "continent", "iso3", "latitude", "longitude",
  "waterbody_type", "water_regime", "area_treated", "area_unit", "area_notes",
  "depth_m", "volume_m3", "max_flow_m3s", "water_temp_c",
  "invasive_species", "invasive_taxa",
  "invasion_year", "start_year", "end_year", "duration_days", "driver",
  "beneficiary_species", "beneficiary_taxa",
  "methods", "method_classes", "method_notes", "method_description",
  "labour_person_days", "cost_estimate", "cost_currency",
  "toxin_conc_mg_l", "conc_target_notes", "conc_measured_notes",
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
# `last_updated` - a property of the row's maintenance, not of the eradication.
FW_EXPORT_EXCLUDE <- c("status", "notes_for_fwise", "last_updated")

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
      depth_m, volume_m3, max_flow_m3s, water_temp_c,
      invasion_year, start_year, end_year, duration_days, driver,
      method_description, labour_person_days, cost_estimate, cost_currency,
      toxin_conc_mg_l, conc_target_notes, conc_measured_notes,
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

#' The caveats, computed from the data rather than written down
#'
#' THE SAME TEXT travels into every export and sits beside every result on the
#' report builder. That is a requirement, not a nicety: a spreadsheet that turns
#' up in somebody's inbox six months later has to carry its own qualifications,
#' because by then nobody remembers what was on screen when it was downloaded.
#'
#' The counts are derived so they cannot drift away from the data they describe.
#' A caveat with a stale number in it is worse than no caveat, because it reads
#' as precision.
fw_caveats <- function(data) {
  a <- data$attempt
  n <- nrow(a)
  pct <- function(x) paste0(round(100 * x / n), "%")

  successful  <- sum(a$outcome == "Successful", na.rm = TRUE)
  unverified  <- sum(a$outcome == "Successful" &
                       (is.na(a$verification_notes) | a$verification_notes == ""),
                     na.rm = TRUE)
  no_size     <- sum(is.na(a$area_treated))
  no_start    <- sum(is.na(a$start_year))
  no_end      <- sum(is.na(a$end_year))

  c(
    "HOW SUCCESS IS DEFINED",
    paste(
      "Success follows Genovesi (2005): the complete and permanent removal of",
      "all wild populations of a species from a defined area, by a time-limited",
      "campaign. It is a binary judgement about an attempt, not a measure of",
      "how well the attempt went."
    ),
    "",
    "CLAIMED IS NOT THE SAME AS VALIDATED",
    paste0(
      "An outcome may be recorded as successful without formal proof of absence. ",
      "Of the ", format(successful, big.mark = ","), " attempts recorded as ",
      "successful, ", format(unverified, big.mark = ","), " carry no verification ",
      "note. Treat the success count as claimed outcomes unless you have checked ",
      "the verification fields on the rows you are relying on."
    ),
    "",
    "ALL FOUR OUTCOMES CARRY INFORMATION",
    paste(
      "Successful, Failed, Ongoing and Unknown are reported separately and are",
      "not collapsed into a success rate. Failure teaches as much as success,",
      "and ongoing attempts indicate where the next results will come from.",
      "Any percentage computed from this data should state its denominator."
    ),
    "",
    "MISSING VALUES",
    paste0(
      "Fields are missing at meaningful rates and blanks are genuine absences, ",
      "not zeros. In this dataset: ", format(no_size, big.mark = ","), " attempts (",
      pct(no_size), ") have no treated size, ", format(no_start, big.mark = ","),
      " (", pct(no_start), ") have no start year, and ", format(no_end, big.mark = ","),
      " (", pct(no_end), ") have no end year. Many of the last group are ongoing."
    ),
    "",
    "SIZES ARE NOT COMPARABLE ACROSS UNITS",
    paste(
      "Treated size is recorded in hectares for some attempts and kilometers for",
      "others, in the area_unit column. An area and a length are different",
      "quantities. Never total, average or rank the area_treated column without",
      "splitting it by unit first."
    ),
    "",
    "WHAT THIS EVIDENCE BASE ACTUALLY SHOWS",
    paste(
      "FWISE records where eradication work has been REPORTED, not where it has",
      "happened. Attempts that were never written up, never published in a",
      "language or venue the compilers reached, or never shared by the people who",
      "ran them are absent. Successful attempts are more likely to be written up",
      "than failed ones. So the geographic spread describes the reporting, and",
      "the outcome mix is likely to be more favourable than reality. An absence",
      "in this data is not evidence that nothing happened."
    ),
    "",
    "MULTI-VALUE FIELDS",
    paste(
      "Species, taxa and methods are semicolon-delimited lists in a single",
      "column. Split on '; '. The underscore nesting used in the source",
      "spreadsheet does not appear here."
    )
  )
}

# ---- Field definitions -------------------------------------------------------

#' The data dictionary that travels with the export
fw_field_definitions <- function() {
  d <- function(field, definition) data.frame(Field = field, Definition = definition,
                                              stringsAsFactors = FALSE)
  do.call(rbind, list(
    d("attempt_id", "Permanent identifier for the attempt. Minted once and never reassigned, so it is safe to join on across releases."),
    d("site_name", "The treated site or waterbody, as the source described it."),
    d("country", "Country the site sits in. Territories recorded separately appear in region."),
    d("region", "State, province or territory, where the country alone is not specific enough."),
    d("continent", "Derived from country and region, so a territory is assigned its own continent."),
    d("iso3", "ISO 3166-1 alpha-3 country code."),
    d("latitude", "Decimal degrees, WGS 84. Positive north."),
    d("longitude", "Decimal degrees, WGS 84. Positive east."),
    d("waterbody_type", "The kind of waterbody treated, e.g. Lake, Pond, Stream."),
    d("water_regime", "Lentic (still water) or Lotic (flowing water)."),
    d("area_treated", "Size of the treated area. NOT COMPARABLE ACROSS UNITS - read area_unit."),
    d("area_unit", "ha (hectares, an area) or km (kilometers, a length). Do not combine the two."),
    d("area_notes", "Free text qualifying the size figure."),
    d("depth_m", "Average or estimated depth, meters."),
    d("volume_m3", "Estimated volume, cubic meters."),
    d("max_flow_m3s", "Maximum flow, cubic meters per second. Flowing water only."),
    d("water_temp_c", "Water temperature, degrees Celsius."),
    d("invasive_species", "Species targeted, as 'Common name (Scientific name)'. Semicolon-delimited."),
    d("invasive_taxa", "Broad group of each target, e.g. Fish, Crayfish. Semicolon-delimited."),
    d("invasion_year", "Year the invasion is recorded as having happened, where known."),
    d("start_year", "Year the eradication attempt began."),
    d("end_year", "Year the attempt ended. Blank where the attempt is ongoing."),
    d("duration_days", "Estimated total duration of the intervention, days."),
    d("driver", "The main reason the eradication was carried out."),
    d("beneficiary_species", "Species the eradication was intended to help. Semicolon-delimited. Under-reported - see the caveats."),
    d("beneficiary_taxa", "Broad group of each beneficiary. Semicolon-delimited."),
    d("methods", "Methods used, semicolon-delimited. An unordered set, not a ranking."),
    d("method_classes", "chemical, mechanical or other, for each method used."),
    d("method_notes", "Free text on how each method was applied."),
    d("method_description", "A fuller description of the approach at this site."),
    d("labour_person_days", "Effort required, person-days."),
    d("cost_estimate", "Estimated cost, where recorded."),
    d("cost_currency", "Currency of cost_estimate."),
    d("toxin_conc_mg_l", "Target toxin concentration, mg/L. Chemical methods only."),
    d("conc_target_notes", "Free text on the target concentration."),
    d("conc_measured_notes", "Free text on the measured concentration, where it differed."),
    d("neutralising_agent", "Neutralizing agent used, where any."),
    d("neutralising_notes", "Free text on the neutralizing agent."),
    d("outcome", "Successful, Failed, Ongoing or Unknown. See the caveats for the success definition."),
    d("verification_method", "How the outcome was verified."),
    d("verification_notes", "Free text on verification. Blank on a successful attempt means the success is claimed rather than validated."),
    d("reference", "Citation for the underlying evidence."),
    d("reference_link", "DOI or URL for the reference, where one exists."),
    d("source", "Where the record came from."),
    d("primary_contact_name", "Person associated with the attempt."),
    d("primary_contact_org", "Their organization."),
    d("primary_contact_email", "Their email, where they agreed to it being listed. Blank means no published address, not no contact."),
    d("secondary_contact_name", "A second person associated with the attempt."),
    d("secondary_contact_org", "Their organization."),
    d("secondary_contact_email", "Their email, where they agreed to it being listed.")
  ))
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
fw_filters_sheet <- function(filters, n_rows, n_total, meta = NULL) {
  val <- function(x) {
    if (is.null(x) || length(x) == 0 || all(!nzchar(as.character(x)))) "All"
    else paste(x, collapse = ", ")
  }
  data.frame(
    Setting = c(
      "Generated at (UTC)", "Attempts in this extract", "Attempts in the database",
      "Data release", "Continent", "Country", "Invasive species", "Invasive group",
      "Method", "Waterbody type", "Outcome", "Start year from", "Start year to",
      "Attempts with no start year"
    ),
    Value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format(n_rows,  big.mark = ","),
      format(n_total, big.mark = ","),
      meta$release %||% "unknown",
      val(filters$continent), val(filters$country),
      val(filters$species), val(filters$taxa), val(filters$method),
      val(filters$regime), val(filters$outcome),
      val(filters$year_from), val(filters$year_to),
      if (isTRUE(filters$include_no_year)) "Included" else "Excluded"
    ),
    stringsAsFactors = FALSE
  )
}

#' Write the workbook
#'
#' Four sheets, always. The data is useless to a careful reader without the other
#' three, and a reader who did not ask for the caveats is exactly the reader who
#' needs them.
fw_write_workbook <- function(path, data, export, filters, meta = NULL) {
  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(
    fgFill = FW_COLOURS$deep, fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "left", border = "bottom", borderColour = FW_COLOURS$deep
  )
  add <- function(name, x, widths = "auto") {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, x, headerStyle = header)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(max(1, ncol(x))), widths = widths)
  }

  add("Attempts", export)
  add("Caveats", fw_text_sheet(fw_caveats(data), "Caveats and limitations"), widths = 110)
  add("Field definitions", fw_field_definitions(), widths = c(26, 100))
  add("Filters applied",
      fw_filters_sheet(filters, nrow(export), nrow(data$attempt), meta),
      widths = c(30, 60))

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Filename for a download
fw_export_filename <- function() {
  paste0("fwise-attempts_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
}
