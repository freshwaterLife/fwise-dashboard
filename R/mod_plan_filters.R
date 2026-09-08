# mod_plan_filters.R
# The filter panel for the report builder, and the filtering itself.
#
# ANY-OF, NOT ONE-OF. Species and methods live in bridge tables, so an attempt
# can carry several of each. Selecting "Rotenone" and "Draining" must match an
# attempt that used EITHER, not one that used both, and must not return the same
# attempt twice. That is why those two filters resolve through a set of attempt
# ids rather than through a column comparison.
#
# NO SIZE FILTER. Treated size is recorded in hectares for still water and
# kilometers for flowing water, which are different quantities that cannot share
# a slider, and 173 of 914 attempts have no size at all. Size is still in the
# table and the export; it is deliberately not filterable.
#
# EVERY OPTION LIST COMES FROM THE APPROVED DATA, because fw_load_data() has
# already filtered the tables it is handed. Nothing here needs to think about
# approval - see fw_filter_approved() in data_load.R.

library(shiny)
library(dplyr)

#' Choices for the filter panel, built once at startup
fw_plan_choices <- function(data) {
  species <- fw_species_label(data$species)

  # Ordered by how often each actually appears, so the answers a user is likely
  # to want are near the top rather than buried alphabetically.
  by_freq <- function(ids, labels) {
    tab <- sort(table(ids), decreasing = TRUE)
    unname(labels[match(names(tab), names(labels))])
  }

  inv <- data$attempt_species |> filter(role == "invasive")
  sp_labels <- setNames(species$label, species$species_id)

  list(
    continent = sort(unique(data$attempt$continent)),
    country   = sort(unique(data$attempt$country)),
    taxa      = sort(unique(species$taxa[species$species_id %in% inv$species_id &
                                           !is.na(species$taxa)])),
    species   = by_freq(inv$species_id, sp_labels),
    method    = data$method$method_name[order(match(
      data$method$method_id,
      names(sort(table(data$attempt_method$method_id), decreasing = TRUE))
    ))],
    regime    = sort(unique(data$attempt$water_regime[!is.na(data$attempt$water_regime)])),
    # FIXED, not derived. These four are the analysis categories the paper uses
    # and the interface must show all of them even if a slice contains none.
    outcome   = c("Successful", "Failed", "Ongoing", "Unknown"),
    year_min  = min(data$attempt$start_year, na.rm = TRUE),
    year_max  = max(data$attempt$start_year, na.rm = TRUE),
    n_no_year = sum(is.na(data$attempt$start_year))
  )
}

fw_plan_filters_ui <- function(ns, ch) {
  multi <- function(id, label, choices, help = NULL) {
    div(
      class = "fw-field",
      tags$label(class = "form-label", `for` = ns(id), label),
      if (!is.null(help)) div(class = "fw-field__help", help),
      selectizeInput(ns(id), label = NULL, choices = choices, selected = NULL,
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = "All", plugins = list("remove_button")))
    )
  }

  tagList(
    h2(class = "fw-filters__heading", fw_t("plan", "f_heading")),

    multi("continent", fw_t("plan", "f_continent"), ch$continent),
    multi("country",   fw_t("plan", "f_country"),   ch$country),
    multi("taxa",      fw_t("plan", "f_taxa"),      ch$taxa),
    multi("species",   fw_t("plan", "f_species"),   ch$species,
          help = fw_t("plan", "f_any_note")),
    multi("method",    fw_t("plan", "f_method"),    ch$method,
          help = fw_t("plan", "f_any_note")),
    multi("regime",    fw_t("plan", "f_regime"),    ch$regime),
    multi("outcome",   fw_t("plan", "f_outcome"),   ch$outcome),

    div(
      class = "fw-field",
      tags$label(class = "form-label", `for` = ns("years"), fw_t("plan", "f_years")),
      sliderInput(ns("years"), label = NULL,
                  min = ch$year_min, max = ch$year_max,
                  value = c(ch$year_min, ch$year_max),
                  step = 1, sep = "", width = "100%")
    ),
    # DEFAULTS ON. A year range silently dropping every attempt with no start
    # year would quietly remove 53 records, and the user would never know the
    # difference between "none match" and "none were dated".
    div(
      class = "fw-field fw-field--check",
      checkboxInput(ns("include_no_year"), fw_t("plan", "f_no_year"), value = TRUE),
      div(class = "fw-field__help",
          sub("{n}", ch$n_no_year, fw_t("plan", "f_no_year_help"), fixed = TRUE))
    ),

    div(
      class = "fw-plan__actions",
      actionButton(ns("build"), fw_t("plan", "build"), class = "btn btn-primary"),
      actionButton(ns("clear"), fw_t("plan", "clear"),
                   class = "btn btn-outline-primary btn-sm")
    )
  )
}

#' Read the filter inputs into a plain list
#'
#' Snapshotted at the moment Build is pressed, so the results and the export
#' describe the same selection even if the user then changes a control.
fw_plan_filter_state <- function(input) {
  g <- function(nm) {
    v <- input[[nm]]
    if (is.null(v)) character(0) else v[nzchar(v)]
  }
  list(
    continent = g("continent"), country = g("country"),
    taxa      = g("taxa"),      species = g("species"),
    method    = g("method"),    regime  = g("regime"),
    outcome   = g("outcome"),
    year_from = input$years[1], year_to = input$years[2],
    include_no_year = isTRUE(input$include_no_year)
  )
}

#' Apply the filters, returning the matching attempt rows
fw_plan_apply <- function(data, f) {
  a <- data$attempt

  keep_in <- function(a, col, vals) if (length(vals)) a[a[[col]] %in% vals, ] else a
  a <- keep_in(a, "continent",    f$continent)
  a <- keep_in(a, "country",      f$country)
  a <- keep_in(a, "water_regime", f$regime)
  a <- keep_in(a, "outcome",      f$outcome)

  # ANY-OF over the species bridge. Only invasive roles: filtering on a species
  # here means "attempts against this species", never "attempts that happened to
  # benefit it".
  if (length(f$species) || length(f$taxa)) {
    species <- fw_species_label(data$species)
    ids <- data$attempt_species |>
      filter(role == "invasive") |>
      left_join(select(species, species_id, label, taxa), by = "species_id")
    if (length(f$species)) ids <- filter(ids, label %in% f$species)
    if (length(f$taxa))    ids <- filter(ids, taxa  %in% f$taxa)
    a <- a[a$attempt_id %in% unique(ids$attempt_id), ]
  }

  if (length(f$method)) {
    ids <- data$attempt_method |>
      left_join(select(data$method, method_id, method_name), by = "method_id") |>
      filter(method_name %in% f$method)
    a <- a[a$attempt_id %in% unique(ids$attempt_id), ]
  }

  # The year range, and the deliberate decision about undated attempts.
  #
  # A missing bound means "no bound", not "nothing matches". The slider lives in
  # a renderUI, so its value is NULL until the browser paints it, and a NULL
  # silently collapsing the comparison to logical(0) would empty the whole
  # selection rather than leave it alone.
  from <- if (length(f$year_from)) f$year_from else -Inf
  to   <- if (length(f$year_to))   f$year_to   else  Inf
  in_range <- !is.na(a$start_year) & a$start_year >= from & a$start_year <= to
  a <- a[in_range | (is.na(a$start_year) & isTRUE(f$include_no_year)), ]

  a
}

#' Which filter to suggest relaxing when nothing matches
#'
#' Reported in the order that most often causes an empty result: the narrowest
#' choice first. Naming the filter is the difference between a dead end and a
#' next step.
fw_plan_zero_hints <- function(data, f) {
  # Drop one filter at a time and see which one alone unblocks the result.
  candidates <- c(species = "f_species", method = "f_method", taxa = "f_taxa",
                  country = "f_country", regime = "f_regime",
                  outcome = "f_outcome", continent = "f_continent")
  hints <- character(0)
  for (nm in names(candidates)) {
    if (!length(f[[nm]])) next
    relaxed <- f; relaxed[[nm]] <- character(0)
    if (nrow(fw_plan_apply(data, relaxed)) > 0) {
      hints <- c(hints, fw_t("plan", candidates[[nm]]))
    }
  }
  # The year range is the one filter that is always set, so it is only worth
  # suggesting when widening it actually helps.
  relaxed <- f
  relaxed$year_from <- -Inf; relaxed$year_to <- Inf; relaxed$include_no_year <- TRUE
  if (!length(hints) && nrow(fw_plan_apply(data, relaxed)) > 0) {
    hints <- fw_t("plan", "f_years")
  }
  hints
}
