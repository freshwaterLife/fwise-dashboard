# mod_plan_filters.R
# Plan's filter panel, a grid above the results (so readers describe their
# situation before seeing anything). The filter engine is R/filters.R.

library(shiny)

# Not offered on Plan (client): outcome and method are what the reader comes
# to learn, not to pre-select. Filtering on success breeds false optimism.
FW_PLAN_DROP = c("outcome", "method")

fw_plan_filter_ids = function() fw_filter_ids(drop = FW_PLAN_DROP)

fw_plan_filters_ui = function(ns, ch) {
  # fw_field(), as on the contribute form: label and (i) on one row.
  multi = function(id) {
    fw_field(
      selectizeInput(
        ns(id), label = NULL, choices = ch[[id]], selected = NULL,
        multiple = TRUE, width = "100%",
        options = list(placeholder = fw_t("filters", "all"),
                       plugins = list("remove_button"))
      ),
      label = fw_filter_label(id),
      tooltip = fw_filter_tip(id, ch),
      input_id = ns(id)
    )
  }

  years = function() {
    # Slider and "include undated" share one grid cell.
    div(
      class = "fw-plan-filters__span",
      div(
        class = "fw-field fw-field--range",
        div(
          class = "fw-field__label-row",
          fw_with_info(
            fw_filter_label("years"),
            fw_info(fw_filter_tip("years", ch), fw_filter_label("years")),
            wrap = function(x) tags$label(class = "form-label", `for` = ns("years"), x)
          )
        ),
        # No ticks (they pile up); the range is printed underneath.
        sliderInput(ns("years"), label = NULL,
                    min = ch$year_min, max = ch$year_max,
                    value = c(ch$year_min, ch$year_max),
                    step = 1, sep = "", ticks = FALSE, dragRange = TRUE,
                    width = "100%"),
        fw_range_ends(ch$year_min, ch$year_max),
        div(class = "fw-field__range-readout",
            textOutput(ns("years_readout"), inline = TRUE))
      ),
      # On by default, so undated attempts aren't silently dropped. The (i)
      # follows the checkbox's inline label.
      div(
        class = "fw-field fw-field--check",
        checkboxInput(ns("include_no_year"), fw_t("filters", "no_year"),
                      value = TRUE),
        fw_info(fw_fill(fw_t("filters", "tip_no_year"), n = ch$n_no_year),
                fw_t("filters", "no_year"))
      )
    )
  }

  # A slot for the size cell, rendered on its own (output$size_control) so a
  # regime change doesn't reset the whole panel.
  size = function() {
    div(class = "fw-plan-filters__span", uiOutput(ns("size_control")))
  }

  controls = lapply(fw_filter_draw_order(drop = FW_PLAN_DROP), function(id) {
    switch(FW_FILTERS[[id]]$kind,
           range = years(),
           size  = size(),
           fw_filter_when_panel(ns, id, multi(id)))
  })

  tags$section(
    class = "fw-plan-filters",
    `aria-labelledby` = ns("filters_heading"),
    h2(id = ns("filters_heading"), class = "fw-plan-filters__heading fw-visually-hidden",
       fw_t("plan", "f_heading")),

    # Always open (client).
    div(class = "fw-plan-filters__grid", controls),
    div(
      class = "fw-plan-filters__actions",
      actionButton(ns("build"), fw_t("plan", "build"), class = "btn btn-primary"),
      actionButton(ns("clear"), fw_t("plan", "clear"),
                   class = "btn btn-outline-primary")
    )
  )
}

#' The size cell: one log slider per unit the regime selection allows
#'
#' Hidden sliders must also be dropped from the state (fw_filter_state()), or
#' they keep filtering. Positions are log10(1 + x); the readout and handle
#' bubbles show real units (fwSizePretty in ui_helpers.R). ionRangeSlider's own
#' end labels are off; fw_range_ends() prints the whole scale instead.
#'
#' @param units "ha", "km", or both, from fw_size_units()
fw_plan_size_ui = function(ns, ch, units = FW_SIZE_UNITS) {
  units = intersect(FW_SIZE_UNITS, units)
  # One unit: it goes in the heading. Both: each slider has its own label.
  one_unit = length(units) == 1
  unit_phrase = if (one_unit) fw_t("filters", paste0("size_in_", units)) else
    fw_t("filters", "size_in_both")

  slider = function(unit) {
    r = fw_size_log_range(ch$size[[unit]])
    # No slider for a unit with no data.
    if (is.null(r)) return(NULL)
    div(
      class = "fw-field fw-field--range",
      # Only when there are two sliders.
      if (!one_unit) {
        tags$label(class = "form-label", `for` = ns(paste0("size_", unit)),
                   fw_t("filters", paste0("unit_", unit)))
      },
      # No ticks: they'd show log10 values. Bubbles are converted by
      # fw_slider_prettify().
      fw_slider_prettify(
        sliderInput(ns(paste0("size_", unit)), label = NULL,
                    min = r[1], max = r[2], value = r,
                    step = fw_size_log_step(r), sep = "", ticks = FALSE,
                    dragRange = TRUE, width = "100%"),
        unit
      ),
      fw_range_ends(
        paste(fw_size_label(fw_size_unlog(r[1])), fw_t("filters", paste0("unit_short_", unit))),
        paste(fw_size_label(fw_size_unlog(r[2])), fw_t("filters", paste0("unit_short_", unit)))
      ),
      div(class = "fw-field__range-readout",
          textOutput(ns(paste0("size_readout_", unit)), inline = TRUE))
    )
  }

  tagList(
    div(
      class = "fw-field__label-row",
      fw_with_info(
        paste0(fw_filter_label("size"), " ", unit_phrase),
        fw_info(fw_filter_tip("size", ch), fw_filter_label("size")),
        wrap = function(x) tags$label(class = "form-label", x)
      )
    ),
    lapply(units, slider),
    # On by default, so unmeasured attempts aren't silently dropped.
    div(
      class = "fw-field fw-field--check",
      checkboxInput(ns("include_no_size"), fw_t("filters", "no_size"),
                    value = TRUE),
      fw_info(fw_fill(fw_t("filters", "tip_no_size"), n = ch$size$n_no_size),
              fw_t("filters", "no_size"))
    )
  )
}

#' A slider's two ends, printed as a fixed line under it
#'
#' Always the whole scale. Used by the size and year sliders.
fw_range_ends = function(lo, hi) {
  div(class = "fw-field__range-ends", `aria-hidden` = "true",
      span(lo), span(hi))
}
