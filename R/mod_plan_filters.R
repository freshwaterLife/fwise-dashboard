# mod_plan_filters.R
# The report builder's filter PANEL. The filtering itself lives in R/filters.R
# and is shared with the dashboard, so the two pages cannot drift apart.
#
# THIS PAGE DROPS THE OUTCOME FILTER. That is the one place the two pages
# differ, and it is deliberate - see the note at the top of mod_plan.R.
#
# THE PANEL SITS ABOVE THE RESULTS, NOT BESIDE THEM. It used to be a sidebar
# shared with the dashboard. Three reasons it moved:
#
#   1. ORDER IS THE INSTRUCTION. This page is a form followed by its answer, and
#      the client's steer was that a reader should commit to a description of
#      their own situation before they see anything. A sidebar puts the
#      questions and the results side by side, which invites reading the results
#      first and reverse-engineering the filters until they say something nice.
#      Stacked, there is nothing to read until the questions have been answered.
#   2. BOTH HALVES GET THE FULL WIDTH. In the sidebar layout the results lived
#      in a column about 20rem narrower than the page, which is where the method
#      and duration charts were being crushed.
#   3. TEN CONTROLS IN A COLUMN IS A LONG SCROLL. Across a row they are a
#      glanceable grid - see .fw-plan-filters in _components.scss.
#
# The dashboard keeps its sidebar, because browsing IS watching the picture
# change under the controls and the controls have to stay in reach.

library(shiny)

# Passed to fw_filter_ids() everywhere on this page. Both are the client's
# decisions and the reasoning for each is at the top of mod_plan.R: outcome is
# an answer this page must not let the reader pre-select, and method is the
# thing the reader came here to learn rather than to assert. The two fish
# family filters are the Explore page's alone (Sept 2026 user testing).
FW_PLAN_DROP <- c("outcome", "method", "family", "family_beneficiary")

fw_plan_filter_ids <- function() fw_filter_ids(drop = FW_PLAN_DROP)

fw_plan_filters_ui <- function(ns, ch) {
  # fw_field() rather than a hand-built div.fw-field: it puts the label and the
  # information icon on one row (.fw-field__label-row) and is the same wrapper
  # the contribute form uses, so a filter and a form field behave identically
  # for a keyboard and a screen reader. This panel was the last place in the app
  # duplicating that markup by hand.
  multi <- function(id) {
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

  years <- function() {
    # The year range and its "include undated" companion are ONE cell of the
    # grid, not two. They are a single question, and a checkbox that wraps onto
    # a different row from the slider it qualifies reads as unrelated to it.
    div(
      class = "fw-plan-filters__span",
      div(
        class = "fw-field fw-field--range",
        div(
          class = "fw-field__label-row",
          tags$label(class = "form-label", `for` = ns("years"),
                     fw_filter_label("years")),
          fw_info(fw_filter_tip("years", ch), fw_filter_label("years"))
        ),
        # ticks = FALSE is not cosmetic. ionRangeSlider draws a grid of labels
        # across a 90-year span that overlap and pile up at both ends, which is
        # unreadable. The chosen range is printed underneath in words instead,
        # where it can always be read.
        sliderInput(ns("years"), label = NULL,
                    min = ch$year_min, max = ch$year_max,
                    value = c(ch$year_min, ch$year_max),
                    step = 1, sep = "", ticks = FALSE, dragRange = TRUE,
                    width = "100%"),
        div(class = "fw-field__range-readout",
            textOutput(ns("years_readout"), inline = TRUE))
      ),
      # DEFAULTS ON. A year range silently dropping every attempt with no start
      # year would quietly remove those records, and the user would never know
      # the difference between "none match" and "none were dated".
      #
      # The icon sits AFTER the checkbox rather than in a label row above it:
      # a checkbox carries its own inline label, and a second label above it
      # would read as a separate field.
      div(
        class = "fw-field fw-field--check",
        checkboxInput(ns("include_no_year"), fw_t("filters", "no_year"),
                      value = TRUE),
        fw_info(fw_fill(fw_t("filters", "tip_no_year"), n = ch$n_no_year),
                fw_t("filters", "no_year"))
      )
    )
  }

  # A PLACEHOLDER, not the control. Which sliders apply depends on the regime
  # selection, so the size cell is re-rendered on its own from the server. It
  # cannot be built here: rebuilding this whole panel to swap a slider would
  # return every selectize above to its default and throw away the reader's
  # other nine answers. See output$size_control in mod_plan.R.
  size <- function() {
    div(class = "fw-plan-filters__span", uiOutput(ns("size_control")))
  }

  controls <- lapply(fw_filter_draw_order(drop = FW_PLAN_DROP), function(id) {
    switch(FW_FILTERS[[id]]$kind,
           range = years(),
           size  = size(),
           multi(id))
  })

  tags$section(
    class = "fw-plan-filters",
    `aria-labelledby` = ns("filters_heading"),
    h2(id = ns("filters_heading"), class = "fw-plan-filters__heading fw-visually-hidden",
       fw_t("plan", "f_heading")),
    p(class = "fw-plan-filters__lead", fw_t("plan", "f_lead")),

    # COLLAPSIBLE, AND ONLY AFTER A BUILD. A native <details> rather than a
    # scripted panel: it opens and closes without JavaScript, it is a disclosure
    # to a screen reader for free, and the server only ever has to close it
    # (fw-collapse in R/ui_helpers.R). It is rendered ONCE and open; nothing
    # re-renders it, because that would reset every control inside.
    #
    # The summary carries a description of what was built, so a collapsed panel
    # still says what the reader is looking at rather than reading as a lid.
    tags$details(
      id = ns("filters_disclosure"), class = "fw-plan-filters__disclosure",
      open = NA,
      tags$summary(
        class = "fw-plan-filters__summary",
        span(class = "fw-plan-filters__summary-label", fw_t("plan", "f_heading")),
        uiOutput(ns("filters_summary"), inline = TRUE)
      ),
      div(class = "fw-plan-filters__grid", controls),
      div(
        class = "fw-plan-filters__actions",
        actionButton(ns("build"), fw_t("plan", "build"), class = "btn btn-primary"),
        actionButton(ns("clear"), fw_t("plan", "clear"),
                     class = "btn btn-outline-primary")
      )
    )
  )
}

#' The size cell: one log slider per unit the water-body selection is measured in
#'
#' TWO UNITS THAT CANNOT SHARE A SLIDER. Still water is measured in hectares and
#' flowing water in kilometres. Choosing "still water" above leaves the kilometre
#' slider with nothing to say, so only the applicable one is drawn - and the
#' heading says which unit the reader is looking at, rather than leaving them to
#' infer it from a regime they chose four controls further up.
#'
#' A SLIDER THAT IS NOT DRAWN DOES NOT FILTER. Shiny keeps the value of an input
#' whose UI has gone, so the drop has to happen in the state as well as here -
#' fw_filter_state() does it, and the reasoning is at fw_size_units() in
#' filters.R. Do not hide a slider without that.
#'
#' LOG SCALED, and not for elegance. Hectares run from 0.0014 to 237,500 with a
#' median of 3.4; on a linear slider every value a reader might want sits inside
#' the first pixel. The positions are log10 and the reader never sees one: the
#' readout underneath prints the bounds in real units, and the handle's own
#' bubble is converted by fwSizePretty in R/ui_helpers.R.
#'
#' @param units "ha", "km", or both, from fw_size_units()
fw_plan_size_ui <- function(ns, ch, units = FW_SIZE_UNITS) {
  units <- intersect(FW_SIZE_UNITS, units)
  # The heading carries the unit when there is one unit to carry. With both on
  # the page it names them both and each slider keeps its own label, because a
  # heading cannot say which of two sliders it is describing.
  one_unit <- length(units) == 1
  unit_phrase <- if (one_unit) fw_t("filters", paste0("size_in_", units)) else
    fw_t("filters", "size_in_both")

  slider <- function(unit) {
    r <- fw_size_log_range(ch$size[[unit]])
    # A unit with no rows in the data has no range to offer. Drawing a dead
    # slider would invite the reader to set a bound that matches nothing.
    if (is.null(r)) return(NULL)
    div(
      class = "fw-field fw-field--range",
      # Only when there are two of them. With one slider under a heading that
      # already names the unit, a second label saying it again is noise.
      if (!one_unit) {
        tags$label(class = "form-label", `for` = ns(paste0("size_", unit)),
                   fw_t("filters", paste0("unit_", unit)))
      },
      # ticks = FALSE for the same reason as the year slider: ionRangeSlider's
      # own labels would be log10 values, which is the one thing the reader must
      # never be shown. The handle bubbles are log10 too, which is what
      # fw_slider_prettify() hands to the client to convert.
      fw_slider_prettify(
        sliderInput(ns(paste0("size_", unit)), label = NULL,
                    min = r[1], max = r[2], value = r,
                    step = FW_SIZE_LOG_STEP, sep = "", ticks = FALSE,
                    dragRange = TRUE, width = "100%"),
        unit
      ),
      div(class = "fw-field__range-readout",
          textOutput(ns(paste0("size_readout_", unit)), inline = TRUE))
    )
  }

  tagList(
    div(
      class = "fw-field__label-row",
      tags$label(class = "form-label",
                 paste0(fw_filter_label("size"), " ", unit_phrase)),
      fw_info(fw_filter_tip("size", ch), fw_filter_label("size"))
    ),
    lapply(units, slider),
    # DEFAULTS ON, exactly as the year range's companion does: a size range
    # silently dropping every attempt with no size recorded would remove those
    # records, and the reader would never know the difference between "none
    # match" and "none were measured".
    div(
      class = "fw-field fw-field--check",
      checkboxInput(ns("include_no_size"), fw_t("filters", "no_size"),
                    value = TRUE),
      fw_info(fw_fill(fw_t("filters", "tip_no_size"), n = ch$size$n_no_size),
              fw_t("filters", "no_size"))
    )
  )
}
