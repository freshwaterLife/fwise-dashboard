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

# Passed to fw_filter_ids() everywhere on this page.
FW_PLAN_DROP <- "outcome"

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

  controls <- lapply(fw_filter_draw_order(drop = FW_PLAN_DROP), function(id) {
    if (identical(FW_FILTERS[[id]]$kind, "range")) years() else multi(id)
  })

  tags$section(
    class = "fw-plan-filters",
    `aria-labelledby` = ns("filters_heading"),
    h2(id = ns("filters_heading"), class = "fw-plan-filters__heading",
       fw_t("plan", "f_heading")),
    p(class = "fw-plan-filters__lead", fw_t("plan", "f_lead")),
    div(class = "fw-plan-filters__grid", controls),
    div(
      class = "fw-plan-filters__actions",
      actionButton(ns("build"), fw_t("plan", "build"), class = "btn btn-primary"),
      actionButton(ns("clear"), fw_t("plan", "clear"),
                   class = "btn btn-outline-primary")
    )
  )
}
