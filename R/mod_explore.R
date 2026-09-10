# mod_explore.R
# THE DASHBOARD. The whole database at a glance, live under the filters.
#
# LIVE, NOT GATED - THE OPPOSITE OF THE REPORT BUILDER, ON PURPOSE. The report
# builder makes the user commit to a selection before it draws anything, because
# a report is something they will cite. This page is for browsing, where the
# whole value is watching the picture change as you move a control. Do not
# "make the two pages consistent": the difference is the design.
#
# OUTCOME IS FILTERABLE HERE and is deliberately NOT filterable on the report
# builder. Reasoning in mod_plan.R.
#
# LAYOUT follows the client's steer: the map is the lead visual, full width at
# the top of the results column, with the plots stacked BELOW it rather than
# beside it. On a laptop, map and figures side by side is too much at once.
#
# NO SUCCESS-RATE HEADLINE. The metrics framework proposed one; it was declined.
# See the note in charts.R.

library(shiny)
library(dplyr)

# ---- STUBBED ON REQUEST ------------------------------------------------------
#
# The page is back to the shared "in development" panel. NOTHING BELOW WAS
# DELETED: the whole dashboard - filters, KPI strip, map and every chart - is
# still in this file and still wired to its server, it is simply not rendered.
#
# TO PUT IT BACK, make mod_explore_ui() call fw_explore_full_ui(id) again. That
# is the only change; the server needs no edit, because Shiny does not run a
# render function whose output is absent from the page, so the charts below cost
# nothing while the stub is up.
#
# TWO ROUTES NOW LAND ON THE STUB, and both were built to arrive somewhere
# useful:
#   - Networking's "view this contact's attempts" (mod_networking.R) sets a
#     contact request and navigates here expecting the dashboard to narrow to
#     that person. The request is still set and still read; there is just
#     nothing on screen to show it.
#   - The contribute form's "explore the data" button (mod_contribute.R).
# Neither is broken, but neither does anything worth doing until this is
# unstubbed. Decide about them at the same time as the page.
mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description")),
    tags$main(
      id = "fw-main",
      fw_section(fw_container(fw_stub_panel()))
    )
  )
}

#' The built dashboard, kept whole while the page is stubbed
#'
#' See the note on mod_explore_ui() above. This is the page as it was.
fw_explore_full_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          fw_sidebar_layout(
            sidebar = uiOutput(ns("filters")),
            main = tagList(
              uiOutput(ns("incoming")),
              uiOutput(ns("kpis")),

              fw_plan_block(
                fw_t("explore", "map"), fw_t("explore", "map_note"),
                fw_map_output(ns("map"))
              ),

              fw_plan_block(
                fw_t("plan", "r_cumulative"), fw_t("plan", "r_cumulative_note"),
                plotly::plotlyOutput(ns("cumulative"), height = "auto")
              ),

              fw_plan_block(
                fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
                uiOutput(ns("outcomes"))
              ),

              fw_plan_block(
                fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
                tagList(
                  div(
                    class = "fw-segmented",
                    radioButtons(
                      ns("method_mode"), label = fw_t("plan", "r_method_mode"),
                      choices = stats::setNames(
                        c("share", "count"),
                        c(fw_t("plan", "r_method_share"),
                          fw_t("plan", "r_method_count"))
                      ),
                      selected = "share", inline = TRUE
                    )
                  ),
                  plotly::plotlyOutput(ns("methods"), height = "auto")
                )
              ),

              fw_plan_block(
                fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
                plotly::plotlyOutput(ns("duration"), height = "auto")
              ),

              fw_plan_block(
                fw_t("explore", "waterbody"), fw_t("explore", "waterbody_note"),
                plotly::plotlyOutput(ns("waterbody"), height = "auto")
              ),

              fw_plan_block(
                fw_t("explore", "driver"), fw_t("explore", "driver_note"),
                plotly::plotlyOutput(ns("driver"), height = "auto")
              ),

              fw_plan_block(
                fw_t("explore", "invasive"), fw_t("explore", "invasive_note"),
                plotly::plotlyOutput(ns("invasive"), height = "auto")
              ),

              # SUPPLEMENTARY AND CAVEATED, at the bottom, because the reporting
              # bias here is severe enough that a reader who takes it at face
              # value will draw the wrong conclusion.
              fw_plan_block(
                fw_t("explore", "beneficiary"), fw_t("explore", "beneficiary_note"),
                plotly::plotlyOutput(ns("beneficiary"), height = "auto")
              ),

              fw_plan_caveats_ui_deferred(ns)
            ),
            summary = fw_t("explore", "f_heading")
          )
        )
      )
    )
  )
}

# The caveats need the data, which the UI function does not have. One output
# rather than passing FW_DATA into a UI builder.
fw_plan_caveats_ui_deferred <- function(ns) uiOutput(ns("caveats"))

#' @param in_review how many submissions are waiting on review. Passed in
#'   rather than read from a global: app.R evaluates in its own environment, so
#'   FW_IN_REVIEW is not visible to a module sourced from R/.
mod_explore_server <- function(id, data, in_review = 0L) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choices <- fw_filter_choices(data)
    ids <- fw_filter_ids()

    output$filters <- renderUI(fw_explore_filters_ui(ns, choices))
    output$caveats <- renderUI(fw_plan_caveats_ui(data))

    observeEvent(input$clear, {
      fw_filter_clear(session, ids, choices)
      fw_set_explore_request(NULL)
    })

    output$years_readout <- renderText({
      y <- input$years
      if (is.null(y)) return("")
      paste(y[1], fw_t("filters", "range_of"), y[2])
    })

    # ---- The selection ------------------------------------------------------
    #
    # Live. Every visual below reads this one reactive, so they cannot disagree
    # about what is being shown.
    sel <- reactive({
      out <- fw_filter_apply(data, fw_filter_state(input, ids))
      # A deep link from the Networking page narrows to one person's attempts.
      # Applied AFTER the filters so the sidebar still does what it says.
      cid <- fw_explore_request()
      if (!is.null(cid)) {
        keep <- fw_contact_attempt_ids(data, cid)
        out <- out[out$attempt_id %in% keep, ]
      }
      out
    })

    # ---- Arriving from the Networking page ----------------------------------

    output$incoming <- renderUI({
      cid <- fw_explore_request()
      req(cid)
      contact <- data$contact[data$contact$contact_id == cid, ]
      if (nrow(contact) == 0) return(NULL)
      div(
        class = "fw-notice", role = "status",
        span(sub("{name}", contact$contact_name[1],
                 fw_t("explore", "incoming"), fixed = TRUE)),
        actionButton(ns("clear_contact"), fw_t("explore", "incoming_clear"),
                     class = "btn btn-outline-primary btn-sm")
      )
    })

    observeEvent(input$clear_contact, fw_set_explore_request(NULL))

    # ---- The visuals --------------------------------------------------------

    output$kpis <- renderUI({
      s <- sel()
      inv <- data$attempt_species |>
        filter(attempt_id %in% s$attempt_id, role == "invasive")
      me <- data$attempt_method |> filter(attempt_id %in% s$attempt_id)
      fw_kpi_strip(
        fw_kpi_stat(fw_fmt_num(nrow(s)), fw_t("plan", "r_attempts")),
        fw_kpi_stat(fw_fmt_num(n_distinct(s$country)), fw_t("plan", "r_countries")),
        fw_kpi_stat(fw_fmt_num(n_distinct(inv$species_id)), fw_t("plan", "r_species")),
        fw_kpi_stat(fw_fmt_num(n_distinct(me$method_id)), fw_t("plan", "r_methods")),
        # Not part of the filtered set: it is a property of the database, and
        # showing it here is what tells a visitor that submissions go somewhere.
        fw_kpi_stat(fw_fmt_num(in_review), fw_t("explore", "in_review"),
                    tooltip = fw_t("explore", "in_review_tip"))
      )
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
        fw_add_basemaps() |>
        fw_add_attempt_markers(data, sel())
    })

    output$outcomes    <- renderUI(fw_outcome_bars_ui(sel()))
    output$cumulative  <- plotly::renderPlotly(fw_chart_cumulative(sel()))
    output$duration    <- plotly::renderPlotly(fw_chart_duration(data, sel()))
    output$methods     <- plotly::renderPlotly(
      fw_chart_method(data, sel(), mode = input$method_mode %||% "share"))
    output$waterbody   <- plotly::renderPlotly(fw_chart_waterbody(sel()))
    output$driver      <- plotly::renderPlotly(fw_chart_driver(sel()))
    output$invasive    <- plotly::renderPlotly(
      fw_chart_species(data, sel(), "invasive"))
    output$beneficiary <- plotly::renderPlotly(
      fw_chart_species(data, sel(), "beneficiary"))
  })
}

#' The dashboard's filter panel: every filter, including outcome
fw_explore_filters_ui <- function(ns, ch) {
  multi <- function(id) {
    spec <- FW_FILTERS[[id]]
    div(
      class = "fw-field",
      tags$label(class = "form-label", `for` = ns(id), fw_filter_label(id)),
      if (!is.null(spec$help)) {
        div(class = "fw-field__help", fw_t("filters", spec$help))
      },
      selectizeInput(ns(id), label = NULL, choices = ch[[id]], selected = NULL,
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = fw_t("filters", "all"),
                                    plugins = list("remove_button")))
    )
  }

  years <- tagList(
    div(
      class = "fw-field fw-field--range",
      tags$label(class = "form-label", `for` = ns("years"),
                 fw_filter_label("years")),
      sliderInput(ns("years"), label = NULL, min = ch$year_min, max = ch$year_max,
                  value = c(ch$year_min, ch$year_max), step = 1, sep = "",
                  ticks = FALSE, dragRange = TRUE, width = "100%"),
      div(class = "fw-field__range-readout",
          textOutput(ns("years_readout"), inline = TRUE))
    ),
    div(
      class = "fw-field fw-field--check",
      checkboxInput(ns("include_no_year"), fw_t("filters", "no_year"),
                    value = TRUE)
    )
  )

  tagList(
    h2(class = "fw-filters__heading", fw_t("explore", "f_heading")),
    p(class = "fw-caption", fw_t("explore", "f_note")),
    lapply(fw_filter_draw_order(), function(id) {
      if (identical(FW_FILTERS[[id]]$kind, "range")) years else multi(id)
    }),
    div(
      # Its own class rather than the report builder's. The two pages no longer
      # share a filter layout - this one is still a sidebar - so borrowing
      # .fw-plan__actions would tie the dashboard to a rule that exists for a
      # panel it does not use.
      class = "fw-filters__actions",
      actionButton(ns("clear"), fw_t("plan", "clear"),
                   class = "btn btn-outline-primary btn-sm")
    )
  )
}

# Set by the Networking page, read by the dashboard. A tiny shared reactive
# rather than a module return, because the two modules are siblings and neither
# owns the other.
.fw_explore_request <- shiny::reactiveVal(NULL)
fw_explore_request <- function() .fw_explore_request()
fw_set_explore_request <- function(contact_id) .fw_explore_request(contact_id)

#' Every attempt a contact is attached to, in either slot
fw_contact_attempt_ids <- function(data, contact_id) {
  a <- data$attempt
  a$attempt_id[which(a$primary_contact_id == contact_id |
                       a$secondary_contact_id == contact_id)]
}
