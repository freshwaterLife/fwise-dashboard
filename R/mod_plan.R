# mod_plan.R
# BUILT. The report builder.
#
# A FILTER PANEL AND A DELIBERATE BUILD STEP, not a live dashboard. Results
# render only when Build report is pressed. That is a DESIGN decision before it
# is a performance one: the client's steer was "controlled, informative, not
# random clicking", and the reason is credibility. A view that redraws under the
# cursor invites someone to land on a narrow, unrepresentative slice by accident
# and then cite it. Making the user commit to a selection makes the selection
# something they chose.
#
# So do not "make the results reactive to the filters". The gap between changing
# a filter and seeing a result is the feature.
#
# WHAT THIS REPLACED. The stub here described a guided questionnaire with outcome
# deliberately excluded from the questions. Both points are superseded: the
# client's metrics framework lists outcome among the filters, and the filter
# panel was chosen over the questionnaire. The reasoning that outcome filtering
# can produce false optimism has not gone away - it is answered here by keeping
# all four outcome states visible in every result and by the caveats panel,
# rather than by hiding the control.
#
# Filters live in mod_plan_filters.R, rendering in mod_plan_results.R, and the
# export in export.R, which is shared with the future Zenodo release.

library(shiny)
library(bslib)
library(dplyr)

mod_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          layout_sidebar(
            fillable = FALSE,
            sidebar = sidebar(
              width = 320, class = "fw-plan__sidebar",
              uiOutput(ns("filters"))
            ),
            uiOutput(ns("results"))
          )
        )
      )
    )
  )
}

mod_plan_server <- function(id, data, meta = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Built once from the approved data. fw_load_data() has already removed
    # anything unapproved, so no option list here can leak a pending record.
    choices <- fw_plan_choices(data)

    output$filters <- renderUI(fw_plan_filters_ui(ns, choices))

    observeEvent(input$clear, {
      for (nm in c("continent", "country", "taxa", "species", "method",
                   "regime", "outcome")) {
        updateSelectizeInput(session, nm, selected = character(0))
      }
      updateSliderInput(session, "years",
                        value = c(choices$year_min, choices$year_max))
      updateCheckboxInput(session, "include_no_year", value = TRUE)
    })

    # THE GATE. eventReactive, so nothing below recomputes until Build is
    # pressed. The filter state is snapshotted here too, so the results, the
    # caveats and the download all describe the same selection even if the user
    # goes on to change a control.
    report <- eventReactive(input$build, {
      f <- fw_plan_filter_state(input)
      sel <- fw_plan_apply(data, f)
      list(
        filters = f,
        sel     = sel,
        export  = fw_export_frame(data, sel$attempt_id)
      )
    })

    # "Not yet asked" and "asked and got nothing" are different states and must
    # not look the same, so this is checked BEFORE report() is touched. The
    # filter panel is a renderUI, so its inputs - input$build included - are NULL
    # until the browser has painted it; reading report() before then would filter
    # against a year range that does not exist yet.
    built <- reactive(!is.null(input$build) && input$build > 0)

    output$results <- renderUI({
      if (!built()) return(fw_plan_empty_ui())
      r <- report()

      if (nrow(r$sel) == 0) {
        return(tagList(
          fw_plan_zero_ui(fw_plan_zero_hints(data, r$filters)),
          fw_plan_caveats_ui(data)
        ))
      }

      s <- fw_plan_summary(data, r$sel)
      n_no_coords <- sum(is.na(r$sel$latitude) | is.na(r$sel$longitude))

      tagList(
        h2(fw_t("plan", "r_heading")),
        fw_plan_summary_ui(s),

        fw_plan_block(
          fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
          fw_plan_outcome_ui(r$sel)
        ),

        fw_plan_block(
          fw_t("plan", "r_map"), fw_t("plan", "r_map_note"),
          tagList(
            leaflet::leafletOutput(ns("map"), height = 420),
            if (n_no_coords > 0) {
              p(class = "fw-caption",
                sub("{n}", fw_fmt_num(n_no_coords),
                    fw_t("plan", "r_map_missing"), fixed = TRUE))
            }
          )
        ),

        fw_plan_block(
          fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
          plotly::plotlyOutput(ns("methods"), height = "auto")
        ),

        fw_plan_block(
          fw_t("plan", "r_cumulative"), fw_t("plan", "r_cumulative_note"),
          plotly::plotlyOutput(ns("cumulative"), height = "auto")
        ),

        fw_plan_block(
          fw_t("plan", "r_table"),
          sub("{n}", min(FW_PLAN_TABLE_ROWS, nrow(r$export)),
              fw_t("plan", "r_table_note"), fixed = TRUE),
          div(class = "fw-table-scroll", fw_plan_table(r$export))
        ),

        div(
          class = "fw-plan__download",
          downloadButton(ns("download"), fw_t("plan", "download"),
                         class = "btn btn-primary"),
          p(class = "fw-caption", fw_t("plan", "download_note"))
        ),

        # ALWAYS VISIBLE, never collapsed. Same text as the export's caveats
        # sheet, from the same function, so the two cannot disagree.
        fw_plan_caveats_ui(data)
      )
    })

    output$map        <- leaflet::renderLeaflet({ req(built()); fw_plan_map(report()$sel) })
    output$methods    <- plotly::renderPlotly({ req(built()); fw_plan_method_chart(data, report()$sel) })
    output$cumulative <- plotly::renderPlotly({ req(built()); fw_plan_cumulative_chart(report()$sel) })

    output$download <- downloadHandler(
      filename = function() fw_export_filename(),
      content = function(file) {
        r <- report()
        fw_write_workbook(file, data, r$export, r$filters, meta)
      }
    )
  })
}

# ---- The three states --------------------------------------------------------

#' A titled results block with its qualification directly beneath the heading
#'
#' The note sits ABOVE the chart, not below it. A caveat under a chart is read
#' after the reader has already drawn their conclusion.
fw_plan_block <- function(title, note, content) {
  div(
    class = "fw-plan__block",
    h3(title),
    if (!is.null(note)) p(class = "fw-plan__note", note),
    content
  )
}

#' Before anything has been built
fw_plan_empty_ui <- function() {
  div(
    class = "fw-plan__empty fw-prose",
    h2(fw_t("plan", "empty_heading")),
    p(class = "fw-lead", fw_t("plan", "empty_body")),
    p(fw_t("plan", "empty_body2")),
    p(class = "fw-caption", fw_t("plan", "empty_note"))
  )
}

#' Built, but nothing matched
#'
#' Says so plainly and names which filter to relax. "No results" with no next
#' step is where a user leaves.
fw_plan_zero_ui <- function(hints) {
  div(
    class = "fw-plan__zero fw-prose", role = "status",
    h2(fw_t("plan", "zero_heading")),
    p(class = "fw-lead", fw_t("plan", "zero_body")),
    if (length(hints) > 0) {
      tagList(
        p(fw_t("plan", "zero_hint_lead")),
        tags$ul(lapply(hints, tags$li))
      )
    } else {
      p(fw_t("plan", "zero_hint_none"))
    }
  )
}
