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
# NO OUTCOME FILTER ON THIS PAGE. The dashboard has one; this page does not, and
# that asymmetry is the client's decision, not an oversight. The reasoning
# (Graden, metrics framework): given their situation - region, species,
# waterbody type and size - a user planning an eradication should see EVERYTHING
# that has been tried there and its association with success and failure.
# Letting them filter to successes only produces false optimism about their own
# site, and letting them filter to failures is no better. Browsing the evidence
# base is a different activity, so the control lives there instead.
#
# All four outcome states stay visible in every result regardless, and the
# caveats panel sits beside them.
#
# The filter panel lives in mod_plan_filters.R, the shared filter engine in
# filters.R, rendering in mod_plan_results.R, and the export in export.R, which
# is shared with the future Zenodo release.

library(shiny)
library(bslib)
library(dplyr)

#' The report builder page
#'
#' STACKED, NOT SIDE BY SIDE. The filter panel is the whole width of the page
#' and the results sit underneath it. The dashboard still uses
#' fw_sidebar_layout(); this page deliberately does not. See the note at the top
#' of mod_plan_filters.R for the three reasons, the first of which is that a
#' reader should answer the questions before they can see any answer.
#'
#' The results carry an id because the server scrolls to them on Build. With the
#' panel above rather than beside, a rebuild otherwise leaves the reader looking
#' at the controls with no sign that anything happened below the fold.
mod_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          uiOutput(ns("filters")),
          div(id = ns("results_anchor"), class = "fw-plan__results",
              uiOutput(ns("results")))
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
    choices <- fw_filter_choices(data)
    ids <- fw_plan_filter_ids()

    output$filters <- renderUI(fw_plan_filters_ui(ns, choices))

    # Driven from the same id list as everything else, so a filter added to
    # FW_FILTERS is cleared without a second edit here.
    observeEvent(input$clear, fw_filter_clear(session, ids, choices))

    # The slider's own tick labels are switched off because they pile up over a
    # 90-year span, so the chosen range is printed in words instead.
    output$years_readout <- renderText({
      y <- input$years
      if (is.null(y)) return("")
      paste(y[1], fw_t("filters", "range_of"), y[2])
    })

    # THE GATE. eventReactive, so nothing below recomputes until Build is
    # pressed. The filter state is snapshotted here too, so the results, the
    # caveats and the download all describe the same selection even if the user
    # goes on to change a control.
    report <- eventReactive(input$build, {
      f <- fw_filter_state(input, ids)
      sel <- fw_filter_apply(data, f)
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

    # The results are BELOW the questions now, so a build that lands off screen
    # looks like a build that did nothing. Scroll to them, and say what happened
    # in the live region for anyone who is not watching the screen.
    observeEvent(input$build, {
      session$sendCustomMessage("fw-scroll-to", ns("results_anchor"))
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    output$results <- renderUI({
      # NOTHING here before a build. The page's introduction moved into the page
      # header, where a reader meets it before the controls rather than after
      # them, so there is no longer a second block to show in the meantime.
      if (!built()) return(NULL)
      r <- report()

      if (nrow(r$sel) == 0) {
        return(tagList(
          fw_plan_zero_ui(fw_filter_zero_hints(data, r$filters)),
          fw_plan_caveats_ui(data)
        ))
      }

      s <- fw_plan_summary(data, r$sel)
      n_no_coords <- sum(is.na(r$sel$latitude) | is.na(r$sel$longitude))
      n_duration <- sum(!is.na(r$sel$duration_days) & r$sel$duration_days > 0)
      # How many distinct species of each role are in the selection. The tiles
      # show ten; the note has to say what the ten are ten OF, or a reader takes
      # them for the whole list.
      n_invasive <- dplyr::n_distinct(
        fw_species_rows(data, r$sel, "invasive")$species_id)
      n_beneficiary <- dplyr::n_distinct(
        fw_species_rows(data, r$sel, "beneficiary")$species_id)

      tagList(
        h2(fw_t("plan", "r_heading")),
        fw_plan_summary_ui(s),

        # THE ORDER IS A FUNNEL: the whole picture first, then the parts of it,
        # then the detail. Where, then what happened, then in what kind of
        # water, then by what means, then to which species. A reader who stops
        # part way down has still seen the more general answer.
        #
        # The map leads because it is the only view that shows a reader whether
        # this evidence is anywhere near them before they read anything into it.

        fw_plan_block(
          fw_t("plan", "r_map"), fw_t("plan", "r_map_note"),
          tagList(
            fw_map_output(ns("map")),
            if (n_no_coords > 0) {
              p(class = "fw-caption",
                fw_fill(fw_t("plan", "r_map_missing"), n = fw_fmt_num(n_no_coords)))
            }
          )
        ),

        fw_plan_block(
          fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
          fw_outcome_bars_ui(r$sel)
        ),

        # chart_ PREFIX, AND IT IS NOT DECORATION. Inputs and outputs share one
        # DOM id space, and "waterbody" is already a filter's input id - so an
        # output of that name renders a second element with the same id, the
        # output binding attaches to the selectize control instead, and the
        # chart silently never draws. Any chart named after the thing it plots
        # has to clear the filter registry in R/filters.R first.
        fw_plan_block(
          fw_t("plan", "r_waterbody"), fw_fill(fw_t("plan", "r_waterbody_note"), n_word = fw_num_word(FW_TOP_N)),
          plotly::plotlyOutput(ns("chart_waterbody"), height = "auto")
        ),

        fw_plan_block(
          fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
          tagList(
            # One chart, two questions. "Count" answers how much evidence stands
            # behind a method; "share" answers how often it worked. Count leads,
            # so nobody reads a share off three attempts as a success rate.
            div(
              class = "fw-segmented",
              radioButtons(
                ns("method_mode"), label = fw_t("plan", "r_method_mode"),
                choices = stats::setNames(
                  c("count", "share"),
                  c(fw_t("plan", "r_method_count"), fw_t("plan", "r_method_share"))
                ),
                selected = "count", inline = TRUE
              )
            ),
            plotly::plotlyOutput(ns("methods"), height = "auto")
          )
        ),

        # Segmented by METHOD, not by outcome, and on its own colour scale. The
        # toggle above drives this too: it is the same question asked of the
        # same numbers, so two separate controls would be a distinction the
        # reader has to work out for themselves.
        fw_plan_block(
          fw_t("plan", "r_method_wb"), fw_t("plan", "r_method_wb_note"),
          plotly::plotlyOutput(ns("chart_method_waterbody"), height = "auto")
        ),

        fw_plan_block(
          fw_t("plan", "r_invasive"),
          fw_fill(fw_t("plan", "r_invasive_note"), n = fw_fmt_num(n_invasive), n_word = fw_num_word(FW_TOP_N)),
          fw_species_tiles_ui(data, r$sel, "invasive")
        ),

        # Beneficiaries are recorded far less consistently than targets, so this
        # sits after the species that were targeted and carries its own warning
        # rather than being presented as the mirror image of it.
        if (n_beneficiary > 0) {
          fw_plan_block(
            fw_t("plan", "r_beneficiary"),
            fw_fill(fw_t("plan", "r_beneficiary_note"), n = fw_fmt_num(n_beneficiary), n_word = fw_num_word(FW_TOP_N)),
            fw_species_tiles_ui(data, r$sel, "beneficiary")
          )
        },

        # ---- The narrow end ---------------------------------------------------
        # Both of these are about time rather than about the reader's situation,
        # and both are drawn from less than the full selection. They belong
        # after the question "what has been tried here" has been answered.

        fw_plan_block(
          fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
          tagList(
            plotly::plotlyOutput(ns("duration"), height = "auto"),
            p(class = "fw-caption",
              fw_fill(fw_t("plan", "r_duration_missing"), n = fw_fmt_num(n_duration)))
          )
        ),

        fw_plan_block(
          fw_t("plan", "r_cumulative"), fw_t("plan", "r_cumulative_note"),
          plotly::plotlyOutput(ns("cumulative"), height = "auto")
        ),

        fw_plan_block(
          fw_t("plan", "r_table"),
          fw_fill(fw_t("plan", "r_table_note"), n = fw_fmt_num(nrow(r$export))),
          tagList(
            # The page-size select sits HERE, not inside the table's own
            # uiOutput. A select rebuilt by renderUI comes back at its default,
            # so a reader's choice of 100 would be thrown away every time they
            # turned a page. Same lesson as the contacts directory.
            div(
              class = "fw-table-toolbar",
              div(
                class = "fw-table-toolbar__size",
                tags$label(class = "form-label", `for` = ns("table_size"),
                           fw_t("plan", "r_table_size")),
                selectInput(ns("table_size"), label = NULL,
                            choices = FW_PLAN_PAGE_SIZES,
                            selected = FW_PLAN_PAGE_SIZES[1],
                            selectize = FALSE, width = "auto")
              )
            ),
            div(class = "fw-table-scroll", uiOutput(ns("table_body"))),
            uiOutput(ns("table_pager"))
          )
        ),

        div(
          class = "fw-plan__download",
          h3(fw_t("plan", "download_heading")),
          div(
            class = "fw-plan__download-grid",
            div(
              class = "fw-plan__download-option",
              downloadButton(ns("download"), fw_t("plan", "download"),
                             class = "btn btn-primary"),
              p(class = "fw-caption", fw_t("plan", "download_note"))
            ),
            div(
              class = "fw-plan__download-option",
              # An ordinary downloadHandler. It used to be an actionButton that
              # asked the browser to photograph every chart before a hidden
              # download button could be clicked on the reader's behalf; the
              # HTML report needs no pictures, so all of that is gone. See the
              # header of R/report_html.R.
              downloadButton(ns("download_html"), fw_t("plan", "download_html"),
                             class = "btn btn-primary"),
              p(class = "fw-caption", fw_t("plan", "download_html_note"))
            )
          )
        ),

        # ALWAYS VISIBLE, never collapsed. Same text as the export's caveats
        # sheet, from the same function, so the two cannot disagree.
        fw_plan_caveats_ui(data)
      )
    })

    output$map <- leaflet::renderLeaflet({ req(built()); fw_plan_map(data, report()$sel) })

    # The mode toggle is the ONE control that redraws without a rebuild. It does
    # not change the selection, only how the same numbers are drawn, so it does
    # not undermine the deliberate build step above.
    output$methods <- plotly::renderPlotly({
      req(built())
      fw_chart_method(data, report()$sel, mode = input$method_mode %||% "count")
    })
    # Same toggle, same reason: it redraws the same numbers a different way and
    # does not change the selection.
    output$chart_method_waterbody <- plotly::renderPlotly({
      req(built())
      fw_chart_method_waterbody(data, report()$sel,
                                mode = input$method_mode %||% "count")
    })
    output$chart_waterbody <- plotly::renderPlotly({ req(built()); fw_chart_waterbody(report()$sel) })
    output$duration   <- plotly::renderPlotly({ req(built()); fw_chart_duration(data, report()$sel) })
    output$cumulative <- plotly::renderPlotly({ req(built()); fw_chart_cumulative(report()$sel) })

    # ---- The results table's paging -----------------------------------------
    #
    # These read report() but do NOT rebuild it, so turning a page or changing
    # the page size redraws the table alone and leaves the rest of the report
    # standing.

    per_page <- reactive(as.integer(input$table_size %||% FW_PLAN_PAGE_SIZES[1]))

    # A new report, or a bigger page size, can leave the reader on a page that
    # no longer exists. Clamping beats an empty table with no explanation.
    table_page <- reactive({
      n_pages <- fw_plan_pages(nrow(report()$export), per_page())
      min(max(1L, as.integer(input$table_page %||% 1L)), n_pages)
    })

    observeEvent(report(), updateTextInput(session, "table_page", value = 1L),
                 ignoreInit = TRUE)
    observeEvent(input$table_size, {
      session$sendInputMessage("table_page", list(value = 1L))
    }, ignoreInit = TRUE)

    output$table_body <- renderUI({
      req(built())
      fw_plan_table(report()$export, table_page(), per_page())
    })

    output$table_pager <- renderUI({
      req(built())
      n_rows <- nrow(report()$export)
      n_pages <- fw_plan_pages(n_rows, per_page())
      from <- (table_page() - 1L) * per_page() + 1L
      to <- min(n_rows, table_page() * per_page())
      div(
        class = "fw-pager",
        p(class = "fw-caption",
          sprintf("%s %s-%s %s %s", fw_t("plan", "r_table_showing"),
                  fw_fmt_num(from), fw_fmt_num(to), fw_t("common", "of"),
                  fw_fmt_num(n_rows))),
        fw_page_numbers(ns("table_page"), table_page(), n_pages)
      )
    })

    output$download <- downloadHandler(
      filename = function() fw_export_filename(),
      content = function(file) {
        r <- report()
        fw_write_workbook(file, data, r$export, r$filters, meta)
      }
    )

    # ---- The report ---------------------------------------------------------
    #
    # ONE download handler and nothing else. The Word export this replaced took
    # three steps and a round trip to the browser, because a .docx can only hold
    # a chart as a picture and only the browser could produce one. The HTML
    # report carries the plotly figures and the leaflet map as themselves, so
    # the server builds the whole file on its own.
    #
    # method_mode travels with it, so the document shows the method chart in
    # whichever mode the reader is looking at rather than re-deciding for them.
    output$download_html <- downloadHandler(
      filename = function() fw_html_filename(),
      content = function(file) {
        r <- report()
        fw_write_html_report(
          path = file, data = data, sel = r$sel, export = r$export,
          filters = r$filters, meta = meta,
          method_mode = input$method_mode %||% "count"
        )
      }
    )
  })
}

# ---- The two states ----------------------------------------------------------

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
