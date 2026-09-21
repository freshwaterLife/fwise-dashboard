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
# NO OUTCOME FILTER AND NO METHOD FILTER. Both are the client's decision rather
# than oversights, and they are different decisions.
#
# OUTCOME is not filterable ANYWHERE any more - the dashboard's copy of it went
# when that page was cut back to four simple filters. The reasoning (Graden,
# metrics framework): given their situation - region, species, waterbody type
# and size - a user planning an eradication should see EVERYTHING that has been
# tried there and its association with success and failure. Letting them filter
# to successes only produces false optimism about their own site, and letting
# them filter to failures is no better. All four outcome states stay visible in
# every result instead.
#
# METHOD is not filterable here because it is an ANSWER, not a question. This
# page exists to tell a reader what has been tried in a situation like theirs;
# pre-selecting the method inverts that into "show me evidence for the thing I
# had already decided to do". The two method charts are where method belongs.
#
# THE FILTERS DESCRIBE THE SITUATION, then: where it is, what kind of water,
# how big, which animals, and when. Size is the newest of them and the only one
# that is unit-aware - see the header of R/filters.R.
#
# THE CAVEATS ARE ON THE ABOUT PAGE. They are properties of the whole database
# rather than of any one selection, and under a freshly built result they read
# as qualifications of that selection alone. They still travel inside every
# download, including as a plain text file that is never optional.
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
#' and the results sit underneath it. See the note at the top
#' of mod_plan_filters.R for the three reasons, the first of which is that a
#' reader should answer the questions before they can see any answer.
#'
#' The results carry an id because the server scrolls to them on Build. With the
#' panel above rather than beside, a rebuild otherwise leaves the reader looking
#' at the controls with no sign that anything happened below the fold.
mod_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          uiOutput(ns("filters")),
          div(id = ns("results_anchor"), class = "fw-plan__results",
              uiOutput(ns("zero")),
              # Shown only while the last build matched something. See
              # output$state in the server.
              conditionalPanel("output.state == 'results'", ns = ns,
                               fw_plan_results_ui(ns)))
        )
      )
    )
  )
}

#' The results, as a fixed skeleton
#'
#' THE CHARTS AND THE MAP ARE STATIC OUTPUTS, and they used to live inside one
#' renderUI that was rebuilt on every Build. That was the bug the client
#' reported as "some figures update when I change country and some do not":
#' each rebuild destroyed and recreated every chart's element while the chart
#' itself was re-rendering, and whether a chart's new value reached the new
#' element or the dying one came down to message timing. The map, rebuilt from
#' scratch into a fresh element, sometimes came up grey for the same reason.
#' Same fix as the Explore page: the elements exist once, and only their
#' contents change. What still varies with the selection - the summary, the
#' species tiles, the outcome bars and the captions - is plain HTML in small
#' uiOutputs that contain no outputs of their own. Keep it that way: an output
#' inside any of those renderUIs brings the bug back.
#'
#' Ids here share one DOM id space with the filters - see the note above the
#' waterbody chart.
fw_plan_results_ui <- function(ns) {
  tagList(
    # THE DOWNLOAD SITS AT THE TOP, beside the heading. It used to be the
    # last thing on the page, below two tables, and the client's objection
    # was that a reader had no way of knowing any of this was exportable
    # until they had scrolled past all of it. Exporting is the point of this
    # page, so it is the first thing the results say.
    #
    # IT DOES NOT NEED DISABLING BEFORE A BUILD. This whole skeleton is hidden
    # until a build has matched something, so the button cannot be seen until
    # there is a report behind it - which is the same guarantee, without a
    # disabled control sitting on the page inviting a click.
    div(
      class = "fw-plan__results-head",
      h2(class = "fw-visually-hidden", fw_t("plan", "r_heading")),
      actionButton(ns("download_open"), fw_t("plan", "download_open"),
                   class = "btn btn-primary fw-plan__download-open",
                   icon = icon("download"))
    ),
    uiOutput(ns("summary")),

    # THE ORDER, AND IT IS THE CLIENT'S. It used to be a pure funnel -
    # general to specific, with the species photographs at the narrow end -
    # and the objection was that the photographs were the one part of this
    # report a reader recognises on sight and they were below the fold.
    #
    # So: WHAT, then WHERE, then WHAT HAPPENED, then HOW LONG, then IN WHAT
    # KIND OF WATER, then WHO. The species lead because they are the thing a
    # reader can identify with their own site; the map follows because it is
    # the only view that says whether this evidence is anywhere near them
    # before they read anything into it; the two outcome charts sit together
    # because the second is the first broken down by method; and duration
    # follows them because "how long" is the next question after "did it
    # work", not a footnote after the waterbody pair.
    #
    # The species block is drawn by output$species; the reasoning for its
    # layout is there.
    uiOutput(ns("species")),

    fw_block(
      fw_t("plan", "r_map"), fw_t("plan", "r_map_note"),
      tagList(
        fw_map_output(ns("map")),
        uiOutput(ns("map_missing"))
      )
    ),

    fw_block(
      fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
      uiOutput(ns("outcome_bars"))
    ),

    # ABOVE THE WATERBODY PAIR, at the client's request. This is the
    # Outcomes block broken down by method, so the two belong together: a
    # reader who has just seen four outcome bars reads this as the same
    # four bars split by what was tried, which is not what it looks like
    # after two blocks about water in between.
    fw_block(
      fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
      tagList(
        # One chart, two questions. "Count" answers how much evidence stands
        # behind a method; "share" answers how often it worked. Count leads,
        # so nobody reads a share off three attempts as a success rate.
        fw_mode_toggle(ns("method_mode"),
                       fw_t("plan", "r_method_count"),
                       fw_t("plan", "r_method_share")),
        plotly::plotlyOutput(ns("methods"), height = "auto"),
        uiOutput(ns("method_missing"))
      )
    ),

    # ---- How long ---------------------------------------------------------
    #
    # STRAIGHT AFTER THE TWO OUTCOME CHARTS, at the client's request. It sat
    # at the foot of the results, on the reasoning that it is about time
    # rather than about the reader's situation and is drawn from less than
    # the full selection. The client's answer is that "how long will this
    # take" is the second question a planner asks after "does it work", and
    # burying it under the waterbody charts answered it last.
    #
    # It still carries its caption saying how much of the selection it
    # actually draws, which is the part that made it a narrow-end block.
    #
    # The cumulative chart used to sit beside it and is now on the dashboard
    # (FW_COPY$explore$cumulative). It answers how the DATABASE has grown,
    # which is not a question about the reader's situation at all, and on a
    # narrow selection it was actively misleading.
    fw_block(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      tagList(
        plotly::plotlyOutput(ns("duration"), height = "auto"),
        uiOutput(ns("duration_missing"))
      )
    ),

    # ---- What kind of water -----------------------------------------------
    #
    # chart_ PREFIX, AND IT IS NOT DECORATION. Inputs and outputs share one
    # DOM id space, and "waterbody" is already a filter's input id - so an
    # output of that name renders a second element with the same id, the
    # output binding attaches to the selectize control instead, and the
    # chart silently never draws. Any chart named after the thing it plots
    # has to clear the filter registry in R/filters.R first.
    fw_block(
      fw_t("plan", "r_waterbody"), fw_fill(fw_t("plan", "r_waterbody_note"), n_word = fw_num_word(FW_TOP_N)),
      tagList(
        # Its own toggle, like the two method charts. The denominator here is
        # the kind of water's own attempts, so share answers "in a lake, how
        # often did it work" - and the count stays in the bar's label either
        # way, so a share off four attempts still shows it is off four.
        fw_mode_toggle(ns("waterbody_mode"),
                       fw_t("plan", "r_waterbody_count"),
                       fw_t("plan", "r_waterbody_share")),
        plotly::plotlyOutput(ns("chart_waterbody"), height = "auto")
      )
    ),

    # Segmented by METHOD, not by outcome, and on its own colour scale. ITS
    # OWN TOGGLE, because its denominator is different: a bar here is
    # uses (one per attempt-method pair), not attempts, so the control
    # says "uses" and switches this chart alone.
    #
    # DIRECTLY UNDER THE WATERBODY CHART it breaks down, which is the same
    # pairing the two method charts above have.
    fw_block(
      fw_t("plan", "r_method_wb"), fw_t("plan", "r_method_wb_note"),
      tagList(
        fw_mode_toggle(ns("method_wb_mode"),
                       fw_t("plan", "r_method_wb_count"),
                       fw_t("plan", "r_method_wb_share")),
        plotly::plotlyOutput(ns("chart_method_waterbody"), height = "auto")
      )
    ),

    # THE TABLE OF MATCHING ATTEMPTS USED TO SIT HERE, and the client
    # removed it. It was the same rows, in the same order, that the Explore
    # page was showing on its own tab - and a reader who has just been given
    # eight figures about a slice does not then read three hundred raw rows
    # of it. Every record is still in the export, and the map above is still
    # the way to open one.
    #
    # THE CONTACTS TABLE BELOW IS NOT THE SAME THING and stays: it is the
    # one part of this page that tells a reader who to talk to rather than
    # what happened, which is a stated year-one success measure.

    # THE NETWORKING SIDE, at the point it is useful. A reader has just seen
    # what was tried near them; who did it is the next question, and it is
    # one of the client's stated year-one success measures. Reads only from
    # fw_contacts_summary(), so a redacted address cannot reach this page.
    fw_block(
      fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
      tagList(
        div(
          class = "fw-table-toolbar",
          div(
            class = "fw-table-toolbar__size",
            tags$label(class = "form-label", `for` = ns("contacts_size"),
                       fw_t("plan", "r_contacts_size")),
            selectInput(ns("contacts_size"), label = NULL,
                        choices = FW_PLAN_CONTACTS_PAGE_SIZES,
                        selected = FW_PLAN_CONTACTS_PAGE_SIZES[1],
                        selectize = FALSE, width = "auto")
          )
        ),
        div(class = "fw-table-scroll", uiOutput(ns("contacts_body"))),
        uiOutput(ns("contacts_pager")),
        p(tags$a(
          href = "#",
          onclick = "Shiny.setInputValue('fw_nav_to','networking',{priority:'event'}); return false;",
          fw_t("plan", "r_contacts_all")
        ))
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

    # ---- The two geography filters, linked ----------------------------------
    #
    # THIS PAGE WENT WITHOUT IT FOR A RELEASE, and that was the bug. The
    # dashboard's pickers narrowed each other and this page's did not, so a
    # reader who asked for Europe and then Australia here still got the honest
    # answer of nothing at all - which is indistinguishable from a page that has
    # broken, and is what the client reported.
    #
    # Same call as mod_explore.R. The reasoning, and why it cannot loop, is at
    # fw_link_geo_filters() in R/filters.R.
    fw_link_geo_filters(input, session, data, choices)

    # Driven from the same id list as everything else, so a filter added to
    # FW_FILTERS is cleared without a second edit here.
    observeEvent(input$clear, fw_filter_clear(session, ids, choices))

    # What the collapsed panel says it is showing. Built from fw_filter_summary(),
    # the same function that writes the workbook's Filters sheet, so the line the
    # reader sees and the record in their download cannot disagree. Only the
    # filters they actually set, because listing the nine they left alone is how
    # a summary becomes unreadable.
    output$filters_summary <- renderUI({
      if (!built()) return(NULL)
      f <- report()$filters
      rows <- fw_filter_summary(f)

      # Only what the reader actually CHOSE. Three kinds of default to drop:
      # a picker left alone, which records "All"; the two "include unrecorded"
      # checkboxes, which record "Yes" untouched - unticking one IS a
      # narrowing, so a "No" stays; and a year slider still spanning the whole
      # record, which is a bound in the sheet but not a decision here.
      #
      # THE SHEET STILL RECORDS ALL OF IT. This is the difference between a
      # summary and a record: the workbook has to say what every filter was set
      # to months later, and this line has to be readable at a glance.
      full_years <- identical(f$year_from, choices$year_min) &&
        identical(f$year_to, choices$year_max)
      years_label <- fw_filter_label("years")

      set <- Filter(function(r) {
        if (identical(r$value, fw_t("export", "filter_all"))) return(FALSE)
        if (identical(r$value, fw_t("export", "filter_yes"))) return(FALSE)
        if (full_years && identical(r$setting, years_label)) return(FALSE)
        TRUE
      }, rows)
      if (!length(set)) return(span(class = "fw-caption", fw_t("filters", "all")))
      span(
        class = "fw-plan-filters__summary-list",
        lapply(set, function(r) {
          span(class = "fw-plan-filters__summary-item",
               tags$b(r$setting), ": ", r$value)
        })
      )
    })

    # The slider's own tick labels are switched off because they pile up over a
    # 90-year span, so the chosen range is printed in words instead.
    output$years_readout <- renderText({
      y <- input$years
      if (is.null(y)) return("")
      paste(y[1], fw_t("filters", "range_of"), y[2])
    })

    # ---- The size control ---------------------------------------------------
    #
    # ITS OWN uiOutput, NESTED INSIDE THE PANEL, and that is the whole design.
    # Which sliders apply depends on the regime selection, so this has to
    # re-render when regime changes - and re-rendering the WHOLE panel to
    # achieve that would rebuild every selectize in it at its default and throw
    # away the nine other filters the reader had set. Only this cell redraws.
    #
    # The id is "size_control", which is deliberately NOT a name in FW_FILTERS:
    # inputs and outputs share one DOM id space. See the note above the charts.
    output$size_control <- renderUI({
      fw_plan_size_ui(
        ns, choices,
        fw_size_units(input$regime %||% character(0))
      )
    })

    # Real units under each log slider. The reader never sees the logarithm.
    for (unit in FW_SIZE_UNITS) {
      local({
        u <- unit
        output[[paste0("size_readout_", u)]] <- renderText({
          v <- input[[paste0("size_", u)]]
          if (is.null(v)) return("")
          paste(fw_size_label(fw_size_unlog(v[1])), fw_t("filters", "range_of"),
                fw_size_label(fw_size_unlog(v[2])))
        })
      })
    }

    # THE GATE. eventReactive, so nothing below recomputes until Build is
    # pressed. The filter state is snapshotted here too, so the results, the
    # caveats and the download all describe the same selection even if the user
    # goes on to change a control.
    report <- eventReactive(input$build, {
      f <- fw_filter_state(input, ids, ch = choices)
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
      # Fold the questions away now they have been answered. Client-side, never
      # by re-rendering the panel - see the fw-collapse handler in
      # R/ui_helpers.R for why that distinction is load-bearing.
      session$sendCustomMessage("fw-collapse", ns("filters_disclosure"))
      session$sendCustomMessage("fw-scroll-to", ns("results_anchor"))
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    # WHICH OF THE THREE STATES THE PAGE IS IN: nothing asked yet, asked and
    # nothing matched, or results. The skeleton in fw_plan_results_ui() is
    # shown only for the last, through a conditionalPanel that reads this.
    # Never suspended - it is not drawn anywhere, and a suspended output would
    # leave the panel's condition unanswered.
    output$state <- renderText({
      if (!built()) return("none")
      if (nrow(report()$sel) == 0) "zero" else "results"
    })
    outputOptions(output, "state", suspendWhenHidden = FALSE)

    # NOTHING here before a build. The page's introduction moved into the page
    # header, where a reader meets it before the controls rather than after
    # them, so there is no longer a second block to show in the meantime.
    output$zero <- renderUI({
      if (!built()) return(NULL)
      r <- report()
      if (nrow(r$sel) > 0) return(NULL)
      fw_plan_zero_ui(fw_filter_zero_hints(data, r$filters))
    })

    # Everything below reads a report with at least one attempt in it. While
    # the page is in either other state the skeleton is hidden and these are
    # suspended, so the req() is for the moment the state changes.
    results <- reactive({
      req(built())
      r <- report()
      req(nrow(r$sel) > 0)
      r
    })

    # A count caption, or nothing when there is nothing to say.
    caption <- function(key, n) {
      if (n > 0) p(class = "fw-caption", fw_fill(fw_t("plan", key), n = fw_fmt_num(n)))
    }

    output$summary <- renderUI(fw_plan_summary_ui(fw_plan_summary(data, results()$sel)))

    # ---- What these attempts were about -------------------------------------
    #
    # ONE BLOCK, TWO ROWS - the targeted species, then the ones that stood
    # to gain, each row across the full width of the page with its tiles
    # sharing that width equally. The two used to sit side by side, which
    # at three tiles each left half-width photographs and dead space at
    # the end of every row; the client asked for the width to be used.
    # See .fw-plan__species-pair and .fw-species-tiles.
    #
    # THE HALF NOTES ARE GONE, and the beneficiary half used to keep one on
    # the explicit reasoning that a warning about how thinly beneficiaries
    # are recorded has to sit with the tiles it qualifies. The client has
    # reversed that: three tiles under a heading under a note under a
    # heading was more apparatus than the grids themselves. The warning now
    # sits in the block note above both halves (r_species_pair_note), which
    # is the one place left that says it - so that note is load-bearing, not
    # an introduction that can be trimmed next.
    output$species <- renderUI({
      sel <- results()$sel
      # Whether the selection records any beneficiary at all. This used to be a
      # pair of counts feeding the two notes under the tile grids - "the five
      # named most often, of 212" - and the client removed the notes. The
      # beneficiary count survives them because the half itself is dropped when
      # it would be empty, which is a layout decision rather than a caption.
      n_beneficiary <- dplyr::n_distinct(
        fw_species_rows(data, sel, "beneficiary")$species_id)
      fw_block(
        fw_t("plan", "r_species_pair"), fw_t("plan", "r_species_pair_note"),
        div(
          class = "fw-plan__species-pair",
          div(
            class = "fw-plan__species-half",
            h4(fw_t("plan", "r_invasive")),
            fw_species_tiles_ui(data, sel, "invasive", limit = FW_PLAN_SPECIES_N)
          ),
          # Dropped entirely rather than shown empty: the grid is two columns
          # of 1fr, so the surviving half takes the full width on its own.
          if (n_beneficiary > 0) {
            div(
              class = "fw-plan__species-half",
              h4(fw_t("plan", "r_beneficiary")),
              fw_species_tiles_ui(data, sel, "beneficiary", limit = FW_PLAN_SPECIES_N)
            )
          }
        )
      )
    })

    output$outcome_bars <- renderUI(fw_outcome_bars_ui(results()$sel))

    output$map_missing <- renderUI({
      sel <- results()$sel
      caption("r_map_missing", sum(is.na(sel$latitude) | is.na(sel$longitude)))
    })
    output$method_missing <- renderUI(
      caption("r_method_missing", fw_n_no_method(data, results()$sel)))
    # THE COUNT THE CHART ACTUALLY DRAWS, not the count with a duration. The
    # chart drops attempts that used more than one method - see
    # fw_duration_sel() - so counting duration alone here would promise the
    # reader more points than they can see. Shown even at zero, as it always
    # was: "drawn from 0 attempts" explains an empty chart.
    output$duration_missing <- renderUI(p(
      class = "fw-caption",
      fw_fill(fw_t("plan", "r_duration_missing"),
              n = fw_fmt_num(nrow(fw_duration_sel(data, results()$sel))))))

    # COUNT LEADS ON EVERY BUILD. The toggles used to be redrawn with the
    # results, which put them back to count each time; now that they persist,
    # a build does that explicitly. See fw_mode_toggle().
    observeEvent(input$build, {
      for (id in c("method_mode", "method_wb_mode", "waterbody_mode")) {
        if (!identical(input[[id]] %||% "count", "count")) {
          updateRadioButtons(session, id, selected = "count")
        }
      }
    })

    # ---- The download picker, in an overlay ---------------------------------
    #
    # IT USED TO BE A BLOCK AT THE FOOT OF THE PAGE. The client asked for it as
    # a pop-up to stop the page being quite so long a scroll, and moving it here
    # is what let the button above sit at the top without the page carrying the
    # picker twice.
    #
    # EXACTLY ONE COPY OF THE PICKER EXISTS AT A TIME, which is not a style
    # preference. Every input in this module shares one DOM id space (see the
    # note above fw_plan_download_ui), so rendering the picker on the page AND
    # in the modal would put two controls called download_parts in the document
    # and the handler would read whichever Shiny bound last.
    #
    # NOTHING IN export.R CHANGED. The picker is a pure function of ns, and the
    # handler reads input$download_parts when the button is clicked rather than
    # when it is drawn, so it does not care where the checkboxes live.
    observeEvent(input$download_open, {
      showModal(modalDialog(
        title = fw_t("plan", "download_heading"),
        fw_plan_download_ui(ns),
        footer = modalButton(fw_t("plan", "download_close")),
        easyClose = TRUE,
        class = "fw-download-modal"
      ))
    })

    # A downloadButton inside a modal does not dismiss it - the browser handles
    # the download without Shiny ever seeing a click - so the button carries an
    # onclick that tells the server it went. It does not preventDefault, so the
    # download still happens; this only closes the box behind it.
    observeEvent(input$download_taken, removeModal())

    # ---- The map: drawn once, markers swapped -------------------------------
    #
    # THE SAME PATTERN AS THE EXPLORE PAGE, and the reasoning is at its copy in
    # mod_explore.R. It used to be rebuilt whole on every Build - tiles, legend,
    # card script and markers - which was slow and, inside the old renderUI,
    # sometimes left the map grey.
    #
    # detail = "lazy": the map carries the hover card for every marker and
    # fetches the full record when one is clicked. The photograph dictionary is
    # the whole database's, so any selection the proxy draws is covered by it.
    #
    # The widget is first drawn when the results are first shown - it is
    # suspended while the skeleton is hidden - so it opens at the right size.
    output$map <- leaflet::renderLeaflet({
      fw_leaflet() |>
        fw_add_basemaps() |>
        fw_add_outcome_legend() |>
        leaflet::setView(FW_MAP$empty_view$lng, FW_MAP$empty_view$lat,
                         zoom = FW_MAP$empty_view$zoom) |>
        fw_map_card_render(ns("map_detail"), fw_map_thumbs_all(data))
    })

    # Waits for the widget, and skips a redraw of what is already there. See
    # the same three pieces in mod_explore.R.
    map_ready <- reactiveVal(FALSE)
    observeEvent(input$map_bounds, map_ready(TRUE))
    drawn <- NULL
    observe({
      req(map_ready())
      s <- results()$sel
      if (isTRUE(session$clientData[[paste0("output_", ns("map"), "_hidden")]])) return()
      key <- s$attempt_id
      if (identical(key, drawn)) return()
      drawn <<- key

      pts <- fw_map_points(data, s)
      proxy <- leaflet::leafletProxy("map", session = session) |>
        leaflet::clearMarkerClusters()
      if (!nrow(pts)) {
        v <- FW_MAP$empty_view
        leaflet::setView(proxy, v$lng, v$lat, zoom = v$zoom)
        return()
      }
      fw_add_marker_layer(proxy, data, pts, detail = "lazy",
                          thumbs = fw_map_thumbs_all(data))
    })
    fw_map_detail_server(input, session, "map_detail", data, reactive(results()$sel))

    # The mode toggle is the ONE control that redraws without a rebuild. It does
    # not change the selection, only how the same numbers are drawn, so it does
    # not undermine the deliberate build step above.
    output$methods <- plotly::renderPlotly({
      fw_chart_or_empty(
        fw_chart_method(data, results()$sel, mode = input$method_mode %||% "count"))
    })
    # Its own toggle, same reason: it redraws the same numbers a different way
    # and does not change the selection.
    output$chart_method_waterbody <- plotly::renderPlotly({
      fw_chart_or_empty(
        fw_chart_method_waterbody(data, results()$sel,
                                  mode = input$method_wb_mode %||% "count"))
    })
    # Same again: the toggle redraws the same numbers, it does not reselect.
    output$chart_waterbody <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_waterbody(results()$sel,
                                           mode = input$waterbody_mode %||% "count")))
    output$duration <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_duration(data, results()$sel)))

    # THE RESULTS TABLE'S PAGING WENT WITH THE TABLE. It owned its page number
    # as server state rather than reading it back off the buttons, because
    # fw_page_numbers() writes through Shiny.setInputValue() and an input set
    # that way has no binding for update*Input() to talk to. The contacts
    # directory below still does exactly that, and the comment explaining why
    # now lives there.

    # ---- Potential relevant contacts ----------------------------------------
    #
    # Reads fw_contacts_summary() ONLY. Redaction happens inside that function
    # and nowhere downstream, so there is no code path here that can see an
    # address a contact asked to keep private.
    contacts <- reactive(fw_plan_contacts(data, results()$sel))

    contacts_per_page <- reactive(
      as.integer(input$contacts_size %||% FW_PLAN_CONTACTS_PAGE_SIZES[1]))

    # THE PAGE IS STATE THE SERVER OWNS, not something read back off the
    # buttons. fw_page_numbers() writes the chosen page into the input with
    # Shiny.setInputValue(), and an input set that way has no binding in the
    # page for update*Input() or sendInputMessage() to talk to - so a reset
    # written that way is silently ignored, and a rebuild leaves the reader on
    # whatever page they had reached in the previous report: "Showing 11-17 of
    # 17". Same pattern as the contacts directory on the Networking page.
    contacts_page <- reactiveVal(1L)
    observeEvent(input$contacts_page_to, {
      n <- suppressWarnings(as.integer(input$contacts_page_to))
      if (length(n) == 1 && !is.na(n)) contacts_page(max(1L, n))
    })
    observeEvent(report(), contacts_page(1L))
    observeEvent(input$contacts_size, contacts_page(1L), ignoreInit = TRUE)

    contacts_shown <- reactive(
      min(contacts_page(), fw_plan_pages(nrow(contacts()), contacts_per_page())))

    output$contacts_body <- renderUI({
      fw_plan_contacts_ui(contacts(), contacts_shown(), contacts_per_page())
    })

    output$contacts_pager <- renderUI({
      n_rows <- nrow(contacts())
      if (n_rows == 0) return(NULL)
      n_pages <- fw_plan_pages(n_rows, contacts_per_page())
      from <- (contacts_shown() - 1L) * contacts_per_page() + 1L
      to <- min(n_rows, contacts_shown() * contacts_per_page())
      div(
        class = "fw-pager",
        p(class = "fw-caption",
          sprintf("%s %s-%s %s %s", fw_t("plan", "r_table_showing"),
                  fw_fmt_num(from), fw_fmt_num(to), fw_t("common", "of"),
                  fw_fmt_num(n_rows))),
        fw_page_numbers(ns("contacts_page_to"), contacts_shown(), n_pages)
      )
    })

    # ---- The download -------------------------------------------------------
    #
    # ONE HANDLER AND A PICKER, replacing a spreadsheet button and a report
    # button. Two buttons made the reader choose between the data and the
    # document when most of them wanted both, and neither carried the methods
    # and the caveats out of the building with it.
    #
    # The methods-and-caveats text is NOT one of the choices. It always travels,
    # for the same reason the workbook's caveats sheet is not optional: a file
    # that turns up in an inbox six months later has to carry its own
    # qualifications, and by then nobody remembers what was on screen.
    #
    # filename is a function evaluated at click time, so it can read the
    # checkboxes and name a .zip or the single file as appropriate.
    output$download <- downloadHandler(
      filename = function() fw_bundle_filename(input$download_parts),
      content = function(file) {
        r <- report()
        fw_write_bundle(
          path = file, parts = input$download_parts, data = data,
          sel = r$sel, export = r$export, filters = r$filters, meta = meta,
          method_mode = input$method_mode %||% "count",
          method_wb_mode = input$method_wb_mode %||% "count",
          waterbody_mode = input$waterbody_mode %||% "count"
        )
      }
    )

  })
}

# ---- The two states ----------------------------------------------------------

# fw_block() is now fw_block() in R/ui_helpers.R - the dashboard draws
# titled blocks of its own since the summary graphics moved there.

#' A count/share switch for one chart
#'
#' A radio group drawn as a row of buttons (see .fw-segmented in
#' _components.scss). Two of these exist and they used to be written out twice,
#' and the two copies disagreed on which option came first and which was
#' selected. Here there is no argument for either: COUNT LEADS AND IS SELECTED,
#' because a 100% bar answers "how often did this work" before the reader has
#' been told how much evidence is behind it. The labels are the only thing a
#' caller chooses, because the two charts count different things.
fw_mode_toggle <- function(id, count_label, share_label) {
  div(
    class = "fw-segmented",
    radioButtons(
      id, label = fw_t("plan", "r_method_mode"),
      choices = stats::setNames(c("count", "share"), c(count_label, share_label)),
      selected = "count", inline = TRUE
    )
  )
}

#' How many of these attempts have no method recorded
#'
#' The two method charts draw from attempt_method, so an attempt with no row
#' there is simply absent from both. The caption under the chart says how many,
#' and the report's copy of it comes from this same function.
fw_n_no_method <- function(data, sel) {
  sum(!sel$attempt_id %in% data$attempt_method$attempt_id)
}

#' The download picker: one button, and a choice of what goes in the bundle
#'
#' ONE BUTTON, NOT THREE. The page used to offer a spreadsheet button and a
#' report button, which made the reader choose between the data and the document
#' when most of them wanted both - and neither carried the methods and the
#' caveats with it once it left the building.
#'
#' THE TEXT FILE IS NOT A CHECKBOX. It is listed so the reader knows it is
#' coming, and it always comes. Same reasoning as the workbook's caveats sheet:
#' a reader who did not ask for the caveats is exactly the reader who needs them.
#'
#' Ids here must not collide with anything in FW_FILTERS - inputs and outputs
#' share one DOM id space on this page.
fw_plan_download_ui <- function(ns) {
  part <- function(id, label, note) {
    list(id = id, label = label, note = note)
  }
  parts <- list(
    part("xlsx", fw_t("plan", "download_xlsx"), fw_t("plan", "download_xlsx_note")),
    part("csv",  fw_t("plan", "download_csv"),  fw_t("plan", "download_csv_note")),
    part("html", fw_t("plan", "download_html"), fw_t("plan", "download_html_note")),
    part("pdf",  fw_t("plan", "download_pdf"),  fw_t("plan", "download_pdf_note"))
  )

  div(
    class = "fw-plan__download",
    # NO HEADING OF ITS OWN. This is drawn inside a modal whose title is already
    # fw_t("plan", "download_heading"), and a second copy of the same words
    # under it read as a duplicate rather than as a section.
    #
    # THE .txt IS STATED HERE, NOT LISTED BELOW. It is not a choice, and a line
    # sitting under four checkboxes with no box of its own reads as an option
    # that failed to render. Said once at the top, it is a fact about every
    # download the page produces.
    p(class = "fw-plan__note",
      fw_t("plan", "download_lead"), " ",
      tags$b(fw_t("plan", "download_txt")), " ", fw_t("plan", "download_txt_note")),
    div(
      class = "fw-download-picker",
      tags$fieldset(
        class = "fw-download-picker__parts",
        tags$legend(class = "form-label", fw_t("plan", "download_parts")),
        # checkboxGroupInput rather than four checkboxInputs: one input to read,
        # one to name in the handler, and the browser groups them for a screen
        # reader without any help from us.
        checkboxGroupInput(
          ns("download_parts"), label = NULL,
          # Name and note on ONE line beside the box, not stacked under it.
          # Four options each taking three lines - box, name, note - made a
          # short list of file formats fill a screen, and the notes are a few
          # words each.
          choiceNames = lapply(parts, function(p) {
            tagList(span(class = "fw-download-picker__label", p$label),
                    span(class = "fw-download-picker__note", p$note))
          }),
          choiceValues = vapply(parts, function(p) p$id, character(1)),
          selected = c("xlsx", "html")
        ),
      ),
      # THE onclick IS WHAT CLOSES THE MODAL. A downloadButton is an ordinary
      # link the browser follows on its own, so Shiny never sees the click and
      # nothing server-side knows the reader is done. This tells it. It does not
      # return false, so the download itself is untouched.
      downloadButton(ns("download"), fw_t("plan", "download"),
                     class = "btn btn-primary",
                     onclick = sprintf(
                       "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
                       ns("download_taken")))
    )
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
