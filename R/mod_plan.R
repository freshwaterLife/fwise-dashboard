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

    output$results <- renderUI({
      # NOTHING here before a build. The page's introduction moved into the page
      # header, where a reader meets it before the controls rather than after
      # them, so there is no longer a second block to show in the meantime.
      if (!built()) return(NULL)
      r <- report()

      if (nrow(r$sel) == 0) {
        return(fw_plan_zero_ui(fw_filter_zero_hints(data, r$filters)))
      }

      s <- fw_plan_summary(data, r$sel)
      n_no_coords <- sum(is.na(r$sel$latitude) | is.na(r$sel$longitude))
      n_no_method <- fw_n_no_method(data, r$sel)
      n_duration <- sum(!is.na(r$sel$duration_days) & r$sel$duration_days > 0)
      # How many distinct species of each role are in the selection. The tiles
      # show FW_PLAN_SPECIES_N of them; the note has to say what those are a
      # subset OF, or a reader takes them for the whole list.
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

        fw_block(
          fw_t("plan", "r_map"), fw_t("plan", "r_map_note"),
          tagList(
            fw_map_output(ns("map")),
            if (n_no_coords > 0) {
              p(class = "fw-caption",
                fw_fill(fw_t("plan", "r_map_missing"), n = fw_fmt_num(n_no_coords)))
            }
          )
        ),

        fw_block(
          fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
          fw_outcome_bars_ui(r$sel)
        ),

        # chart_ PREFIX, AND IT IS NOT DECORATION. Inputs and outputs share one
        # DOM id space, and "waterbody" is already a filter's input id - so an
        # output of that name renders a second element with the same id, the
        # output binding attaches to the selectize control instead, and the
        # chart silently never draws. Any chart named after the thing it plots
        # has to clear the filter registry in R/filters.R first.
        fw_block(
          fw_t("plan", "r_waterbody"), fw_fill(fw_t("plan", "r_waterbody_note"), n_word = fw_num_word(FW_TOP_N)),
          plotly::plotlyOutput(ns("chart_waterbody"), height = "auto")
        ),

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
            if (n_no_method > 0) {
              p(class = "fw-caption",
                fw_fill(fw_t("plan", "r_method_missing"), n = fw_fmt_num(n_no_method)))
            }
          )
        ),

        # Segmented by METHOD, not by outcome, and on its own colour scale. ITS
        # OWN TOGGLE, because its denominator is different: a bar here is
        # uses (one per attempt-method pair), not attempts, so the control
        # says "uses" and switches this chart alone.
        fw_block(
          fw_t("plan", "r_method_wb"), fw_t("plan", "r_method_wb_note"),
          tagList(
            fw_mode_toggle(ns("method_wb_mode"),
                           fw_t("plan", "r_method_wb_count"),
                           fw_t("plan", "r_method_wb_share")),
            plotly::plotlyOutput(ns("chart_method_waterbody"), height = "auto")
          )
        ),

        fw_block(
          fw_t("plan", "r_invasive"),
          fw_fill(fw_t("plan", "r_invasive_note"), n = fw_fmt_num(n_invasive),
                  n_word = fw_num_word(FW_PLAN_SPECIES_N)),
          fw_species_tiles_ui(data, r$sel, "invasive", limit = FW_PLAN_SPECIES_N)
        ),

        # Beneficiaries are recorded far less consistently than targets, so this
        # sits after the species that were targeted and carries its own warning
        # rather than being presented as the mirror image of it.
        if (n_beneficiary > 0) {
          fw_block(
            fw_t("plan", "r_beneficiary"),
            fw_fill(fw_t("plan", "r_beneficiary_note"), n = fw_fmt_num(n_beneficiary),
                    n_word = fw_num_word(FW_PLAN_SPECIES_N)),
            fw_species_tiles_ui(data, r$sel, "beneficiary", limit = FW_PLAN_SPECIES_N)
          )
        },

        # ---- The narrow end ---------------------------------------------------
        # About time rather than about the reader's situation, and drawn from
        # less than the full selection. It belongs after the question "what has
        # been tried here" has been answered.
        #
        # The cumulative chart used to sit beside it and is now on the dashboard
        # (FW_COPY$explore$cumulative). It answers how the DATABASE has grown,
        # which is not a question about the reader's situation at all, and on a
        # narrow selection it was actively misleading.

        fw_block(
          fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
          tagList(
            plotly::plotlyOutput(ns("duration"), height = "auto"),
            p(class = "fw-caption",
              fw_fill(fw_t("plan", "r_duration_missing"), n = fw_fmt_num(n_duration)))
          )
        ),

        fw_block(
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

        # THE NETWORKING SIDE, at the point it is useful. A reader has just seen
        # what was tried near them; who did it is the next question, and it is
        # one of the client's stated year-one success measures. Reads only from
        # fw_contacts_summary(), so a redacted address cannot reach this page.
        fw_block(
          fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
          tagList(
            # Page size in static UI, same reason as the table above.
            div(
              class = "fw-table-toolbar",
              div(
                class = "fw-table-toolbar__size",
                tags$label(class = "form-label", `for` = ns("contacts_size"),
                           fw_t("plan", "r_contacts_size")),
                selectInput(ns("contacts_size"), label = NULL,
                            choices = FW_CONTACTS_PAGE_SIZES,
                            selected = FW_CONTACTS_PAGE_SIZES[1],
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
        ),

        fw_plan_download_ui(ns)
      )
    })

    # detail = "lazy": the map carries the hover card for every marker and
    # fetches the full record when one is clicked, which is what keeps a
    # 900-marker build to a fraction of the payload. See fw_add_attempt_markers().
    output$map <- leaflet::renderLeaflet({
      req(built())
      fw_plan_map(data, report()$sel, detail = "lazy", detail_input = ns("map_detail"))
    })
    fw_map_detail_server(input, session, "map_detail", data, reactive(report()$sel))

    # The mode toggle is the ONE control that redraws without a rebuild. It does
    # not change the selection, only how the same numbers are drawn, so it does
    # not undermine the deliberate build step above.
    output$methods <- plotly::renderPlotly({
      req(built())
      fw_chart_or_empty(
        fw_chart_method(data, report()$sel, mode = input$method_mode %||% "count"))
    })
    # Its own toggle, same reason: it redraws the same numbers a different way
    # and does not change the selection.
    output$chart_method_waterbody <- plotly::renderPlotly({
      req(built())
      fw_chart_or_empty(
        fw_chart_method_waterbody(data, report()$sel,
                                  mode = input$method_wb_mode %||% "count"))
    })
    output$chart_waterbody <- plotly::renderPlotly({
      req(built()); fw_chart_or_empty(fw_chart_waterbody(report()$sel)) })
    output$duration   <- plotly::renderPlotly({
      req(built()); fw_chart_or_empty(fw_chart_duration(data, report()$sel)) })

    # ---- The results table's paging -----------------------------------------
    #
    # These read report() but do NOT rebuild it, so turning a page or changing
    # the page size redraws the table alone and leaves the rest of the report
    # standing.

    per_page <- reactive(as.integer(input$table_size %||% FW_PLAN_PAGE_SIZES[1]))

    # THE PAGE IS STATE THE SERVER OWNS, not something read back off the
    # buttons. fw_page_numbers() writes the chosen page into input$table_page
    # with Shiny.setInputValue(), and an input set that way has no binding in
    # the page for update*Input() or sendInputMessage() to talk to - so the
    # reset that used to live here was silently ignored, and a rebuild left the
    # reader on whatever page they had reached in the previous report: "Showing
    # 11-17 of 17". Same pattern as the contacts directory (mod_networking.R).
    page <- reactiveVal(1L)
    observeEvent(input$table_page, {
      n <- suppressWarnings(as.integer(input$table_page))
      if (length(n) == 1 && !is.na(n)) page(max(1L, n))
    })
    observeEvent(report(), page(1L))
    observeEvent(input$table_size, page(1L), ignoreInit = TRUE)

    # A bigger page size can still leave the reader past the last page.
    # Clamping beats an empty table with no explanation.
    table_page <- reactive({
      n_pages <- fw_plan_pages(nrow(report()$export), per_page())
      min(page(), n_pages)
    })

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

    # ---- Potential relevant contacts ----------------------------------------
    #
    # Reads fw_contacts_summary() ONLY. Redaction happens inside that function
    # and nowhere downstream, so there is no code path here that can see an
    # address a contact asked to keep private.
    contacts <- reactive({
      req(built())
      fw_plan_contacts(data, report()$sel)
    })

    contacts_per_page <- reactive(
      as.integer(input$contacts_size %||% FW_CONTACTS_PAGE_SIZES[1]))

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
          method_wb_mode = input$method_wb_mode %||% "count"
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
    h3(fw_t("plan", "download_heading")),
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
      downloadButton(ns("download"), fw_t("plan", "download"),
                     class = "btn btn-primary")
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
