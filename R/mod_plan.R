# mod_plan.R
# The report builder.

library(shiny)
library(bslib)
library(dplyr)

#' The report builder page
#'

mod_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          # THE GREEN CARD, above the filters (client, 24 Sept 2026): what the
          # report gives you before you are asked to describe anything. It
          # shares .fw-preamble's treatment - the app's one "read this first"
          # block - through the .fw-callout selector in _components.scss.
          div(class = "fw-callout",
              lapply(fw_t("plan", "callout"), function(x) p(fw_emphasis(x)))),
          uiOutput(ns("filters")),
          # THE ORDER UNDER THE BUTTONS IS THE CLIENT'S (29 Sept 2026): the
          # filters, Build and Clear, then what was built - the filters it was
          # built from, then the counts - then the results. Both are empty
          # until the first build.
          div(id = ns("built_anchor"), class = "fw-plan__built",
              uiOutput(ns("filters_summary")),
              uiOutput(ns("summary"))),
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

fw_plan_results_ui <- function(ns) {
  tagList(
    # THE DOWNLOAD FLOATS OVER THE RESULTS (client, 24 Sept 2026). It used to
    # be the last thing on the page, below two tables, and the client's
    # objection was that a reader had no way of knowing any of this was
    # exportable until they had scrolled past all of it. It then sat in a
    # sticky row of its own at the top of the results, which cost a line of
    # the page for one button; it is now fixed in the bottom corner instead,
    # out of the flow entirely. See .fw-plan__results-head in
    # _components.scss - the row is still the element, it just has no height.
    #
    # IT STAYS FIRST IN THE DOM even though it is drawn last on the screen, so
    # the results open on the thing this page is for rather than ending on it
    # - dev/plan_test.R asserts that order.
    #
    # IT DOES NOT NEED DISABLING BEFORE A BUILD. This whole skeleton is hidden
    # until a build has matched something, so the button cannot be seen until
    # there is a report behind it - which is the same guarantee, without a
    # disabled control sitting on the page inviting a click. It is why the
    # floating button appears on the build and not before it.
    div(
      class = "fw-plan__results-head",
      h2(class = "fw-visually-hidden", fw_t("plan", "r_heading")),
      actionButton(ns("download_open"), fw_t("plan", "download_open"),
                   class = "btn btn-primary fw-plan__download-open",
                   icon = icon("download"))
    ),
    # THE ORDER BELOW IS THE CLIENT'S (23 Sept 2026) and is not arbitrary: the
    # counts, then who is involved, then where, then what happened, then what
    # it happened in, then what was done, then how long it took. Setting and
    # outcome before method, so a reader meets the evidence base before the
    # techniques. The PDF report follows the same sequence - see fw_pdf_body()
    # in R/report_pdf.R - with one exception noted there. The counts
    # (output$summary) moved up under the Build button on 29 Sept 2026 - see
    # mod_plan_ui().
    uiOutput(ns("species")),

    fw_block(
      fw_t("maps", "title"), fw_t("maps", "note"),
      tagList(
        fw_map_output(ns("map")),
        fw_map_note(),
        uiOutput(ns("map_missing"))
      ),
      # Visible above the map, as on Explore (client, 29 Sept 2026).
      note_as = "text", figure = TRUE
    ),

    fw_block(
      fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
      uiOutput(ns("outcome_bars")),
      figure = TRUE
    ),

    # ---- What kind of water -----------------------------------------------

    # NOT DRAWN when the build filtered to one regime (client, 30 Sept 2026):
    # a still/flowing split of only still water is one bar restating the
    # filter. See fw_show_waterbody() and output$waterbody_shown.
    conditionalPanel("output.waterbody_shown == 'yes'", ns = ns, fw_block(
      fw_t("plan", "r_waterbody"), fw_t("plan", "r_waterbody_note"),
      tagList(
        # Its own toggle, like the method chart. The denominator here is
        # the kind of water's own attempts, so share answers "in a lake, how
        # often did it work" - and the count stays in the bar's label either
        # way, so a share off four attempts still shows it is off four.
        fw_mode_toggle(ns("waterbody_mode"),
                       fw_t("plan", "r_waterbody_count"),
                       fw_t("plan", "r_waterbody_share")),
        plotly::plotlyOutput(ns("chart_waterbody"), height = "auto")
      ),
      figure = TRUE
    )),

    fw_block(
      fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
      tagList(
        # One chart, two questions. "Count" answers how much evidence stands
        # behind a method; "Success rate" answers how often it worked. Count
        # still leads, which is what stops three attempts being read as a rate:
        # the mode is named for the question it answers, and the bar's label
        # carries the total in both modes so the n is never off the chart.
        fw_mode_toggle(ns("method_mode"),
                       fw_t("plan", "r_method_count"),
                       fw_t("plan", "r_method_share")),
        plotly::plotlyOutput(ns("methods"), height = "auto"),
        uiOutput(ns("method_caption"))
      ),
      figure = TRUE
    ),

    # ---- How long ---------------------------------------------------------
    #
    # THERE WAS A METHODS-BY-WATERBODY CHART AFTER THIS ONE and the client
    # deleted it outright (23 Sept 2026), along with its data half, its ggplot
    # twin in the PDF, its copy, its palettes and its toggle. It asked a
    # crossed question that neither axis answered well, and the two charts
    # above it already carry both halves.

    fw_block(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      tagList(
        plotly::plotlyOutput(ns("duration"), height = "auto"),
        uiOutput(ns("duration_missing"))
      ),
      figure = TRUE
    ),

    fw_block(
      # THE NOTE IS AN (i) HERE TOO (client, 23 Sept 2026). It was the one
      # block held back as visible text, on the grounds that "they are not
      # expecting to hear from you" has to be read rather than asked for. The
      # client has since asked for it in the popup with every other note, so
      # nothing on the results page carries a note as a paragraph any more.
      fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
      tagList(
        div(class = "fw-table-scroll", uiOutput(ns("contacts_body"))),
        uiOutput(ns("contacts_pager")),
        # The way on to the whole directory, in the green box Explore uses for
        # its own (client, 1 Oct 2026).
        div(class = "fw-callout fw-callout--next",
            p(fw_home_links(fw_t("plan", "r_contacts_all"))))
      ),
      # ON THE TITLE'S ROW (client, 1 Oct 2026), so the page size does not
      # cost a line of its own between the heading and the table.
      tools = div(
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
      )
    ),

    # ---- Finding one record (client, 1 Oct 2026) ----------------------------
    #
    # LAST, so the report reads as before and this is the place to look a
    # record up. A page of ten rows rather than the Detailed report's scroll
    # of every card, so the footer and its links stay in reach below it.
    fw_block(
      fw_t("plan", "r_records"), fw_t("plan", "r_records_note"),
      tagList(
        div(
          class = "fw-records",
          div(
            class = "fw-records__find",
            tags$label(class = "form-label", `for` = ns("records_q"),
                       fw_t("export", "records_filter_label")),
            textInput(ns("records_q"), label = NULL, width = "100%",
                      placeholder = fw_t("export", "records_filter_hint"))
          ),
          div(class = "fw-table-scroll", uiOutput(ns("records_body"))),
          uiOutput(ns("records_pager"))
        ),
        fw_plan_records_script()
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
    # Same call as mod_explore.R. The reasoning, and why it cannot loop, is at
    # fw_link_geo_filters() in R/filters.R.
    fw_link_geo_filters(input, session, data, choices)

    # The invasive species follow the invasive kind of animal and family; the
    # three protected pickers offer only what the attempts matching every other
    # filter protected. See fw_link_species_filters().
    fw_link_species_filters(input, session, data, choices, ids)

    # The fish family pair empties itself when Fish is deselected, as on
    # Explore. See fw_filter_when_observers() in R/filters.R.
    fw_filter_when_observers(input, session, ids)


    output$filters_summary <- renderUI({
      if (!built()) return(NULL)
      f <- report()$filters
      rows <- fw_filter_summary(f)

      # An untouched year slider is not a filter. fw_filter_summary() already
      # says which it is; comparing the bounds here with identical() never
      # matched, because the slider sends doubles and the data's ends are
      # integers, so every build listed "1934 to 2025".
      set <- Filter(function(r) {
        if (identical(r$value, fw_t("export", "filter_all"))) return(FALSE)
        if (identical(r$value, fw_t("export", "filter_yes"))) return(FALSE)
        if (isTRUE(r$untouched)) return(FALSE)
        TRUE
      }, rows)
      div(
        class = "fw-plan-filters__applied",
        span(class = "fw-plan-filters__summary-label", fw_t("plan", "f_applied")),
        if (!length(set)) span(class = "fw-plan-filters__summary-list", fw_t("filters", "all"))
        else span(
          class = "fw-plan-filters__summary-list",
          lapply(set, function(r) {
            span(class = "fw-plan-filters__summary-item",
                 tags$b(r$setting), ": ", r$value)
          })
        )
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
    # ITS OWN uiOutput, NESTED INSIDE THE PANEL
    output$size_control <- renderUI({
      fw_plan_size_ui(
        ns, choices,
        fw_size_units(input$regime %||% character(0))
      )
    })

    # Real units under each log slider - mo log units.
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

    # ---- Building ----------------------------------------------------------
    #
    # A VALUE, SET BY TWO BUTTONS. Build snapshots the controls; Clear resets
    # them and builds the default report straight away (client, 29 Sept 2026)
    # rather than leaving the last report on screen under a cleared panel.
    #
    # CLEAR BUILDS FROM fw_filter_defaults(), NOT FROM THE INPUTS. The update
    # calls in fw_filter_clear() only reach the browser at the end of this
    # flush, so the inputs still hold the old selection while this runs.
    report <- reactiveVal(NULL)
    build_from <- function(f) {
      # AN ERROR HERE MUST NOT END THE SESSION. fw_export_frame() refuses, by
      # design, to hand over anything that fails its privacy check, and an
      # uncaught error in an observer closes the connection - the reader saw
      # "Disconnected from the server". See fw_safely() in R/ui_helpers.R.
      fw_safely(session, {
        sel <- fw_filter_apply(data, f)
        report(list(
          filters = f,
          sel     = sel,
          export  = fw_export_frame(data, sel$attempt_id)
        ))
        fw_track(session, "report_built",
                 c(fw_track_filters(f, ids, choices), list(n_records = nrow(sel))))
        TRUE
      })
    }

    built <- reactive(!is.null(report()))

    observeEvent(input$build, {
      if (!isTRUE(build_from(fw_filter_state(input, ids, ch = choices)))) return()
      session$sendCustomMessage("fw-scroll-to", ns("built_anchor"))
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    # Driven from the same id list as everything else, so a filter added to
    # FW_FILTERS is cleared without a second edit here.
    observeEvent(input$clear, {
      fw_filter_clear(session, ids, choices)
      if (!isTRUE(build_from(fw_filter_defaults(ids, choices)))) return()
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    output$state <- renderText({
      if (!built()) return("none")
      if (nrow(report()$sel) == 0) "zero" else "results"
    })
    outputOptions(output, "state", suspendWhenHidden = FALSE)

    # NOTHING here before a build.
    output$zero <- renderUI({
      if (!built()) return(NULL)
      r <- report()
      if (nrow(r$sel) > 0) return(NULL)
      fw_plan_zero_ui(fw_filter_zero_hints(data, r$filters))
    })

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
    # TWO PLAIN BLOCKS, each titled like every other block on the page: "Top
    # three invasive species targeted (of 41 total)", then "Top three species
    # protected (of >12 total)". 
    #
    # INSIDE THE BUILD'S OWN FILTERS (client, 30 Sept 2026): a family or kind
    # of animal narrows the species counted, and a species pick removes that
    # role's row. See fw_species_rows() and fw_species_row_shown().
    output$species <- renderUI({
      sel <- results()$sel
      f <- results()$filters
      role_block <- function(role_name) {
        title <- fw_species_top_title(data, sel, role_name, f = f)
        if (is.null(title)) return(NULL)
        fw_block(title, NULL,
                 fw_species_tiles_ui(data, sel, role_name, limit = FW_PLAN_SPECIES_N,
                                     f = f))
      }
      tagList(role_block("invasive"), role_block("beneficiary"))
    })

    output$outcome_bars <- renderUI(fw_outcome_bars_ui(results()$sel))

    output$map_missing <- renderUI({
      sel <- results()$sel
      caption("r_map_missing", sum(is.na(sel$latitude) | is.na(sel$longitude)))
    })
    output$method_caption <- renderUI({
      txt <- fw_method_caption_text(data, results()$sel)
      if (!is.null(txt)) p(class = "fw-caption", txt)
    })
    output$waterbody_shown <- renderText(
      if (built() && fw_show_waterbody(report()$filters)) "yes" else "no")
    outputOptions(output, "waterbody_shown", suspendWhenHidden = FALSE)
    output$duration_missing <- renderUI(p(
      class = "fw-caption",
      fw_fill(fw_t("plan", "r_duration_missing"),
              n = fw_fmt_num(nrow(fw_duration_sel(data, results()$sel))))))

    # COUNT LEADS ON EVERY BUILD, and on every Clear. toggle for %.
    observeEvent(report(), {
      for (id in c("method_mode", "waterbody_mode")) {
        if (!identical(input[[id]] %||% "count", "count")) {
          updateRadioButtons(session, id, selected = "count")
        }
      }
    })

    # ---- The download picker, in an overlay ---------------------------------
    #
    observeEvent(input$download_open, {
      showModal(modalDialog(
        title = fw_t("plan", "download_heading"),
        fw_plan_download_ui(ns),
        footer = modalButton(fw_t("plan", "download_close")),
        easyClose = TRUE,
        class = "fw-download-modal"
      ))
    })

    observeEvent(input$download_taken, removeModal())

    # ---- The PDF size warning -----------------------------------------------
    #
    # BEFORE THE DOWNLOAD, NOT AFTER IT. Size is ESTIMATED from the selection -
    # what grows a PDF is the contacts table and the species photographs, both
    # of which are known creation. See fw_pdf_size_estimate() in R/report_pdf.R.
    #
    # Once per build, not once per tick: the estimate reads the selection only.
    pdf_estimate <- reactive(fw_pdf_size_estimate(data, results()$sel, results()$filters))
    output$download_warn <- renderUI({
      if (!"pdf" %in% (input$download_parts %||% character(0))) return(NULL)
      est <- pdf_estimate()
      if (est$mb < FW_PDF$warn_mb && est$pages < FW_PDF$warn_pages) return(NULL)
      p(class = "fw-plan__download-warn", role = "status",
        icon("triangle-exclamation"), " ",
        fw_fill(fw_t("plan", "pdf_warn"), mb = sprintf("%.1f", est$mb),
                pages = fw_fmt_num(est$pages)))
    })

    # ---- The map: drawn once, markers swapped -------------------------------
    #
    # THE SAME PATTERN AS THE EXPLORE PAGE.
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
    # fw_safely(): a failed redraw leaves the old markers up and says so,
    # rather than ending the session. See R/ui_helpers.R.
    observe(fw_safely(session, {
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
    }))
    fw_map_detail_server(input, session, "map_detail", data, reactive(results()$sel))

    # The mode toggle is the ONE control that redraws without a rebuild. It does
    # not change the selection, only how the same numbers are drawn.
    output$methods <- plotly::renderPlotly({
      fw_chart_or_empty(
        fw_chart_method(data, results()$sel, mode = input$method_mode %||% "count"))
    })
    # Its own toggle, same reason: it redraws the same numbers a different way
    # and does not change the selection.
    output$chart_waterbody <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_waterbody(results()$sel,
                                           mode = input$waterbody_mode %||% "count")))
    output$duration <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_duration(data, results()$sel)))

    # THE RESULTS TABLE'S PAGING WENT WITH THE TABLE.
    contacts <- reactive(fw_plan_contacts(data, results()$sel))

    contacts_per_page <- reactive(
      as.integer(input$contacts_size %||% FW_PLAN_CONTACTS_PAGE_SIZES[1]))

    # THE PAGE IS STATE THE SERVER OWNS,
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

    # ---- The record search --------------------------------------------------
    #
    # The same paging as the contacts above. The search strings are built once
    # per report, not per keystroke; the box is debounced so typing a word is
    # one search, not one per letter.
    records_haystack <- reactive(fw_plan_records_haystack(results()$export))
    records_q <- debounce(reactive(input$records_q %||% ""), FW_PLAN_RECORDS_DEBOUNCE_MS)
    records_rows <- reactive(fw_plan_records_find(records_haystack(), records_q()))

    records_page <- reactiveVal(1L)
    observeEvent(input$records_page_to, {
      n <- suppressWarnings(as.integer(input$records_page_to))
      if (length(n) == 1 && !is.na(n)) records_page(max(1L, n))
    })
    observeEvent(report(), records_page(1L))
    observeEvent(records_q(), records_page(1L), ignoreInit = TRUE)
    # A new build starts with an empty box: the last report's search would
    # otherwise quietly narrow the new one.
    observeEvent(report(), updateTextInput(session, "records_q", value = ""),
                 ignoreInit = TRUE)

    records_shown <- reactive(
      min(records_page(), fw_plan_pages(length(records_rows()), FW_PLAN_RECORDS_PAGE_SIZE)))

    output$records_body <- renderUI(
      fw_plan_records_ui(results()$export, records_rows(), records_shown()))

    output$records_pager <- renderUI({
      n_rows <- length(records_rows())
      if (n_rows == 0) return(NULL)
      per <- FW_PLAN_RECORDS_PAGE_SIZE
      n_pages <- fw_plan_pages(n_rows, per)
      from <- (records_shown() - 1L) * per + 1L
      to <- min(n_rows, records_shown() * per)
      div(
        class = "fw-pager",
        p(class = "fw-caption",
          sprintf("%s %s-%s %s %s", fw_t("plan", "r_table_showing"),
                  fw_fmt_num(from), fw_fmt_num(to), fw_t("common", "of"),
                  fw_fmt_num(n_rows))),
        fw_page_numbers(ns("records_page_to"), records_shown(), n_pages)
      )
    })

    # ---- The download -------------------------------------------------------
    #
    # ONE HANDLER AND A PICKER, replacing a spreadsheet button and a report
    # button. Two buttons made the reader choose between the data and the
    # document when most of them wanted both.
    #
    # WHAT YOU TICK IS WHAT YOU GET (client, 24 Sept 2026). A methods-and-
    # caveats .txt used to ride along whatever else was chosen, so ticking the
    # PDF handed back a zip of two files. Each document now closes on that
    # section itself - see fw_closing_blocks() in R/export.R - which is what
    # the travelling text file was for, and one tick now downloads one file.
    #
    # filename is a function evaluated at click time, so it can read the
    # checkboxes and name a .zip or the single file as appropriate.
    #
    # A PROGRESS BAR WHILE IT IS BUILT (client, 21 Sept 2026). The PDF takes
    # several seconds - Quarto renders it on the server - and the browser shows
    # nothing until the first byte arrives, so the click looked ignored.
    # withProgress rather than anything drawn in the modal: Shiny sends its
    # progress messages straight away, while ordinary output updates wait for
    # a flush that does not happen until the download is done. The bar wears
    # the FWISE badge (.shiny-notification in _components.scss).
    output$download <- downloadHandler(
      filename = function() fw_bundle_filename(input$download_parts),
      content = function(file) {
        r <- report()
        withProgress(message = fw_t("plan", "progress_title"), value = 0, {
          fw_write_bundle(
            path = file, parts = input$download_parts, data = data,
            sel = r$sel, export = r$export, filters = r$filters, meta = meta,
            progress = function(value, detail) setProgress(value, detail = detail)
          )
        })
        fw_track(session, "download",
                 fw_track_download_detail(input$download_parts, nrow(r$sel)))
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

fw_n_no_method <- function(data, sel) {
  sum(!sel$attempt_id %in% data$attempt_method$attempt_id)
}

# ONE LINE UNDER THE METHOD CHART (client, 30 Sept 2026): the no-method and
# more-than-one-method counts, joined by " - " when both apply. NULL when
# neither does. Plan page, Detailed report and PDF all print this.
fw_method_caption_text <- function(data, sel) {
  parts <- c(
    if ((n <- fw_n_no_method(data, sel)) > 0)
      fw_fill(fw_t("plan", "r_method_missing"), n = fw_fmt_num(n)),
    if ((n <- fw_n_multi_method(data, sel)) > 0)
      fw_fill(fw_t("plan", "r_method_multi"), n = fw_fmt_num(n))
  )
  if (length(parts)) paste0(paste(sub("\\.$", "", parts), collapse = " - "), ".")
}

fw_plan_download_ui <- function(ns, pdf = fw_pdf_available()) {
  part <- function(id, label, note) {
    list(id = id, label = label, note = note)
  }
  # THE READING ORDER, NOT THE FILE-SIZE ORDER (client, 23 Sept 2026): the
  # finished report first, then every record in full, then the raw spreadsheet
  # last. It matches FW_BUNDLE_PARTS in R/export.R, which is the order the
  # files are written and named in; keep the two in step.
  parts <- list(
    part("pdf",  fw_t("plan", "download_pdf"),  fw_t("plan", "download_pdf_note")),
    part("records", fw_t("plan", "download_records"), fw_t("plan", "download_records_note")),
    part("xlsx", fw_t("plan", "download_xlsx"), fw_t("plan", "download_xlsx_note"))
  )
  # NO PDF CHECKBOX WHERE NO PDF CAN BE MADE. Only tickable is quarto is present - it is in
  # Posit - sometimes not locally if working locally. 
  if (!pdf) parts <- Filter(function(p) p$id != "pdf", parts)

  div(
    class = "fw-plan__download",
    p(class = "fw-plan__note", fw_t("plan", "download_lead")),
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
          # The two reports (client, 29 Sept 2026: the detailed report is the
          # one a keen reader lives in).
          selected = if (pdf) c("pdf", "records") else "records"
        ),
        if (!pdf) p(class = "fw-plan__note", fw_t("plan", "pdf_unavailable")),
        p(class = "fw-plan__note", fw_t("plan", "download_none")),
        # THE SIZE WARNING, drawn by the server from the reader's selection and
        # their ticks - see output$download_warn in mod_plan_server(). Inside
        # the fieldset, under the boxes, so it is read with the choice it is
        # about and before the Download button.
        uiOutput(ns("download_warn"))
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

# THE DOWNLOAD BUTTON IS DISABLED WHILE NOTHING IS TICKED, and that is done in
# fw_client_script() (R/ui_helpers.R) rather than here.
#
# There is nothing to download with an empty selection any more: the methods-
# and-caveats text that used to travel regardless is gone (client, 24 Sept
# 2026), so an empty picker would ask the server for a file that has no name.
# fw_bundle_parts() has a floor for that case; a reader should not have to
# reach it.
#
# IT CANNOT BE AN INLINE SCRIPT IN THIS PICKER. The picker is built into a
# modalDialog, and this app does not trust a <script> inserted with dynamic
# content to run - which is why fw_popover_script() re-initialises popovers
# from a MutationObserver instead of shipping one per render. The guard is a
# delegated listener on the document, mounted once with everything else.
#
# The picker's default selection is never empty, so there is no initial state
# to set: no aria-disabled attribute means enabled, which is correct on open.

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
