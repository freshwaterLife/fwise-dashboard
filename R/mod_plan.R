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
    uiOutput(ns("species")),

    fw_block(
      fw_t("plan", "r_map"), fw_t("plan", "r_map_note"),
      tagList(
        fw_map_output(ns("map")),
        fw_map_note(),
        uiOutput(ns("map_missing"))
      )
    ),

    fw_block(
      fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
      uiOutput(ns("outcome_bars"))
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
        uiOutput(ns("method_missing"))
      )
    ),

    # ---- How long ---------------------------------------------------------
    #

    fw_block(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      tagList(
        plotly::plotlyOutput(ns("duration"), height = "auto"),
        uiOutput(ns("duration_missing"))
      )
    ),

    # ---- What kind of water -----------------------------------------------

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
      fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
      note_as = "text",
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
    # Same call as mod_explore.R. The reasoning, and why it cannot loop, is at
    # fw_link_geo_filters() in R/filters.R.
    fw_link_geo_filters(input, session, data, choices)

    # Driven from the same id list as everything else, so a filter added to
    # FW_FILTERS is cleared without a second edit here.
    observeEvent(input$clear, fw_filter_clear(session, ids, choices))


    output$filters_summary <- renderUI({
      if (!built()) return(NULL)
      f <- report()$filters
      rows <- fw_filter_summary(f)
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

    report <- eventReactive(input$build, {
      f <- fw_filter_state(input, ids, ch = choices)
      sel <- fw_filter_apply(data, f)
      list(
        filters = f,
        sel     = sel,
        export  = fw_export_frame(data, sel$attempt_id)
      )
    })

    built <- reactive(!is.null(input$build) && input$build > 0)

    observeEvent(input$build, {
      session$sendCustomMessage("fw-collapse", ns("filters_disclosure"))
      session$sendCustomMessage("fw-scroll-to", ns("results_anchor"))
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
    output$species <- renderUI({
      sel <- results()$sel
      role_block <- function(role_name) {
        title <- fw_species_top_title(data, sel, role_name)
        if (is.null(title)) return(NULL)
        fw_block(title, NULL,
                 fw_species_tiles_ui(data, sel, role_name, limit = FW_PLAN_SPECIES_N))
      }
      tagList(role_block("invasive"), role_block("beneficiary"))
    })

    output$outcome_bars <- renderUI(fw_outcome_bars_ui(results()$sel))

    output$map_missing <- renderUI({
      sel <- results()$sel
      caption("r_map_missing", sum(is.na(sel$latitude) | is.na(sel$longitude)))
    })
    output$method_missing <- renderUI(
      caption("r_method_missing", fw_n_no_method(data, results()$sel)))
    output$duration_missing <- renderUI(p(
      class = "fw-caption",
      fw_fill(fw_t("plan", "r_duration_missing"),
              n = fw_fmt_num(nrow(fw_duration_sel(data, results()$sel))))))

    # COUNT LEADS ON EVERY BUILD. toggle for %.
    observeEvent(input$build, {
      for (id in c("method_mode", "method_wb_mode", "waterbody_mode")) {
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
    pdf_estimate <- reactive(fw_pdf_size_estimate(data, results()$sel))
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
    # not change the selection, only how the same numbers are drawn.
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
            method_mode = input$method_mode %||% "count",
            method_wb_mode = input$method_wb_mode %||% "count",
            waterbody_mode = input$waterbody_mode %||% "count",
            progress = function(value, detail) setProgress(value, detail = detail)
          )
        })
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

fw_plan_download_ui <- function(ns, pdf = fw_pdf_available()) {
  part <- function(id, label, note) {
    list(id = id, label = label, note = note)
  }
  parts <- list(
    part("xlsx", fw_t("plan", "download_xlsx"), fw_t("plan", "download_xlsx_note")),
    part("csv",  fw_t("plan", "download_csv"),  fw_t("plan", "download_csv_note")),
    part("pdf",  fw_t("plan", "download_pdf"),  fw_t("plan", "download_pdf_note")),
    part("records", fw_t("plan", "download_records"), fw_t("plan", "download_records_note"))
  )
  # NO PDF CHECKBOX WHERE NO PDF CAN BE MADE. Only tickable is quarto is present - it is in
  # Posit - sometimes not locally if working locally. 
  if (!pdf) parts <- Filter(function(p) p$id != "pdf", parts)

  div(
    class = "fw-plan__download",
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
          selected = if (pdf) c("xlsx", "pdf") else "xlsx"
        ),
        if (!pdf) p(class = "fw-plan__note", fw_t("plan", "pdf_unavailable")),
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
