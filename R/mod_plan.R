# mod_plan.R
# The Plan page: the report builder. Results render only when Build report is
# pressed (deliberate: no live redraw to cherry-pick from). Filters UI in
# mod_plan_filters.R, results UI in mod_plan_results.R.

library(shiny)
library(bslib)
library(dplyr)

#' The report builder page
mod_plan_ui = function(id) {
  ns = NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          # Green callout above the filters: what the report gives you.
          div(class = "fw-callout",
              lapply(fw_t("plan", "callout"), function(x) p(fw_emphasis(x)))),
          uiOutput(ns("filters")),
          # Client order: filters and buttons, the filters used, counts,
          # results. Empty until the first build.
          div(id = ns("built_anchor"), class = "fw-plan__built",
              uiOutput(ns("filters_summary")),
              uiOutput(ns("summary"))),
          div(id = ns("results_anchor"), class = "fw-plan__results",
              uiOutput(ns("zero")),
              # Only while the last build matched something (output$state).
              conditionalPanel("output.state == 'results'", ns = ns,
                               fw_plan_results_ui(ns)))
        )
      )
    )
  )
}

fw_plan_results_ui = function(ns) {
  tagList(
    # The download button floats in the bottom corner (client) but stays first
    # in the DOM (dev/plan_test.R checks). Hidden until a build matches
    # something, so it never needs disabling. See .fw-plan__results-head.
    div(
      class = "fw-plan__results-head",
      h2(class = "fw-visually-hidden", fw_t("plan", "r_heading")),
      actionButton(ns("download_open"), fw_t("plan", "download_open"),
                   class = "btn btn-primary fw-plan__download-open",
                   icon = icon("download"))
    ),
    # Client order: species, where, outcomes, water, methods, duration,
    # contacts, records. The PDF follows it (fw_pdf_body()).
    uiOutput(ns("species")),

    fw_block(
      fw_t("maps", "title"), fw_t("maps", "note"),
      tagList(
        fw_map_output(ns("map")),
        fw_map_note(),
        uiOutput(ns("map_missing"))
      ),
      # Note shown as text above the map, as on Explore.
      note_as = "text", figure = TRUE
    ),

    fw_block(
      fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
      uiOutput(ns("outcome_bars")),
      figure = TRUE
    ),

    # ---- What kind of water -----------------------------------------------

    # Hidden when one regime is filtered (fw_show_waterbody()).
    conditionalPanel("output.waterbody_shown == 'yes'", ns = ns, fw_block(
      fw_t("plan", "r_waterbody"), fw_t("plan", "r_waterbody_note"),
      tagList(
        # Count/rate toggle; bar labels keep the count in both.
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
        # Count leads; the rate view keeps the n in each label.
        fw_mode_toggle(ns("method_mode"),
                       fw_t("plan", "r_method_count"),
                       fw_t("plan", "r_method_share")),
        plotly::plotlyOutput(ns("methods"), height = "auto"),
        uiOutput(ns("method_caption"))
      ),
      figure = TRUE
    ),

    # ---- How long ---------------------------------------------------------

    fw_block(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      tagList(
        plotly::plotlyOutput(ns("duration"), height = "auto"),
        uiOutput(ns("duration_missing"))
      ),
      figure = TRUE
    ),

    fw_block(
      fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
      tagList(
        div(class = "fw-table-scroll", uiOutput(ns("contacts_body"))),
        uiOutput(ns("contacts_pager")),
        # Link to the full directory.
        div(class = "fw-callout fw-callout--next",
            p(fw_home_links(fw_t("plan", "r_contacts_all"))))
      ),
      # Page size on the title row.
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

    # ---- Finding one record -------------------------------------------------
    #
    # Last: a searchable table, ten rows a page.
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

mod_plan_server = function(id, data, meta = NULL) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    # From approved data only.
    choices = fw_filter_choices(data)
    ids = fw_plan_filter_ids()

    output$filters = renderUI(fw_plan_filters_ui(ns, choices))

    # ---- Linked filters --------------------------------------------------------
    fw_link_geo_filters(input, session, data, choices)

    fw_link_species_filters(input, session, data, choices, ids)

    fw_filter_when_observers(input, session, ids)


    output$filters_summary = renderUI({
      if (!built()) return(NULL)
      f = report()$filters
      rows = fw_filter_summary(f)

      # Skip untouched filters (fw_filter_summary() flags sliders).
      set = Filter(function(r) {
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

    # The slider's own labels are off, so print the range.
    output$years_readout = renderText({
      y = input$years
      if (is.null(y)) return("")
      paste(y[1], fw_t("filters", "range_of"), y[2])
    })

    # ---- The size control ---------------------------------------------------
    #
    # Its own uiOutput, redrawn when the regime changes the units on offer.
    output$size_control = renderUI({
      fw_plan_size_ui(
        ns, choices,
        fw_size_units(input$regime %||% character(0))
      )
    })

    # Real units under each log slider.
    for (unit in FW_SIZE_UNITS) {
      local({
        u = unit
        output[[paste0("size_readout_", u)]] = renderText({
          v = input[[paste0("size_", u)]]
          if (is.null(v)) return("")
          paste(fw_size_label(fw_size_unlog(v[1])), fw_t("filters", "range_of"),
                fw_size_label(fw_size_unlog(v[2])))
        })
      })
    }

    # ---- Building ----------------------------------------------------------
    #
    # Build snapshots the controls. Clear resets them and builds the default
    # report from fw_filter_defaults(), since the inputs don't update until the
    # flush ends.
    report = reactiveVal(NULL)
    build_from = function(f) {
      # fw_safely(), so an error (e.g. the export privacy check) doesn't end
      # the session.
      fw_safely(session, {
        sel = fw_filter_apply(data, f)
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

    built = reactive(!is.null(report()))

    observeEvent(input$build, {
      if (!isTRUE(build_from(fw_filter_state(input, ids, ch = choices)))) return()
      session$sendCustomMessage("fw-scroll-to", ns("built_anchor"))
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    observeEvent(input$clear, {
      fw_filter_clear(session, ids, choices)
      if (!isTRUE(build_from(fw_filter_defaults(ids, choices)))) return()
      session$sendCustomMessage("fw-announce", fw_fill(fw_t("plan", "built_announce"), n = fw_fmt_num(nrow(report()$sel))))
    })

    output$state = renderText({
      if (!built()) return("none")
      if (nrow(report()$sel) == 0) "zero" else "results"
    })
    outputOptions(output, "state", suspendWhenHidden = FALSE)

    output$zero = renderUI({
      if (!built()) return(NULL)
      r = report()
      if (nrow(r$sel) > 0) return(NULL)
      fw_plan_zero_ui(fw_filter_zero_hints(data, r$filters))
    })

    results = reactive({
      req(built())
      r = report()
      req(nrow(r$sel) > 0)
      r
    })

    # A count caption, or nothing when there is nothing to say.
    caption = function(key, n) {
      if (n > 0) p(class = "fw-caption", fw_fill(fw_t("plan", key), n = fw_fmt_num(n)))
    }

    output$summary = renderUI(fw_plan_summary_ui(fw_plan_summary(data, results()$sel)))

    # ---- Top species ----------------------------------------------------------
    #
    # One block per role, counted inside the build's own filters; a species
    # pick hides that role's block (fw_species_row_shown()).
    output$species = renderUI({
      sel = results()$sel
      f = results()$filters
      role_block = function(role_name) {
        title = fw_species_top_title(data, sel, role_name, f = f)
        if (is.null(title)) return(NULL)
        fw_block(title, NULL,
                 fw_species_tiles_ui(data, sel, role_name, limit = FW_PLAN_SPECIES_N,
                                     f = f))
      }
      tagList(role_block("invasive"), role_block("beneficiary"))
    })

    output$outcome_bars = renderUI(fw_outcome_bars_ui(results()$sel))

    output$map_missing = renderUI({
      sel = results()$sel
      caption("r_map_missing", sum(is.na(sel$latitude) | is.na(sel$longitude)))
    })
    output$method_caption = renderUI({
      txt = fw_method_caption_text(data, results()$sel)
      if (!is.null(txt)) p(class = "fw-caption", txt)
    })
    output$waterbody_shown = renderText(
      if (built() && fw_show_waterbody(report()$filters)) "yes" else "no")
    outputOptions(output, "waterbody_shown", suspendWhenHidden = FALSE)
    output$duration_missing = renderUI(p(
      class = "fw-caption",
      fw_fill(fw_t("plan", "r_duration_missing"),
              n = fw_fmt_num(nrow(fw_duration_sel(data, results()$sel))))))

    # Reset the toggles to count on every build and Clear.
    observeEvent(report(), {
      for (id in c("method_mode", "waterbody_mode")) {
        if (!identical(input[[id]] %||% "count", "count")) {
          updateRadioButtons(session, id, selected = "count")
        }
      }
    })

    # ---- The download picker, in a modal --------------------------------------
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
    # Estimated before the download (fw_pdf_size_estimate()), once per build.
    pdf_estimate = reactive(fw_pdf_size_estimate(data, results()$sel, results()$filters))
    output$download_warn = renderUI({
      if (!"pdf" %in% (input$download_parts %||% character(0))) return(NULL)
      est = pdf_estimate()
      if (est$mb < FW_PDF$warn_mb && est$pages < FW_PDF$warn_pages) return(NULL)
      p(class = "fw-plan__download-warn", role = "status",
        icon("triangle-exclamation"), " ",
        fw_fill(fw_t("plan", "pdf_warn"), mb = sprintf("%.1f", est$mb),
                pages = fw_fmt_num(est$pages)))
    })

    # ---- The map: drawn once, markers swapped -------------------------------
    #
    # Same pattern as Explore.
    output$map = leaflet::renderLeaflet({
      fw_leaflet() |>
        fw_add_basemaps() |>
        fw_add_outcome_legend() |>
        leaflet::setView(FW_MAP$empty_view$lng, FW_MAP$empty_view$lat,
                         zoom = FW_MAP$empty_view$zoom) |>
        fw_map_card_render(ns("map_detail"), fw_map_thumbs_all(data))
    })

    # Wait for the widget; skip redrawing what's already there.
    map_ready = reactiveVal(FALSE)
    observeEvent(input$map_bounds, map_ready(TRUE))
    drawn = NULL
    observe(fw_safely(session, {
      req(map_ready())
      s = results()$sel
      if (isTRUE(session$clientData[[paste0("output_", ns("map"), "_hidden")]])) return()
      key = s$attempt_id
      if (identical(key, drawn)) return()
      drawn <<- key

      pts = fw_map_points(data, s)
      proxy = leaflet::leafletProxy("map", session = session) |>
        leaflet::clearMarkerClusters()
      if (!nrow(pts)) {
        v = FW_MAP$empty_view
        leaflet::setView(proxy, v$lng, v$lat, zoom = v$zoom)
        return()
      }
      fw_add_marker_layer(proxy, data, pts, detail = "lazy",
                          thumbs = fw_map_thumbs_all(data))
    }))
    fw_map_detail_server(input, session, "map_detail", data, reactive(results()$sel))

    # The mode toggles redraw without a rebuild: same selection, other view.
    output$methods = plotly::renderPlotly({
      fw_chart_or_empty(
        fw_chart_method(data, results()$sel, mode = input$method_mode %||% "count"))
    })
    output$chart_waterbody = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_waterbody(results()$sel,
                                           mode = input$waterbody_mode %||% "count")))
    output$duration = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_duration(data, results()$sel)))

    contacts = reactive(fw_plan_contacts(data, results()$sel))

    contacts_per_page = reactive(
      as.integer(input$contacts_size %||% FW_PLAN_CONTACTS_PAGE_SIZES[1]))

    # The current page, owned by the server.
    contacts_page = reactiveVal(1L)
    observeEvent(input$contacts_page_to, {
      n = suppressWarnings(as.integer(input$contacts_page_to))
      if (length(n) == 1 && !is.na(n)) contacts_page(max(1L, n))
    })
    observeEvent(report(), contacts_page(1L))
    observeEvent(input$contacts_size, contacts_page(1L), ignoreInit = TRUE)

    contacts_shown = reactive(
      min(contacts_page(), fw_plan_pages(nrow(contacts()), contacts_per_page())))

    output$contacts_body = renderUI({
      fw_plan_contacts_ui(contacts(), contacts_shown(), contacts_per_page())
    })

    output$contacts_pager = renderUI({
      n_rows = nrow(contacts())
      if (n_rows == 0) return(NULL)
      n_pages = fw_plan_pages(n_rows, contacts_per_page())
      from = (contacts_shown() - 1L) * contacts_per_page() + 1L
      to = min(n_rows, contacts_shown() * contacts_per_page())
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
    # Paged like the contacts. Search strings built once per report; the box
    # is debounced.
    records_haystack = reactive(fw_plan_records_haystack(results()$export))
    records_q = debounce(reactive(input$records_q %||% ""), FW_PLAN_RECORDS_DEBOUNCE_MS)
    records_rows = reactive(fw_plan_records_find(records_haystack(), records_q()))

    records_page = reactiveVal(1L)
    observeEvent(input$records_page_to, {
      n = suppressWarnings(as.integer(input$records_page_to))
      if (length(n) == 1 && !is.na(n)) records_page(max(1L, n))
    })
    observeEvent(report(), records_page(1L))
    observeEvent(records_q(), records_page(1L), ignoreInit = TRUE)
    # Clear the search on a new build.
    observeEvent(report(), updateTextInput(session, "records_q", value = ""),
                 ignoreInit = TRUE)

    records_shown = reactive(
      min(records_page(), fw_plan_pages(length(records_rows()), FW_PLAN_RECORDS_PAGE_SIZE)))

    output$records_body = renderUI(
      fw_plan_records_ui(results()$export, records_rows(), records_shown(),
                         species = data$species))

    output$records_pager = renderUI({
      n_rows = length(records_rows())
      if (n_rows == 0) return(NULL)
      per = FW_PLAN_RECORDS_PAGE_SIZE
      n_pages = fw_plan_pages(n_rows, per)
      from = (records_shown() - 1L) * per + 1L
      to = min(n_rows, records_shown() * per)
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
    # One handler for whatever is ticked. filename() runs at click time.
    # withProgress shows a bar while the PDF renders (normal outputs wait for
    # the flush). Styled in .shiny-notification.
    output$download = downloadHandler(
      filename = function() fw_bundle_filename(input$download_parts),
      content = function(file) {
        r = report()
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

#' A count/share switch for one chart
#'
#' Radio buttons styled as a segmented control (.fw-segmented). Count first
#' and selected, so the evidence base is seen before the rate.
fw_mode_toggle = function(id, count_label, share_label) {
  div(
    class = "fw-segmented",
    radioButtons(
      id, label = fw_t("plan", "r_method_mode"),
      choices = stats::setNames(c("count", "share"), c(count_label, share_label)),
      selected = "count", inline = TRUE
    )
  )
}

fw_n_no_method = function(data, sel) {
  sum(!sel$attempt_id %in% data$attempt_method$attempt_id)
}

# The line under the method chart: attempts with no method and with several.
# NULL when neither. Used by Plan, the Detailed report and the PDF.
fw_method_caption_text = function(data, sel) {
  parts = c(
    if ((n = fw_n_no_method(data, sel)) > 0)
      fw_fill(fw_t("plan", "r_method_missing"), n = fw_fmt_num(n)),
    if ((n = fw_n_multi_method(data, sel)) > 0)
      fw_fill(fw_t("plan", "r_method_multi"), n = fw_fmt_num(n))
  )
  if (length(parts)) paste0(paste(sub("\\.$", "", parts), collapse = " - "), ".")
}

fw_plan_download_ui = function(ns, pdf = fw_pdf_available()) {
  part = function(id, label, note) {
    list(id = id, label = label, note = note)
  }
  # Same order as FW_BUNDLE_PARTS; keep in step.
  parts = list(
    part("pdf",  fw_t("plan", "download_pdf"),  fw_t("plan", "download_pdf_note")),
    part("records", fw_t("plan", "download_records"), fw_t("plan", "download_records_note")),
    part("xlsx", fw_t("plan", "download_xlsx"), fw_t("plan", "download_xlsx_note"))
  )
  # No PDF option without Quarto (usually only an issue locally).
  if (!pdf) parts = Filter(function(p) p$id != "pdf", parts)

  div(
    class = "fw-plan__download",
    p(class = "fw-plan__note", fw_t("plan", "download_lead")),
    div(
      class = "fw-download-picker",
      tags$fieldset(
        class = "fw-download-picker__parts",
        tags$legend(class = "form-label", fw_t("plan", "download_parts")),
        checkboxGroupInput(
          ns("download_parts"), label = NULL,
          # Name and note on one line.
          choiceNames = lapply(parts, function(p) {
            tagList(span(class = "fw-download-picker__label", p$label),
                    span(class = "fw-download-picker__note", p$note))
          }),
          choiceValues = vapply(parts, function(p) p$id, character(1)),
          # Both reports ticked by default (client).
          selected = if (pdf) c("pdf", "records") else "records"
        ),
        if (!pdf) p(class = "fw-plan__note", fw_t("plan", "pdf_unavailable")),
        p(class = "fw-plan__note", fw_t("plan", "download_none")),
        # The PDF size warning (output$download_warn).
        uiOutput(ns("download_warn"))
      ),
      # The onclick tells the server to close the modal (Shiny never sees a
      # download link's click). The download itself is unaffected.
      downloadButton(ns("download"), fw_t("plan", "download"),
                     class = "btn btn-primary",
                     onclick = sprintf(
                       "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
                       ns("download_taken")))
    )
  )
}

# The Download button is disabled while nothing is ticked, by a delegated
# listener in fw_client_script() (scripts inserted into a modal don't run).

fw_plan_zero_ui = function(hints) {
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
