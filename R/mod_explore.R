# mod_explore.R
# The Explore page: live filters, a whole-database panel, the map and three
# chart views.

library(shiny)
library(dplyr)

# The filters this page offers, by FW_FILTERS id.
FW_EXPLORE_FILTERS = c("continent", "country", "regime", "taxa", "family",
                        "taxa_beneficiary", "family_beneficiary")

#' @param choices fw_filter_choices() of the loaded data. The filter bar is
#'   static UI, not renderUI, so the page computes its selection once on load.
mod_explore_ui = function(id, choices) {
  ns = NS(id)
  ids = fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))
  tagList(
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          uiOutput(ns("kpis")),
          fw_explore_filter_bar(ns, choices, ids),
          uiOutput(ns("summary")),

          # The map note is visible above the map, plus Explore's own
          # contribute line, a <br> apart in one paragraph.
          div(
            class = "fw-explore-block",
            fw_block(fw_t("maps", "title"), NULL,
                     # .noWS: the note is pre-line, so stray newlines show.
                     tagList(p(class = "fw-plan__note", .noWS = "inside",
                               fw_t("maps", "note"), tags$br(.noWS = "outside"),
                               fw_home_links(fw_t("explore", "map_contribute"))),
                             fw_map_output(ns("map")), fw_map_note()),
                     note_as = "text", figure = TRUE)
          ),

          # One block, three views picked by "Show": number of attempts
          # (cumulative + methods), success rate (only the methods chart
          # changes), and duration. conditionalPanel, so hidden charts are
          # suspended rather than rebuilt.
          tags$section(
            class = "fw-explore-block fw-explore-views",
            h2(fw_t("explore", "views_title")),
            div(
              class = "fw-segmented",
              radioButtons(
                ns("view"), label = fw_t("explore", "views_show"),
                choices = stats::setNames(
                  c("count", "rate", "duration"),
                  c(fw_t("explore", "view_count"), fw_t("explore", "view_rate"),
                    fw_t("explore", "view_duration"))),
                selected = "count", inline = TRUE
              )
            ),
            # Blocks are subgrids (title, chart, caption) so both plots align.
            # See .fw-explore-charts.
            conditionalPanel(
              "input.view != 'duration'", ns = ns,
              div(
                class = "fw-explore-charts",
                fw_block(fw_t("explore", "cumulative"), NULL,
                         plotly::plotlyOutput(ns("cumulative"), height = "auto"),
                         figure = TRUE),
                conditionalPanel(
                  "input.view == 'count'", ns = ns,
                  fw_block(fw_t("explore", "method"), fw_t("explore", "method_note"),
                           tagList(plotly::plotlyOutput(ns("method_count"), height = "auto"),
                                   uiOutput(ns("method_multi_count"))),
                           figure = TRUE)
                ),
                conditionalPanel(
                  "input.view == 'rate'", ns = ns,
                  fw_block(fw_t("explore", "method"), fw_t("explore", "method_rate_note"),
                           tagList(plotly::plotlyOutput(ns("method_rate"), height = "auto"),
                                   uiOutput(ns("method_multi_rate"))),
                           figure = TRUE)
                )
              )
            ),
            conditionalPanel(
              "input.view == 'duration'", ns = ns,
              fw_block(fw_t("explore", "duration"), fw_t("plan", "r_duration_note"),
                       tagList(plotly::plotlyOutput(ns("duration"), height = "auto"),
                               uiOutput(ns("duration_caption"))),
                       figure = TRUE)
            )
          ),

          # Link on to Plan, styled like Plan's callout.
          div(class = "fw-callout fw-callout--next",
              p(fw_home_links(fw_t("explore", "to_plan"))))
        )
      )
    )
  )
}


mod_explore_server = function(id, data, in_review = 0L) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    choices = fw_filter_choices(data)
    ids = fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))

    observeEvent(input$clear, fw_filter_clear(session, ids, choices))

    fw_link_geo_filters(input, session, data, choices)

    # Fish family empties when Fish is deselected. See R/filters.R.
    fw_filter_when_observers(input, session, ids)

    # See fw_link_species_filters().
    fw_link_species_filters(input, session, data, choices, ids)


    # A number, or a reactive kept current by app.R. The database counts are
    # computed once.
    stats = fw_headline_stats(data)
    output$kpis = renderUI(fw_explore_db_panel(
      stats, if (is.function(in_review)) in_review() else in_review
    ))


    sel = reactive(fw_filter_apply(data, fw_filter_state(input, ids)))

    # ---- The strip and the map ----------------------------------------------

    output$summary = renderUI(fw_plan_summary_ui(fw_plan_summary(data, sel())))

    # ---- The map: drawn once, markers swapped ------------------------------
    #
    # Tiles, legend, card script and the whole database's photo dictionary are
    # sent once; filter changes send only markers, through a proxy.
    all_pts = fw_map_points(data, data$attempt)
    output$map = leaflet::renderLeaflet({
      m = fw_leaflet() |>
        fw_add_basemaps() |>
        fw_add_outcome_legend()
      # Opens on the whole database's extent, so the view doesn't jump.
      m = if (nrow(all_pts)) fw_fit_points(m, all_pts) else {
        v = FW_MAP$empty_view
        leaflet::setView(m, v$lng, v$lat, zoom = v$zoom)
      }
      fw_map_card_render(m, ns("map_detail"), fw_map_thumbs_all(data))
    })

    # Wait for the widget (proxy calls before it exists are dropped).
    # input$map_bounds signals it; copied to a reactiveVal so panning doesn't
    # rerun the observer.
    map_ready = reactiveVal(FALSE)
    observeEvent(input$map_bounds, map_ready(TRUE))

    # Ids currently drawn, so redrawing the same selection sends nothing.
    drawn = NULL
    # fw_safely(): a failed redraw keeps the old markers and says so.
    observe(fw_safely(session, {
      req(map_ready())
      s = sel()
      # Not while the tab is hidden (fitting a zero-size map breaks). Reading
      # it also reruns the draw when the tab is shown.
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
      # The card script wires the new markers via layeradd.
      fw_add_marker_layer(proxy, data, pts, detail = "lazy",
                          thumbs = fw_map_thumbs_all(data))
    }))
    fw_map_detail_server(input, session, "map_detail", data, sel)

    # Live charts. Only the picked view's are drawn; hidden ones are suspended.
    output$cumulative = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_cumulative(sel())))
    output$method_count = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_method(data, sel(), mode = "count")))
    output$method_rate = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_method(data, sel(), mode = "share")))
    output$duration = plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_duration(data, sel())))

    multi = function() {
      n = fw_n_multi_method(data, sel())
      if (n > 0) p(class = "fw-caption",
                   fw_fill(fw_t("plan", "r_method_multi"), n = fw_fmt_num(n)))
    }
    output$method_multi_count = renderUI(multi())
    output$method_multi_rate = renderUI(multi())
    output$duration_caption = renderUI(p(
      class = "fw-caption",
      fw_fill(fw_t("plan", "r_duration_missing"),
              n = fw_fmt_num(nrow(fw_duration_sel(data, sel()))))))
  })
}

# ---- Pieces ------------------------------------------------------------------

#' The whole-database panel
#'
#' Equal tiles, counts only, no rate (client).
fw_explore_db_panel = function(s, in_review = 0L) {
  tags$section(
    class = "fw-explore-db",
    h2(class = "fw-visually-hidden", fw_t("explore", "db_heading")),
    fw_kpi_strip(
      fw_kpi_stat(fw_fmt_num(s$attempts), fw_t("explore", "db_attempts")),
      fw_kpi_stat(fw_fmt_num(s$countries), fw_t("explore", "db_countries")),
      fw_kpi_stat(fw_fmt_num(s$species), fw_t("explore", "db_invasive")),
      # Species protected by successful attempts only, with ">" since
      # beneficiaries are under-recorded. Same as Home and Plan.
      fw_kpi_stat(paste0(">", fw_fmt_num(s$protected)), fw_t("explore", "db_beneficiary"),
                  tooltip = fw_t("explore", "db_beneficiary_tip")),
      # No success count: it invites a headline success rate (see charts.R).
      fw_kpi_stat(fw_fmt_num(in_review), fw_t("explore", "in_review"),
                  tooltip = fw_t("explore", "in_review_tip"))
    )
  )
}

#' The filter bar: the page's filters in one wrapping row
#'
#' Tooltips come from FW_FILTERS, shared with Plan.
fw_explore_filter_bar = function(ns, ch, ids) {
  multi = function(id) {
    fw_field(
      selectizeInput(ns(id), label = NULL, choices = ch[[id]], selected = NULL,
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = fw_t("filters", "all"),
                                    plugins = list("remove_button"))),
      label = fw_filter_label(id),
      tooltip = fw_filter_tip(id, ch),
      input_id = ns(id)
    )
  }
  # `when` filters (fish family) show only while Fish is picked.
  control = function(id) fw_filter_when_panel(ns, id, multi(id))
  drop = setdiff(names(FW_FILTERS), ids)

  tags$section(
    class = "fw-explore-filters",
    h2(class = "fw-explore-filters__heading fw-visually-hidden", fw_t("explore", "f_heading")),
    div(
      class = "fw-explore-filters__grid",
      lapply(fw_filter_draw_order(drop), control),
      div(
        class = "fw-filters__actions",
        actionButton(ns("clear"), fw_t("plan", "clear"),
                     class = "btn btn-outline-primary btn-sm")
      )
    )
  )
}
