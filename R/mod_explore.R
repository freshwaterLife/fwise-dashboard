# mod_explore.R

library(shiny)
library(dplyr)

# The filters this page offers, by registry id. Everything else in FW_FILTERS
# is dropped here and kept on the report builder.
FW_EXPLORE_FILTERS <- c("continent", "country", "regime", "taxa", "family",
                        "taxa_beneficiary", "family_beneficiary")

#' @param choices fw_filter_choices() of the loaded data. The filter bar is
#'   built here rather than in a renderUI, and that is the fix for the page
#'   loading twice: an input drawn by renderUI does not exist on the first
#'   flush, so the selection was computed without it, the map and both charts
#'   were drawn and sent, and then the input's first (empty) value arrived as a
#'   change and all of it went again. An input in the static UI is in the set
#'   the browser sends when it connects, so the selection is computed once.
mod_explore_ui <- function(id, choices) {
  ns <- NS(id)
  ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))
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

          # A TITLED BLOCK LIKE EVERY OTHER, since 23 Sept 2026: the map's
          # note used to be a paragraph above it and is now the (i) beside a
          # heading, which is the rule the rest of the app already followed.
          # The Mercator note stays a visible line under the map - it is a
          # statement about the picture the reader is looking at, not an
          # explanation they have to ask for.
          div(
            class = "fw-explore-block",
            fw_block(fw_t("maps", "title"), fw_t("maps", "note"),
                     tagList(fw_map_output(ns("map")), fw_map_note()))
          ),

          div(
            class = "fw-explore-charts",
            # THE EMPTY CONTROLS ROW IS NOT A LEFTOVER. The method chart has a
            # count/share switch above it and this one has none; each block is
            # a subgrid of the same three rows (see .fw-explore-charts), so an
            # empty row here is what keeps the two plots level side by side.
            fw_block(fw_t("explore", "cumulative"),
                     NULL,
                     tagList(
                       div(class = "fw-explore-charts__controls"),
                       plotly::plotlyOutput(ns("cumulative"), height = "auto")
                     )),
            fw_block(fw_t("explore", "method"),
                     fw_t("explore", "method_note"),
                     tagList(
                       div(class = "fw-explore-charts__controls",
                           fw_mode_toggle(ns("method_mode"),
                                          fw_t("plan", "r_method_count"),
                                          fw_t("plan", "r_method_share"))),
                       plotly::plotlyOutput(ns("method"), height = "auto")
                     ))
          )
        )
      )
    )
  )
}


mod_explore_server <- function(id, data, in_review = 0L) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choices <- fw_filter_choices(data)
    ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))

    observeEvent(input$clear, fw_filter_clear(session, ids, choices))

    fw_link_geo_filters(input, session, data, choices)

    # The fish family pair empties itself when Fish is deselected. Shared with
    # the report builder - see R/filters.R.
    fw_filter_when_observers(input, session, ids)


    output$kpis <- renderUI(fw_explore_db_panel(fw_headline_stats(data), in_review))


    sel <- reactive(fw_filter_apply(data, fw_filter_state(input, ids)))

    # ---- The strip and the map ----------------------------------------------

    output$summary <- renderUI(fw_plan_summary_ui(fw_plan_summary(data, sel())))

    # ---- The map: drawn once, markers swapped ------------------------------
    #
    # THE WIDGET DOES NOT DEPEND ON THE SELECTION. It used to, so every filter
    # change rebuilt the whole map - tiles, legend, card script and 911 cards,
    # 2.5 MB - and the reader watched it blank and reload. Now the tiles, the
    # legend, the card script and the photograph dictionary are sent once, and
    # a filter change sends only the markers, through a proxy.
    #
    # THE DICTIONARY IS THE WHOLE DATABASE'S, so any selection the proxy draws
    # later is already covered by it. See fw_map_card_render().
    all_pts <- fw_map_points(data, data$attempt)
    output$map <- leaflet::renderLeaflet({
      m <- fw_leaflet() |>
        fw_add_basemaps() |>
        fw_add_outcome_legend()
      # Opened on the whole database's extent, which is what the first
      # selection is, so the markers arrive without the view jumping.
      m <- if (nrow(all_pts)) fw_fit_points(m, all_pts) else {
        v <- FW_MAP$empty_view
        leaflet::setView(m, v$lng, v$lat, zoom = v$zoom)
      }
      fw_map_card_render(m, ns("map_detail"), fw_map_thumbs_all(data))
    })

    # WAITS FOR THE MAP TO EXIST. A proxy call sent before the widget has been
    # drawn is dropped by the browser, and the widget is only drawn once the
    # tab is shown. input$map_bounds is the widget announcing itself; it is
    # copied into a reactiveVal so that panning, which sends it again with a
    # new value, does not rerun the observer below.
    map_ready <- reactiveVal(FALSE)
    observeEvent(input$map_bounds, map_ready(TRUE))

    # WHAT IS ON THE MAP NOW, as the ids drawn. Not reactive: it is only there
    # so a rerun that would draw the same thing - the tab being shown again -
    # sends nothing.
    drawn <- NULL
    observe({
      req(map_ready())
      s <- sel()
      # NOT WHILE THE TAB IS HIDDEN. Leaflet fits bounds against a hidden
      # map's zero size. Reading this also reruns the observer when the tab is
      # shown, which is when the deferred draw happens.
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
      # The card script wires the new cluster group itself: it listens for
      # layers added to the map, not only for the ones there when it ran.
      fw_add_marker_layer(proxy, data, pts, detail = "lazy",
                          thumbs = fw_map_thumbs_all(data))
    })
    fw_map_detail_server(input, session, "map_detail", data, sel)

    # LIVE, like everything else on this page. These are cheap - two
    # aggregations over at most 914 rows - and watching them move under the
    # filters is the whole reason they are here rather than on Plan.
    output$method <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_method(data, sel(),
                                        mode = input$method_mode %||% "count")))
    output$cumulative <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_cumulative(sel())))
  })
}

# ---- Pieces ------------------------------------------------------------------

#' The whole-database panel
#'
#' Equal tiles and no sentence: the "Attempts recorded from ... to ..." line
#' came out at the client's request (24 Sept 2026). Nothing here is a rate,
#' and nothing is bigger than the rest: the client's steer is to show the shape
#' of the record rather than push one number.
fw_explore_db_panel <- function(s, in_review = 0L) {
  tags$section(
    class = "fw-explore-db",
    h2(class = "fw-visually-hidden", fw_t("explore", "db_heading")),
    fw_kpi_strip(
      fw_kpi_stat(fw_fmt_num(s$attempts), fw_t("explore", "db_attempts")),
      fw_kpi_stat(fw_fmt_num(s$countries), fw_t("explore", "db_countries")),
      fw_kpi_stat(fw_fmt_num(s$species), fw_t("explore", "db_invasive")),
      # SUCCESSFUL ATTEMPTS ONLY (client, 23 Sept 2026). s$protected is the
      # count of beneficiary species on attempts that succeeded; s$beneficiaries
      # counts them on every attempt, successful or not, and a species whose
      # eradication failed has not been protected by it. The Welcome page has
      # always used s$protected - this is the rest of the app catching up.
      # THE ">" IS ON THE FIGURE (client, 24 Sept 2026): beneficiaries are
      # under-recorded, so the count is a floor. Home and Plan build theirs the
      # same way.
      fw_kpi_stat(paste0(">", fw_fmt_num(s$protected)), fw_t("explore", "db_beneficiary"),
                  tooltip = fw_t("explore", "db_beneficiary_tip")),
      # THE SUCCESS COUNT USED TO SIT HERE and is gone at the client's request.
      # It was the one figure in the strip that was an OUTCOME rather than a
      # size: four counts saying how much is in the database, and a fifth
      # inviting the reader to divide it by the first. That ratio is the
      # headline this app deliberately does not publish - the reasoning is at
      # the top of charts.R - and the outcome donut below the filters gives the
      # whole breakdown without handing anyone a numerator on its own.
      fw_kpi_stat(fw_fmt_num(in_review), fw_t("explore", "in_review"),
                  tooltip = fw_t("explore", "in_review_tip"))
    )
  )
}

#' The filter bar: the page's filters in one wrapping row
#'
#' Tooltips come from FW_FILTERS$<id>$tip, the same registry the report builder
#' reads, so the two pages explain a filter in the same words.
fw_explore_filter_bar <- function(ns, ch, ids) {
  multi <- function(id) {
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
  # A filter with a `when` (the fish family pair) is drawn only while its
  # kind-of-animal filter includes that value. See fw_filter_when_panel().
  control <- function(id) fw_filter_when_panel(ns, id, multi(id))
  drop <- setdiff(names(FW_FILTERS), ids)

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
