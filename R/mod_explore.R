# mod_explore.R
# BUILT. The database at a glance, then every attempt in it as a row you can
# open.
#
# WHAT IS IN HERE, AND EVERY ONE OF IT. This page answers two questions that
# belong together: what does FWISE hold, and where is the individual attempt I
# am looking for. The database panel and the two summary graphics answer the
# first; the map answers the second.
#
# THE TABLE OF ATTEMPTS IS GONE, at the client's request, and so is the page's
# only other route into a record. The reasoning was that nobody scrolls a
# few hundred alphabetical rows of raw data, and the map does the same job
# while being worth looking at. What it cost: there is now no way to reach an
# unlocated attempt from this page at all - see the note on the map below.
#
# PLAN IS ABOUT A SITUATION, THIS PAGE IS ABOUT THE DATABASE. The report builder
# narrows to attempts like the reader's own and produces a citable file about
# THAT. Everything here describes the whole record, or the whole of a very
# lightly filtered slice of it - which is why the graphics here are proportions
# and growth, and the graphics there are comparisons within a selection.
#
# THERE ARE CHARTS HERE NOW, and this is a reversal. This page used to carry
# none, on the reasoning that a chart of the selection was Plan's answer and a
# weaker copy of it here made the two pages indistinguishable. That reasoning
# held while the two pages had the same ten filters. They no longer do: four
# simple filters cannot produce the narrow, unrepresentative slice that made a
# chart here dangerous, and the three graphics below are about the shape of the
# record rather than about a comparison the reader might act on.
#
# LIVE, NOT GATED - the opposite of the report builder, on purpose. A report is
# something the reader will cite, so Plan makes them commit before it draws. A
# list is something they scan, and the whole value is watching it narrow as
# they move a control. Do not "make the two pages consistent".
#
# FOUR FILTERS, NOT TEN, and they are all SIMPLE ones: where, and which animals.
# Everything else - waterbody, regime, size, the named-species pickers and the
# year range - is a question about a group of attempts and belongs with the
# report builder. The set is FW_EXPLORE_FILTERS; the controls, the state and the
# matching all come from the one registry in filters.R, so a filter here is the
# same filter there.
#
# OUTCOME IS NOT FILTERABLE, HERE OR ANYWHERE. It used to be filterable on this
# page and deliberately not on the report builder; the client's decision is now
# that it is an answer rather than a question, on both pages. It is shown in
# every graphic, the map markers and the record panel - just never used to
# narrow. The reasoning is in mod_plan.R and it applies with more force here,
# where the whole point is to show what the database contains.
#
# THE RECORD OPENS IN THE MAP'S PANEL, and now that is the only place it opens
# from. A marker click writes the attempt id and fw_map_detail_server() in
# maps.R shows it; previous/next steps through sorted(), which is the whole
# selection in most-recent-first order rather than only the located part of it.
#
# ATTEMPTS WITH NO COORDINATES ARE NOT REACHABLE HERE. They were rows in the
# table and they are not markers on the map. They are still in every count,
# every chart and every export, and the report builder still lists them. If
# that becomes a complaint, the answer is a route to them from this page - not
# putting the table back.
#
# THE DATABASE PANEL IS NEVER FILTERED. It is the size of the whole record,
# and it sits above the filters so it cannot be read as the size of a
# selection. Counts only, all the same size, no rate: see the note in
# mod_plan_results.R on why there is no success percentage anywhere.

library(shiny)
library(dplyr)

# The filters this page offers, by registry id. Everything else in FW_FILTERS
# is dropped here and kept on the report builder.
FW_EXPLORE_FILTERS <- c("continent", "country", "taxa", "taxa_beneficiary")

# The list's sort orders. Copy keys are "sort_<value>" in FW_COPY$explore.
FW_EXPLORE_SORTS <- c("newest", "oldest", "site", "country")

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
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          uiOutput(ns("kpis")),
          fw_explore_filter_bar(ns, choices, ids),
          uiOutput(ns("incoming")),
          uiOutput(ns("summary")),

          # ---- Where it happened ------------------------------------------
          #
          # THE MAP COMES FIRST, at the client's request and against the order
          # this page shipped with. The reasoning is that the map is the only
          # thing here a reader can act on, and a reader who has moved it is
          # far more likely to scroll on to the figures than one who met two
          # static charts at the top. Do not put the charts back above it
          # without that conversation.
          div(
            class = "fw-explore-block",
            h2(fw_t("explore", "map")),
            p(class = "fw-explore-block__note", fw_t("explore", "map_note")),
            fw_map_output(ns("map"))
          ),

          # ---- The shape of the selection --------------------------------
          #
          # Proportions and growth, which are questions about the record. The
          # report builder's charts are comparisons within a slice; these are
          # not, and that is what keeps the two pages from being copies.
          #
          # TWO CHARTS ON ONE ROW. There were three - an outcome donut sat above
          # the method one in a right-hand column - and the client removed it,
          # so the pair now sit side by side with no wrapper div between them
          # and the grid. Below $fw-bp-lg they fold onto two rows. See
          # .fw-explore-charts.
          #
          # THE METHOD DONUT IS GONE and these bars replace it. The client's
          # reasoning: the outcome-by-method chart says everything the ring said
          # and adds what happened, so the ring was the weaker of two tellings
          # of the same split. THE DENOMINATOR CHANGED WITH IT - the ring
          # counted uses of a method, the bars count attempts - and the note
          # under the chart says so rather than leaving it to a footnote.
          div(
            class = "fw-explore-charts",
            fw_block(fw_t("explore", "cumulative"),
                     fw_t("explore", "cumulative_note"),
                     plotly::plotlyOutput(ns("cumulative"), height = "auto")),
            fw_block(fw_t("explore", "method"),
                     fw_t("explore", "method_note"),
                     plotly::plotlyOutput(ns("method"), height = "auto"))
          )
        )
      )
    )
  )
}

#' @param in_review how many submissions are waiting on review. Passed in
#'   rather than read from a global: app.R evaluates in its own environment, so
#'   FW_IN_REVIEW is not visible to a module sourced from R/.
#' @param request a reactiveVal holding the contact id the Networking page
#'   asked this page to narrow to, or NULL. ONE PER SESSION, created in app.R's
#'   server and handed to both modules. It used to be a reactiveVal at the top
#'   of this file, which is one for the whole PROCESS: a click on "view
#'   attempts" by one visitor narrowed, and redrew, every other visitor's page.
mod_explore_server <- function(id, data, in_review = 0L,
                               request = shiny::reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choices <- fw_filter_choices(data)
    ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))

    observeEvent(input$clear, {
      fw_filter_clear(session, ids, choices)
      request(NULL)
    })

    # ---- The two geography filters, linked ----------------------------------
    #
    # THEY USED TO BE INDEPENDENT, AND THAT WAS THE BUG. Both narrow with AND,
    # so picking Europe and then Australia asked for attempts that are in both
    # and got the honest answer: nothing. The page was not wrong, but "I chose
    # two things and the map went blank" is indistinguishable from broken, and
    # that is what the client reported.
    #
    # The fix is to make the contradiction unreachable rather than to explain
    # it: each picker only offers values that are still possible given the
    # other. Choosing Europe leaves twelve countries in the country list, and
    # Australia is not one of them.
    #
    # THE OBSERVERS THEMSELVES LIVE IN filters.R, and did not always - the
    # report builder offers the same two pickers and went without this for a
    # release. Why it cannot loop, and why the pairs come from the attempt
    # table rather than the ISO lookup, are both at fw_link_geo_filters().
    fw_link_geo_filters(input, session, data, choices)

    # ---- The database panel -------------------------------------------------
    #
    # Whole-database figures, computed once per session from the loaded data.
    # in_review is not part of the data; it is what tells a visitor that
    # submissions go somewhere.
    output$kpis <- renderUI(fw_explore_db_panel(fw_headline_stats(data), in_review))

    # ---- The selection ------------------------------------------------------
    #
    # Live. The strip, the map and the list all read this one reactive, so they
    # cannot disagree about what is being shown.
    sel <- reactive({
      out <- fw_filter_apply(data, fw_filter_state(input, ids))
      # A deep link from the Networking page narrows to one person's attempts.
      # Applied AFTER the filters so the bar still does what it says.
      cid <- request()
      if (!is.null(cid)) {
        keep <- fw_contact_attempt_ids(data, cid)
        out <- out[out$attempt_id %in% keep, ]
      }
      out
    })

    # The order previous/next steps through in the record panel. THE LIST THIS
    # ORDERED IS GONE - the client removed the table - but the panel still has
    # to walk the selection in SOME stated order, and "most recent first" is
    # the one the table defaulted to, so a reader who knew the old page finds
    # the same sequence. fw_explore_order() keeps the rule that undated
    # attempts sort last rather than first.
    sorted <- reactive(sel()[fw_explore_order(sel(), FW_EXPLORE_SORTS[1]), ])

    # ---- Arriving from the Networking page ----------------------------------

    output$incoming <- renderUI({
      cid <- request()
      req(cid)
      contact <- data$contact[data$contact$contact_id == cid, ]
      if (nrow(contact) == 0) return(NULL)
      div(
        class = "fw-notice", role = "status",
        span(fw_fill(fw_t("explore", "incoming"), name = contact$contact_name[1])),
        actionButton(ns("clear_contact"), fw_t("explore", "incoming_clear"),
                     class = "btn btn-outline-primary btn-sm")
      )
    })

    observeEvent(input$clear_contact, request(NULL))

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
      m <- leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
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
      # NOT WHILE THE TAB IS HIDDEN. A selection can change from another page
      # (the Networking link), and Leaflet fits bounds against a hidden map's
      # zero size. Reading this also reruns the observer when the tab is shown,
      # which is when the deferred draw happens.
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
    fw_map_detail_server(input, session, "map_detail", data, sorted)

    # LIVE, like everything else on this page. These are cheap - two
    # aggregations over at most 914 rows - and watching them move under the
    # filters is the whole reason they are here rather than on Plan.
    output$method <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_method(data, sel(), mode = "count")))
    output$cumulative <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_cumulative(sel())))

    # THE "SHOW ON MAP" OBSERVER WENT WITH THE TABLE. It flew the map to a row's
    # marker, and the table's link was the only thing that ever wrote input$locate.
  })
}

# ---- Pieces ------------------------------------------------------------------

#' The whole-database panel
#'
#' Six equal tiles and one sentence. Nothing here is a rate, and nothing is
#' bigger than the rest: the client's steer is to show the shape of the record
#' rather than push one number.
fw_explore_db_panel <- function(s, in_review = 0L) {
  tags$section(
    class = "fw-explore-db",
    h2(fw_t("explore", "db_heading")),
    p(class = "fw-explore-db__span",
      fw_fill(fw_t("explore", "db_span"),
              from = as.character(s$earliest_year),
              to = as.character(s$latest_year))),
    fw_kpi_strip(
      fw_kpi_stat(fw_fmt_num(s$attempts), fw_t("explore", "db_attempts")),
      fw_kpi_stat(fw_fmt_num(s$countries), fw_t("explore", "db_countries")),
      fw_kpi_stat(fw_fmt_num(s$species), fw_t("explore", "db_invasive")),
      fw_kpi_stat(fw_fmt_num(s$beneficiaries), fw_t("explore", "db_beneficiary"),
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
  drop <- setdiff(names(FW_FILTERS), ids)

  tags$section(
    class = "fw-explore-filters",
    h2(class = "fw-explore-filters__heading", fw_t("explore", "f_heading")),
    p(class = "fw-caption", fw_t("explore", "f_note")),
    div(
      class = "fw-explore-filters__grid",
      lapply(fw_filter_draw_order(drop), multi),
      div(
        class = "fw-filters__actions",
        actionButton(ns("clear"), fw_t("plan", "clear"),
                     class = "btn btn-outline-primary btn-sm")
      )
    )
  )
}

#' The row order of the list for a sort setting
#'
#' Undated attempts go last under either year order, never first: "most recent"
#' with a blank at the top reads as a fault. Ties break on site name so the
#' order is stable between renders.
#'
#' @return an integer permutation of seq_len(nrow(s))
fw_explore_order <- function(s, sort = FW_EXPLORE_SORTS[1]) {
  site <- tolower(s$site_name)
  switch(
    match.arg(sort, FW_EXPLORE_SORTS),
    newest  = order(-s$start_year, site, na.last = TRUE),
    oldest  = order(s$start_year, site, na.last = TRUE),
    site    = order(site, s$country, na.last = TRUE),
    country = order(s$country, site, na.last = TRUE)
  )
}

#' Every attempt a contact is attached to, in either slot
fw_contact_attempt_ids <- function(data, contact_id) {
  a <- data$attempt
  a$attempt_id[which(a$primary_contact_id == contact_id |
                       a$secondary_contact_id == contact_id)]
}
