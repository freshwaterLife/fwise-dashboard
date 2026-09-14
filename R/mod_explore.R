# mod_explore.R
# BUILT. The database at a glance, then every attempt in it as a row you can
# open.
#
# WHAT IS IN HERE, AND EVERY ONE OF IT. This page answers two questions that
# belong together: what does FWISE hold, and where is the individual attempt I
# am looking for. The database panel and the three summary graphics answer the
# first; the map and the table answer the second.
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
# every graphic, the donut, the map markers and the table - just never used to
# narrow. The reasoning is in mod_plan.R and it applies with more force here,
# where the whole point is to show what the database contains.
#
# THE RECORD OPENS IN THE MAP'S PANEL. A table row click writes the attempt id
# into the same input a marker click does, so there is one way of asking for a
# record and one panel that shows it (fw_map_detail_server() in maps.R). That
# is also what makes previous/next work from either: the server steps through
# the list in the order the reader is looking at.
#
# A TABLE, NOT A GRID OF CARDS. The cards showed one photograph each and one
# attempt per card; the table shows both species - the one targeted and the one
# meant to benefit - which is the pairing a reader scans for, and fits more of
# them on a screen. Every photograph still carries its credit and its licence,
# which is a condition of use rather than decoration: see fw_explore_table().
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

mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          uiOutput(ns("kpis")),
          uiOutput(ns("filters")),
          uiOutput(ns("incoming")),
          uiOutput(ns("summary")),

          # ---- The shape of the selection --------------------------------
          #
          # Proportions and growth, which are questions about the record. The
          # report builder's charts are comparisons within a slice; these are
          # not, and that is what keeps the two pages from being copies.
          #
          # TWO CHARTS ON ONE ROW. There were three - an outcome donut sat above
          # the method one in a right-hand column - and the client removed it,
          # so the pair now sit side by side with no wrapper div between them
          # and the grid. Growth takes the wider cell: it is a time series and
          # squeezing its x-axis is what makes it unreadable, whereas a ring
          # only gets more crowded. Below $fw-bp-lg they fold onto two rows.
          # See .fw-explore-charts.
          div(
            class = "fw-explore-charts",
            fw_block(fw_t("explore", "cumulative"),
                     fw_t("explore", "cumulative_note"),
                     plotly::plotlyOutput(ns("cumulative"), height = "auto")),
            # ITS DENOMINATOR IS USES, NOT ATTEMPTS, and the note says so rather
            # than leaving it to a footnote. See fw_chart_method_donut().
            fw_block(fw_t("explore", "donut_method"),
                     fw_t("explore", "donut_method_note"),
                     plotly::plotlyOutput(ns("donut_method"), height = "auto"))
          ),

          # One link past the map's seventy-odd focusable cluster markers. See
          # the note on .fw-skip-inline in _components.scss.
          tags$a(class = "fw-skip-inline", href = paste0("#", ns("list")),
                 fw_t("explore", "skip_map")),

          div(
            class = "fw-explore-block",
            h2(fw_t("explore", "map")),
            p(class = "fw-explore-block__note", fw_t("explore", "map_note")),
            fw_map_output(ns("map"))
          ),

          div(
            id = ns("list"), class = "fw-explore-block", tabindex = "-1",
            div(
              class = "fw-records-head",
              div(
                h2(fw_t("explore", "list_heading")),
                p(class = "fw-explore-block__note", fw_t("explore", "list_note")),
                uiOutput(ns("list_count"))
              ),
              # The sort and page-size controls live in the static UI, NOT
              # inside a renderUI. A select rebuilt by renderUI comes back at
              # its default, so the reader's choice would be thrown away every
              # time they changed a filter. Same rule as mod_networking.R.
              div(
                class = "fw-records-head__controls",
                div(
                  class = "fw-records-head__control",
                  tags$label(class = "form-label", `for` = ns("sort"),
                             fw_t("explore", "sort_label")),
                  selectInput(
                    ns("sort"), label = NULL, selectize = FALSE, width = "auto",
                    choices = stats::setNames(
                      FW_EXPLORE_SORTS,
                      vapply(FW_EXPLORE_SORTS,
                             function(s) fw_t("explore", paste0("sort_", s)),
                             character(1))
                    )
                  )
                ),
                div(
                  class = "fw-records-head__control",
                  tags$label(class = "form-label", `for` = ns("list_size"),
                             fw_t("explore", "page_size")),
                  selectInput(ns("list_size"), label = NULL, selectize = FALSE,
                              width = "auto", choices = FW_PLAN_PAGE_SIZES,
                              selected = FW_PLAN_PAGE_SIZES[1])
                )
              )
            ),
            uiOutput(ns("records")),
            uiOutput(ns("records_pager"))
          )
        )
      )
    )
  )
}

#' @param in_review how many submissions are waiting on review. Passed in
#'   rather than read from a global: app.R evaluates in its own environment, so
#'   FW_IN_REVIEW is not visible to a module sourced from R/.
mod_explore_server <- function(id, data, in_review = 0L) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choices <- fw_filter_choices(data)
    ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))

    output$filters <- renderUI(fw_explore_filter_bar(ns, choices, ids))

    observeEvent(input$clear, {
      fw_filter_clear(session, ids, choices)
      fw_set_explore_request(NULL)
    })

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
      cid <- fw_explore_request()
      if (!is.null(cid)) {
        keep <- fw_contact_attempt_ids(data, cid)
        out <- out[out$attempt_id %in% keep, ]
      }
      out
    })

    # The selection in the order the list shows it. The detail server steps
    # through THIS, so previous/next in the panel follow the list.
    sorted <- reactive({
      s <- sel()
      s[fw_explore_order(s, input$sort %||% FW_EXPLORE_SORTS[1]), ]
    })

    # ---- Arriving from the Networking page ----------------------------------

    output$incoming <- renderUI({
      cid <- fw_explore_request()
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

    observeEvent(input$clear_contact, fw_set_explore_request(NULL))

    # ---- The strip and the map ----------------------------------------------

    output$summary <- renderUI(fw_plan_summary_ui(fw_plan_summary(data, sel())))

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
        fw_add_basemaps() |>
        fw_add_attempt_markers(data, sel(), detail = "lazy",
                               detail_input = ns("map_detail"))
    })
    fw_map_detail_server(input, session, "map_detail", data, sorted)

    # LIVE, like everything else on this page. These are cheap - two
    # aggregations over at most 914 rows - and watching them move under the
    # filters is the whole reason they are here rather than on Plan.
    output$donut_method <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_method_donut(data, sel())))
    output$cumulative <- plotly::renderPlotly(
      fw_chart_or_empty(fw_chart_cumulative(sel())))

    # "Show on map" on a card: fly to the marker and bring the map into view.
    # The hover card closes itself on movestart, so nothing is left floating.
    observeEvent(input$locate, {
      s <- sel()
      row <- s[!is.na(s$attempt_id) & s$attempt_id == input$locate, ]
      if (!nrow(row) || is.na(row$latitude[1]) || is.na(row$longitude[1])) return()
      session$sendCustomMessage("fw-scroll-to", ns("map"))
      leaflet::leafletProxy("map", session) |>
        leaflet::flyTo(row$longitude[1], row$latitude[1],
                       zoom = FW_MAP$cluster$fine_zoom)
    })

    # ---- The list -----------------------------------------------------------
    #
    # Paged, the same way as the report builder's table and the contacts
    # directory: the page is state the server owns, because fw_page_numbers()
    # writes through Shiny.setInputValue() and an input set that way has no
    # binding to reset. See the note in mod_plan.R.

    per_page <- reactive({
      n <- suppressWarnings(as.integer(input$list_size))
      if (length(n) != 1 || is.na(n) || n <= 0) FW_PLAN_PAGE_SIZES[1] else n
    })

    page <- reactiveVal(1L)
    observeEvent(input$records_page, {
      n <- suppressWarnings(as.integer(input$records_page))
      if (length(n) == 1 && !is.na(n)) page(max(1L, n))
    })
    observeEvent(sel(), page(1L))
    observeEvent(list(input$sort, input$list_size), page(1L), ignoreInit = TRUE)

    list_page <- reactive(min(page(), fw_plan_pages(nrow(sel()), per_page())))

    output$list_count <- renderUI({
      p(class = "fw-caption",
        fw_fill(fw_t("explore", "list_count"), n = fw_fmt_num(nrow(sel()))))
    })

    output$records <- renderUI({
      s <- sorted()
      if (!nrow(s)) return(p(fw_t("common", "no_results")))
      from <- (list_page() - 1L) * per_page() + 1L
      to <- min(nrow(s), list_page() * per_page())
      rec <- fw_attempt_records(data, s[seq(from, to), ])
      # The figures for THIS PAGE, rendered once per species. Cache only, never
      # a live fetch: a page of rows is built at once and none of them may reach
      # Wikimedia while the page is rendering. The cache covers BOTH roles -
      # see fw_map_figure_cache() - because every row now shows two.
      cache <- fw_map_figure_cache(data, rec)
      div(
        class = "fw-table-scroll",
        fw_explore_table(rec, figures = cache,
                         detail_input = ns("map_detail"),
                         locate_input = ns("locate"))
      )
    })

    output$records_pager <- renderUI({
      n_rows <- nrow(sel())
      if (n_rows == 0) return(NULL)
      n_pages <- fw_plan_pages(n_rows, per_page())
      from <- (list_page() - 1L) * per_page() + 1L
      to <- min(n_rows, list_page() * per_page())
      div(
        class = "fw-pager",
        tags$span(
          class = "fw-pager__status", role = "status",
          fw_t("explore", "page_showing"), " ",
          tags$span(class = "fw-num", fw_fmt_num(from)), "-",
          tags$span(class = "fw-num", fw_fmt_num(to)),
          " ", fw_t("common", "of"), " ",
          tags$span(class = "fw-num", fw_fmt_num(n_rows))
        ),
        fw_page_numbers(ns("records_page"), list_page(), n_pages)
      )
    })
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

# Set by the Networking page, read by the list. A tiny shared reactive rather
# than a module return, because the two modules are siblings and neither owns
# the other.
.fw_explore_request <- shiny::reactiveVal(NULL)
fw_explore_request <- function() .fw_explore_request()
fw_set_explore_request <- function(contact_id) .fw_explore_request(contact_id)

#' Every attempt a contact is attached to, in either slot
fw_contact_attempt_ids <- function(data, contact_id) {
  a <- data$attempt
  a$attempt_id[which(a$primary_contact_id == contact_id |
                       a$secondary_contact_id == contact_id)]
}
