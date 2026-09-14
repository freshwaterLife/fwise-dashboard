# mod_explore.R
# BUILT. The record browser: the whole database at the top, then every attempt
# as a card you can open.
#
# PLAN IS ABOUT THE MANY, THIS PAGE IS ABOUT THE ONE. The report builder
# aggregates attempts like yours into charts and a citable file. Nothing else
# in the app lets a reader find an individual attempt, see where it sits, read
# its whole record and step to the next one. That is the job here, and it is
# why there are NO CHARTS on this page: a chart of the selection is Plan's
# answer, and offering a weaker copy of it here is what made the two pages
# indistinguishable before this one was rebuilt.
#
# LIVE, NOT GATED - the opposite of the report builder, on purpose. A report is
# something the reader will cite, so Plan makes them commit before it draws. A
# list is something they scan, and the whole value is watching it narrow as
# they move a control. Do not "make the two pages consistent".
#
# SIX FILTERS, NOT TEN. Place, animal (either side), method and outcome are the
# questions a reader has when looking for an attempt. Waterbody, regime, the
# named-species pickers and the year range are questions about a group of
# attempts, and belong with the charts on Plan. The set is FW_EXPLORE_FILTERS;
# the controls, the state and the matching all come from the one registry in
# filters.R, so a filter here is the same filter there.
#
# OUTCOME IS FILTERABLE HERE and deliberately not on the report builder.
# Reasoning in mod_plan.R.
#
# THE RECORD OPENS IN THE MAP'S PANEL. A card click writes the attempt id into
# the same input a marker click does, so there is one way of asking for a
# record and one panel that shows it (fw_map_detail_server() in maps.R). That
# is also what makes previous/next work from either: the server steps through
# the list in the order the reader is looking at.
#
# THE DATABASE PANEL IS NEVER FILTERED. It is the size of the whole record,
# and it sits above the filters so it cannot be read as the size of a
# selection. Counts only, all the same size, no rate: see the note in
# mod_plan_results.R on why there is no success percentage anywhere.

library(shiny)
library(dplyr)

# The filters this page offers, by registry id. Everything else in FW_FILTERS
# is dropped here and kept on the report builder.
FW_EXPLORE_FILTERS <- c("continent", "country", "taxa", "taxa_beneficiary",
                        "method", "outcome")

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
      # a live fetch: twenty cards are built at once and none of them may reach
      # Wikimedia while the page is rendering.
      cache <- fw_map_figure_cache(data, rec)
      div(
        class = "fw-records",
        lapply(seq_len(nrow(rec)), function(i) {
          # The lead invasive species' photograph. NULL where there is none:
          # fw_record_card() draws the placeholder, so the fallback lives in
          # one place rather than at every call site.
          lead <- fw_popup_parts(rec$inv_ids[i])[1]
          fig <- if (!is.na(lead) && lead %in% names(cache)) unname(cache[lead])
          fw_record_card(rec[i, ], figure = fig,
                         detail_input = ns("map_detail"),
                         locate_input = ns("locate"))
        })
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
      fw_kpi_stat(fw_fmt_num(s$successful), fw_t("explore", "db_successful"),
                  tooltip = fw_t("explore", "db_successful_tip")),
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
