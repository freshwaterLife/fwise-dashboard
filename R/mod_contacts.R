# mod_contacts.R
# BUILT. The contacts page.
#
# This page exists to make people reach out to each other. Networking is one of
# the client's stated year-one success measures, so it is treated as a feature
# rather than a directory listing.
#
# LAYOUT DECISION. Filters plus a summary count strip, rather than a collapsible
# table grouped by continent and country. With 237 contacts spread over 29
# countries, a nested grouping produces roughly 29 groups of which most hold one
# to three people, and the visitor spends the visit expanding things. Filters let
# someone jump straight to their region, and the strip keeps the roll-up visible.
#
# EMAIL REDACTION. This module reads ONLY from fw_contacts_summary(), which
# removes the address of any contact flagged not-public before the data reaches
# the session. There is no code path here that can see a redacted address, so
# none can reach the browser.

library(shiny)
library(dplyr)

mod_contacts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("contacts", "title"), fw_t("contacts", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          div(class = "fw-prose", p(class = "fw-lead", fw_t("contacts", "intro"))),

          div(
            class = "fw-filters",
            selectInput(ns("continent"), fw_t("contacts", "filter_continent"),
                        choices = NULL, selectize = FALSE),
            selectInput(ns("country"), fw_t("contacts", "filter_country"),
                        choices = NULL, selectize = FALSE),
            textInput(ns("search"), fw_t("contacts", "filter_search"),
                      placeholder = "")
          ),
          div(
            style = "margin-block-end: 1.5rem;",
            actionButton(ns("clear"), fw_t("common", "clear_filters"),
                         class = "btn btn-outline-primary btn-sm")
          ),

          uiOutput(ns("summary")),

          # The page-size control lives in the static UI, NOT inside the pager's
          # uiOutput. A select rebuilt by renderUI comes back at its default, so
          # the reader's choice of 100 would be thrown away every time they
          # changed a filter.
          div(
            class = "fw-table-toolbar",
            div(
              class = "fw-table-toolbar__size",
              tags$label(class = "form-label", `for` = ns("page_size"),
                         fw_t("contacts", "page_size")),
              selectInput(ns("page_size"), label = NULL,
                          choices = FW_CONTACTS_PAGE_SIZES,
                          selected = FW_CONTACTS_PAGE_SIZES[1],
                          selectize = FALSE, width = "auto")
            )
          ),

          div(class = "fw-table-scroll", uiOutput(ns("table"))),
          uiOutput(ns("pager")),

          fw_dots_divider(),

          div(
            class = "fw-panel fw-prose",
            h2(fw_t("contacts", "outro_heading")),
            p(fw_t("contacts", "outro")),
            tags$a(
              class = "btn btn-primary",
              # Built at click time rather than served as a mailto, for the same
              # scraping reason as the contact rows below.
              href = "#",
              onclick = sprintf(
                "window.location.href='mail'+'to:'+%s; return false;",
                jsonlite_quote(fw_t("contacts", "outro_email"))
              ),
              fw_t("contacts", "outro_action")
            )
          )
        )
      )
    )
  )
}

# Small helper so the address is assembled in JavaScript rather than sitting in
# the served markup as a mailto href.
jsonlite_quote <- function(x) paste0("'", gsub("'", "\\\\'", x), "'")

mod_contacts_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The ONLY source of contact data on this page. Redaction has already
    # happened inside this function.
    contacts <- fw_contacts_summary(data)

    # Continent and country come from the attempts a contact is attached to, so
    # the filter options are derived the same way rather than from the contact
    # table, which deliberately has no country of its own.
    all_continents <- sort(unique(unlist(contacts$continents)))
    all_countries  <- sort(unique(unlist(contacts$countries)))

    updateSelectInput(
      session, "continent",
      choices = c(stats::setNames(list(""), fw_t("contacts", "filter_all")),
                  stats::setNames(as.list(all_continents), all_continents))
    )
    updateSelectInput(
      session, "country",
      choices = c(stats::setNames(list(""), fw_t("contacts", "filter_all")),
                  stats::setNames(as.list(all_countries), all_countries))
    )

    # Narrow the country list to the chosen continent, so the two filters cannot
    # be set to a combination that returns nothing.
    observeEvent(input$continent, ignoreInit = TRUE, {
      countries <- if (identical(input$continent, "")) {
        all_countries
      } else {
        keep <- vapply(contacts$continents, function(x) input$continent %in% x, logical(1))
        sort(unique(unlist(contacts$countries[keep])))
      }
      selected <- if (input$country %in% countries) input$country else ""
      updateSelectInput(
        session, "country",
        choices = c(stats::setNames(list(""), fw_t("contacts", "filter_all")),
                    stats::setNames(as.list(countries), countries)),
        selected = selected
      )
    })

    observeEvent(input$clear, {
      updateSelectInput(session, "continent", selected = "")
      updateSelectInput(session, "country", selected = "")
      updateTextInput(session, "search", value = "")
    })

    filtered <- reactive({
      out <- contacts

      if (!identical(input$continent %||% "", "")) {
        keep <- vapply(out$continents, function(x) input$continent %in% x, logical(1))
        out <- out[keep, ]
      }
      if (!identical(input$country %||% "", "")) {
        keep <- vapply(out$countries, function(x) input$country %in% x, logical(1))
        out <- out[keep, ]
      }
      term <- trimws(input$search %||% "")
      if (nzchar(term)) {
        hay <- paste(out$contact_name, coalesce(out$organisation, ""))
        out <- out[grepl(term, hay, ignore.case = TRUE, fixed = FALSE), ]
      }
      out
    })

    output$summary <- renderUI({
      f <- filtered()
      div(
        class = "fw-summary-strip",
        role = "status",
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value", fw_fmt_num(nrow(f))),
            span(class = "fw-summary-strip__label", fw_t("contacts", "summary_contacts"))),
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value",
                 fw_fmt_num(length(unique(unlist(f$countries))))),
            span(class = "fw-summary-strip__label", fw_t("contacts", "summary_countries"))),
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value",
                 fw_fmt_num(length(unique(unlist(f$continents))))),
            span(class = "fw-summary-strip__label", fw_t("contacts", "summary_continents")))
      )
    })

    # 237 contacts in one table is a 17,000 pixel scroll, which defeats the point
    # of a page meant to help someone find one person. Paged instead, with the
    # reader choosing how many they want at a time.
    page <- reactiveVal(1L)

    page_size <- reactive({
      n <- suppressWarnings(as.integer(input$page_size))
      if (length(n) != 1 || is.na(n) || n <= 0) FW_CONTACTS_PAGE_SIZES[1] else n
    })

    # Any filter change, or a change of page size, puts the reader back on the
    # first page. Otherwise they can be left on page 8 of a two-page result and
    # see nothing.
    observeEvent(list(input$continent, input$country, input$search,
                      input$page_size), {
      page(1L)
    }, ignoreInit = TRUE)

    n_pages <- reactive(max(1L, ceiling(nrow(filtered()) / page_size())))

    paged <- reactive({
      f <- filtered()
      p <- min(page(), n_pages())
      from <- (p - 1L) * page_size() + 1L
      to <- min(nrow(f), p * page_size())
      if (nrow(f) == 0) f else f[from:to, ]
    })

    # One input for every page button, rather than one observer per button. A
    # numbered pager rebuilt on every filter change would otherwise accumulate a
    # dead observer per page number per render.
    observeEvent(input$goto_page, {
      n <- suppressWarnings(as.integer(input$goto_page))
      if (length(n) == 1 && !is.na(n)) page(max(1L, min(n, n_pages())))
    })

    output$pager <- renderUI({
      if (nrow(filtered()) == 0) return(NULL)
      p <- min(page(), n_pages())
      from <- (p - 1L) * page_size() + 1L
      to <- min(nrow(filtered()), p * page_size())

      div(
        class = "fw-pager",
        tags$span(
          class = "fw-pager__status", role = "status",
          fw_t("contacts", "page_showing"), " ",
          tags$span(class = "fw-num", from), "-", tags$span(class = "fw-num", to),
          " ", fw_t("common", "of"), " ",
          tags$span(class = "fw-num", nrow(filtered()))
        ),
        fw_page_numbers(ns("goto_page"), p, n_pages())
      )
    })

    output$table <- renderUI({
      f <- paged()
      if (nrow(f) == 0) return(p(fw_t("common", "no_results")))

      rows <- lapply(seq_len(nrow(f)), function(i) {
        r <- f[i, ]
        tags$tr(
          tags$td(r$contact_name),
          tags$td(r$organisation %|na|% fw_t("contacts", "no_organisation")),
          tags$td(r$country_label),
          tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
          tags$td(fw_contact_action(r$contact_email, r$contact_name)),
          tags$td(
            actionLink(
              ns(paste0("view_", r$contact_id)),
              fw_t("contacts", "view_attempts"),
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority:'event'});",
                ns("view_contact"), r$contact_id
              )
            )
          )
        )
      })

      tags$table(
        class = "fw-table",
        tags$caption(
          class = "fw-visually-hidden",
          paste(fw_t("contacts", "summary_showing"), nrow(f), fw_t("common", "of"),
                nrow(filtered()), fw_t("contacts", "summary_contacts"))
        ),
        tags$thead(tags$tr(
          tags$th(scope = "col", fw_t("contacts", "col_name")),
          tags$th(scope = "col", fw_t("contacts", "col_organisation")),
          tags$th(scope = "col", fw_t("contacts", "col_country")),
          tags$th(scope = "col", class = "fw-col-num", fw_t("contacts", "col_attempts")),
          tags$th(scope = "col", fw_t("contacts", "col_contact")),
          tags$th(scope = "col", tags$span(class = "fw-visually-hidden", "Attempts link"))
        )),
        tags$tbody(rows)
      )
    })

    # Route through to the explore page. That view is not built yet, so the
    # explore stub acknowledges the request rather than dead-ending.
    observeEvent(input$view_contact, {
      fw_set_explore_request(input$view_contact)
      session$sendCustomMessage("fw-nav", "explore")
    })
  })
}

#' Render a contact action without serving the address in the markup
#'
#' Public addresses on a public site are a scraping target. The address is split
#' and reassembled in JavaScript at click time, so a naive scraper reading the
#' served HTML does not harvest it in one pass.
#'
#' This is a speed bump, NOT security. Anyone running the page's JavaScript, or
#' willing to read it, can recover a public address. The real control is the
#' email_public flag: an address flagged not-public never reaches this function
#' at all, because fw_contacts_summary() has already replaced it with NA.
fw_contact_action <- function(email, name) {
  if (is.na(email) || !nzchar(email)) {
    # An empty cell, not a "hidden" badge. A badge advertises that there is
    # something to go looking for.
    return(tags$span(
      tags$span(class = "fw-visually-hidden", fw_t("contacts", "email_none_label"))
    ))
  }
  parts <- strsplit(email, "@", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(tags$span(""))

  tags$a(
    href = "#",
    class = "fw-contact-link",
    `data-u` = parts[1],
    `data-d` = parts[2],
    `aria-label` = paste("Email", name),
    onclick = paste0(
      "window.location.href='mail'+'to:'+this.dataset.u+String.fromCharCode(64)",
      "+this.dataset.d; return false;"
    ),
    fw_t("contacts", "email_action")
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
`%|na|%` <- function(x, y) if (is.na(x) || !nzchar(x)) y else x
