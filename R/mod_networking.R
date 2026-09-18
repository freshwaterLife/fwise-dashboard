# mod_networking.R
# BUILT. The Networking page.
#
# THE NAME IS THE POINT. This was "Contacts", which described a static address
# book. The page exists to make people reach out to each other - networking is
# one of the client's stated year-one success measures - so it is named for the
# thing it is meant to cause rather than for the table it happens to contain.
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

mod_networking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("networking", "title"), fw_t("networking", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          div(
            class = "fw-filters fw-filters--networking",
            # The role scope only means something once a species is picked, so
            # it waits underneath the species box until then, in the same cell.
            div(
              class = "fw-filters__stack",
              fw_networking_picker(ns, "species"),
              conditionalPanel(
                "input.species && input.species.length > 0", ns = ns,
                fw_field(
                  selectInput(ns("category"), label = NULL, width = "100%",
                              selectize = FALSE,
                              choices = stats::setNames(
                                FW_NETWORKING_CATEGORIES,
                                vapply(FW_NETWORKING_CATEGORIES, function(k)
                                  fw_t("networking", paste0("category_", k)), character(1)))),
                  label = fw_t("networking", "filter_category"),
                  tooltip = fw_t("networking", "tip_category"),
                  input_id = ns("category")
                )
              )
            ),
            fw_networking_picker(ns, "continent"),
            fw_networking_picker(ns, "country"),
            fw_networking_picker(ns, "organisation"),
            fw_field(textInput(ns("search"), label = NULL,
                               placeholder = fw_t("networking", "search_placeholder")),
                     label = fw_t("networking", "filter_search"),
                     input_id = ns("search")),
            div(
              class = "fw-filters__clear",
              actionButton(ns("clear"), fw_t("common", "clear_filters"),
                           class = "btn btn-outline-primary btn-sm")
            )
          ),

          uiOutput(ns("summary")),
          uiOutput(ns("coverage")),

          # The page-size control lives in the static UI, NOT inside the pager's
          # uiOutput. A select rebuilt by renderUI comes back at its default, so
          # the reader's choice of 100 would be thrown away every time they
          # changed a filter.
          div(
            class = "fw-table-toolbar",
            div(
              class = "fw-table-toolbar__size",
              tags$label(class = "form-label", `for` = ns("page_size"),
                         fw_t("networking", "page_size")),
              selectInput(ns("page_size"), label = NULL,
                          choices = FW_CONTACTS_PAGE_SIZES,
                          selected = FW_CONTACTS_PAGE_SIZES[1],
                          selectize = FALSE, width = "auto")
            )
          ),

          div(class = "fw-table-scroll", uiOutput(ns("table"))),
          uiOutput(ns("pager")),

          div(
            class = "fw-panel fw-prose",
            h2(class = "fw-visually-hidden", fw_t("networking", "outro_heading")),
            p(fw_t("networking", "outro")),
            tags$a(
              class = "btn btn-primary",
              # Built at click time rather than served as a mailto, for the same
              # scraping reason as the contact rows below.
              href = "#",
              onclick = sprintf(
                "window.location.href='mail'+'to:'+%s; return false;",
                jsonlite_quote(fw_t("networking", "outro_email"))
              ),
              fw_t("networking", "outro_action")
            )
          )
        )
      )
    )
  )
}

# Species filter scope: either role, the invasive species, or the protected one.
FW_NETWORKING_CATEGORIES <- c("either", "invasive", "beneficiary")

fw_networking_picker <- function(ns, id) {
  fw_field(
    selectizeInput(ns(id), label = NULL, choices = NULL, multiple = TRUE,
                   width = "100%",
                   options = list(placeholder = fw_t("filters", "all"),
                                  plugins = list("remove_button"))),
    label = fw_t("networking", paste0("filter_", id)),
    input_id = ns(id)
  )
}

#' The contacts that match the Networking page's filters
#'
#' MATCHED ON ATTEMPTS, then lifted to people. Place and species are properties
#' of an attempt, so a contact is kept when ONE of their attempts satisfies all
#' of them together - "someone who removed a fish in Chile", not "someone who
#' worked in Chile and, separately, on some fish somewhere". Organisation and
#' the name search are properties of the person and are applied to the contact.
#'
#' @param contacts fw_contacts_summary(data)
#' @param f list(continent, country, species, category, organisation, search);
#'   an empty or NULL entry is no filter. `species` holds labels as the picker
#'   shows them; `category` scopes them to a role ("either" by default).
fw_networking_filter <- function(data, contacts, f) {
  picked <- function(x) if (length(x)) as.character(x[nzchar(x)]) else character(0)
  cont <- picked(f$continent); ctry <- picked(f$country); sp <- picked(f$species)
  out <- contacts

  if (length(cont) || length(ctry) || length(sp)) {
    keep_ids <- data$attempt$attempt_id
    if (length(cont) || length(ctry)) {
      keep_ids <- fw_filter_apply(
        data, list(continent = cont, country = ctry, .ids = c("continent", "country"))
      )$attempt_id
    }
    if (length(sp)) {
      roles <- switch(f$category %||% "either",
                      invasive = "invasive", beneficiary = "beneficiary",
                      c("invasive", "beneficiary"))
      lab <- fw_species_label(data$species)
      asp <- data$attempt_species
      hit <- asp$attempt_id[asp$role %in% roles &
                              asp$species_id %in% lab$species_id[lab$label %in% sp]]
      keep_ids <- intersect(keep_ids, hit)
    }
    out <- out[vapply(out$attempt_ids, function(x) any(x %in% keep_ids), logical(1)), ]
  }

  org <- picked(f$organisation)
  if (length(org)) out <- out[!is.na(out$organisation) & out$organisation %in% org, ]

  term <- trimws(f$search %||% "")
  if (nzchar(term)) {
    hay <- paste(out$contact_name, coalesce(out$organisation, ""))
    out <- out[grepl(term, hay, ignore.case = TRUE, fixed = FALSE), ]
  }
  out
}

#' The choice lists for the Networking page's pickers
#'
#' From the attempts that HAVE a contact on this page only, so every option
#' finds somebody. Species are listed most-recorded first, as elsewhere, and
#' per role (the page offers the "either" list; the category scopes the match).
fw_networking_choices <- function(data, contacts) {
  ids <- unique(unlist(contacts$attempt_ids))
  att <- data$attempt[data$attempt$attempt_id %in% ids, ]
  lab <- fw_species_label(data$species)
  asp <- data$attempt_species[data$attempt_species$attempt_id %in% ids, ]
  by_freq <- function(sid) {
    tab <- sort(table(sid), decreasing = TRUE)
    lab$label[match(names(tab), lab$species_id)]
  }
  inv <- by_freq(asp$species_id[asp$role == "invasive"])
  ben <- by_freq(asp$species_id[asp$role == "beneficiary"])
  list(
    attempt = att,
    continent = sort(unique(stats::na.omit(att$continent))),
    country = sort(unique(stats::na.omit(att$country))),
    organisation = sort(unique(stats::na.omit(contacts$organisation))),
    species = list(invasive = inv, beneficiary = ben,
                   either = by_freq(asp$species_id))
  )
}

# Small helper so the address is assembled in JavaScript rather than sitting in
# the served markup as a mailto href.
jsonlite_quote <- function(x) paste0("'", gsub("'", "\\\\'", x), "'")

mod_networking_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The ONLY source of contact data on this page. Redaction has already
    # happened inside this function.
    contacts <- fw_contacts_summary(data)

    # Options come from the attempts a contact is attached to, so every one
    # finds somebody. See fw_networking_choices().
    ch <- fw_networking_choices(data, contacts)
    for (id in c("continent", "country", "organisation")) {
      updateSelectizeInput(session, id, choices = ch[[id]], selected = character(0),
                           server = FALSE)
    }

    # Every species, whatever its role. The category box only appears once a
    # species is picked, so it scopes the match rather than narrowing this list:
    # narrowing would drop the pick and hide the box it had just revealed.
    updateSelectizeInput(session, "species", choices = ch$species$either,
                         selected = character(0), server = TRUE)

    # Clearing the species hides the category box, and a scope the reader can
    # no longer see should not come back set when the box reappears.
    observeEvent(input$species, {
      if (!length(input$species)) updateSelectInput(session, "category", selected = "either")
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Continent and country narrow each other, as on every other page - over
    # the contacted attempts only, so a narrowed list never offers a country
    # nobody here worked in.
    fw_link_geo_filters(input, session, list(attempt = ch$attempt), ch)

    observeEvent(input$clear, {
      for (id in c("species", "continent", "country", "organisation")) {
        updateSelectizeInput(session, id, selected = character(0))
      }
      updateSelectInput(session, "category", selected = "either")
      updateTextInput(session, "search", value = "")
    })

    filtered <- reactive({
      fw_networking_filter(data, contacts, list(
        continent = input$continent, country = input$country,
        species = input$species, category = input$category,
        organisation = input$organisation, search = input$search
      ))
    })

    # WHAT THIS PAGE DOES NOT COVER, stated on the page itself.
    #
    # The directory is contact-grained, so an attempt with nobody attached is
    # invisible here rather than shown as a gap - which reads as full coverage
    # unless the page says otherwise. Two thirds of attempts are reachable and a
    # fifth have no contact at all, so "otherwise" matters.
    #
    # Computed from the loaded data rather than written into the copy, so it
    # cannot drift when the client's contact cleaning lands.
    output$coverage <- renderUI({
      contact_ids <- contacts$contact_id[!is.na(contacts$contact_email)]
      reachable <- sum(
        data$attempt$primary_contact_id   %in% contact_ids |
        data$attempt$secondary_contact_id %in% contact_ids
      )
      no_contact <- sum(is.na(data$attempt$primary_contact_id) &
                        is.na(data$attempt$secondary_contact_id))

      text <- fw_fill(
        fw_t("networking", "coverage"),
        reachable  = fw_fmt_num(reachable),
        total      = fw_fmt_num(nrow(data$attempt)),
        no_email   = fw_fmt_num(sum(is.na(contacts$contact_email))),
        contacts   = fw_fmt_num(nrow(contacts)),
        no_contact = fw_fmt_num(no_contact)
      )

      p(class = "fw-caption fw-coverage-note", text)
    })

    output$summary <- renderUI({
      f <- filtered()
      div(
        class = "fw-summary-strip",
        role = "status",
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value", fw_fmt_num(nrow(f))),
            span(class = "fw-summary-strip__label", fw_t("networking", "summary_contacts"))),
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value",
                 fw_fmt_num(length(unique(unlist(f$countries))))),
            span(class = "fw-summary-strip__label", fw_t("networking", "summary_countries"))),
        div(class = "fw-summary-strip__item",
            span(class = "fw-summary-strip__value",
                 fw_fmt_num(length(unique(unlist(f$continents))))),
            span(class = "fw-summary-strip__label", fw_t("networking", "summary_continents")))
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
    observeEvent(list(input$continent, input$country, input$species,
                      input$category, input$organisation, input$search,
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
          fw_t("networking", "page_showing"), " ",
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
          tags$td(r$organisation %|na|% fw_t("networking", "no_organisation")),
          # Continent as well as country. Someone looking for "anyone in
          # Africa" should not have to know which 29 countries are in the
          # database to find out there are two. Both are derived from the
          # attempts a contact is attached to - see fw_contacts_summary().
          tags$td(r$continent_label),
          tags$td(r$country_label),
          tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
          tags$td(fw_contact_action(r$contact_email, r$contact_name))
        )
      })

      tags$table(
        class = "fw-table",
        tags$caption(
          class = "fw-visually-hidden",
          paste(fw_t("networking", "summary_showing"), nrow(f), fw_t("common", "of"),
                nrow(filtered()), fw_t("networking", "summary_contacts"))
        ),
        tags$thead(tags$tr(
          tags$th(scope = "col", fw_t("networking", "col_name")),
          tags$th(scope = "col", fw_t("networking", "col_organisation")),
          tags$th(scope = "col", fw_t("networking", "col_continent")),
          tags$th(scope = "col", fw_t("networking", "col_country")),
          tags$th(scope = "col", class = "fw-col-num", fw_t("networking", "col_attempts")),
          tags$th(scope = "col", fw_t("networking", "col_contact"))
        )),
        tags$tbody(rows)
      )
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
      tags$span(class = "fw-visually-hidden", fw_t("networking", "email_none_label"))
    ))
  }
  parts <- strsplit(email, "@", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(tags$span(""))

  tags$a(
    href = "#",
    class = "fw-contact-link",
    `data-u` = parts[1],
    `data-d` = parts[2],
    `aria-label` = fw_fill(fw_t("a11y", "email_name"), name = name),
    onclick = paste0(
      "window.location.href='mail'+'to:'+this.dataset.u+String.fromCharCode(64)",
      "+this.dataset.d; return false;"
    ),
    fw_t("networking", "email_action")
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
`%|na|%` <- function(x, y) if (is.na(x) || !nzchar(x)) y else x
