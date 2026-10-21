# mod_networking.R
# The "Connect with community" page: a filtered, paged contact directory with a
# summary strip.
#
# Blocker to ensure no privacy leaks: reads only fw_contacts_summary(), which
# drops private contacts before any session sees them.

library(shiny)
library(dplyr)

mod_networking_ui = function(id) {
  ns = NS(id)
  tagList(
    fw_page_header(fw_t("networking", "title"), fw_t("networking", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          div(
            class = "fw-filters fw-filters--networking",
            # The role scope appears under the species box once one is picked.
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
            # Full size, not btn-sm, so it matches the inputs' height.
            div(
              class = "fw-filters__clear",
              actionButton(ns("clear"), fw_t("common", "clear_filters"),
                           class = "btn btn-outline-primary")
            )
          ),

          uiOutput(ns("summary")),
          uiOutput(ns("coverage")),

          # Page size is static UI so renderUI doesn't reset it.
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

          # Callout asking for corrections to the directory.
          div(
            class = "fw-callout fw-callout--next",
            p(
              fw_t("networking", "outro_text"), " ",
              # Revealed in place (fw_email_reveal()); never write it into
              # the markup. No full stop: the sentence ends in the Copy button.
              fw_email_reveal(fw_t("footer", "contact_email"),
                              label = fw_t("networking", "outro_action"))
            )
          )
        )
      )
    )
  )
}

# Species filter scope: either role, the invasive species, or the protected one.
FW_NETWORKING_CATEGORIES = c("either", "invasive", "beneficiary")

fw_networking_picker = function(ns, id) {
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
#' Place and species match per attempt (one attempt must satisfy them all);
#' organisation and name search match the person.
#'
#' @param contacts fw_contacts_summary(data)
#' @param f list(continent, country, species, category, organisation, search);
#'   an empty or NULL entry is no filter. `species` holds labels as the picker
#'   shows them; `category` scopes them to a role ("either" by default).
fw_networking_filter = function(data, contacts, f) {
  picked = function(x) if (length(x)) as.character(x[nzchar(x)]) else character(0)
  cont = picked(f$continent); ctry = picked(f$country); sp = picked(f$species)
  out = contacts

  if (length(cont) || length(ctry) || length(sp)) {
    keep_ids = data$attempt$attempt_id
    if (length(cont) || length(ctry)) {
      keep_ids = fw_filter_apply(
        data, list(continent = cont, country = ctry, .ids = c("continent", "country"))
      )$attempt_id
    }
    if (length(sp)) {
      roles = switch(f$category %||% "either",
                      invasive = "invasive", beneficiary = "beneficiary",
                      c("invasive", "beneficiary"))
      lab = fw_species_label(data$species)
      asp = data$attempt_species
      hit = asp$attempt_id[asp$role %in% roles &
                              asp$species_id %in% lab$species_id[lab$label %in% sp]]
      keep_ids = intersect(keep_ids, hit)
    }
    out = out[vapply(out$attempt_ids, function(x) any(x %in% keep_ids), logical(1)), ]
  }

  org = picked(f$organisation)
  if (length(org)) out = out[!is.na(out$organisation) & out$organisation %in% org, ]

  term = trimws(f$search %||% "")
  if (nzchar(term)) {
    hay = paste(coalesce(out$contact_name, ""), coalesce(out$organisation, ""))
    out = out[grepl(term, hay, ignore.case = TRUE, fixed = FALSE), ]
  }
  out
}

#' The choice lists for the Networking page's pickers
#'
#' From attempts with a listed contact only, so every option finds someone.
#' Species most-recorded first, per role.
fw_networking_choices = function(data, contacts) {
  ids = unique(unlist(contacts$attempt_ids))
  att = data$attempt[data$attempt$attempt_id %in% ids, ]
  lab = fw_species_label(data$species)
  asp = data$attempt_species[data$attempt_species$attempt_id %in% ids, ]
  by_freq = function(sid) {
    tab = sort(table(sid), decreasing = TRUE)
    lab$label[match(names(tab), lab$species_id)]
  }
  inv = by_freq(asp$species_id[asp$role == "invasive"])
  ben = by_freq(asp$species_id[asp$role == "beneficiary"])
  list(
    attempt = att,
    continent = sort(unique(stats::na.omit(att$continent))),
    country = sort(unique(stats::na.omit(att$country))),
    organisation = sort(unique(stats::na.omit(contacts$organisation))),
    species = list(invasive = inv, beneficiary = ben,
                   either = by_freq(asp$species_id))
  )
}

mod_networking_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    # The only contact source here; already redacted.
    contacts = fw_contacts_summary(data)

    ch = fw_networking_choices(data, contacts)
    for (id in c("continent", "country", "organisation")) {
      updateSelectizeInput(session, id, choices = ch[[id]], selected = character(0),
                           server = FALSE)
    }

    # All species: the category scopes the match, not this list.
    updateSelectizeInput(session, "species", choices = ch$species$either,
                         selected = character(0), server = TRUE)

    # Reset the hidden category when species is cleared.
    observeEvent(input$species, {
      if (!length(input$species)) updateSelectInput(session, "category", selected = "either")
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Continent and country narrow each other, over contacted attempts only.
    fw_link_geo_filters(input, session, list(attempt = ch$attempt), ch)

    observeEvent(input$clear, {
      for (id in c("species", "continent", "country", "organisation")) {
        updateSelectizeInput(session, id, selected = character(0))
      }
      updateSelectInput(session, "category", selected = "either")
      updateTextInput(session, "search", value = "")
    })

    filtered = reactive({
      fw_networking_filter(data, contacts, list(
        continent = input$continent, country = input$country,
        species = input$species, category = input$category,
        organisation = input$organisation, search = input$search
      ))
    })

    # Coverage line: what the directory doesn't reach, computed from the data.
    output$coverage = renderUI({
      contact_ids = contacts$contact_id[!is.na(contacts$contact_email)]
      reachable = sum(
        data$attempt$primary_contact_id   %in% contact_ids |
        data$attempt$secondary_contact_id %in% contact_ids
      )
      no_contact = sum(is.na(data$attempt$primary_contact_id) &
                        is.na(data$attempt$secondary_contact_id))

      text = fw_fill(
        fw_t("networking", "coverage"),
        reachable  = fw_fmt_num(reachable),
        total      = fw_fmt_num(nrow(data$attempt)),
        no_email   = fw_fmt_num(sum(is.na(contacts$contact_email))),
        contacts   = fw_fmt_num(nrow(contacts)),
        no_contact = fw_fmt_num(no_contact)
      )

      p(class = "fw-caption fw-coverage-note", text)
    })

    output$summary = renderUI({
      f = filtered()
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

    # Paged, with a reader-chosen page size.
    page = reactiveVal(1L)

    page_size = reactive({
      n = suppressWarnings(as.integer(input$page_size))
      if (length(n) != 1 || is.na(n) || n <= 0) FW_CONTACTS_PAGE_SIZES[1] else n
    })

    # Back to page 1 on any filter or page-size change.
    observeEvent(list(input$continent, input$country, input$species,
                      input$category, input$organisation, input$search,
                      input$page_size), {
      page(1L)
    }, ignoreInit = TRUE)

    n_pages = reactive(max(1L, ceiling(nrow(filtered()) / page_size())))

    paged = reactive({
      f = filtered()
      p = min(page(), n_pages())
      from = (p - 1L) * page_size() + 1L
      to = min(nrow(f), p * page_size())
      if (nrow(f) == 0) f else f[from:to, ]
    })

    # One input for all page buttons, so observers don't pile up.
    observeEvent(input$goto_page, {
      n = suppressWarnings(as.integer(input$goto_page))
      if (length(n) == 1 && !is.na(n)) page(max(1L, min(n, n_pages())))
    })

    output$pager = renderUI({
      if (nrow(filtered()) == 0) return(NULL)
      p = min(page(), n_pages())
      from = (p - 1L) * page_size() + 1L
      to = min(nrow(filtered()), p * page_size())

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

    output$table = renderUI({
      f = paged()
      if (nrow(f) == 0) return(p(fw_t("common", "no_results")))

      rows = lapply(seq_len(nrow(f)), function(i) {
        r = f[i, ]
        tags$tr(
          tags$td(r$contact_name %|na|% ""),
          tags$td(r$organisation %|na|% fw_t("networking", "no_organisation")),
          # Continent and country, from the contact's attempts.
          tags$td(r$continent_label),
          tags$td(r$country_label),
          tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
          tags$td(fw_contact_action(r$contact_email, fw_contact_who(r),
                                    contact_id = r$contact_id))
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

#' A contact's address, in a directory row
#'
#' fw_email_reveal() in the app; a plain mailto link in downloads
#' (reveal = FALSE). Private contacts never get here (fw_contacts_summary()).
fw_contact_action = function(email, name, reveal = TRUE, contact_id = NULL) {
  if (is.na(email) || !nzchar(email)) {
    # An empty cell, not a "hidden" badge.
    return(tags$span(
      tags$span(class = "fw-visually-hidden", fw_t("networking", "email_none_label"))
    ))
  }
  if (!reveal) return(tags$a(href = paste0("mailto:", email), email))
  fw_email_reveal(
    email,
    label = fw_t("networking", "email_action"),
    # Not when the "name" is the address itself (fw_contact_who() fallback).
    aria = if (!identical(name, email)) fw_fill(fw_t("a11y", "email_name"), name = name),
    contact_id = contact_id
  )
}

#' What to call a contact: the person, or the organisation when no one is named
fw_contact_who = function(r) (r$contact_name %|na|% r$organisation) %|na|% r$contact_email

`%||%` = function(x, y) if (is.null(x) || length(x) == 0) y else x
`%|na|%` = function(x, y) if (is.na(x) || !nzchar(x)) y else x
