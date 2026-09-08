# mod_explore.R
# STUB. The explore page is not built.
#
# ==============================================================================
# EXPLORE THE DATA - INTENDED STRUCTURE
# ==============================================================================
#
# The browsing counterpart to the planning page. A user arrives wanting to look
# things up rather than be guided, so this is a filter panel and not a
# questionnaire.
#
# LAYOUT
#   bslib::layout_sidebar() with the filters in the sidebar and the map plus a
#   plot area in the main panel.
#
# FILTER SIDEBAR
#   continent, country, species, method, outcome.
#   Populate every one from FW_CHOICES (built by fw_startup_choices() at
#   startup) so the client's data cleaning flows through automatically. Never
#   hardcode a filter's options.
#
#   NOTE ON OUTCOME. Outcome IS filterable here and is deliberately NOT
#   filterable on the planning page. The reason is in mod_plan.R: on a planning
#   page, letting a user filter to successes only produces false optimism about
#   their own site. Browsing the evidence base is a different activity and the
#   filter is appropriate here. Do not "make the two pages consistent".
#
#   NOTE ON POLYGON DRAWING. Drawing a custom area on the map to filter by it was
#   considered and RULED OUT. Do not add it back without asking the client.
#
# MAP
#   Points for the filtered attempts, coloured by outcome using FW_PALETTE only.
#   Muted low-chroma base tiles so the data carries the colour.
#   Needs a text alternative and a keyboard route to the same information, which
#   in practice means the table below has to be able to stand alone.
#
# PLOT AREA
#   Charts of the filtered set. Wong palette throughout, so figures match the
#   paper and the exported reports.
#
# ATTEMPT DETAIL
#   The contacts page links here with a contact_id, expecting to land on that
#   contact's attempts. That route currently dead-ends on the stub. When this
#   page is built, read the incoming value and pre-filter to those attempt ids.
#   fw_contacts_summary() already returns an attempt_ids list column per contact.
# ==============================================================================

mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("explore", "title"), fw_t("explore", "description")),
    tags$main(
      id = "fw-main",
      fw_section(fw_container(fw_stub_panel(extra = uiOutput(ns("incoming")))))
    )
  )
}

mod_explore_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # The contacts page routes here with a contact_id. Until the page is built,
    # acknowledge it rather than silently dropping it, so the link does not feel
    # broken to someone testing the flow.
    output$incoming <- renderUI({
      cid <- fw_explore_request()
      req(cid)
      contact <- data$contact[data$contact$contact_id == cid, ]
      if (nrow(contact) == 0) return(NULL)
      div(
        class = "fw-skeleton-note",
        paste0(
          "You followed a link to the attempts recorded by ",
          contact$contact_name[1],
          ". That view is part of this page and is not built yet."
        )
      )
    })
  })
}

# Set by the contacts page, read by the explore page. A tiny shared reactive
# value rather than a module return, because the two modules are siblings and
# neither owns the other.
.fw_explore_request <- shiny::reactiveVal(NULL)
fw_explore_request <- function() .fw_explore_request()
fw_set_explore_request <- function(contact_id) .fw_explore_request(contact_id)
