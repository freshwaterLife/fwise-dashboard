# mod_about.R
# BUILT. What FWISE is, how it was made, how to cite it, and how to tell us it
# is wrong.
#
# MOSTLY CLIENT PROSE. Every claim about the database, the organisation or the
# partners belongs to the client and lives in FW_COPY. The one thing this file
# is allowed to assert on its own is a COUNT, and only by computing it from the
# loaded data - never by typing a number that will be wrong by next quarter.
#
# THE FEEDBACK BOX HAS NO BACKEND. It composes a mailto: and hands the message
# to the reader's own mail client. No service account, no sheet, no inbox to go
# stale, and nothing that can silently swallow a message - if the mail client
# does not open, the reader can see that it did not. The cost is that they have
# to press send themselves, so the button says exactly that.

library(shiny)

mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("about", "title"), fw_t("about", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          div(class = "fw-about", uiOutput(ns("body")))
        )
      )
    )
  )
}

mod_about_server <- function(id, data, meta = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- renderUI({
      s <- fw_headline_stats(data)
      n_contributors <- sum(!is.na(data$contact$contact_name) &
                              nzchar(data$contact$contact_name))

      section <- function(key, ...) {
        tagList(tags$h2(fw_t("about", paste0(key, "_heading"))), ...)
      }
      para <- function(key) p(fw_t("about", key))

      tagList(
        # THE DEFINITION, from the same source as the contribute page. A reader
        # and a contributor working from different definitions is how a database
        # like this stops meaning anything.
        fw_preamble(),

        div(
          class = "fw-prose",

          section("database", para("database"), para("database2")),

          # Computed, never typed. The sentence is the client's; the numbers in
          # it come from the data that is loaded right now.
          p(class = "fw-lead", fw_about_scale(s, n_contributors, ns)),

          section("method", para("method"), para("method2"), para("method3")),
          section("images", p(fw_t("species", "image_note"))),
          section("team", para("team")),

          section("cite",
                  para("cite"),
                  tags$pre(class = "fw-citation",
                           fw_about_citation(meta, s)),
                  p(tags$a(href = fw_t("footer", "doi_url"),
                           fw_t("footer", "doi_label")))),

          section("licence", p(fw_t("footer", "licence"))),

          section("links", tags$ul(
            tags$li(tags$a(href = fw_t("footer", "github_url"),
                           fw_t("footer", "github_label"))),
            tags$li(tags$a(href = fw_t("footer", "fwise_url"),
                           fw_t("about", "link_fwise"))),
            tags$li(tags$a(href = fw_t("footer", "doi_url"),
                           fw_t("about", "link_zenodo")))
          ))
        ),

        fw_feedback_panel(ns)
      )
    })

    # The whole of the feedback box. Building the mailto in the browser keeps
    # the address out of the served markup, the same anti-scraping reasoning as
    # the Networking page, and means the message never touches the server.
    observeEvent(input$send_feedback, {
      body <- trimws(input$feedback %||% "")
      if (!nzchar(body)) {
        session$sendCustomMessage("fw-announce", fw_t("about", "fb_empty"))
        return()
      }
      session$sendCustomMessage("fw-mailto", list(
        to = fw_t("about", "feedback_email"),
        subject = paste0(fw_t("about", "fb_subject"), " (",
                         input$feedback_page %||% "-", ")"),
        body = body
      ))
      session$sendCustomMessage("fw-announce", fw_t("about", "fb_sent"))
    })
  })
}

# ---- Pieces ------------------------------------------------------------------

#' The one sentence in the app that states the size of the database
#'
#' Assembled from live counts, with the invitation to be the next contributor
#' wired to the contribute page rather than written as a dead sentence.
fw_about_scale <- function(s, n_contributors, ns) {
  text <- fw_fill(
    fw_t("about", "scale"),
    attempts     = fw_fmt_num(s$attempts),
    countries    = fw_fmt_num(s$countries),
    contributors = fw_fmt_num(n_contributors),
    species      = fw_fmt_num(s$species),
    year         = as.character(s$earliest_year)
  )

  tagList(
    text, " ",
    tags$a(
      href = "#",
      onclick = "Shiny.setInputValue('fw_nav_to','contribute',{priority:'event'}); return false;",
      fw_fill(fw_t("about", "scale_action"), n = fw_ordinal(n_contributors + 1L))
    )
  )
}

#' A copyable citation, with the release the reader is actually looking at
fw_about_citation <- function(meta, s) {
  release <- meta$release %||% format(Sys.Date())
  year <- substr(as.character(release), 1, 4)
  fw_fill(fw_t("about", "citation"), year = year,
          release = as.character(release), n = fw_fmt_num(s$attempts))
}

#' Tell us it is wrong
fw_feedback_panel <- function(ns) {
  div(
    class = "fw-panel fw-feedback",
    tags$h2(fw_t("about", "fb_heading")),
    p(fw_t("about", "fb_body")),
    div(
      class = "fw-field",
      tags$label(class = "form-label", `for` = ns("feedback_page"),
                 fw_t("about", "fb_where")),
      selectInput(ns("feedback_page"), label = NULL, selectize = FALSE,
                  choices = unname(unlist(FW_COPY$nav)))
    ),
    div(
      class = "fw-field",
      tags$label(class = "form-label", `for` = ns("feedback"),
                 fw_t("about", "fb_label")),
      tags$textarea(id = ns("feedback"), class = "form-control", rows = 5,
                    placeholder = fw_t("about", "fb_placeholder"))
    ),
    actionButton(ns("send_feedback"), fw_t("about", "fb_action"),
                 class = "btn btn-primary"),
    p(class = "fw-caption", fw_t("about", "fb_note"))
  )
}
