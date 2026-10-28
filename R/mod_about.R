# mod_about.R
# The About page: a sign-up card, six click-to-open panels (client order), and
# a feedback band at the foot. All prose is client copy in FW_COPY.
#
# The sign-up card and the feedback band open the newsletter and feedback
# dialogs (fw_form_open()).

library(shiny)

mod_about_ui = function(id) {
  ns = NS(id)
  tagList(
    fw_page_header(fw_t("about", "title"), fw_t("about", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          div(class = "fw-about", uiOutput(ns("body")))
        )
      ),
      # Its own tinted band, so it reads as the end of the page, not a panel.
      fw_section(
        variant = "shoal",
        fw_container(uiOutput(ns("feedback")))
      )
    )
  )
}

mod_about_server = function(id, data, meta = NULL) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    output$body = renderUI({
      s = fw_headline_stats(data)

      para = function(key) p(fw_t("about", key))

      tagList(
        div(
          class = "fw-prose",

          # ---- Hear about it ---------------------------------------------
          fw_about_signup()
        ),

        # ---- The panels ------------------------------------------------------
        #
        # Client order. dev/value_test.R counts six: update it if this changes.
        div(
          class = "fw-about__panels",

          fw_disclosure(
            fw_t("about", "cite_heading"),
            note = fw_t("about", "cite_summary"),
            para("cite"),
            fw_about_citations(meta, s)
          ),

          fw_disclosure(
            fw_t("about", "method_heading"),
            note = fw_t("about", "method_summary"),
            # Ends with a link to the FWISE-FACTS project.
            p(fw_t("about", "method"), " ",
              tags$a(href = fw_t("about", "facts_url"), target = "_blank",
                     rel = "noopener", fw_t("about", "method_facts")))
          ),

          fw_disclosure(
            fw_t("about", "caveats_heading"),
            note = fw_t("about", "caveats_summary"),
            fw_caveats_ui(data, heading = FALSE)
          ),

          fw_disclosure(
            fw_t("about", "glossary_heading"),
            note = fw_t("about", "glossary_summary"),
            fw_about_glossary()
          ),

          fw_disclosure(
            fw_t("about", "related_heading"),
            note = fw_t("about", "related_summary"),
            para("related"),
            fw_about_related()
          ),

          fw_disclosure(
            fw_t("about", "other_heading"),
            note = fw_t("about", "other_summary"),
            tags$h3(fw_t("about", "images_heading")),
            p(fw_t("species", "image_note")),
            tags$h3(fw_t("about", "licence_heading")),
            p(fw_t("footer", "licence")),
            tags$h3(fw_t("about", "links_heading")),
            tags$ul(
              tags$li(tags$a(href = fw_t("footer", "github_url"),
                             fw_t("footer", "github_label"))),
              tags$li(fw_doi_link(fw_t("about", "link_zenodo")))
            )
          )
        )
      )
    })

    output$feedback = renderUI(fw_feedback_panel())
  })
}

# ---- Pieces ------------------------------------------------------------------

#' Hear about new releases
#'
#' The button opens the newsletter dialog (R/mod_newsletter.R).
fw_about_signup = function() {
  div(
    class = "fw-panel fw-signup",
    tags$h2(fw_t("about", "signup_heading")),
    p(fw_t("about", "signup_body")),
    fw_form_open("newsletter", fw_t("about", "signup_action"), source = "about")
  )
}

#' The copyable citation
#'
#' Via fw_citation_text(), shared with the downloads.
fw_about_citations = function(meta, s) {
  tagList(
    tags$pre(class = "fw-citation", fw_citation_text(meta, s$attempts)),
    p(fw_doi_link(fw_t("footer", "doi_label")))
  )
}

#' Where to look for what FWISE does not hold
#'
#' An entry's `note` is drawn if present.
fw_about_related = function() {
  items = fw_t("about", "related_items")
  tags$ul(
    class = "fw-linklist",
    lapply(items, function(it) {
      tags$li(
        tags$a(href = it$url, target = "_blank", rel = "noopener noreferrer",
               it$name),
        if (!is.null(it$note)) tags$span(class = "fw-linklist__note", it$note)
      )
    })
  )
}

#' What each method on the charts actually means
fw_about_glossary = function() {
  items = fw_t("about", "glossary_items")
  tags$dl(
    class = "fw-glossary",
    lapply(items, function(it) {
      tagList(
        tags$dt(class = "fw-glossary__term", it$term),
        tags$dd(class = "fw-glossary__body", it$body)
      )
    })
  )
}

#' Tell us it is wrong
#'
#' The paragraph and a button that opens the feedback dialog.
fw_feedback_panel = function() {
  div(
    class = "fw-feedback",
    p(fw_t("about", "fb_body")),
    p(
      class = "fw-feedback__action",
      fw_form_open("feedback", fw_t("about", "fb_action"), source = "about")
    )
  )
}
