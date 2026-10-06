# mod_about.R
# BUILT. What FWISE is, how to cite it, what to be careful of, and how to tell
# us it is wrong.
#
# MOSTLY CLIENT PROSE. Every claim about the database, the organisation or the
# partners belongs to the client and lives in FW_COPY. The one thing this file
# is allowed to assert on its own is a COUNT, and only by computing it from the
# loaded data - never by typing a number that will be wrong by next quarter.
#
# THE SHAPE OF THE PAGE, which is a client decision and not a layout accident:
# a short always-visible opening - now the sign-up card alone, since the
# paragraphs above it were placeholder text and came out on 24 Sept 2026 - and
# then five click-to-open panels holding the depth, the
# citation and the small print included. A reader who wants to know whether to
# trust a figure opens the caveats; a reader who wants to know where the records
# came from opens the methods; nobody has to scroll past either to reach the
# feedback box.
#
# WHAT COUNTS AS AN ERADICATION USED TO OPEN THIS PAGE. It is the definition the
# whole database is built on, shared with the contribute form through
# fw_preamble(), and it now appears only there - where the person who has to
# APPLY the definition is. On About it was two headings of scope rules before a
# reader had been told what they were reading about.
#
# THE SIGN-UP CARD AND THE FEEDBACK BAND OPEN THE TWO FORMS (Alex, 2 Oct 2026).
# Both are buttons made by fw_form_open() - the newsletter and feedback dialogs
# in R/mod_newsletter.R and R/mod_feedback.R, which write to the forms sheet.
# The band used to show the FWISE address through fw_email_reveal(); the
# button replaced it, and the address is still in the footer for anyone who
# would rather write. The band sits on its own at the foot of the page and is
# deliberately NOT one of the panels: a reader who has found something wrong
# should not have to open anything first.

library(shiny)

mod_about_ui <- function(id) {
  ns <- NS(id)
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
      # ITS OWN BAND, outside .fw-about. On the page ground it read as a fifth
      # panel among the ones above it; on a tinted band of its own it reads as
      # the end of the page, which is what it is.
      fw_section(
        variant = "shoal",
        fw_container(uiOutput(ns("feedback")))
      )
    )
  )
}

mod_about_server <- function(id, data, meta = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- renderUI({
      s <- fw_headline_stats(data)

      para <- function(key) p(fw_t("about", key))

      tagList(
        div(
          class = "fw-prose",

          # ---- Hear about it ---------------------------------------------
          #
          # THE PAGE OPENS ON THIS. The "what FWISE is" paragraphs that used to
          # sit above it were Lorem Ipsum awaiting client copy and were taken
          # out on 24 Sept 2026 - see the note at `about` in R/copy.R for what
          # to put back when the text arrives.
          fw_about_signup()
        ),

        # ---- The depth, behind disclosures --------------------------------
        #
        # ORDER IS THE CLIENT'S (23 Sept 2026), and it runs from the thing a
        # reader most often arrives wanting to the thing they read last: how to
        # cite it, how it was made, what to be careful of, what the methods on
        # the charts mean, where else to look, and the page's small print.
        #
        # SIX PANELS. dev/value_test.R counts them - add one here and update
        # the count there.
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
            para("method")
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

    output$feedback <- renderUI(fw_feedback_panel())
  })
}

# ---- Pieces ------------------------------------------------------------------

#' Hear about new releases
#'
#' The button opens the "Get FWISE updates" dialog (R/mod_newsletter.R). It
#' used to be a link to a mailing list that did not exist yet, which with no
#' address opened the app's own home page in a new tab.
fw_about_signup <- function() {
  div(
    class = "fw-panel fw-signup",
    # Visible since 24 Sept 2026 (client): the card needs a name now it leads
    # the page.
    tags$h2(fw_t("about", "signup_heading")),
    p(fw_t("about", "signup_body")),
    fw_form_open("newsletter", fw_t("about", "signup_action"), source = "about")
  )
}

#' The copyable citation
#'
#' ONE CITATION, FOR THE DATABASE, in the client's literal form - see
#' about$citation_db in R/copy.R, and fw_citation_text() in R/export.R, which
#' the downloads' closing section shares.
fw_about_citations <- function(meta, s) {
  tagList(
    tags$pre(class = "fw-citation", fw_citation_text(meta, s$attempts)),
    p(fw_doi_link(fw_t("footer", "doi_label")))
  )
}

#' Where to look for what FWISE does not hold
#'
#' THE NOTE IS OPTIONAL. Entries used to carry a line saying what was behind
#' each one; the client's own list (23 Sept 2026) is eight well-known resources
#' by name and URL, with no characterisation of ours attached. An entry that
#' does carry a `note` still draws it, so a future list can mix the two.
fw_about_related <- function() {
  items <- fw_t("about", "related_items")
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
fw_about_glossary <- function() {
  items <- fw_t("about", "glossary_items")
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
fw_feedback_panel <- function() {
  div(
    class = "fw-feedback",
    # NO HIDDEN HEADING (client, 23 Sept 2026). fb_heading - "Tell us what is
    # wrong" - was visually hidden and announced to a screen reader only, so
    # deleting it costs sighted readers nothing and removes a line nobody
    # could see. The body paragraph opens the panel now.
    p(fw_t("about", "fb_body")),
    p(
      class = "fw-feedback__action",
      fw_form_open("feedback", fw_t("about", "fb_action"), source = "about")
    )
  )
}
