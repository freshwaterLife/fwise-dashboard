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
# a short summary that is always visible - what FWISE is and how
# to hear about it - and then five click-to-open panels holding the depth, the
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
# THE FEEDBACK BOX HAS NO BACKEND. It composes a mailto: and hands the message
# to the reader's own mail client. No service account, no sheet, no inbox to go
# stale, and nothing that can silently swallow a message - if the mail client
# does not open, the reader can see that it did not. The cost is that they have
# to press send themselves, so the button says exactly that. It sits in a band
# of its own at the foot of the page and is deliberately NOT one of the panels:
# a reader who has found something wrong should not have to open anything first.

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

      section <- function(key, ...) {
        tagList(tags$h2(class = "fw-visually-hidden", fw_t("about", paste0(key, "_heading"))), ...)
      }
      para <- function(key) p(fw_t("about", key))

      tagList(
        div(
          class = "fw-prose",

          # ---- What this is, and how big it is ---------------------------
          section("database", para("database"), para("database2")),

          # ---- Hear about it ---------------------------------------------
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
            para("method"), para("method2"), para("method3"),
            # Reserved for the paper's methods section, which the client is
            # writing. A vector, so it takes however many paragraphs arrive.
            tags$h3(fw_t("about", "method_paper_heading")),
            lapply(fw_t("about", "method_paper"), function(x) p(x))
          ),

          fw_disclosure(
            fw_t("about", "caveats_heading"),
            note = fw_t("about", "caveats_summary"),
            p(class = "fw-lead", fw_t("about", "caveats_lead")),
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
              tags$li(tags$a(href = fw_t("footer", "fwl_url"),
                             fw_t("about", "link_fwise"))),
              tags$li(tags$a(href = fw_t("footer", "doi_url"),
                             fw_t("about", "link_zenodo")))
            )
          )
        )
      )
    })

    output$feedback <- renderUI(fw_feedback_panel(ns))

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

#' Hear about new releases
#'
#' A LINK OUT, NOT A FORM. The app collects no address, so there is nothing here
#' to secure, nothing to store and no delivery that can fail silently - the same
#' reasoning as the feedback box, which hands the message to the reader's own
#' mail client rather than pretending to send it.
fw_about_signup <- function() {
  div(
    class = "fw-panel fw-signup",
    tags$h2(class = "fw-visually-hidden", fw_t("about", "signup_heading")),
    p(fw_t("about", "signup_body")),
    tags$a(
      class = "btn btn-primary",
      href = fw_t("about", "signup_url"),
      target = "_blank", rel = "noopener noreferrer",
      fw_t("about", "signup_action")
    )
  )
}

#' Two copyable citations, with the release the reader is actually looking at
#'
#' THE DASHBOARD AND THE DATABASE ARE TWO THINGS TO CITE and a reader quoting a
#' figure needs the one that pins the release they read it in. Only the database
#' citation carries the attempt count: it is a property of the data, and putting
#' it on the dashboard citation would suggest the dashboard is a version of a
#' number rather than a way of reading one.
fw_about_citations <- function(meta, s) {
  release <- meta$release %||% format(Sys.Date())
  year <- substr(as.character(release), 1, 4)

  tagList(
    tags$pre(class = "fw-citation",
             fw_fill(fw_t("about", "citation_db"),
                     year = year, release = as.character(release),
                     n = fw_fmt_num(s$attempts))),
    p(tags$a(href = fw_t("footer", "doi_url"), fw_t("footer", "doi_label")))
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
#'
#' A DESCRIPTION LIST, because that is what it is: a term and its definition,
#' which <dl> says to a screen reader and a <ul> of bolded run-ons does not.
#'
#' THE TERMS MATCH THE DATA. Every `term` in about$glossary_items is a value
#' that appears in attempts.csv$methods, so a reader who meets "Antimycin-A" on
#' the method chart finds it here spelled the same way. dev/value_test.R checks
#' the two lists against each other.
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
fw_feedback_panel <- function(ns) {
  div(
    class = "fw-feedback",
    # NO HIDDEN HEADING (client, 23 Sept 2026). fb_heading - "Tell us what is
    # wrong" - was visually hidden and announced to a screen reader only, so
    # deleting it costs sighted readers nothing and removes a line nobody
    # could see. The body paragraph opens the panel now.
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
