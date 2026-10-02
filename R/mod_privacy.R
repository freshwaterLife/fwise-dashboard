# mod_privacy.R
# The Privacy and data terms page.
#
# A PAGE WITH NO NAVBAR LINK. It is a nav_panel like any other (value
# "privacy"), so nav_select() can show it and it sits in the page's own
# structure, but its link is hidden by CSS - see the rule for
# .nav-link[data-value="privacy"] in _components.scss. CSS rather than
# nav_hide() from the server, because the stylesheet is in place before the
# first paint and a server call is not: the link would flash in and out.
#
# HOW PEOPLE ARRIVE. The footer's "Privacy and data terms" link, and links of
# the form ?page=privacy&section=feedback from the forms, which open in a new
# tab. app.R reads that query string once at the start of a session, opens the
# page and asks the browser to scroll to the section; while the page is open
# the address says ?page=privacy, so it can be copied and shared. The
# observers are in app.R because the navbar is the app's, not this page's.
#
# THE TEXT IS A MARKDOWN FILE, content/privacy_and_data_terms.md, read once
# when the UI is built. Its header comment says how to edit it.

FW_PRIVACY_FILE <- file.path("content", "privacy_and_data_terms.md")

mod_privacy_ui <- function(id) {
  ns <- NS(id)
  doc <- fw_privacy_html()
  updated <- as.Date(fw_t("privacy", "last_updated"))

  tagList(
    # THE TITLE IS VISIBLE HERE, unlike the main pages, which keep theirs for
    # screen readers only. A reader who lands on this page from a link in a
    # form has no navbar item lit to say where they are.
    fw_page_header(fw_t("privacy", "title")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          # THE FULL CONTAINER WIDTH, like every other page (Alex, 2 Oct
          # 2026). It was held to a 68ch reading column.
          div(
            class = "fw-privacy",
            p(class = "fw-privacy__version",
              fw_t("privacy", "version_label"), " ",
              tags$span(class = "fw-num", fw_t("privacy", "version")),
              tags$span(class = "fw-privacy__sep", `aria-hidden` = "true", " · "),
              fw_t("privacy", "updated_label"), " ",
              tags$span(class = "fw-num",
                        paste(as.integer(format(updated, "%d")),
                              format(updated, "%B %Y")))),
            # The opening paragraph, then the contents, then the rest: a
            # reader is told what the page is before being shown its parts.
            div(class = "fw-prose fw-privacy__intro", doc$intro),
            fw_privacy_contents(ns, doc$toc),
            div(class = "fw-prose fw-privacy__body", doc$html)
          )
        )
      )
    )
  )
}

#' The privacy text as HTML, with its headings listed for the contents
#'
#' Four passes over commonmark's output, all on markup this app wrote:
#'
#'   1. "## Title {#id}" headings get the id (and tabindex -1, so a deep link
#'      can move keyboard focus to the section it scrolled to). commonmark
#'      leaves the braces in the heading text, which is what makes this a
#'      simple substitution rather than a parser.
#'   2. Every [TO CONFIRM: ...] is wrapped in <mark>, so none can go live
#'      without being seen.
#'   3. {contact_email} becomes the click-to-reveal address.
#'   4. The file's own HTML comment (its editing notes) is dropped first, so
#'      the notes are not served and the token inside them is not replaced.
#'
#' @return list(html = HTML, intro = HTML, toc = data.frame(level, id, title)).
#'   `intro` is whatever comes before the first ## heading.
fw_privacy_html <- function(path = FW_PRIVACY_FILE) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  text <- gsub("(?s)<!--.*?-->", "", text, perl = TRUE)
  html <- as.character(shiny::markdown(text))

  head_re <- "<h([23])>(.*?) \\{#([a-z0-9-]+)\\}</h\\1>"
  found <- regmatches(html, gregexpr(head_re, html, perl = TRUE))[[1]]
  toc <- data.frame(
    level = as.integer(sub(head_re, "\\1", found, perl = TRUE)),
    id    = sub(head_re, "\\3", found, perl = TRUE),
    title = gsub("<[^>]+>", "", sub(head_re, "\\2", found, perl = TRUE)),
    stringsAsFactors = FALSE
  )
  html <- gsub(head_re, '<h\\1 id="\\3" tabindex="-1">\\2</h\\1>', html, perl = TRUE)

  html <- gsub("\\[TO CONFIRM:([^]]*)\\]",
               '<mark class="fw-placeholder">[TO CONFIRM:\\1]</mark>', html, perl = TRUE)

  reveal <- as.character(fw_email_reveal(fw_t("footer", "contact_email"),
                                         label = fw_t("footer", "contact_label"),
                                         aria = fw_t("footer", "contact_aria")))
  html <- gsub("{contact_email}", reveal, html, fixed = TRUE)

  # Everything before the first part heading is the page's opening, shown
  # above the contents list.
  cut <- regexpr("<h2 ", html, fixed = TRUE)
  intro <- if (cut > 0) substr(html, 1, cut - 1) else ""
  body <- if (cut > 0) substring(html, cut) else html

  list(html = HTML(body), intro = HTML(intro), toc = toc)
}

#' The contents list: each part, with its sections under it
#'
#' Built from the headings fw_privacy_html() found, so it cannot list a
#' section the text no longer has.
fw_privacy_contents <- function(ns, toc) {
  item <- function(i) tags$a(href = paste0("#", toc$id[i]), HTML(toc$title[i]))
  parts <- which(toc$level == 2)
  ends <- c(parts[-1] - 1L, nrow(toc))
  tags$nav(
    class = "fw-privacy__contents",
    `aria-labelledby` = ns("contents"),
    tags$h2(id = ns("contents"), class = "fw-privacy__contents-title",
            fw_t("privacy", "contents")),
    tags$ul(lapply(seq_along(parts), function(k) {
      kids <- seq_len(nrow(toc))[seq_len(nrow(toc)) > parts[k] &
                                   seq_len(nrow(toc)) <= ends[k]]
      tags$li(item(parts[k]),
              if (length(kids)) tags$ul(lapply(kids, function(i) tags$li(item(i)))))
    }))
  )
}

#' Browser side of the page's address: scroll to a section, clear the query
#' string on the way out, and follow Back and Forward
#'
#' Messages and one input, nothing more; app.R decides what happens.
fw_privacy_script <- function() {
  tags$script(HTML("
    $(function () {
      // Scroll to one section, once its page is showing. A deep link selects
      // the page and asks for the section in the same breath, so the pane may
      // not be active yet: then wait for Bootstrap to say it has been shown.
      Shiny.addCustomMessageHandler('fw-scroll-to-section', function (id) {
        var go = function () {
          var el = document.getElementById(id);
          if (!el) return;
          el.scrollIntoView({ block: 'start' });
          el.focus({ preventScroll: true });
        };
        var el = document.getElementById(id);
        var pane = el && el.closest('.tab-pane');
        if (!pane || pane.classList.contains('active')) { setTimeout(go, 0); return; }
        $(document).one('shown.bs.tab', function () { setTimeout(go, 0); });
      });
      // Leaving the page: the address goes back to the bare app, as a new
      // history entry, so Back returns to the privacy page.
      Shiny.addCustomMessageHandler('fw-url-clear', function (_) {
        history.pushState(null, '', location.pathname);
      });
      // Back and Forward change the address without reloading; tell the
      // server, which moves the page to match.
      window.addEventListener('popstate', function () {
        Shiny.setInputValue('fw_popstate', location.search, { priority: 'event' });
      });
    });
  "))
}
