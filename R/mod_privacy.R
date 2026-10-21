# mod_privacy.R
# The Privacy and data terms page. A nav_panel whose navbar link is hidden by
# CSS (nav_hide() would flash). Reached from the footer and ?page=privacy
# links; app.R handles the URL. Text is content/privacy_and_data_terms.md,
# read once.

FW_PRIVACY_FILE = file.path("content", "privacy_and_data_terms.md")

mod_privacy_ui = function(id) {
  ns = NS(id)
  doc = fw_privacy_html()
  updated = as.Date(fw_t("privacy", "last_updated"))

  tagList(
    fw_page_header(fw_t("privacy", "title")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
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
            div(class = "fw-prose fw-privacy__intro", doc$intro),
            fw_privacy_contents(ns, doc$toc),
            div(class = "fw-prose fw-privacy__body", doc$html)
          )
        )
      )
    )
  )
}


fw_privacy_html = function(path = FW_PRIVACY_FILE) {
  text = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  text = gsub("(?s)<!--.*?-->", "", text, perl = TRUE)
  html = as.character(shiny::markdown(text))

  head_re = "<h([23])>(.*?) \\{#([a-z0-9-]+)\\}</h\\1>"
  found = regmatches(html, gregexpr(head_re, html, perl = TRUE))[[1]]
  toc = data.frame(
    level = as.integer(sub(head_re, "\\1", found, perl = TRUE)),
    id    = sub(head_re, "\\3", found, perl = TRUE),
    title = gsub("<[^>]+>", "", sub(head_re, "\\2", found, perl = TRUE)),
    stringsAsFactors = FALSE
  )
  html = gsub(head_re, '<h\\1 id="\\3" tabindex="-1">\\2</h\\1>', html, perl = TRUE)

  html = gsub("\\[TO CONFIRM:([^]]*)\\]",
               '<mark class="fw-placeholder">[TO CONFIRM:\\1]</mark>', html, perl = TRUE)

  reveal = as.character(fw_email_reveal(fw_t("footer", "contact_email"),
                                         label = fw_t("footer", "contact_label"),
                                         aria = fw_t("footer", "contact_aria")))
  html = gsub("{contact_email}", reveal, html, fixed = TRUE)

  # Everything before the first part heading is the page's opening, shown
  # above the contents list.
  cut = regexpr("<h2 ", html, fixed = TRUE)
  intro = if (cut > 0) substr(html, 1, cut - 1) else ""
  body = if (cut > 0) substring(html, cut) else html

  list(html = HTML(body), intro = HTML(intro), toc = toc)
}

#' The contents list: each part, with its sections under it
#'
#' Built from the headings fw_privacy_html() found.
fw_privacy_contents = function(ns, toc) {
  item = function(i) tags$a(href = paste0("#", toc$id[i]), HTML(toc$title[i]))
  parts = which(toc$level == 2)
  ends = c(parts[-1] - 1L, nrow(toc))
  tags$nav(
    class = "fw-privacy__contents",
    `aria-labelledby` = ns("contents"),
    tags$h2(id = ns("contents"), class = "fw-privacy__contents-title",
            fw_t("privacy", "contents")),
    tags$ul(lapply(seq_along(parts), function(k) {
      kids = seq_len(nrow(toc))[seq_len(nrow(toc)) > parts[k] &
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
fw_privacy_script = function() {
  tags$script(HTML("
    $(function () {
      // Scroll to a section once the page is shown (wait for Bootstrap if
      // the pane isn't active yet).
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
      // Leaving: push the bare address, so Back returns here.
      Shiny.addCustomMessageHandler('fw-url-clear', function (_) {
        history.pushState(null, '', location.pathname);
      });
      // Back/Forward: tell the server, which moves the page.
      window.addEventListener('popstate', function () {
        Shiny.setInputValue('fw_popstate', location.search, { priority: 'event' });
      });
    });
  "))
}
