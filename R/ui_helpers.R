# ui_helpers.R
# Reusable UI pieces. Every one of these has a matching block in
# www/scss/_components.scss, so a look changes in one function and one rule.

library(shiny)
library(htmltools)
library(bslib)

# ---- Styles ------------------------------------------------------------------

#' Compile a stylesheet with the design tokens injected
#'
#' THE ONE ROUTE FROM R/brand.R TO CSS. fw_sass_variables() is placed ahead of
#' the entry file, so every `$fw-*` variable the stylesheet uses is defined
#' from R before a line of Sass is read. www/scss/_tokens.scss carries no
#' literal values for that reason: there is nothing there to fall out of step.
#'
#' Two entry files use this: www/scss/main.scss (the app, also inlined into
#' the HTML report) and www/scss/report.scss (the report's own frame and print
#' rules). Both see the same tokens.
#'
#' sass hashes its input - the token list included - into its cache key, so an
#' edit to R/brand.R is picked up on restart. cache_key_extra covers what the
#' hash cannot see: sass keys on the entry file alone and never looks at the
#' partials it @imports. Without the digest below, an edit to _components.scss
#' compiles to the previously cached CSS and appears to have done nothing.
#'
#' @param entry path to the Sass entry file
fw_compile_css <- function(entry = "www/scss/main.scss") {
  as.character(sass::sass(
    list(fw_sass_variables(), sass::sass_file(entry)),
    options = sass::sass_options(output_style = "compressed"),
    cache_key_extra = fw_scss_digest(dirname(entry))
  ))
}

#' A digest of every Sass source file in a directory
fw_scss_digest <- function(dir = "www/scss") {
  files <- sort(list.files(dir, pattern = "[.]scss$", full.names = TRUE))
  paste(tools::md5sum(files), collapse = "-")
}

# ---- Layout ------------------------------------------------------------------

#' The standard content column
fw_container <- function(...) div(class = "fw-container", ...)

#' A vertical band of content
#'
#' @param variant one of "default", "paper", "shoal", "lagoon", "indigo".
#'   "lagoon" and "indigo" are the workshop poster's two coloured bands; both
#'   are meant to be used with `bleed = TRUE` and neither may be nested inside
#'   the other. See .fw-section in _components.scss.
#' @param tight   halve the vertical padding
#' @param bleed   break out of the container to the full viewport width
fw_section <- function(..., variant = c("default", "paper", "shoal",
                                        "lagoon", "indigo"),
                       tight = FALSE, bleed = FALSE, flush = NULL, id = NULL) {
  variant <- match.arg(variant)
  classes <- c(
    "fw-section",
    if (variant != "default") paste0("fw-section--", variant),
    if (tight) "fw-section--tight",
    if (!is.null(flush)) paste0("fw-section--flush-", flush),
    if (bleed) "fw-bleed"
  )
  tags$section(class = paste(classes, collapse = " "), id = id, ...)
}

#' Render the one piece of markup the copy file is allowed to carry
#'
#' The copy deck is plain text so it stays diffable and easy to hand back to the
#' client. A couple of sentences need a single word emphasised mid-clause, and
#' chopping those strings into fragments to wrap in tags$strong() makes them
#' unreadable at the point they are written. So **this** is understood, nothing
#' else is, and every part still goes through htmltools' escaping.
fw_emphasis <- function(text) {
  parts <- strsplit(text, "**", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(text)
  do.call(tagList, lapply(seq_along(parts), function(i) {
    if (i %% 2 == 0) tags$strong(parts[[i]]) else parts[[i]]
  }))
}

#' Page header: title plus a description of what the page does
#'
#' `description` may be several paragraphs. They are emitted as siblings under
#' ONE class rather than as a mix of .fw-lead, bare <p> and .fw-caption, so a
#' page's introduction is a single voice at a single size. The report builder's
#' intro used to be split across the header and a second block below the filter
#' panel, in three different treatments; that is what this replaces.
fw_page_header <- function(title, description = NULL) {
  tags$header(
    class = "fw-page-header",
    fw_container(
      h1(class = "fw-page-header__title", title),
      lapply(description, function(para) {
        p(class = "fw-page-header__description", fw_emphasis(para))
      })
    )
  )
}

#' A filter sidebar beside a results column
#'
#' Used by the report builder and the dashboard. A CSS grid rather than
#' bslib::layout_sidebar(), whose width is set in pixels and which brings its own
#' collapse behaviour; see .fw-layout in _components.scss.
#'
#' The sidebar is a real <details>, open by default. Below 900px that lets a
#' reader fold ten controls away and get to the results in one scroll; above it
#' the marker is hidden and the panel simply sits beside the content.
#'
#' @param summary the disclosure label, shown only on narrow screens
fw_sidebar_layout <- function(sidebar, main, summary) {
  div(
    class = "fw-layout",
    tags$details(
      class = "fw-layout__sidebar", open = NA,
      tags$summary(class = "fw-layout__summary", summary),
      sidebar
    ),
    div(class = "fw-layout__main", main)
  )
}

# ---- KPI ---------------------------------------------------------------------

#' A single headline figure
#'
#' Figures appear at their value. There is deliberately no count-up animation:
#' numbers that perform their own arrival read as persuasion, which is exactly
#' what this audience needs least.
#'
#' @param value   the figure, already formatted
#' @param label   a plain sentence-case description
#' @param tooltip optional clarification, shown through a keyboard-reachable icon
fw_kpi_stat <- function(value, label, tooltip = NULL) {
  div(
    class = "fw-kpi",
    tags$span(class = "fw-kpi__value", value),
    tags$span(
      class = "fw-kpi__label",
      label,
      if (!is.null(tooltip)) fw_info(tooltip, label)
    )
  )
}

fw_kpi_strip <- function(...) div(class = "fw-kpi-strip", ...)

#' Thousands separators, for figures shown to the reader
fw_fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(fw_t("common", "empty_value"))
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' 1st, 2nd, 3rd, 4th. Used in the submission confirmation.
fw_ordinal <- function(n) {
  if (is.na(n)) return("")
  last_two <- n %% 100
  suffix <- if (last_two %in% 11:13) "th" else {
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  }
  paste0(format(n, big.mark = ",", trim = TRUE), suffix)
}

# ---- Information icon --------------------------------------------------------

#' A keyboard-accessible information icon
#'
#' A real button with a popover, not a `title` attribute. A title attribute
#' cannot be reached from the keyboard and is not announced reliably, so it is
#' not an acceptable way to carry field guidance.
#'
#' @param text  the tooltip content
#' @param label what the icon relates to, for the accessible name
fw_info <- function(text, label = NULL) {
  aria <- if (is.null(label)) {
    fw_t("common", "info_icon_label")
  } else {
    fw_fill(fw_t("a11y", "more_about"), label = label)
  }
  tags$button(
    type = "button",
    class = "fw-info-btn",
    `aria-label` = aria,
    `data-bs-toggle` = "popover",
    `data-bs-trigger` = "focus hover",
    `data-bs-placement` = "top",
    `data-bs-content` = text,
    "i"
  )
}

# Bootstrap popovers are opt-in and have to be initialised. bslib loads the
# Bootstrap bundle, so this only needs to find the triggers, including any added
# later by insertUI.
fw_popover_script <- function() {
  tags$script(HTML("
    (function () {
      function initPopovers(root) {
        if (!window.bootstrap) return;
        root.querySelectorAll('[data-bs-toggle=\"popover\"]').forEach(function (el) {
          if (!bootstrap.Popover.getInstance(el)) new bootstrap.Popover(el);
        });
      }
      document.addEventListener('DOMContentLoaded', function () {
        initPopovers(document);
        // Repeatable rows are added by insertUI after load, so watch for them.
        new MutationObserver(function (muts) {
          muts.forEach(function (m) {
            m.addedNodes.forEach(function (n) {
              if (n.nodeType === 1) initPopovers(n);
            });
          });
        }).observe(document.body, { childList: true, subtree: true });
      });
    })();
  "))
}

# ---- Form field wrapper ------------------------------------------------------

#' Wrap an input with a label, a required marker, an information icon and help
#'
#' Keeping this in one place is what makes every field on the contribute form
#' behave the same way for a keyboard and a screen reader.
#'
#' @param input    the shiny input, built WITHOUT its own label
#' @param label    the visible field label
#' @param required marks the field and appends a screen-reader-only "required"
#' @param tooltip  optional text for the information icon
#' @param help     optional help text shown under the control
fw_field <- function(input, label, required = FALSE, tooltip = NULL,
                     help = NULL, input_id = NULL) {
  div(
    class = "fw-field",
    div(
      class = "fw-field__label-row",
      tags$label(
        class = "form-label",
        `for` = input_id,
        label,
        if (required) {
          tagList(
            tags$span(class = "fw-required-mark", `aria-hidden` = "true", "*"),
            # The asterisk is decorative; this is what is actually announced, so
            # the requirement is never carried by a symbol alone.
            tags$span(class = "fw-visually-hidden", fw_t("a11y", "required"))
          )
        }
      ),
      if (!is.null(tooltip)) fw_info(tooltip, label)
    ),
    input,
    if (!is.null(help)) div(class = "fw-field__help", help)
  )
}

# ---- Stub panel --------------------------------------------------------------

#' The "in development" state shared by every unbuilt page
#'
#' One helper so the four stubs stay consistent and can be removed in one place
#' as each page is built.
fw_stub_panel <- function(extra = NULL) {
  div(
    class = "fw-stub",
    div(class = "fw-stub__badge", fw_t("stub", "badge")),
    h2(fw_t("stub", "heading")),
    p(fw_t("stub", "body")),
    div(
      class = "fw-stub__actions",
      tags$a(
        class = "btn btn-primary",
        href = "#", onclick = "Shiny.setInputValue('fw_nav_to', 'contribute', {priority:'event'}); return false;",
        fw_t("stub", "action_contribute")
      ),
      tags$a(
        class = "btn btn-outline-primary",
        href = "#", onclick = "Shiny.setInputValue('fw_nav_to', 'networking', {priority:'event'}); return false;",
        fw_t("stub", "action_networking")
      )
    ),
    extra
  )
}

# ---- Paging ------------------------------------------------------------------

# The page sizes offered on paged tables are FW_CONTACTS_PAGE_SIZES and
# FW_PLAN_PAGE_SIZES in R/config.R.

#' A numbered pager
#'
#' Plain buttons writing into ONE Shiny input rather than one actionButton per
#' page. A numbered pager is rebuilt whenever the filters change, and a fresh
#' actionButton per page per render leaves a dead observer behind every time.
#'
#' Long results are windowed: first page, an ellipsis, the pages either side of
#' the current one, an ellipsis, last page. Twelve pages of numbers is its own
#' kind of unusable.
#'
#' @param input_id the NAMESPACED input to write the chosen page into
#' @param current  the page being shown
#' @param total    how many pages there are
#' @param window   how many pages to show either side of the current one
fw_page_numbers <- function(input_id, current, total, window = 2L) {
  if (total <= 1L) return(NULL)

  wanted <- unique(c(1L, seq(max(1L, current - window),
                             min(total, current + window)), total))
  wanted <- sort(wanted)

  btn <- function(n) {
    is_current <- n == current
    tags$button(
      type = "button",
      class = paste("fw-pager__page", if (is_current) "is-current"),
      `aria-label` = fw_fill(fw_t("a11y", "page_n"), n = n),
      `aria-current` = if (is_current) "page",
      onclick = sprintf(
        "Shiny.setInputValue('%s', %d, {priority:'event'});", input_id, n
      ),
      tags$span(class = "fw-num", n)
    )
  }

  items <- list()
  previous <- 0L
  for (n in wanted) {
    if (n - previous > 1L) {
      items[[length(items) + 1L]] <- tags$span(
        class = "fw-pager__gap", `aria-hidden` = "true", "\u2026"
      )
    }
    items[[length(items) + 1L]] <- btn(n)
    previous <- n
  }

  tags$nav(class = "fw-pager__pages", `aria-label` = fw_t("a11y", "pagination"), items)
}

# ---- Accessibility -----------------------------------------------------------

#' A polite live region
#'
#' Validation errors and step changes are written here so they are announced,
#' not only shown. Without this, a screen-reader user gets no feedback when the
#' Next button refuses to advance.
fw_live_region <- function(id) {
  div(
    id = id,
    class = "fw-visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  )
}

fw_skip_link <- function(target = "#fw-main") {
  tags$a(class = "fw-skip-link", href = target, fw_t("a11y", "skip_link"))
}

# ---- Chrome ------------------------------------------------------------------

fw_brand <- function() {
  tags$a(
    class = "navbar-brand",
    href = "#",
    onclick = "Shiny.setInputValue('fw_nav_to', 'home', {priority:'event'}); return false;",
    tags$img(
      src = "img/FWISE-LOGO-ALL-6.png",
      alt = fw_t("footer", "logo_alt_fwise")
    )
  )
}

#' The footer, on every page
#'
#' Two tiers. The upper one carries both logos on the page ground: the FWISE
#' lockup's wordmark is indigo, so it cannot sit on the indigo band. The lower
#' one is that band, with the release date, the links and the licence. Each
#' tier is full width and holds its own container, so the grounds run edge to
#' edge. See .fw-footer in _components.scss.
#'
#' @param last_updated the release date from metadata.json
#' @param in_review how many records are waiting on review. Shown quietly rather
#'   than as a badge: it is a sign the database is alive and that submissions go
#'   somewhere, not a call to action. Omitted entirely when there are none, so
#'   the footer never says "0 records in review", which reads as a broken pipe
#'   rather than an empty queue.
fw_footer <- function(last_updated, in_review = 0L) {
  logo <- function(href, src, alt) {
    tags$a(
      href = href, target = "_blank", rel = "noopener noreferrer",
      tags$img(src = src, alt = alt)
    )
  }
  tags$footer(
    class = "fw-footer",
    div(
      class = "fw-footer__top",
      fw_container(
        div(
          class = "fw-footer__logos",
          logo(fw_t("footer", "fwise_url"), "img/FWISE-LOGO-ALL-6.png",
               fw_t("footer", "logo_alt_fwise")),
          logo(fw_t("footer", "wfa_url"), "img/wfa-logo-rect-dark.png",
               fw_t("footer", "logo_alt_wfa"))
        ),
        p(class = "fw-footer__built-by", fw_t("app", "built_by"))
      )
    ),
    div(
      class = "fw-footer__meta",
      fw_container(
        tags$span(
          fw_t("footer", "last_updated"), " ",
          tags$span(class = "fw-num",
                    if (is.na(last_updated)) fw_t("common", "empty_value")
                    else format(last_updated, "%d %B %Y"))
        ),
        if (isTRUE(in_review > 0)) {
          tags$span(
            class = "fw-footer__review",
            tags$span(class = "fw-num", fw_fmt_num(in_review)), " ",
            if (in_review == 1) fw_t("footer", "in_review_one")
            else fw_t("footer", "in_review_many")
          )
        },
        tags$a(href = fw_t("footer", "doi_url"), fw_t("footer", "doi_label")),
        tags$a(href = fw_t("footer", "github_url"), fw_t("footer", "github_label")),
        tags$span(fw_t("footer", "licence"))
      )
    )
  )
}

#' Client-side handlers shared by every page
#'
#' Two messages: one writes into the polite live region so validation and step
#' changes are announced, the other moves the navbar from the server, which is
#' how the stub actions and the contacts page change page.
fw_client_script <- function() {
  tags$script(HTML("
    $(function () {
      Shiny.addCustomMessageHandler('fw-announce', function (msg) {
        var el = document.getElementById('fw_announce');
        if (!el) return;
        // Clearing first makes a repeated identical message announce again.
        el.textContent = '';
        setTimeout(function () { el.textContent = msg; }, 60);
      });
      Shiny.addCustomMessageHandler('fw-nav', function (value) {
        Shiny.setInputValue('fw_nav_to', value, { priority: 'event' });
      });
      // Bring a block into view by id. The report builder uses this after a
      // build: its results now sit BELOW the questions rather than beside them,
      // so without this the reader presses Build and nothing visibly happens.
      // block:'start' rather than 'center' so the results heading lands at the
      // top of the screen and the reader starts at the beginning of the report.
      Shiny.addCustomMessageHandler('fw-scroll-to', function (id) {
        var el = document.getElementById(id);
        if (!el) return;
        el.scrollIntoView({ behavior: 'smooth', block: 'start' });
      });
      // The feedback box. The address is assembled here rather than served as
      // a mailto href, for the same scraping reason as the contacts page, and
      // the message never reaches the server at all.
      Shiny.addCustomMessageHandler('fw-mailto', function (msg) {
        var href = 'mail' + 'to:' + msg.to +
          '?subject=' + encodeURIComponent(msg.subject) +
          '&body=' + encodeURIComponent(msg.body);
        window.location.href = href;
      });
    });
  "))
}
