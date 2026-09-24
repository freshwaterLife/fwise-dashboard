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
#' Two entry files use this: www/scss/main.scss (the app) and
#' www/scss/records.scss (the attempts download, inlined into that file by
#' R/report_records.R). Both see the same tokens.
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


fw_emphasis <- function(text) {
  parts <- strsplit(text, "**", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(text)
  do.call(tagList, lapply(seq_along(parts), function(i) {
    # No whitespace around the tag, or the tagList's newlines collapse to a
    # space and "**word**." renders as "word ." - the same trap as the links.
    if (i %% 2 == 0) tags$strong(.noWS = "outside", parts[[i]]) else parts[[i]]
  }))
}

#' Page header: title plus a description of what the page does
#'
#' `description` may be several paragraphs. They are emitted as siblings under
#' ONE class rather than as a mix of .fw-lead, bare <p> and .fw-caption, so a
#' page's introduction is a single voice at a single size. The report builder's
#' intro used to be split across the header and a second block below the filter
#' panel, in three different treatments; that is what this replaces.
#'
#' @param format   turns one paragraph of copy into tags; the Welcome page
#'   passes one that also makes [[page|words]] links
#' @param modifier adds .fw-page-header--{modifier} for a page-specific size
#' @param show_title FALSE keeps the h1 for screen readers only. The client
#'   asked for the visible title on The solution page alone; every page still
#'   needs one heading at the top of its outline.
fw_page_header <- function(title, description = NULL, format = fw_emphasis,
                           modifier = NULL, show_title = TRUE) {
  tags$header(
    class = paste(c("fw-page-header", if (!is.null(modifier)) paste0("fw-page-header--", modifier)),
                  collapse = " "),
    fw_container(
      # **bold** works in a title as it does in the description. A title with
      # no ** comes back from fw_emphasis() as the same plain string.
      h1(class = paste(c("fw-page-header__title",
                         if (!show_title) "fw-visually-hidden"), collapse = " "),
         fw_emphasis(title)),
      lapply(description, function(para) {
        p(class = "fw-page-header__description", format(para))
      })
    )
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
    # The label text in its own span so a label that wraps keeps the (i) to
    # its right, level with the first line, rather than dropping it under the
    # last word (client, 21 Sept 2026). See .fw-kpi__label.
    tags$span(
      class = "fw-kpi__label",
      fw_with_info(label, if (!is.null(tooltip)) fw_info(tooltip, label))
    )
  )
}

fw_kpi_strip <- function(...) div(class = "fw-kpi-strip", ...)

#' Thousands separators, for figures shown to the reader
fw_fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(fw_t("common", "empty_value"))
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Pick the singular or the plural label for a figure
#'
#' A selection of one country was labelled "countries" (client, 23 Sept 2026).
#' Exactly one takes the singular; everything else, zero included, takes the
#' plural - "0 countries" is right and "0 country" is not.
#'
#' The caller supplies both words rather than this guessing from the plural,
#' because several of the labels it is used on do not inflect at all ("invasive
#' species", "species protected") and a rule that strips an "s" would break
#' them.
#'
#' @param n the figure the label sits under
#' @param one,many the two forms, both from the copy deck
fw_plural <- function(n, one, many) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return(many)
  if (n == 1) one else many
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

#' Text with an (i) that stays beside its last word
#'
#' EVERY (i) SITS BESIDE ITS TEXT (client, 21 Sept 2026). An inline button
#' after a label can wrap onto a line of its own when the label fills its line
#' exactly - five filters across Explore did it at most laptop widths - and a
#' word joiner does not stop it. So the last word and the button share a
#' nowrap span and break as one.
#'
#' @param text  the label, as a single string. Anything else (a tag) is drawn
#'   as before, text then button.
#' @param info  the fw_info() button, or NULL for none
#' @param wrap  wraps each run of text, for a <label for> that has to hold it
#'   (a field label is split into two labels for the same control, which is
#'   valid HTML, and a screen reader joins them into one name)
#' @param tail  anything that belongs after the last word, inside the wrap
#'   (the required marker)
fw_with_info <- function(text, info, wrap = identity, tail = NULL) {
  if (is.null(info)) return(wrap(tagList(text, tail)))
  if (!is.character(text) || length(text) != 1) return(tagList(wrap(tagList(text, tail)), info))
  words <- trimws(text)
  cut <- regexpr("[[:space:]][^[:space:]]+$", words)
  head <- if (cut > 0) paste0(substr(words, 1, cut), "") else ""
  last <- if (cut > 0) substr(words, cut + 1, nchar(words)) else words
  tagList(
    if (nzchar(head)) wrap(head),
    tags$span(class = "fw-nowrap", wrap(tagList(last, tail)), info)
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
      fw_with_info(
        label,
        if (!is.null(tooltip)) fw_info(tooltip, label),
        wrap = function(x) tags$label(class = "form-label", `for` = input_id, x),
        tail = if (required) {
          tagList(
            tags$span(class = "fw-required-mark", `aria-hidden` = "true", "*"),
            # The asterisk is decorative; this is what is actually announced, so
            # the requirement is never carried by a symbol alone.
            tags$span(class = "fw-visually-hidden", fw_t("a11y", "required"))
          )
        }
      )
    ),
    input,
    if (!is.null(help)) div(class = "fw-field__help", help)
  )
}

# ---- Paging ------------------------------------------------------------------

# The page sizes offered on paged tables are FW_CONTACTS_PAGE_SIZES (the
# Networking directory) and FW_PLAN_CONTACTS_PAGE_SIZES (the report builder's
# contacts block, which opens at ten), both in R/config.R.

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

#' The navbar's logo
#'
#' THE FWISE-SIMPLE WORDMARK, at the client's request. It was the badge, which
#' replaced the full lockup because the lockup's subtext was unreadable at bar
#' height; SIMPLE is the lockup without the subtext, so it keeps the word FWISE
#' - which the badge alone did not carry - at a size that can still be read.
#' The badge moved to the loader and the busy spinner. See FW_LOGO in config.R
#' for the files and why they are copies.
fw_brand <- function() {
  tags$a(
    class = "navbar-brand",
    href = "#",
    onclick = "Shiny.setInputValue('fw_nav_to', 'home', {priority:'event'}); return false;",
    tags$img(
      src = FW_LOGO$mark_web,
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
#' The footer's "Contact FWISE" button
#'
#' THE ADDRESS IS NOT IN THE SERVED MARKUP. It is split into a local part and a
#' domain on two data attributes and joined in JavaScript when the button is
#' pressed, which is the same speed bump fw_contact_action() applies to every
#' address in the Networking directory. It is not security - anyone who runs or
#' reads the page's JavaScript recovers it - it just means a scraper reading the
#' HTML does not harvest it in one pass.
#'
#' PRESSING IT REVEALS THE ADDRESS RATHER THAN JUMPING STRAIGHT TO A MAIL
#' CLIENT. A reader on a machine with no mail client configured gets nothing at
#' all from a bare mailto:, so the assembled address is written into the page as
#' a real mailto link they can read, copy, or follow.
fw_footer_contact <- function() {
  div(
    class = "fw-footer__contact",
    tags$button(
      type = "button",
      class = "fw-footer__contact-btn",
      `data-u` = fw_t("footer", "contact_user"),
      `data-d` = fw_t("footer", "contact_domain"),
      `aria-label` = fw_t("footer", "contact_aria"),
      `aria-controls` = "fw-footer-contact-out",
      onclick = paste0(
        "var a=this.dataset.u+String.fromCharCode(64)+this.dataset.d,",
        "o=document.getElementById('fw-footer-contact-out');",
        "o.innerHTML='';",
        "var l=document.createElement('a');",
        "l.href='mail'+'to:'+a; l.textContent=a;",
        "o.appendChild(l); this.hidden=true; l.focus(); return false;"
      ),
      fw_t("footer", "contact_label")
    ),
    # Filled by the button above. aria-live so the revealed address is
    # announced rather than appearing silently.
    tags$span(
      id = "fw-footer-contact-out",
      class = "fw-footer__contact-out",
      `aria-live` = "polite"
    )
  )
}

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
        # ---- Who built what, and whose logos those are ----------------------
        #
        # ORG LOGOS, THEN THE WORDS, THEN A RULE, THEN THE COLLABORATORS, in
        # that order at the client's request. The FWISE and Weird Fishes marks
        # lead the row with the credit beside them, and the collaborating
        # organisations sit in their own group on the right behind the rule.
        #
        # TWO STATEMENTS, NOT ONE. This was a single line reading "Built by
        # Weird Fishes Advisory" under both logos, which - sitting under the
        # FWISE mark - could be read as claiming the database as well as the
        # app. It does not: Weird Fishes Advisory built this tool, and the
        # database is Freshwater Life's and its contributors'. Two sentences
        # rather than one, so neither can be read into the other.
        div(
          class = "fw-footer__row",
          div(
            class = "fw-footer__org",
            # The long FWISE-SIMPLE wordmark, at the client's request, and
            # linked to this app's Welcome page as the navbar's mark is - not
            # out to another site, so it opens in the same tab.
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('fw_nav_to', 'home', {priority:'event'}); window.scrollTo(0, 0); return false;",
              tags$img(src = FW_LOGO$mark_web, alt = fw_t("footer", "logo_alt_fwise"))
            ),
            logo(fw_t("footer", "wfa_url"), "img/wfa-logo-rect-dark-320.png",
                 fw_t("footer", "logo_alt_wfa"))
          ),
          div(
            class = "fw-footer__credits",
            # FWISE FIRST, WEIRD FISHES UNDERNEATH (client, 23 Sept 2026). The
            # database is the thing being credited; the tool that draws it is
            # the second sentence, not the first.
            p(class = "fw-footer__built-by", fw_t("app", "data_by")),
            p(class = "fw-footer__built-by", fw_t("app", "built_by")),
            p(class = "fw-footer__built-by", fw_t("app", "illustrated_by")),
            fw_footer_contact()
          ),
          div(
            class = "fw-footer__logos",
            logo(fw_t("footer", "fwl_url"), "img/collab/FRESHWATER_LIFE.png",
                 fw_t("footer", "logo_alt_fwl")),
            logo(fw_t("footer", "ucsc_url"), "img/collab/UCSC.png",
                 fw_t("footer", "logo_alt_ucsc")),
            logo(fw_t("footer", "scripps_url"), "img/collab/UCSD_SCRIPPS.png",
                 fw_t("footer", "logo_alt_scripps")),
            logo(fw_t("footer", "issg_url"), "img/collab/ISSG_SSC_IUCN.png",
                 fw_t("footer", "logo_alt_issg"))
          )
        )
      )
    ),
    div(
      class = "fw-footer__meta",
      fw_container(
        # The label takes the date's typeface, so the line does not change
        # font halfway through. See .fw-footer__updated.
        tags$span(
          class = "fw-footer__updated",
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

#' Mark a sliderInput so the client prints its handle bubbles in real units
#'
#' The log-scaled size sliders only. A data attribute rather than an option,
#' because Shiny's slider binding owns ionRangeSlider's initialisation and
#' prettify has to be a function - see fwSizePretty in fw_client_script().
#'
#' Walks to the <input> rather than assuming a position in the tag tree, so a
#' change to how Shiny wraps its sliders cannot quietly stop this working.
#'
#' @param unit the stored unit code ("ha" or "km"); its short label is printed
#'   after every figure on the handles and the ends, so a reader dragging the
#'   slider sees "0.03 ha" rather than a bare number (Sept 2026 user testing).
fw_slider_prettify <- function(tag, unit) {
  mark <- function(x) {
    if (!inherits(x, "shiny.tag")) {
      if (is.list(x)) return(lapply(x, mark))
      return(x)
    }
    if (identical(x$name, "input")) {
      x$attribs$`data-fw-prettify` <- "size"
      x$attribs$`data-fw-unit` <- fw_t("filters", paste0("unit_short_", unit))
      return(x)
    }
    x$children <- lapply(x$children, mark)
    x
  }
  mark(tag)
}

#' Client-side handlers shared by every page
#'
#' The loader: the whole page, until the app has drawn itself
#'
#' The badge over an indeterminate bar, on the page ground, covering everything
#' until Shiny first goes idle - which is the point the first page's charts and
#' map have been sent. Before this a visitor on a cold start saw the navbar and
#' a set of empty boxes for several seconds, which read as a broken page rather
#' than a loading one.
#'
#' IN THE MARKUP, NOT ADDED BY SCRIPT, so it is on screen from the first paint
#' rather than from whenever a script gets to run.
#'
#' IT ALSO GOES ON A DISCONNECT. A server that fails at startup never goes idle,
#' and a loader waiting for it would cover Shiny's own "disconnected" message
#' for ever. There is deliberately no timeout: a cold start on Connect Cloud
#' can legitimately take longer than any number worth picking.
#'
#' The badge's URL goes in as a custom property from here, so the stylesheet's
#' busy spinner (see the .recalculating rule in _components.scss) draws the
#' same file FW_LOGO names rather than a second copy of the path.
fw_loader <- function() {
  tagList(
    tags$style(HTML(sprintf(":root{--fw-badge-url:url('%s');}",
                            FW_LOGO$badge_web))),
    div(
      id = "fw-loader", class = "fw-loader",
      role = "status", `aria-live` = "polite",
      tags$img(class = "fw-loader__badge", src = FW_LOGO$badge_web, alt = ""),
      div(class = "fw-loader__bar", div(class = "fw-loader__fill")),
      span(class = "visually-hidden", fw_t("app", "loading"))
    ),
    tags$script(HTML("
      $(document).one('shiny:idle shiny:disconnected', function () {
        var el = document.getElementById('fw-loader');
        if (!el) return;
        el.classList.add('fw-loader--done');
        // Removed once faded, so an invisible full-screen layer is never left
        // sitting over the page. The timeout covers reduced motion, where the
        // transition is too short to fire an event reliably.
        var gone = function () { if (el.parentNode) el.parentNode.removeChild(el); };
        el.addEventListener('transitionend', gone, { once: true });
        setTimeout(gone, 600);
      });
    "))
  )
}

#' Two messages: one writes into the polite live region so validation and step
#' changes are announced, the other moves the navbar from the server, which is
#' how the stub actions and the contacts page change page.
fw_client_script <- function() {
  # A TOKEN AND sub(), NOT sprintf(). sprintf() caps a format string at 8192
  # characters and this script is longer than that, so threading a value
  # through it fails at load with a message about format length.
  tags$script(HTML(sub("__FW_SPINNER_DELAY__", FW_SPINNER_DELAY_MS, "
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
      // THE NAVBAR BECOMES A HAMBURGER WHEN IT WOULD WRAP, not at a fixed
      // width. The bar is pinned expanded (.navbar-expand), then checked: with
      // the menu class off and labels unbreakable, a row that does not fit
      // overflows its container, and the menu class goes on. Both steps run
      // before the browser paints, so nothing flickers. An open menu is left
      // alone until it closes. See .fw-navbar--menu in _components.scss.
      (function () {
        var nav = document.querySelector('.navbar.fw-navbar');
        if (!nav) return;
        var box = nav.querySelector('.container-fluid');
        nav.classList.add('navbar-expand');
        var fit = function () {
          if (nav.querySelector('.navbar-collapse.show, .navbar-collapse.collapsing')) return;
          nav.classList.remove('fw-navbar--menu');
          var over = box.scrollWidth > box.clientWidth + 1;
          nav.classList.toggle('fw-navbar--menu', over);
        };
        fit();
        if (window.ResizeObserver) new ResizeObserver(fit).observe(nav);
        else window.addEventListener('resize', fit);
        if (document.fonts && document.fonts.ready) document.fonts.ready.then(fit);
        window.addEventListener('load', fit);
        $(nav).on('hidden.bs.collapse', fit);
      })();
      // A map record, fetched on click. The card script (R/maps.R) asks for
      // it with an attempt id; this is the answer arriving. The panel exists
      // by then, because only a click on a drawn marker can have asked, and it
      // is already open on a spinning badge - see panelPending() in R/maps.R.
      Shiny.addCustomMessageHandler('fw-map-detail', function (msg) {
        var panel = document.getElementById('fw-map-detail');
        if (panel && panel.fwOpenDetail) panel.fwOpenDetail(msg.html);
      });
      // NOTHING TICKED, NOTHING TO DOWNLOAD (client, 24 Sept 2026).
      //
      // The methods-and-caveats text that used to travel with every download
      // is gone, so an empty picker has no file to name. fw_bundle_parts() has
      // a floor for a request that arrives empty anyway, but the button should
      // say no first.
      //
      // DELEGATED FROM THE DOCUMENT, not shipped inside the picker. The picker
      // is built into a modalDialog, and a <script> arriving with dynamic
      // content is exactly what fw_popover_script() below exists to avoid
      // depending on. Nothing here needs to run at open time either: the
      // default selection is never empty, and no aria-disabled attribute means
      // enabled, so a freshly opened picker is already in the right state.
      (function () {
        var btnOf = function (node) {
          var box = node.closest && node.closest('.fw-download-picker');
          return box ? box.querySelector('a[download], a.btn') : null;
        };
        document.addEventListener('change', function (e) {
          if (!e.target.closest) return;
          var box = e.target.closest('.fw-download-picker');
          if (!box) return;
          var btn = btnOf(e.target);
          if (!btn) return;
          var none = box.querySelectorAll('input[type=checkbox]:checked').length === 0;
          btn.setAttribute('aria-disabled', none ? 'true' : 'false');
          if (none) btn.setAttribute('tabindex', '-1');
          else btn.removeAttribute('tabindex');
        });
        // IN THE CAPTURE PHASE. The button carries an onclick attribute that
        // closes the modal, and a bubble-phase listener would run after it -
        // so a click on a disabled button would shut the picker and download
        // nothing. Capturing at the document stops both.
        document.addEventListener('click', function (e) {
          if (!e.target.closest) return;
          var btn = e.target.closest('a[aria-disabled=\"true\"]');
          if (!btn || !btn.closest('.fw-download-picker')) return;
          e.preventDefault();
          e.stopPropagation();
        }, true);
      })();
      // THE BADGE ON A MAP THAT IS REDRAWING ITS MARKERS.
      //
      // Both maps are drawn once and then have their markers swapped through
      // leaflet::leafletProxy(), so Shiny never marks the output
      // .recalculating and the global busy spinner - which is what covers
      // every other chart and table on the site - never fires for them. The
      // old markers just sit there until the new ones appear, which on a wide
      // selection is several seconds of a map that looks finished and is wrong.
      //
      // So the overlay is hung on Shiny's own busy signal instead, with the
      // same delay the spinners use so a quick cycle never flashes one. It
      // cannot be driven from the server: an observer that sent busy and then
      // idle around the redraw would have both messages flushed together at
      // the end of the reactive cycle, and nothing would appear.
      //
      // The cost is that a map on screen also wears the badge while unrelated
      // server work runs. That reads correctly - something IS loading - and it
      // is what the global indicator does for every other output.
      (function () {
        var timer = null;
        var maps = function () { return document.querySelectorAll('.fw-map'); };
        var mark = function (on) {
          maps().forEach(function (m) { m.classList.toggle('fw-map--busy', on); });
        };
        $(document).on('shiny:busy', function () {
          if (timer) return;
          timer = setTimeout(function () { timer = null; mark(true); }, __FW_SPINNER_DELAY__);
        });
        $(document).on('shiny:idle shiny:disconnected', function () {
          if (timer) { clearTimeout(timer); timer = null; }
          mark(false);
        });
      })();
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
      // Collapse a <details> from the server. The report builder folds its
      // filter panel away once a report has been built, so the results are not
      // pushed below a screen of controls the reader has finished with.
      //
      // A CLASS TOGGLE RATHER THAN A RE-RENDER, and that is the whole point:
      // re-rendering the panel to close it would rebuild every selectize inside
      // it at its default and throw away the selection the reader just built
      // the report from. The panel is rendered once and only its open state
      // changes. Expanding again is the browser's own job.
      Shiny.addCustomMessageHandler('fw-collapse', function (id) {
        var el = document.getElementById(id);
        if (el) el.open = false;
      });
      // The feedback box. The address is assembled here rather than served as
      // a mailto href, for the same scraping reason as the contacts page, and
      // the message never reaches the server at all.
      // THE SIZE SLIDERS' TOOLTIPS, IN REAL UNITS. Their positions are log10 -
      // hectares run from 0.0014 to 237,500, so a linear slider puts every
      // usable value in the first pixel - and the one thing the reader must
      // never be shown is the logarithm. The readout under the slider was
      // already in real units; the handle's own bubble was not, and read
      // \"0.5 to 3.4\" for a range of 3 ha to 2,500 ha.
      //
      // ionRangeSlider takes a prettify function, but only through JavaScript:
      // a data attribute can only carry a string, and Shiny's slider binding
      // owns the initialisation. So the instance is updated once it is bound.
      // shiny:bound fires after the binding's initialize(), which is where the
      // slider is created, so the instance is always there by now.
      //
      // prettify_enabled HAS TO BE SENT TOO. sliderInput(sep = \"\") writes
      // data-prettify-enabled=false, and ionRangeSlider then ignores the
      // function altogether - which is how testers came to see -2.9 on the
      // hectare slider in September 2026.
      //
      // THE ROUNDING MIRRORS fw_size_label() IN R/filters.R. Two copies of one
      // rule, which is a cost; the alternative is a server round-trip on every
      // pixel of a drag. If the thresholds there change, change them here.
      window.fwSizePretty = function (n) {
        var v = Math.pow(10, Number(n));
        var digits = v >= 100 ? 0 : v >= 10 ? 1 : v >= 1 ? 2 : 4;
        var parts = v.toFixed(digits).split('.');
        parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        return parts.join('.');
      };
      $(document).on('shiny:bound', function (e) {
        var $el = $(e.target);
        if ($el.data('fwPrettify') !== 'size') return;
        var slider = $el.data('ionRangeSlider');
        var unit = $el.data('fwUnit');
        if (slider) slider.update({
          prettify_enabled: true,
          prettify: function (n) {
            return window.fwSizePretty(n) + (unit ? ' ' + unit : '');
          }
        });
      });
      Shiny.addCustomMessageHandler('fw-mailto', function (msg) {
        var href = 'mail' + 'to:' + msg.to +
          '?subject=' + encodeURIComponent(msg.subject) +
          '&body=' + encodeURIComponent(msg.body);
        window.location.href = href;
      });
    });
  ", fixed = TRUE)))
}

# ---- Shared blocks -----------------------------------------------------------

#' A titled block, its qualification behind an (i) beside the heading
#'
#' THE NOTE IS A POPUP NOW, at the client's request (21 Sept 2026): every chart
#' and map note sits in an (i) next to its title rather than as a line of text
#' under it. `note_as = "text"` keeps the old visible line.
#'
#' NOTHING ASKS FOR "text" ANY MORE. The contacts table was the one exception -
#' its note says the people listed are not expecting to be contacted, which was
#' judged too important to hide behind an icon - and the client moved it into
#' the (i) with the rest on 23 Sept 2026. The branch is kept because the reason
#' for it has not gone away: if a note ever has to be read rather than asked
#' for, this is where it goes.
#'
#' Was fw_plan_block() in mod_plan.R. It lives here because the dashboard draws
#' summary graphics of its own now, and two pages laying out a titled block in
#' two different ways is how they drifted apart the first time.
fw_block <- function(title, note, content, note_as = c("info", "text")) {
  note_as <- match.arg(note_as)
  info <- !is.null(note) && note_as == "info"
  div(
    class = "fw-plan__block",
    h3(class = if (info) "fw-block__title",
       fw_with_info(title, if (info) fw_info(note, title))),
    if (!is.null(note) && !info) p(class = "fw-plan__note", note),
    content
  )
}

#' A click-to-open panel
#'
#' A real <details>, not a scripted accordion. That is the same choice the
#' report builder's filter panel already makes: the
#' open and closed states, the keyboard behaviour and what a screen reader
#' announces all come from the browser, and there is nothing to initialise after
#' an insertUI. The server can still close one through the `fw-collapse` message
#' handler in fw_client_script() if it ever needs to.
#'
#' THE SUMMARY CARRIES TWO LINES, and both matter. A reader decides whether to
#' open a panel from the title and the note alone, so a note that only restates
#' the title is a panel nobody opens - or worse, one everybody opens to find out
#' what it was.
#'
#' @param title the disclosure label
#' @param ... the panel body
#' @param note one line under the title saying what is inside
#' @param open whether it starts expanded
fw_disclosure <- function(title, ..., note = NULL, id = NULL, open = FALSE) {
  tags$details(
    class = "fw-disclosure", id = id,
    # NA is how htmltools writes a bare boolean attribute. FALSE would write
    # open="FALSE", which a browser reads as open.
    open = if (isTRUE(open)) NA,
    tags$summary(
      class = "fw-disclosure__summary",
      tags$span(class = "fw-disclosure__title", title),
      if (!is.null(note)) tags$span(class = "fw-disclosure__note", note)
    ),
    div(class = "fw-disclosure__body", ...)
  )
}

#' The caveats panel
#'
#' ON THE ABOUT PAGE NOW, not on the report builder. They are properties of the
#' whole database rather than of any one selection, and sitting under a result
#' the reader had just built they read as qualifications of that selection
#' alone. Moving them does NOT take them out of the downloads: the same
#' fw_caveats() vector is still a sheet in the workbook, a block in the HTML
#' report and the second half of the methods-and-caveats text file, because a
#' file that turns up in an inbox six months later has to carry its own
#' qualifications. See the header of R/export.R.
#'
#' THEY FOLD NOW. This panel was deliberately always visible - "the reader who
#' would collapse it is the reader who needs it" - and the client has since
#' asked for it to be one of the About page's click-to-open panels, alongside
#' the methods and the related databases. The earlier reasoning was not wrong
#' about who needs the caveats; what changed is that the page they sit on is now
#' a summary with depth behind it rather than a run of prose, so a caveats block
#' left permanently open is the only thing on it that cannot be folded. The
#' summary line names what is inside (how success is defined, what is missing,
#' why there is no success rate) rather than saying "caveats", which is what
#' stops it reading as small print to skip. In the downloads they are still not
#' collapsible, because a spreadsheet has nowhere to hide them.
#'
#' @param heading whether to draw the panel's own heading. FALSE where the
#'   caller has already placed one, as the About page has.
fw_caveats_ui <- function(data, heading = TRUE) {
  # Parsed by fw_caveat_blocks() next to fw_caveats(), so the panel never has to
  # know how many blocks there are. Add or remove one there and this reflows.
  blocks <- fw_caveat_blocks(data)

  div(
    class = "fw-caveats",
    if (isTRUE(heading)) h2(class = "fw-visually-hidden", fw_t("about", "caveats_heading")),
    # The blocks sit in their own grid wrapper rather than directly in the
    # panel, so the heading above stays full width and only the blocks column
    # up. See .fw-caveats__grid.
    div(
      class = "fw-caveats__grid",
      lapply(blocks, function(b) {
        title <- fw_caveat_title(b$heading)
        div(
          class = "fw-caveats__block",
          # A BLOCK MAY HAVE NO HEADING and then gets no h3, rather than an
          # empty one holding open a line. The placeholder standing in for the
          # client's caveats is one - see [PLACEHOLDER] in R/copy_export.R.
          if (nzchar(title)) h3(title),
          lapply(b$body, function(x) p(x))
        )
      })
    )
  )
}

# The export sheet wants shouting headings; a web page does not. A block with
# no heading answers "", which every caller tests before drawing anything.
#
# ONLY AN ALL-CAPS HEADING IS CASED DOWN. This used to lowercase everything
# after the first letter unconditionally, which was invisible while every
# heading in the copy deck was already a sentence - and then turned "How FWISE
# was compiled" into "How fwise was compiled" the moment a heading carried an
# acronym. A heading that is already cased is left exactly as it was written.
fw_caveat_title <- function(x) {
  if (!length(x) || is.na(x) || !nzchar(x)) return("")
  if (!identical(x, toupper(x))) return(x)
  paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
}
