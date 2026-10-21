# ui_helpers.R
# Reusable UI pieces. Each has a matching block in www/scss/_components.scss.

library(shiny)
library(htmltools)
library(bslib)

# ---- Styles ------------------------------------------------------------------

#' Compile a stylesheet with the design tokens injected
#'
#' fw_sass_variables() goes ahead of the entry file, so every `$fw-*` comes
#' from R/brand.R. Used for main.scss (the app) and records.scss (the Detailed
#' report).
#'
#' sass's cache key ignores @imported partials, so cache_key_extra adds a
#' digest of them; otherwise an edit to _components.scss compiles to the old
#' CSS.
#'
#' @param entry path to the Sass entry file
fw_compile_css = function(entry = "www/scss/main.scss") {
  as.character(sass::sass(
    list(fw_sass_variables(), sass::sass_file(entry)),
    options = sass::sass_options(output_style = "compressed"),
    cache_key_extra = fw_scss_digest(dirname(entry))
  ))
}

#' A digest of every Sass source file in a directory
fw_scss_digest = function(dir = "www/scss") {
  files = sort(list.files(dir, pattern = "[.]scss$", full.names = TRUE))
  paste(tools::md5sum(files), collapse = "-")
}

# ---- Layout ------------------------------------------------------------------

#' The standard content column
fw_container = function(...) div(class = "fw-container", ...)

#' A page section, optionally on the shoal tint or with tighter padding
fw_section = function(..., variant = c("default", "shoal"), tight = FALSE,
                       id = NULL) {
  variant = match.arg(variant)
  classes = c(
    "fw-section",
    if (variant != "default") paste0("fw-section--", variant),
    if (tight) "fw-section--tight"
  )
  tags$section(class = paste(classes, collapse = " "), id = id, ...)
}

#' Text with **bold** pairs as <strong>
fw_emphasis = function(text) {
  parts = strsplit(text, "**", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(text)
  do.call(tagList, lapply(seq_along(parts), function(i) {
    # .noWS, or the tagList's newlines render "**word**." as "word ."
    if (i %% 2 == 0) tags$strong(.noWS = "outside", parts[[i]]) else parts[[i]]
  }))
}

#' Page header: title plus a description of what the page does
#'
#' Description paragraphs all share one class, so an intro is one voice.
#'
#' @param title    one string, or several to start each on its own line
#' @param format   turns one paragraph of copy into tags; the Welcome page
#'   passes one that also makes [[page|words]] links
#' @param modifier adds .fw-page-header--{modifier} for a page-specific size
#' @param show_title FALSE keeps the h1 for screen readers only (the client
#'   wants a visible title on The solution page alone)
fw_page_header = function(title, description = NULL, format = fw_emphasis,
                           modifier = NULL, show_title = TRUE) {
  tags$header(
    class = paste(c("fw-page-header", if (!is.null(modifier)) paste0("fw-page-header--", modifier)),
                  collapse = " "),
    fw_container(
      # **bold** works in titles too. Several strings: one line each, with a
      # space between for screen readers and copy-paste.
      h1(class = paste(c("fw-page-header__title",
                         if (!show_title) "fw-visually-hidden"), collapse = " "),
         if (length(title) > 1) {
           lapply(title, function(line) {
             tagList(span(class = "fw-page-header__line", fw_emphasis(line)), " ")
           })
         } else {
           fw_emphasis(title)
         }),
      lapply(description, function(para) {
        p(class = "fw-page-header__description", format(para))
      })
    )
  )
}

# ---- KPI ---------------------------------------------------------------------

#' A single headline figure. No count-up animation.
#'
#' @param value   the figure, already formatted
#' @param label   a plain sentence-case description
#' @param tooltip optional clarification, shown through a keyboard-reachable icon
fw_kpi_stat = function(value, label, tooltip = NULL) {
  div(
    class = "fw-kpi",
    tags$span(class = "fw-kpi__value", value),
    # Own span, so a wrapping label keeps the (i) level with its first line.
    tags$span(
      class = "fw-kpi__label",
      fw_with_info(label, if (!is.null(tooltip)) fw_info(tooltip, label))
    )
  )
}

fw_kpi_strip = function(...) div(class = "fw-kpi-strip", ...)

#' Thousands separators, for figures shown to the reader
fw_fmt_num = function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(fw_t("common", "empty_value"))
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Pick the singular or the plural label for a figure
#'
#' Exactly 1 takes the singular; everything else (0 included) the plural. Both
#' forms come from the copy, since some labels don't inflect.
#'
#' @param n the figure the label sits under
#' @param one,many the two forms, both from the copy deck
fw_plural = function(n, one, many) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return(many)
  if (n == 1) one else many
}

#' 1st, 2nd, 3rd, 4th. Used in the submission confirmation.
fw_ordinal = function(n) {
  if (is.na(n)) return("")
  last_two = n %% 100
  suffix = if (last_two %in% 11:13) "th" else {
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  }
  paste0(format(n, big.mark = ",", trim = TRUE), suffix)
}

# ---- Information icon --------------------------------------------------------

#' A keyboard-accessible (i) button with a popover (not a title attribute)
#'
#' @param text  the tooltip content
#' @param label what the icon relates to, for the accessible name
fw_info = function(text, label = NULL) {
  aria = if (is.null(label)) {
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
#' The last word and the button share a nowrap span, so the (i) never wraps
#' onto a line of its own.
#'
#' @param text  the label, as a single string. Anything else (a tag) is drawn
#'   as before, text then button.
#' @param info  the fw_info() button, or NULL for none
#' @param wrap  wraps each run of text, e.g. in a <label for> (two labels for
#'   one control is valid; screen readers join them)
#' @param tail  anything after the last word, inside the wrap (the required
#'   marker)
fw_with_info = function(text, info, wrap = identity, tail = NULL) {
  if (is.null(info)) return(wrap(tagList(text, tail)))
  if (!is.character(text) || length(text) != 1) return(tagList(wrap(tagList(text, tail)), info))
  words = trimws(text)
  cut = regexpr("[[:space:]][^[:space:]]+$", words)
  head = if (cut > 0) paste0(substr(words, 1, cut), "") else ""
  last = if (cut > 0) substr(words, cut + 1, nchar(words)) else words
  tagList(
    if (nzchar(head)) wrap(head),
    tags$span(class = "fw-nowrap", wrap(tagList(last, tail)), info)
  )
}

# Initialises Bootstrap popovers, including ones added later (MutationObserver).
fw_popover_script = function() {
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
#' @param input    the shiny input, built WITHOUT its own label
#' @param label    the visible field label
#' @param required marks the field and appends a screen-reader-only "required"
#' @param tooltip  optional text for the information icon
#' @param help     optional help text shown under the control
fw_field = function(input, label, required = FALSE, tooltip = NULL,
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
            # The asterisk is decorative; this is what screen readers hear.
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

#' A numbered pager
#'
#' Buttons write into one Shiny input, so rebuilt pagers don't leave dead
#' observers. Long runs are windowed: first, ..., nearby pages, ..., last.
#'
#' @param input_id the NAMESPACED input to write the chosen page into
#' @param current  the page being shown
#' @param total    how many pages there are
#' @param window   how many pages to show either side of the current one
fw_page_numbers = function(input_id, current, total, window = 2L) {
  if (total <= 1L) return(NULL)

  wanted = unique(c(1L, seq(max(1L, current - window),
                             min(total, current + window)), total))
  wanted = sort(wanted)

  btn = function(n) {
    is_current = n == current
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

  items = list()
  previous = 0L
  for (n in wanted) {
    if (n - previous > 1L) {
      items[[length(items) + 1L]] = tags$span(
        class = "fw-pager__gap", `aria-hidden` = "true", "…"
      )
    }
    items[[length(items) + 1L]] = btn(n)
    previous = n
  }

  tags$nav(class = "fw-pager__pages", `aria-label` = fw_t("a11y", "pagination"), items)
}

# ---- Accessibility -----------------------------------------------------------

#' A polite live region, for announcing validation and status changes
fw_live_region = function(id) {
  div(
    id = id,
    class = "fw-visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  )
}

fw_skip_link = function(target = "#fw-main") {
  tags$a(class = "fw-skip-link", href = target, fw_t("a11y", "skip_link"))
}

# ---- What a crawler reads ----------------------------------------------------
#
# Firewall crawlers don't run the app, so a plain description goes into the
# first HTML (static UI only, never renderUI), or the domain gets blocked as
# "insufficient content".

#' The <head> tags that describe the site
#'
#' Meta description, canonical URL, Open Graph and Twitter tags, and
#' schema.org WebSite and Dataset data (for Google Dataset Search). All URLs
#' absolute, from FW_SITE_URL.
fw_head_meta = function() {
  title = fw_t("app", "full_title")
  desc  = fw_t("app", "meta_description")
  ld_desc = fw_t("app", "dataset_description")
  # The explicit lockup (2000px copy) as the link-preview image.
  image = paste0(FW_SITE_URL, sub("^www/", "", FW_LOGO$explicit_file))
  fwl   = list(`@type` = "Organization", name = fw_t("footer", "logo_alt_fwl"),
                url = fw_t("footer", "fwl_url"))
  ld = list(
    `@context` = "https://schema.org",
    `@graph` = list(
      list(`@type` = "WebSite", name = fw_t("app", "title"),
           alternateName = title, url = FW_SITE_URL, description = ld_desc,
           publisher = fwl),
      list(`@type` = "Dataset", name = title, description = ld_desc,
           url = FW_SITE_URL, creator = fwl,
           license = "https://creativecommons.org/licenses/by-nc/4.0/",
           isAccessibleForFree = TRUE)
    )
  )
  tagList(
    tags$meta(name = "description", content = desc),
    tags$link(rel = "canonical", href = FW_SITE_URL),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:site_name", content = fw_t("app", "title")),
    tags$meta(property = "og:title", content = title),
    tags$meta(property = "og:description", content = desc),
    tags$meta(property = "og:url", content = FW_SITE_URL),
    tags$meta(property = "og:image", content = image),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    # Which build this page came from. See FW_BUILD_ID in R/config.R.
    tags$meta(name = "fw-build", content = FW_BUILD_ID),
    tags$script(type = "application/ld+json",
                HTML(jsonlite::toJSON(ld, auto_unbox = TRUE)))
  )
}

#' What the page says with JavaScript off
#'
#' The <head> block hides the loader (no script would remove it). The body
#' block describes FWISE with plain links. No contact address.
fw_noscript_head = function() {
  # By id, so it beats .fw-loader in the later stylesheet.
  tags$noscript(tags$style(HTML("#fw-loader{display:none;}")))
}

fw_noscript = function() {
  link = function(href, label) tags$a(href = href, label)
  tags$noscript(
    fw_container(
      class = "fw-noscript",
      tags$h1(fw_t("app", "noscript_heading")),
      lapply(fw_t("app", "noscript_body"), tags$p),
      tags$p(tags$strong(fw_t("app", "noscript_needs_js"))),
      tags$ul(
        tags$li(link(fw_t("footer", "fwl_url"), fw_t("app", "noscript_link_fwl"))),
        tags$li(link(fw_t("footer", "github_url"), fw_t("app", "noscript_link_github"))),
        tags$li(link("?page=privacy", fw_t("app", "noscript_link_privacy")))
      )
    )
  )
}

# ---- Chrome ------------------------------------------------------------------

#' The navbar's logo: the FWISE-SIMPLE wordmark (client). See FW_LOGO.
fw_brand = function() {
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

#' An email address, kept out of the served page until a reader asks for it
#'
#' Every address the live app shows goes through here. The button reveals the
#' address in place, as a mailto link with a Copy button (fw_client_script(),
#' delegated, so it works in injected markup). Until then the address is
#' reversed and base64'd in one attribute, which stops naive scrapers only;
#' the real control is contact_public (private contacts never get here).
#'
#' @param email the address, in full
#' @param label the button's text
#' @param aria the button's accessible name, when the label alone does not say
#'   whose address it is
#' @param class extra classes on the wrapper, for a context that restyles it
#' @param btn_class extra classes on the button (it reads as a link by default)
#' @param contact_id a contributor's contact_id. Pressing the revealed link or
#'   Copy is then logged as contact_click with this id only (R/tracking.R).
#'   NULL for the FWISE address.
fw_email_reveal = function(email, label, aria = NULL, class = NULL,
                            btn_class = NULL, contact_id = NULL) {
  tags$span(
    class = paste(c("fw-email", class), collapse = " "),
    tags$button(
      type = "button",
      class = paste(c("fw-email__btn", btn_class), collapse = " "),
      `data-fw-email` = fw_email_encode(email),
      `aria-label` = aria,
      `data-fw-contact` = contact_id,
      label
    )
  )
}

#' The attribute fw_email_reveal() carries: the address reversed, then base64
#'
#' Reversed by character and UTF-8 encoded, so non-ASCII addresses round-trip.
fw_email_encode = function(email) {
  rev_chr = paste(rev(strsplit(enc2utf8(email), "", fixed = TRUE)[[1]]), collapse = "")
  jsonlite::base64_enc(charToRaw(rev_chr))
}

#' The footer's "Contact FWISE" button (.fw-footer__contact)
fw_footer_contact = function() {
  fw_email_reveal(
    fw_t("footer", "contact_email"),
    label = fw_t("footer", "contact_label"),
    aria = fw_t("footer", "contact_aria"),
    class = "fw-footer__contact"
  )
}

fw_footer_review_label = function(n) {
  if (isTRUE(n == 1)) fw_t("footer", "in_review_one") else fw_t("footer", "in_review_many")
}

#' The footer's "N records in review"
#'
#' Always in the page, hidden at zero, so the server can update it
#' (fw-in-review). Never says "0 records in review".
fw_footer_review = function(in_review = 0L) {
  tags$span(
    id = "fw-footer-review", class = "fw-footer__review",
    hidden = if (!isTRUE(in_review > 0)) NA,
    tags$span(class = "fw-num", fw_fmt_num(in_review)), " ",
    tags$span(class = "fw-footer__review-label", fw_footer_review_label(in_review))
  )
}

#' Send the footer a new in-review count
fw_send_review_count = function(session, n) {
  session$sendCustomMessage("fw-in-review", list(
    n = fw_fmt_num(n), label = fw_footer_review_label(n), show = isTRUE(n > 0)
  ))
}

#' The footer, on every page
#'
#' Two full-width tiers: logos and credits on the page ground (the indigo
#' wordmark can't sit on indigo), then the indigo band with the release date,
#' links and licence.
#'
#' @param last_updated the release date from metadata.json
#' @param in_review how many records are waiting on review
fw_footer = function(last_updated, in_review = 0L) {
  logo = function(href, src, alt) {
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
        # Org logos, the credit lines, a rule, then the collaborators (client).
        # Separate credit lines so the app's builder isn't read as owning the
        # database.
        div(
          class = "fw-footer__row",
          div(
            class = "fw-footer__org",
            # Links to the Welcome page, like the navbar mark.
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
            # The database first, then the app (client).
            p(class = "fw-footer__built-by", fw_emphasis(fw_t("app", "data_by"))),
            p(class = "fw-footer__built-by", fw_emphasis(fw_t("app", "built_by"))),
            p(class = "fw-footer__built-by", fw_emphasis(fw_t("app", "illustrated_by")))
          ),
          div(
            class = "fw-footer__logos",
            logo(fw_t("footer", "fwl_url"), "img/collab/FRESHWATER_LIFE.png",
                 fw_t("footer", "logo_alt_fwl")),
            logo(fw_t("footer", "ucsc_url"), "img/collab/UCSC.png",
                 fw_t("footer", "logo_alt_ucsc")),
            logo(fw_t("footer", "scripps_url"), "img/collab/UCSD_SCRIPPS.png",
                 fw_t("footer", "logo_alt_scripps")),
            logo(fw_t("footer", "unil_url"), "img/collab/UNIL-TRIM.png",
                 fw_t("footer", "logo_alt_unil")),
            logo(fw_t("footer", "issg_url"), "img/collab/ISSG_SSC_IUCN.png",
                 fw_t("footer", "logo_alt_issg")),
            logo(fw_t("footer", "vetinst_url"), "img/collab/VETINST.png",
                 fw_t("footer", "logo_alt_vetinst"))
          )
        )
      )
    ),
    div(
      class = "fw-footer__meta",
      fw_container(
        # One row of links, then the licence on its own line (client).
        div(
          class = "fw-footer__links",
          # Label in the date's typeface (.fw-footer__updated).
          tags$span(
            class = "fw-footer__updated",
            fw_t("footer", "last_updated"), " ",
            tags$span(class = "fw-num",
                      if (is.na(last_updated)) fw_t("common", "empty_value")
                      else format(last_updated, "%d %B %Y"))
          ),
          fw_footer_review(in_review),
          fw_doi_link(fw_t("footer", "doi_label")),
          tags$a(href = fw_t("footer", "github_url"), fw_t("footer", "github_label")),
          fw_footer_contact(),
          # Opens the feedback dialog; see fw_form_open().
          fw_form_open("feedback", fw_t("feedback", "open_label"),
                       source = "footer", class = "fw-link-button"),
          # The privacy page's way in (no navbar link). An actionLink, so no
          # reload; app.R selects the page.
          actionLink("fw_privacy_link", fw_t("privacy", "footer_link"))
        ),
        p(class = "fw-footer__licence", fw_t("footer", "licence")),
        fw_doi_soon()
      )
    )
  )
}

#' A link to the dataset's DOI, or a "coming soon" button until there is one
#'
#' While footer$doi_url is "#", DOI links open the card from fw_doi_soon()
#' (drawn once, in the footer). Set the real URL in R/copy.R and they become
#' plain links.
fw_doi_link = function(label) {
  url = fw_t("footer", "doi_url")
  if (!identical(url, "#") && nzchar(url)) return(tags$a(href = url, label))
  tags$button(type = "button", class = "fw-link-button",
              popovertarget = FW_DOI_SOON_ID, label)
}

FW_DOI_SOON_ID = "fw-doi-soon"

#' The "coming soon" card behind fw_doi_link(). NULL once there is a DOI.
fw_doi_soon = function() {
  url = fw_t("footer", "doi_url")
  if (!identical(url, "#") && nzchar(url)) return(NULL)
  div(
    id = FW_DOI_SOON_ID, popover = "auto", class = "fw-soon-card",
    role = "dialog", `aria-labelledby` = paste0(FW_DOI_SOON_ID, "-title"),
    tags$button(type = "button", class = "fw-soon-card__close",
                popovertarget = FW_DOI_SOON_ID, popovertargetaction = "hide",
                `aria-label` = fw_t("home", "card_close"), HTML("&times;")),
    tags$h2(id = paste0(FW_DOI_SOON_ID, "-title"), class = "fw-soon-card__title",
            fw_t("footer", "doi_soon_title")),
    p(fw_t("footer", "doi_soon"))
  )
}

#' Mark a size slider so its handle bubbles show real units
#'
#' A data attribute read by fwSizePretty in fw_client_script(): prettify must
#' be a JS function and Shiny's binding owns the slider's setup.
#'
#' @param unit "ha" or "km"; its short label follows every figure
fw_slider_prettify = function(tag, unit) {
  mark = function(x) {
    if (!inherits(x, "shiny.tag")) {
      if (is.list(x)) return(lapply(x, mark))
      return(x)
    }
    if (identical(x$name, "input")) {
      x$attribs$`data-fw-prettify` = "size"
      x$attribs$`data-fw-unit` = fw_t("filters", paste0("unit_short_", unit))
      return(x)
    }
    x$children = lapply(x$children, mark)
    x
  }
  mark(tag)
}

#' Run observer code without letting an error end the session
#'
#' An uncaught observer error disconnects the reader. This logs it and shows a
#' dismissable notice instead. req() and validate() pass through.
#'
#' @return the value of `expr`, or NULL if it failed
fw_safely = function(session, expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    message("[fw_safely] ", conditionMessage(e))
    shiny::showNotification(fw_t("common", "went_wrong"), type = "error",
                            session = session)
    NULL
  })
}

#' The loader: covers the page until the app has drawn itself
#'
#' In the markup, so it shows from the first paint. Removed when Shiny first
#' goes idle or disconnects, and gives up after FW_LOADER_TIMEOUT_S (CSS only)
#' for clients whose WebSocket never connects, such as crawlers. Hidden with
#' JavaScript off (fw_noscript_head()).
#'
#' The badge URL is passed as a CSS custom property, also used by the busy
#' spinner.
fw_loader = function() {
  tagList(
    # Feeds the loader's give-up animation in _components.scss.
    tags$style(HTML(sprintf(":root{--fw-badge-url:url('%s');--fw-loader-timeout:%ss;}",
                            FW_LOGO$badge_web, FW_LOADER_TIMEOUT_S))),
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
        // Removed once faded. The timeout covers reduced motion, where the
        // transition may not fire.
        var gone = function () { if (el.parentNode) el.parentNode.removeChild(el); };
        el.addEventListener('transitionend', gone, { once: true });
        setTimeout(gone, 600);
      });
    "))
  )
}

#' Client-side handlers shared by every page
#'
#' Message handlers (announce, nav, in-review, build check, map record,
#' scrolling), the responsive navbar, the download picker guard, the map busy
#' badge, the keepalive, size slider bubbles and the email reveal.
fw_client_script = function() {
  # Values go in by token replacement, not sprintf() (8192-character format
  # limit). The email button words come from the copy deck as JSON strings.
  js_str = function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  words = c(
    "{{EMAIL_COPY}}"       = js_str(fw_t("common", "email_copy")),
    "{{EMAIL_COPY_ARIA}}"  = js_str(fw_t("common", "email_copy_aria")),
    "{{EMAIL_COPIED}}"     = js_str(fw_t("common", "email_copied")),
    "{{EMAIL_COPIED_SAY}}" = js_str(fw_t("common", "email_copied_say"))
  )
  fill_words = function(js) {
    for (k in names(words)) js = gsub(k, words[[k]], js, fixed = TRUE)
    js
  }
  tags$script(HTML(fill_words(sub("{{KEEPALIVE_MS}}", FW_KEEPALIVE_MS, fixed = TRUE, sub("__FW_SPINNER_DELAY__", FW_SPINNER_DELAY_MS, "
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
      // The footer's in-review count (fw_footer_review()).
      Shiny.addCustomMessageHandler('fw-in-review', function (msg) {
        var el = document.getElementById('fw-footer-review');
        if (!el) return;
        el.querySelector('.fw-num').textContent = msg.n;
        el.querySelector('.fw-footer__review-label').textContent = msg.label;
        el.hidden = !msg.show;
      });
      // A page from an older deploy reloads once with ?fwv=<build> to get past
      // caches. It never reloads twice for the same build, and the marker is
      // removed from the address bar. See FW_BUILD_ID.
      (function () {
        var meta = document.querySelector('meta[name=fw-build]');
        var mine = meta ? meta.getAttribute('content') : '';
        var arrived = null;
        try {
          var here = new URL(window.location.href);
          arrived = here.searchParams.get('fwv');
          if (arrived !== null) {
            here.searchParams.delete('fwv');
            history.replaceState(history.state, '', here.toString());
          }
        } catch (e) {}
        Shiny.addCustomMessageHandler('fw-build', function (id) {
          if (!id || !mine || id === mine || id === arrived) return;
          try {
            var next = new URL(window.location.href);
            next.searchParams.set('fwv', id);
            window.location.replace(next.toString());
          } catch (e) {}
        });
      })();
      // The navbar becomes a hamburger only when its row would overflow, checked
      // before paint. An open menu is left alone. See .fw-navbar--menu.
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
      // A map record the card script asked for (see panelPending() in maps.R).
      Shiny.addCustomMessageHandler('fw-map-detail', function (msg) {
        var panel = document.getElementById('fw-map-detail');
        if (panel && panel.fwOpenDetail) panel.fwOpenDetail(msg.html);
      });
      // Disable Download while nothing is ticked. Delegated, since scripts
      // inserted into a modal don't run. The default selection is never empty.
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
        // Capture phase, so it runs before the button's onclick closes the modal.
        document.addEventListener('click', function (e) {
          if (!e.target.closest) return;
          var btn = e.target.closest('a[aria-disabled=\"true\"]');
          if (!btn || !btn.closest('.fw-download-picker')) return;
          e.preventDefault();
          e.stopPropagation();
        }, true);
      })();
      // The busy badge on maps. Proxy marker swaps never mark an output
      // .recalculating, so the global spinner can't see them; follow Shiny's
      // busy/idle events with the same delay instead.
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
      // Scroll a block into view by id (Plan, after Build).
      Shiny.addCustomMessageHandler('fw-scroll-to', function (id) {
        var el = document.getElementById(id);
        if (!el) return;
        el.scrollIntoView({ behavior: 'smooth', block: 'start' });
      });
      // Keepalive, so the proxy doesn't drop an idle socket. Paused in hidden
      // tabs.
      setInterval(function () {
        if (document.hidden || !window.Shiny || !Shiny.setInputValue) return;
        Shiny.setInputValue('fw_keepalive', Date.now());
      }, {{KEEPALIVE_MS}});
      // Scroll to the very top of the page.
      Shiny.addCustomMessageHandler('fw-scroll-top', function (_) {
        window.scrollTo({ top: 0, behavior: 'auto' });
      });
      // Size slider bubbles in real units, not log10. Applied after Shiny
      // binds the slider. Gotcha: prettify_enabled must be sent too, since
      // sliderInput(sep = '') turns prettify off. Rounding mirrors
      // fw_size_label() in R/filters.R: keep them in step.
      window.fwSizePretty = function (n) {
        // Inverse of log10(1 + x).
        var v = Math.pow(10, Number(n)) - 1;
        if (Math.abs(v) < 1e-9) return '0';
        var digits = v >= 100 ? 0 : v >= 10 ? 1 : v >= 1 ? 2 : 4;
        var parts = v.toFixed(digits).split('.');
        parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        return parts.join('.');
      };
      function fwPrettifySlider(target) {
        var $el = $(target);
        if ($el.data('fwPrettify') !== 'size') return;
        var slider = $el.data('ionRangeSlider');
        var unit = $el.data('fwUnit');
        if (slider) slider.update({
          prettify_enabled: true,
          prettify: function (n) {
            return window.fwSizePretty(n) + (unit ? ' ' + unit : '');
          }
        });
      }
      $(document).on('shiny:bound', function (e) { fwPrettifySlider(e.target); });
      // Again after updateSliderInput() (e.g. Clear), which resets prettify.
      $(document).on('shiny:updateinput', function (e) {
        var t = e.target;
        setTimeout(function () { fwPrettifySlider(t); }, 0);
      });
      // Email reveal (fw_email_reveal()). Delegated, so it works in renderUI
      // tables and the map panel. The button is replaced by a mailto link and
      // a Copy button, and focus moves to the link.
      document.addEventListener('click', function (e) {
        var btn = e.target.closest && e.target.closest('.fw-email__btn');
        if (!btn) return;
        e.preventDefault();
        var bytes = Uint8Array.from(atob(btn.getAttribute('data-fw-email')),
                                    function (c) { return c.charCodeAt(0); });
        var addr = Array.from(new TextDecoder().decode(bytes)).reverse().join('');
        var out = document.createElement('span');
        out.className = 'fw-email__out';
        var link = document.createElement('a');
        link.href = 'mail' + 'to:' + addr;
        link.textContent = addr;
        var copy = document.createElement('button');
        copy.type = 'button';
        copy.className = 'fw-email__copy';
        copy.textContent = {{EMAIL_COPY}};
        copy.setAttribute('aria-label', {{EMAIL_COPY_ARIA}}.replace('{address}', addr));
        copy.setAttribute('data-fw-copy', addr);
        out.appendChild(link);
        out.appendChild(document.createTextNode(' '));
        out.appendChild(copy);
        // Carry a contributor's id, so using the address can be logged.
        var cid = btn.getAttribute('data-fw-contact');
        if (cid) out.setAttribute('data-fw-contact', cid);
        btn.replaceWith(out);
        link.focus();
      });
      // A contributor's address used (mailto or Copy): logged by id only.
      document.addEventListener('click', function (e) {
        var hit = e.target.closest &&
          e.target.closest('.fw-email__out[data-fw-contact] a, .fw-email__out[data-fw-contact] .fw-email__copy');
        if (!hit || !window.Shiny || !Shiny.setInputValue) return;
        var cid = hit.closest('.fw-email__out').getAttribute('data-fw-contact');
        Shiny.setInputValue('fw_contact_click', cid, { priority: 'event' });
      });
      // Copy, with a textarea fallback where the clipboard API isn't allowed.
      document.addEventListener('click', function (e) {
        var btn = e.target.closest && e.target.closest('.fw-email__copy');
        if (!btn) return;
        var text = btn.getAttribute('data-fw-copy');
        var done = function () {
          btn.textContent = {{EMAIL_COPIED}};
          var live = document.getElementById('fw_announce');
          if (live) {
            live.textContent = '';
            setTimeout(function () { live.textContent = {{EMAIL_COPIED_SAY}}; }, 60);
          }
          clearTimeout(btn.fwReset);
          btn.fwReset = setTimeout(function () { btn.textContent = {{EMAIL_COPY}}; }, 2500);
        };
        var fallback = function () {
          var ta = document.createElement('textarea');
          ta.value = text;
          ta.setAttribute('readonly', '');
          ta.style.position = 'fixed';
          ta.style.opacity = '0';
          document.body.appendChild(ta);
          ta.select();
          try { document.execCommand('copy'); done(); } catch (err) {}
          ta.remove();
          btn.focus();
        };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(done, fallback);
        } else {
          fallback();
        }
      });
    });
  ", fixed = TRUE)))))
}

# ---- Shared blocks -----------------------------------------------------------

#' A titled block, its note behind an (i) beside the heading
#'
#' `note_as = "text"` shows the note as a visible line instead.
#'
#' @param tools controls drawn on the title's row, right-aligned, wrapping
#'   under the title on a narrow screen (the contacts table's page size)
#' @param figure TRUE for a chart or map, which sits on a card
fw_block = function(title, note, content, note_as = c("info", "text"),
                     tools = NULL, figure = FALSE) {
  note_as = match.arg(note_as)
  info = !is.null(note) && note_as == "info"
  heading = h3(class = if (info) "fw-block__title",
                fw_with_info(title, if (info) fw_info(note, title)))
  div(
    class = paste(c("fw-plan__block", if (figure) "fw-plan__block--figure"),
                  collapse = " "),
    if (is.null(tools)) heading else div(class = "fw-block__head", heading, tools),
    if (!is.null(note) && !info) p(class = "fw-plan__note", note),
    content
  )
}

#' A click-to-open panel: a native <details>
#'
#' The summary has a title and a one-line note saying what's inside.
#'
#' @param title the disclosure label
#' @param ... the panel body
#' @param note one line under the title saying what is inside
#' @param open whether it starts expanded
fw_disclosure = function(title, ..., note = NULL, id = NULL, open = FALSE) {
  tags$details(
    class = "fw-disclosure", id = id,
    # NA writes a bare boolean attribute; FALSE would write open="FALSE".
    open = if (isTRUE(open)) NA,
    tags$summary(
      class = "fw-disclosure__summary",
      tags$span(class = "fw-disclosure__title", title),
      if (!is.null(note)) tags$span(class = "fw-disclosure__note", note)
    ),
    div(class = "fw-disclosure__body", ...)
  )
}

#' The caveats, as shown on the About page
#'
#' From fw_caveat_blocks(), the same text every download carries.
#'
#' @param heading whether to draw the panel's own heading. FALSE where the
#'   caller has already placed one, as the About page has.
fw_caveats_ui = function(data, heading = TRUE) {
  blocks = fw_caveat_blocks(data)

  div(
    class = "fw-caveats",
    if (isTRUE(heading)) h2(class = "fw-visually-hidden", fw_t("about", "caveats_heading")),
    # One column: bold title, then paragraphs (client). See .fw-caveats__block.
    lapply(blocks, function(b) {
      title = fw_caveat_title(b$heading)
      div(
        class = "fw-caveats__block",
        # No h3 for a block with no heading.
        if (nzchar(title)) h3(title),
        lapply(b$body, function(x) p(x))
      )
    })
  )
}

# A caveat heading for the web: all-caps headings are sentence-cased; anything
# already cased is left alone (so acronyms survive). "" for no heading.
fw_caveat_title = function(x) {
  if (!length(x) || is.na(x) || !nzchar(x)) return("")
  if (!identical(x, toupper(x))) return(x)
  paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
}
