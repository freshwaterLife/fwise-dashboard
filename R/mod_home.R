# mod_home.R
# The Welcome page ("The solution"): why freshwater eradication matters,
# that it works, where it has and has not been done, and where to go next.
#
# Sized to fit a 1366x768 window with little or no scroll. Re-measure before
# adding anything below the map.

mod_home_ui = function(id, stats) {
  ns = NS(id)
  keys = names(fw_t("home", "stories"))
  # Numbers are filled before ** becomes <strong>, so each is bold.
  sentence = fw_fill(fw_t("home", "kpi_sentence"),
                      attempts  = fw_fmt_num(stats$successful),
                      protected = fw_fmt_num(stats$protected))

  tagList(
    fw_page_header(fw_t("home", "title"), fw_t("home", "lead"),
                   format = fw_home_links, modifier = "home"),
    tags$main(
      id = "fw-main",
      fw_section(
        tight = TRUE,
        fw_container(
          p(class = "fw-home-kpi", fw_emphasis(sentence)),
          div(
            class = "fw-home-layout",
            div(class = "fw-home-species-grid",
                lapply(FW_HOME_ORDER, function(key) {
                  fw_home_tile(key, fw_t("home", "stories", key), FW_HOME_IMG$stories[[key]])
                })),
            fw_home_compare(ns("map_reveal"))
          ),
          fw_home_footnote()
        )
      ),
      # The card uses the lettered plate; tiles use the plain drawing.
      lapply(keys, function(key) {
        fw_home_card(key, fw_t("home", "stories", key),
                     FW_HOME_IMG$stories_named[[key]])
      })
    )
  )
}

mod_home_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing reactive: figures are filled in mod_home_ui() at startup.
  })
}

# ---- Pieces ------------------------------------------------------------------

fw_home_card_id = function(key) paste0("fw-home-story-", key)

#' The evidence footnote, the last thing on the page above the footer
#'
#' Raw HTML from our own copy (it has links). Full width, at $fw-size-fine.
fw_home_footnote = function() {
  p(class = "fw-home-footnote", HTML(fw_t("home", "footnote")))
}

#' A picture, or the placeholder box when the client has not supplied one
fw_home_art = function(src, alt) {
  if (is.null(src) || is.na(src)) {
    div(class = "fw-story__placeholder", role = "img", `aria-label` = alt,
        fw_t("home", "image_placeholder"))
  } else {
    # Lazy, so plates in closed popovers aren't fetched until opened.
    tags$img(src = src, alt = alt, loading = "lazy", decoding = "async")
  }
}

#' One beneficiary in the strip: a button that opens its story card
fw_home_tile = function(key, s, img) {
  sp = s$beneficiary
  tags$button(
    type = "button",
    class = "fw-home-species",
    popovertarget = fw_home_card_id(key),
    title = sp$name,
    `aria-label` = fw_fill(fw_t("home", "stories_open"),
                           continent = s$continent, name = sp$name),
    # The button's label says it all, so the picture is not announced twice.
    tagAppendAttributes(fw_home_art(img, ""), `aria-hidden` = "true")
  )
}

#' One success story, as a popover card: the beneficiary beside the account
#'
#' Picture 2/5, text 3/5; only the text scrolls. Narrow screens stack and
#' the whole card scrolls (.fw-home-card__body).
fw_home_card = function(key, s, img) {
  sp = s$beneficiary
  title_id = paste0(fw_home_card_id(key), "-title")

  div(
    id = fw_home_card_id(key), popover = "auto",
    class = "fw-home-card", role = "dialog", `aria-labelledby` = title_id,
    tags$button(
      type = "button", class = "fw-home-card__close",
      popovertarget = fw_home_card_id(key), popovertargetaction = "hide",
      `aria-label` = fw_t("home", "card_close"),
      HTML("&times;")
    ),
    div(
      class = "fw-home-card__body",
      # No caption: the plate is lettered. The name is in the alt text.
      tags$figure(
        class = "fw-story__figure",
        div(class = "fw-story__art", fw_home_art(img, sp$alt))
      ),
      div(
        class = "fw-home-card__text",
        # Focusable, so keyboard users can scroll it.
        tabindex = "0",
        tags$h2(
          id = title_id, class = "fw-home-card__title",
          tags$span(class = "fw-story__continent", s$continent), " ", s$title
        ),
        p(class = "fw-home-card__summary", s$summary),
        lapply(s$body, function(para) p(fw_home_story_text(para))),
        fw_home_story_links(s$links)
      )
    )
  )
}

#' A story paragraph, with any [words](https://...) made a link
#'
#' Only absolute http(s) links; everything else is escaped text.
fw_home_story_text = function(text) {
  pattern = "\\[([^]]+)\\]\\((https?://[^) ]+)\\)"
  hits = gregexpr(pattern, text)
  links = regmatches(text, hits)[[1]]
  if (!length(links)) return(text)
  plain = regmatches(text, hits, invert = TRUE)[[1]]
  parts = lapply(regmatches(links, regexec(pattern, links)), function(m) {
    tags$a(.noWS = "outside", href = m[3], target = "_blank",
           rel = "noopener noreferrer", m[2])
  })
  out = vector("list", 2L * length(parts) + 1L)
  out[seq(1L, length(out), by = 2L)] = plain
  out[seq(2L, length(out), by = 2L)] = parts
  do.call(tagList, out)
}

#' The links under a story, each with the line saying what is behind it
#'
#' Styled like fw_about_related() (.fw-linklist).
fw_home_story_links = function(links) {
  if (!length(links)) return(NULL)
  tagList(
    tags$h3(class = "fw-home-card__links-title", fw_t("home", "story_links_heading")),
    tags$ul(
      class = "fw-linklist",
      lapply(links, function(it) {
        tags$li(
          tags$a(href = it$url, target = "_blank", rel = "noopener noreferrer",
                 it$name),
          if (!is.null(it$note)) tags$span(class = "fw-linklist__note", it$note)
        )
      })
    )
  )
}

#' The before/after map reveal
#' --fw-pos is how much of the priorities map shows, from the left. The input
#' writes it on every move; the stylesheet does the rest.
#'
#' Starts in the middle and sways (client) so readers see it moves; stops on
#' touch, and doesn't move under reduced motion. See fw_home_compare_script().
fw_home_compare = function(input_id) {
  tags$figure(
    class = "fw-compare-wrap",
    div(
      class = "fw-compare", style = sprintf("--fw-pos: %d%%;", FW_HOME_COMPARE$start),
      tags$img(class = "fw-compare__base", src = FW_HOME_IMG$map_now,
               alt = "", `aria-hidden` = "true"),
      div(class = "fw-compare__top",
          tags$img(src = FW_HOME_IMG$map_next, alt = "", `aria-hidden` = "true")),
      div(class = "fw-compare__handle", `aria-hidden` = "true"),
      # Not a Shiny input. Value is a % of the width.
      tags$input(
        type = "range", id = input_id, class = "fw-compare__range",
        min = 0, max = 100, step = 1, value = FW_HOME_COMPARE$start,
        `aria-label` = fw_t("home", "map_slider_label"),
        oninput = "this.parentNode.style.setProperty('--fw-pos', this.value + '%')"
      )
    ),
    tags$figcaption(class = "fw-compare__caption", fw_home_map_line()),
    fw_home_compare_script(input_id)
  )
}

#' The sway that shows the map reveal is a control. See fw_home_compare().
#'
#' Plain DOM. Runs each time the map comes into view, until the reader uses
#' the slider.
fw_home_compare_script = function(input_id) {
  cfg = FW_HOME_COMPARE
  tags$script(HTML(sprintf("
    (function () {
      var input = document.getElementById('%s');
      if (!input || !window.requestAnimationFrame) return;
      var box = input.parentNode;
      if (window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches) return;
      var START = %d, SWING = %d, PERIOD = %d, CYCLES = %d, DELAY = %d;
      var touched = false, running = false;
      function set(v) {
        v = Math.round(v * 10) / 10;
        box.style.setProperty('--fw-pos', v + '%%');
        input.value = v;
      }
      function stop() { touched = true; }
      ['pointerdown', 'keydown', 'focus', 'touchstart'].forEach(function (ev) {
        input.addEventListener(ev, stop, { passive: true });
      });
      function sway() {
        if (touched || running) return;
        // Wait for the loader (fw_loader()) to go, or the sway is missed.
        if (document.getElementById('fw-loader')) { setTimeout(sway, 200); return; }
        running = true;
        var t0 = null, total = PERIOD * CYCLES;
        function frame(t) {
          if (touched) { running = false; return; }
          if (t0 === null) t0 = t;
          var e = t - t0;
          if (e >= total) { set(START); running = false; return; }
          // A sine either side of the middle, easing out on the last swing.
          var fade = Math.min(1, (total - e) / PERIOD);
          set(START + SWING * fade * Math.sin(2 * Math.PI * e / PERIOD));
          requestAnimationFrame(frame);
        }
        setTimeout(function () { requestAnimationFrame(frame); }, DELAY);
      }
      if ('IntersectionObserver' in window) {
        new IntersectionObserver(function (entries) {
          entries.forEach(function (en) { if (en.isIntersecting) sway(); });
        }, { threshold: 0.6 }).observe(box);
      } else {
        sway();
      }
    })();",
    input_id, cfg$start, cfg$swing, cfg$period_ms, cfg$cycles, cfg$delay_ms)))
}

#' How to use the map, with its two states in their map colours
#'
#' The named pieces become .fw-compare__now and __later spans.
fw_home_map_line = function() {
  cap = fw_t("home", "map_caption")
  lapply(seq_along(cap), function(i) {
    nm = names(cap)[i]
    if (nzchar(nm)) {
      tags$span(.noWS = "outside", class = paste0("fw-compare__", nm), cap[[i]])
    } else {
      cap[[i]]
    }
  })
}

#' Prose with [[page|words]] links to other tabs, and **bold**
#'
#' A click sets fw_nav_to and app.R switches tab. Also does **bold**, since
#' it replaces fw_emphasis() in fw_page_header(). A ** pair must not span a link.
fw_home_links = function(text) {
  pattern = "\\[\\[([a-z_]+)\\|([^]]+)\\]\\]"
  hits = gregexpr(pattern, text)
  links = regmatches(text, hits)[[1]]
  if (!length(links)) return(fw_emphasis(text))
  # One more stretch of plain text than there are links, interleaved.
  plain = regmatches(text, hits, invert = TRUE)[[1]]
  parts = lapply(regmatches(links, regexec(pattern, links)), function(m) {
    tags$a(
      # No whitespace around the link, or "words ," gets a space before the comma.
      .noWS = "outside",
      href = "#",
      onclick = sprintf("Shiny.setInputValue('fw_nav_to','%s',{priority:'event'}); return false;", m[2]),
      fw_emphasis(m[3])
    )
  })
  out = vector("list", 2L * length(parts) + 1L)
  out[seq(1L, length(out), by = 2L)] = lapply(plain, fw_emphasis)
  out[seq(2L, length(out), by = 2L)] = parts
  do.call(tagList, out)
}
