# mod_home.R
# BUILT. The Welcome page: what the problem is, that the fix works, where it has
# and has not been done, and where to go next.
#
# ==============================================================================
# THE PAGE, in its fixed order
# ==============================================================================
#
# 1. HEADER. The headline and three lead paragraphs in the standard indigo page
#    header. Numbers are bold (**...** in the copy). The count of successful
#    recoveries is filled from the loaded data at startup and is NEVER typed;
#    the "X%" threatened figure comes from outside FWISE and is the client's.
#    Figures appear at their value - no count-up.
#
# 2. SUCCESS STORIES. One row per continent, A-Z. Each row is a native
#    <details>: a subtitle across the row, then the invasive species on the
#    left, a short line of story, and the beneficiary on the right. Opening it
#    shows the longer account. Rows open independently, not as an accordion.
#    The invasive plates are greyscale and the beneficiaries are in colour: that
#    is the client's artwork and is the point, not an accident to "fix".
#    A picture the client has not supplied (NA in FW_HOME_IMG) draws as a
#    placeholder box, so a row keeps its shape until the image arrives.
#
# 3. CURRENT WORK AND GAPS. Two STATIC map pictures, one over the other, with a
#    slider that reveals the priorities map from the left. It starts fully on
#    current work. The only moving part is the reveal - these are not the
#    interactive Leaflet maps from Explore. The slider is a native range input
#    laid over the figure, so drag, click and arrow keys all work with one line
#    of inline JS. The legend printed in the client's pictures was trimmed off
#    the web copies (the two sat in the same place and the slider cut across
#    them) and is drawn as page text above the figure instead.
#
# 4. CLOSING. One paragraph whose phrases link to Explore, Plan and Networking,
#    and a thank-you. No buttons.
#
# THE PICTURES ARE WEB COPIES. The client's originals are in resources/ (not
# served) and are never altered. The copies in www/img/home/ were made with
# macOS sips and base R's png package:
#   maps:    sips -Z 2400 <original> ; sips -c 1140 2400 --cropOffset 90 0
#            (the same crop for both, so they stay in register; it drops the
#            legend strip, which sits below row 1230 at that width)
#   species: sips -Z 1000, trimmed to the drawing's alpha bounding box plus
#            12px (png::readPNG / writePNG), then sips -Z 640
# Redo both steps if the client sends new artwork.
# ==============================================================================

mod_home_ui <- function(id, stats) {
  ns <- NS(id)
  lead <- fw_fill(fw_t("home", "lead"), successful = fw_fmt_num(stats$successful))

  tagList(
    fw_page_header(fw_t("home", "title"), lead),
    tags$main(
      id = "fw-main",

      fw_section(
        fw_container(
          tags$h2(fw_t("home", "stories_heading")),
          p(class = "fw-lead", fw_t("home", "stories_intro")),
          div(
            class = "fw-stories",
            lapply(names(fw_t("home", "stories")), function(key) {
              fw_home_story(fw_t("home", "stories", key), FW_HOME_IMG$stories[[key]])
            })
          )
        )
      ),

      fw_section(
        variant = "paper",
        fw_container(
          tags$h2(fw_t("home", "map_heading")),
          p(class = "fw-lead", fw_t("home", "map_intro")),
          fw_home_compare(ns("map_reveal"))
        )
      ),

      fw_section(
        fw_container(
          div(class = "fw-prose fw-home-closing",
              lapply(fw_t("home", "closing"), function(x) p(fw_home_links(x))))
        )
      )
    )
  )
}

mod_home_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing reactive: the page is drawn once, with its one live figure filled
    # in mod_home_ui() from the data loaded at startup.
  })
}

# ---- Pieces ------------------------------------------------------------------

#' One success story row
#'
#' THE SAME DISCLOSURE AS ABOUT, so the chevron, focus ring and card match. The
#' summary is what a closed row shows, so it carries the pictures as well as the
#' title; only the long account waits behind the click.
fw_home_story <- function(s, img) {
  figure <- function(role) {
    src <- img[[role]]
    sp  <- s[[role]]
    art <- if (is.null(src) || is.na(src)) {
      div(class = "fw-story__placeholder", role = "img", `aria-label` = sp$alt,
          fw_t("home", "image_placeholder"))
    } else {
      tags$img(src = src, alt = sp$alt, loading = "lazy")
    }
    tags$figure(
      class = paste0("fw-story__figure fw-story__figure--", role),
      div(class = "fw-story__art", art),
      tags$figcaption(
        tags$span(class = "fw-story__role", fw_t("home", paste0(role, "_label"))),
        sp$name
      )
    )
  }

  tags$details(
    class = "fw-disclosure fw-story",
    tags$summary(
      class = "fw-disclosure__summary fw-story__summary",
      tags$span(class = "fw-disclosure__title fw-story__title",
                tags$span(class = "fw-story__continent", s$continent), " ", s$title),
      div(
        class = "fw-story__row",
        figure("invasive"),
        p(class = "fw-story__text", s$summary),
        figure("beneficiary")
      )
    ),
    div(class = "fw-disclosure__body", p(s$body))
  )
}

#' The before/after map reveal
#'
#' --fw-pos is how much of the priorities map shows, from the left. The input
#' writes it on every move; the stylesheet does the rest. It starts at 0, fully
#' on current work, matching the input's value so the first paint is right
#' without any script having run.
fw_home_compare <- function(input_id) {
  swatch <- function(which, label) {
    tags$span(class = "fw-compare__key",
              tags$span(class = paste0("fw-compare__swatch fw-compare__swatch--", which),
                        `aria-hidden` = "true"),
              label)
  }

  tags$figure(
    class = "fw-compare-wrap",
    div(
      class = "fw-compare__legend",
      swatch("next", fw_t("home", "map_next_label")),
      swatch("now", fw_t("home", "map_now_label"))
    ),
    div(
      class = "fw-compare", style = "--fw-pos: 0%;",
      tags$img(class = "fw-compare__base", src = FW_HOME_IMG$map_now,
               alt = "", `aria-hidden` = "true"),
      div(class = "fw-compare__top",
          tags$img(src = FW_HOME_IMG$map_next, alt = "", `aria-hidden` = "true")),
      div(class = "fw-compare__handle", `aria-hidden` = "true"),
      # NOT a Shiny input: no server reads it, so it takes a plain id and no
      # binding. The value is a percentage of the width.
      tags$input(
        type = "range", id = input_id, class = "fw-compare__range",
        min = 0, max = 100, step = 1, value = 0,
        `aria-label` = fw_t("home", "map_slider_label"),
        oninput = "this.parentNode.style.setProperty('--fw-pos', this.value + '%')"
      )
    ),
    tags$figcaption(class = "fw-caption", fw_t("home", "map_caption"))
  )
}

#' Prose with [[page|words]] links to other tabs
#'
#' The same cross-page wiring as the About page's contribute link: the click
#' sets fw_nav_to and app.R switches the tab.
fw_home_links <- function(text) {
  pattern <- "\\[\\[([a-z_]+)\\|([^]]+)\\]\\]"
  hits <- gregexpr(pattern, text)
  links <- regmatches(text, hits)[[1]]
  if (!length(links)) return(text)
  # One more stretch of plain text than there are links, interleaved.
  plain <- regmatches(text, hits, invert = TRUE)[[1]]
  parts <- lapply(regmatches(links, regexec(pattern, links)), function(m) {
    tags$a(
      href = "#",
      onclick = sprintf("Shiny.setInputValue('fw_nav_to','%s',{priority:'event'}); return false;", m[2]),
      m[3]
    )
  })
  out <- vector("list", 2L * length(parts) + 1L)
  out[seq(1L, length(out), by = 2L)] <- as.list(plain)
  out[seq(2L, length(out), by = 2L)] <- parts
  do.call(tagList, out)
}
