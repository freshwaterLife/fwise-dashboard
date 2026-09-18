# mod_home.R
# BUILT. The Welcome page ("The solution"): why freshwater eradication matters,
# that it works, where it has and has not been done, and where to go next.
#
# ==============================================================================
# THE PAGE, in its fixed order. BUILT TO FIT ONE LAPTOP SCREEN (client, Sept
# 2026): every part is sized so a 1366x768 window shows the lot with little or
# no scroll. Add nothing below the map without re-measuring.
# ==============================================================================
#
# 1. HEADER. The two-sentence headline and three short lines in the indigo page
#    header, set a step smaller than other pages (.fw-page-header--home). The
#    last line's phrases link to Explore, Plan, Contribute and Networking.
#
# 2. THE STRIP. One figure - species protected, the distinct beneficiaries of
#    SUCCESSFUL attempts, filled from the data and never typed - then one
#    picture per success story, then the hint. Each picture is a button that
#    opens that story's card. The row shows beneficiaries only, in colour.
#
# 3. STORY CARDS. Native popovers (the HTML popover attribute), so opening,
#    Esc, clicking away and the top layer need no script. A card holds the
#    invasive plate (greyscale - the client's artwork, not an accident to
#    "fix") beside the beneficiary, then the title and the account. A picture
#    the client has not supplied (NA in FW_HOME_IMG) draws as a placeholder.
#
# 4. CURRENT WORK AND GAPS. Two STATIC map pictures in register with a slider
#    that reveals the priorities map from the left, starting fully on current
#    work. The slider is a native range input laid over the figure. There is no
#    separate legend: the caption names the two states in the map's own hues.
#    The map's width follows the window HEIGHT (see .fw-compare-wrap).
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
  keys <- names(fw_t("home", "stories"))

  tagList(
    fw_page_header(fw_t("home", "title"), fw_t("home", "lead"),
                   format = fw_home_links, modifier = "home"),
    tags$main(
      id = "fw-main",
      fw_section(
        tight = TRUE,
        fw_container(
          div(
            class = "fw-home-strip",
            div(class = "fw-home-strip__kpi",
                fw_kpi_stat(fw_fmt_num(stats$protected), fw_t("home", "kpi_label"),
                            tooltip = fw_t("home", "kpi_tooltip"))),
            div(class = "fw-home-strip__species",
                lapply(keys, function(key) {
                  fw_home_tile(key, fw_t("home", "stories", key), FW_HOME_IMG$stories[[key]])
                })),
            p(class = "fw-home-strip__hint", fw_t("home", "stories_hint"))
          ),
          fw_home_compare(ns("map_reveal"))
        )
      ),
      lapply(keys, function(key) {
        fw_home_card(key, fw_t("home", "stories", key), FW_HOME_IMG$stories[[key]])
      })
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

fw_home_card_id <- function(key) paste0("fw-home-story-", key)

#' A picture, or the placeholder box when the client has not supplied one
fw_home_art <- function(src, alt) {
  if (is.null(src) || is.na(src)) {
    div(class = "fw-story__placeholder", role = "img", `aria-label` = alt,
        fw_t("home", "image_placeholder"))
  } else {
    tags$img(src = src, alt = alt, loading = "lazy")
  }
}

#' One beneficiary in the strip: a button that opens its story card
fw_home_tile <- function(key, s, img) {
  sp <- s$beneficiary
  tags$button(
    type = "button",
    class = "fw-home-species",
    popovertarget = fw_home_card_id(key),
    title = sp$name,
    `aria-label` = fw_fill(fw_t("home", "stories_open"),
                           continent = s$continent, name = sp$name),
    # The button's label says it all, so the picture is not announced twice.
    tagAppendAttributes(fw_home_art(img$beneficiary, ""), `aria-hidden` = "true")
  )
}

#' One success story, as a popover card
fw_home_card <- function(key, s, img) {
  figure <- function(role) {
    sp <- s[[role]]
    tags$figure(
      class = paste0("fw-story__figure fw-story__figure--", role),
      div(class = "fw-story__art", fw_home_art(img[[role]], sp$alt)),
      tags$figcaption(
        tags$span(class = "fw-story__role", fw_t("home", paste0(role, "_label"))),
        sp$name
      )
    )
  }
  title_id <- paste0(fw_home_card_id(key), "-title")

  div(
    id = fw_home_card_id(key), popover = "auto",
    class = "fw-home-card", role = "dialog", `aria-labelledby` = title_id,
    tags$button(
      type = "button", class = "fw-home-card__close",
      popovertarget = fw_home_card_id(key), popovertargetaction = "hide",
      `aria-label` = fw_t("home", "card_close"),
      HTML("&times;")
    ),
    tags$h2(
      id = title_id, class = "fw-home-card__title",
      tags$span(class = "fw-story__continent", s$continent), " ", s$title
    ),
    div(class = "fw-story__row", figure("invasive"), figure("beneficiary")),
    p(class = "fw-home-card__summary", s$summary),
    p(s$body)
  )
}

#' The before/after map reveal
#'
#' --fw-pos is how much of the priorities map shows, from the left. The input
#' writes it on every move; the stylesheet does the rest. It starts at 0, fully
#' on current work, matching the input's value so the first paint is right
#' without any script having run.
fw_home_compare <- function(input_id) {
  cap <- fw_t("home", "map_caption")
  piece <- function(i) {
    nm <- names(cap)[i]
    if (nzchar(nm)) tags$span(class = paste0("fw-compare__", nm), cap[[i]]) else cap[[i]]
  }

  tags$figure(
    class = "fw-compare-wrap",
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
    tags$figcaption(class = "fw-compare__caption", lapply(seq_along(cap), piece))
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
      # No whitespace around the link, or "words ," gets a space before the comma.
      .noWS = "outside",
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
