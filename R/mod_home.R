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
# 2. THE SENTENCE. One line across the full width in the teal box: successful
#    attempts and species protected (the distinct beneficiaries of those
#    attempts), both filled from the data and never typed, in bold, then the
#    hint to click the pictures.
#
# 3. PICTURES | MAP, side by side (client, Sept 2026). The left third is the
#    six beneficiaries, two columns of three in FW_HOME_ORDER; each is a
#    button that opens its story's card. The right two-thirds is the map.
#    Stacked, pictures first, on a narrow screen.
#
# 4. STORY CARDS. Native popovers (the HTML popover attribute), so opening,
#    Esc, clicking away and the top layer need no script. A card holds the
#    beneficiary only - the client took the invasive plate out - then the title
#    and the account. A picture the client has not supplied (NA in
#    FW_HOME_IMG) draws as a placeholder.
#
# 5. CURRENT WORK AND GAPS, the map. Two STATIC pictures in register with a
#    slider that reveals the priorities map from the left, starting fully on
#    current work. The slider is a native range input laid over the figure.
#    There is no separate legend: the caption names the two states in the
#    map's own hues. The map's width follows the window HEIGHT as well as its
#    column (see .fw-compare-wrap).
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
#
# THE LETTERED PLATES the story cards show are the same trim at a bigger size,
# and that one IS a script: dev/build_success_named.R. It says there why 1600px
# is the ceiling worth serving and why the trim matters more than the resize.
# ==============================================================================

mod_home_ui <- function(id, stats) {
  ns <- NS(id)
  keys <- names(fw_t("home", "stories"))
  # The two numbers are formatted before the ** pairs are turned into <strong>,
  # so each one lands inside its own bold span.
  sentence <- fw_fill(fw_t("home", "kpi_sentence"),
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
          )
        )
      ),
      # THE LETTERED PLATE, not the tile's drawing. The card is where the
      # reader has asked for the story, so it gets the version carrying the
      # common name and the binomial. The strip above keeps the small
      # unlettered one: six lettered plates at tile size would be six pieces of
      # text too small to read.
      lapply(keys, function(key) {
        fw_home_card(key, fw_t("home", "stories", key),
                     FW_HOME_IMG$stories_named[[key]])
      })
    )
  )
}

mod_home_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing reactive: the page is drawn once, with its two live figures filled
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
    # loading = "lazy" IS LOad-BEARING, not a tidy-up. Every story card is in
    # the page from the start (they are native popovers), so without it the six
    # lettered plates would be fetched on arrival by readers who never open a
    # card. A lazy image inside a closed popover is display:none, never near the
    # viewport, and so never fetched until the card opens. decoding = "async"
    # keeps the decode of the larger plate off the main thread when it does.
    tags$img(src = src, alt = alt, loading = "lazy", decoding = "async")
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
    tagAppendAttributes(fw_home_art(img, ""), `aria-hidden` = "true")
  )
}

#' One success story, as a popover card: the beneficiary beside the account
#'
#' Picture on the left, title and text on the right, half the card each
#' (client, Sept 2026 user testing). The two halves stack on a narrow screen -
#' see .fw-home-card__body in _components.scss.
fw_home_card <- function(key, s, img) {
  sp <- s$beneficiary
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
    div(
      class = "fw-home-card__body",
      tags$figure(
        class = "fw-story__figure",
        div(class = "fw-story__art", fw_home_art(img, sp$alt)),
        tags$figcaption(sp$name)
      ),
      div(
        class = "fw-home-card__text",
        tags$h2(
          id = title_id, class = "fw-home-card__title",
          tags$span(class = "fw-story__continent", s$continent), " ", s$title
        ),
        p(class = "fw-home-card__summary", s$summary),
        p(s$body)
      )
    )
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

#' Prose with [[page|words]] links to other tabs, and **bold**
#'
#' The same cross-page wiring as the About page's contribute link: the click
#' sets fw_nav_to and app.R switches the tab. Passing this to fw_page_header()
#' replaces the default formatter, so it has to do the **bold** that
#' fw_emphasis() would otherwise have done - a paragraph with no link at all
#' still goes through here. The emphasis is applied to each stretch of plain
#' text between the links, so a ** pair must sit inside one such stretch.
fw_home_links <- function(text) {
  pattern <- "\\[\\[([a-z_]+)\\|([^]]+)\\]\\]"
  hits <- gregexpr(pattern, text)
  links <- regmatches(text, hits)[[1]]
  if (!length(links)) return(fw_emphasis(text))
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
  out[seq(1L, length(out), by = 2L)] <- lapply(plain, fw_emphasis)
  out[seq(2L, length(out), by = 2L)] <- parts
  do.call(tagList, out)
}
