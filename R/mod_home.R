# mod_home.R
# The Welcome page ("The solution"): why freshwater eradication matters,
# that it works, where it has and has not been done, and where to go next.
#
# ==============================================================================
# THE PAGE, in its fixed order. BUILT TO FIT ONE LAPTOP SCREEN: every part is sized so a 1366x768 window shows the lot with little or
# no scroll. Add nothing below the map without re-measuring.
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
          ),
          fw_home_footnote()
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

#' The evidence footnote, the last thing on the page above the footer
#'
#' RAW HTML, and deliberately not markdown through fw_emphasis(): the two
#' citations are live links and fw_emphasis() only makes <strong>. The copy is
#' ours, in R/copy.R, so there is no untrusted input here to escape.
#'
#' It spans the full container rather than sitting in the species/map grid, and
#' it is the one place in the app set below the type floor - see
#' .fw-home-footnote and $fw-size-fine in _components.scss.
fw_home_footnote <- function() {
  p(class = "fw-home-footnote", HTML(fw_t("home", "footnote")))
}

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
#' Picture on the left, title and text on the right, half the card each. 
#' The two halves stack on a narrow screen - see .fw-home-card__body in _components.scss.
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
      # NO FIGCAPTION (client, 23 Sept 2026). The plate a card shows is the
      # hand-lettered one - FW_HOME_IMG$stories_named - which already carries
      # the common name and the binomial in the artwork, so a caption under it
      # printed the name a second time. The name is still announced: it is in
      # the tile button's label and in this figure's alt text.
      tags$figure(
        class = "fw-story__figure",
        div(class = "fw-story__art", fw_home_art(img, sp$alt))
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
#' --fw-pos is how much of the priorities map shows, from the left. The input
#' writes it on every move; the stylesheet does the rest. It starts at 0, fully
#' on current work, matching the input's value so the first paint is right
#' without any script having run.
fw_home_compare <- function(input_id) {
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
    tags$figcaption(class = "fw-compare__caption", fw_home_map_line())
  )
}

#' How to use the map, with its two states in their map colours
#'
#' The caption under the map. The named pieces of the copy become
#' spans coloured .fw-compare__now and .fw-compare__later.
fw_home_map_line <- function() {
  cap <- fw_t("home", "map_caption")
  lapply(seq_along(cap), function(i) {
    nm <- names(cap)[i]
    if (nzchar(nm)) {
      tags$span(.noWS = "outside", class = paste0("fw-compare__", nm), cap[[i]])
    } else {
      cap[[i]]
    }
  })
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
