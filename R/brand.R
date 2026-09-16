# brand.R
# THE ONE PLACE DESIGN VALUES LIVE. Colour, type, spacing, radius, shadow,
# motion and breakpoints, for the whole application: the Shiny UI, the Bootstrap
# theme (R/theme.R), every plotly chart (R/charts.R), the maps (R/maps.R), the
# workbook export (R/export.R), the Word question list (R/questions_docx.R) and
# the standalone HTML report (R/report_html.R).
#
# HOW IT REACHES THE STYLESHEET. www/scss/_tokens.scss contains NO literal
# values. fw_sass_variables() below flattens everything here into Sass variables
# and fw_compile_css() (R/ui_helpers.R) hands them to sass::sass() ahead of the
# stylesheet, so `$fw-ink` in Sass IS FW_COLOURS$ink. Change a value here and it
# changes everywhere at the next start. There is no second copy to keep in step.
#
# TO CHANGE A COLOUR: edit FW_COLOURS, then run
#     Rscript dev/check_contrast.R    (every text/background pair against WCAG AA)
#     Rscript dev/check_literals.R    (nothing has crept back in as a literal)
#
# THIS FILE HAS NO DEPENDENCIES. It does not call library() and does not read
# any other file, so a dev script can source() it on its own. Shiny sources R/
# in C byte order, which puts brand.R before every file that reads it; keep the
# name, or rename it to something that still sorts first.
#
# NAMING. Tokens are named for their ROLE, not their hue, so a value can change
# without the name lying. If a new role is needed, add a token rather than
# reusing one that happens to have the right value today.

# ---- Colour ------------------------------------------------------------------

# THE TWO CLIENT VALUES ARE #0F8B79 (teal) AND #191144 (indigo). Everything
# else is derived from them or is a neutral tinted towards the indigo.
#
# THERE IS NO WHITE. The page is a cool off-white tinted from the indigo and
# every surface on it - cards, tables, inputs, the navbar, popups, the report
# sheet - is one tonal step lighter, never #ffffff. Surfaces are lifted by
# that step and by FW_SHADOW$card, not by a border.
#
# CONTRAST. The brand teal does not reach 4.5:1 as text on any light surface,
# so it is reserved for NON-TEXT roles (accents, the active nav marker, the KPI
# rule, the focus ring) and every text or button role uses teal_text, which is
# the same hue stepped down until it passes. Never set body-sized text in
# brand_teal. dev/check_contrast.R lists every pair the interface uses and is
# the arbiter; re-run it after touching any value here.
FW_COLOURS <- list(
  # Text
  ink             = "#191144",  # CLIENT VALUE. Body text and headings
  ink_muted       = "#4a4468",  # secondary text, captions, help

  # Brand
  brand_indigo    = "#191144",  # CLIENT VALUE. The page-title band and footer ground
  brand_teal      = "#0f8b79",  # CLIENT VALUE. Accents and markers. NON-TEXT ONLY
  teal_text       = "#0c7565",  # links and primary buttons: the teal that passes AA
  teal_hover      = "#0a6152",  # hover state for links and buttons: a step darker
  teal_tint       = "#c9e7e3",  # chips, progress track, tinted panels
  teal_wash       = "#e4f1ee",  # the faintest teal: notices, subtle washes
  teal_light      = "#7fc5bd",  # links and accents ON the indigo ground

  # Surfaces
  page            = "#e9ebf3",  # the page ground behind everything
  surface         = "#f7f8fc",  # cards, tables, inputs, navbar, popups, report sheet
  sunken          = "#eef0f7",  # table header rows, disabled fields, skeleton base

  # Lines
  border          = "#d9dcea",  # hairlines, dividers, the chart grid. Decorative
  border_input    = "#6b6486",  # input edges: an interactive boundary, needs 3:1

  # Text on the indigo ground
  on_indigo       = "#f7f8fc",
  on_indigo_muted = "#c9c4e3",

  # The two keys of the Welcome page's map pictures. SAMPLED FROM THE CLIENT'S
  # IMAGES (www/img/home/map-*.png), not chosen: they are swatches beside the
  # page-text legend and have to match the fill in the picture. Non-text only.
  map_now         = "#54a1cb",
  map_next        = "#eebf54"
)

# ---- Type --------------------------------------------------------------------

# Ubuntu throughout, self-hosted from www/fonts/. Ubuntu Mono is for numerals
# only (.fw-num): it column-aligns figures. Never set labels or prose in it.
#
# THE 1rem FLOOR. No text that stays on the page may be set below 1rem; that
# is a client instruction. `min` is that floor, and the one exemption is
# `popup`: transient overlay text (popovers, the map card) may sit under it.
# floor_px is the same floor in the unit plotly understands.
#
# THE SCALE MOVED UP. The client asked for subtitles - the qualifying line
# under a block heading, which is `caption` - at 1.15rem rather than 1rem.
# Raising caption ALONE would have made it larger than body prose at 1.0625rem,
# so a chart's qualification would have outranked the paragraph that introduced
# it. The whole scale therefore went up together and the ratios between the
# steps are unchanged; only `min`, `popup` and `credit` stayed where they were,
# because those are the floor and its two exemptions rather than steps on the
# scale. `caption` is now separated from body text by size AS WELL AS by colour.
FW_TYPE <- list(
  font_body = paste0('"Ubuntu", -apple-system, BlinkMacSystemFont, "Segoe UI", ',
                     'Roboto, "Helvetica Neue", Arial, sans-serif'),
  font_mono = paste0('"Ubuntu Mono", ui-monospace, SFMono-Regular, "SF Mono", ',
                     'Menlo, Consolas, "Liberation Mono", monospace'),
  # The family name plotly is given. It cannot take the full stack.
  font_plot = "Ubuntu, system-ui, sans-serif",
  # The Word question list. Word on the reader's machine will not have Ubuntu.
  font_docx = "Calibri",

  # THE SCALE MOVED IN TWO DIRECTIONS AT ONCE, and that was the instruction:
  # headings and the navigation up a step, everything else down a step. The
  # app read as uniformly large - a note and the heading above it were four
  # hundredths of a rem apart - so nothing was emphasised by being big.
  #
  # THEN THE SMALL HALF CAME BACK UP ONE NOTCH, on a later client instruction:
  # the running text had ended up a shade too small to read comfortably. Only
  # the small half moved - lead, body, caption and the floor - so the headings
  # keep the sizes they were given and the emphasis the earlier reduction bought
  # is not spent. The gap between size_h3 and size_lead is narrower than it was;
  # that is the cost, and it was accepted.
  #
  # NOTHING WENT BELOW size_min, which is now 1.05rem. The floor is a client
  # instruction and not a preference. size_popup is the one exemption and did
  # NOT move with the rest: it sizes transient overlays - the (i) popovers and
  # the map hover card - and raising it would grow every card on the map.
  size_display = "3.05rem",
  size_h1      = "2.4rem",
  size_h2      = "1.9rem",
  size_h3      = "1.55rem",
  # The navigation, which is bigger than the body text rather than smaller than
  # it. It used to take size_caption, so the six tabs were the smallest chrome
  # on the page; the client asked for the tabs and the logo to carry the top of
  # the page together. Its own step, because it is neither a heading nor body.
  size_nav     = "1.3rem",
  size_lead    = "1.3rem",
  size_body    = "1.2rem",
  size_caption = "1.15rem",    # subtitles and notes
  size_min     = "1.05rem",    # THE FLOOR. Not a step on the scale; do not lower.
  size_popup   = "0.9rem",     # THE ONE EXEMPTION: transient overlays only

  # THE SECOND EXEMPTION, and a client instruction rather than a drift. A
  # photograph's credit line is an obligation under the licence - it has to be
  # present and legible - but in the dashboard's attempts table it sits under
  # two thumbnails in every row, and at 1rem the attribution was setting the
  # column width and pushing the species name onto three lines. The name is
  # what the reader is scanning for; the credit only has to be readable when
  # they look at it.
  #
  # 0.8rem, roughly 13px, NOT the half of 1rem the instruction says literally:
  # 8px attribution is not legible at arm's length on a laptop and would fail
  # the obligation it exists to meet. Raise or lower it here, in one place.
  size_credit  = "0.8rem",     # credit lines in the dashboard's table only

  # Narrow screens (under FW_BREAKPOINTS$xs) step the headings down.
  size_h1_narrow = "2.1rem",
  size_h2_narrow = "1.7rem",
  size_h3_narrow = "1.45rem",

  floor_px = 16L,              # the 1rem floor for plotly, which takes pixels

  weight_regular = 400L,
  weight_medium  = 500L,
  weight_bold    = 700L,

  leading_body    = 1.6,
  leading_heading = 1.15,

  measure = "68ch"             # opt-in prose width, .fw-measure
)

# ---- Space -------------------------------------------------------------------

# An 8px scale. The content column is a share of the viewport, not a pixel cap.
FW_SPACE <- list(
  s1 = "0.5rem", s2 = "1rem", s3 = "1.5rem", s4 = "2rem",
  s5 = "3rem",   s6 = "4rem", s7 = "6rem",
  container_width = "95%"
)

# ---- Radius ------------------------------------------------------------------

# Radius varies by hierarchy on purpose: one radius everywhere flattens the
# difference between a control and a container.
FW_RADIUS <- list(
  input = "8px",
  card  = "12px",
  round = "999px"
)

# ---- Shadow ------------------------------------------------------------------

# Surfaces sit on the page by tone and by these. `card` is the resting state of
# a card; `raised` is for things that genuinely float (popups, the map card).
# Turn either down here and every surface follows.
FW_SHADOW <- list(
  card   = "0 1px 2px rgba(25, 17, 68, 0.05), 0 6px 20px rgba(25, 17, 68, 0.08)",
  raised = "0 12px 32px rgba(25, 17, 68, 0.18)"
)

# ---- Motion ------------------------------------------------------------------

# Motion only ever answers a user action. No page-load or scroll motion, no
# count-ups.
FW_MOTION <- list(
  fast   = "150ms",
  base   = "200ms",
  easing = "cubic-bezier(0.22, 0.61, 0.36, 1)"
)

# ---- Breakpoints -------------------------------------------------------------

# Named so a media query says what it is for. `xl_max` is the top of the band
# where the navbar brand and the six nav items compete for one row.
FW_BREAKPOINTS <- list(
  xs = "480px", sm = "600px", md = "768px", lg = "900px",
  xl = "992px", xl_max = "1099px"
)

# Fully transparent, for plotly's paper and plot backgrounds so a chart takes
# the colour of whatever surface it sits on.
FW_TRANSPARENT <- "rgba(0,0,0,0)"

# ---- Helpers -----------------------------------------------------------------

#' A token as a CSS rgba() string
#'
#' For the places that need a colour at partial opacity in R rather than in
#' Sass: plotly fills, inline styles.
fw_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], format(alpha))
}

#' Every token above as a named list of Sass variables
#'
#' Names are the Sass names without the `$`: FW_COLOURS$ink becomes `fw-ink`,
#' FW_TYPE$size_h1 becomes `fw-size-h1`, FW_SPACE$s1 becomes `fw-space-1`.
#' Values are pasted into the stylesheet verbatim, so a quoted font stack stays
#' quoted and a number stays a number.
#'
#' The Wong data palette is included as `fw-data-*` so the few CSS rules that
#' need a data colour (validation, the check panel) read the same values the
#' charts do. It lives in R/config.R and is read at call time, which is after
#' every file in R/ has been sourced.
fw_sass_variables <- function() {
  flatten <- function(prefix, x) {
    stats::setNames(as.list(unname(unlist(x))),
                    paste0(prefix, gsub("_", "-", names(x))))
  }
  vars <- c(
    flatten("fw-",        FW_COLOURS),
    flatten("fw-",        FW_TYPE[grepl("^(font|size|weight|leading|measure)", names(FW_TYPE))]),
    flatten("fw-space-",  FW_SPACE[grepl("^s[0-9]$", names(FW_SPACE))]),
    list("fw-container-width" = FW_SPACE$container_width),
    flatten("fw-radius-", FW_RADIUS),
    flatten("fw-shadow-", FW_SHADOW),
    flatten("fw-motion-", FW_MOTION[c("fast", "base")]),
    list("fw-easing" = FW_MOTION$easing),
    flatten("fw-bp-",     FW_BREAKPOINTS)
  )
  if (exists("FW_PALETTE")) {
    vars <- c(vars, flatten("fw-data-", as.list(FW_PALETTE)))
  }
  # `s1` was flattened as `fw-space-s1`; Sass wants `fw-space-1`.
  names(vars) <- sub("^fw-space-s", "fw-space-", names(vars))
  vars
}
