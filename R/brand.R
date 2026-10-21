# brand.R
# Every design value: colour, type, spacing, radius, shadow, motion and
# breakpoints. Read by the UI, the Bootstrap theme, charts, maps and every
# export.
#
# _tokens.scss holds no literals: fw_sass_variables() passes these to Sass, so
# `$fw-ink` is FW_COLOURS$ink. After changing a colour, run
# dev/check_contrast.R and dev/check_literals.R.
#
# No dependencies, so dev scripts can source it alone. Shiny sources R/ in byte
# order, so this file must keep a name that sorts first.
#
# Tokens are named for their role, not their hue.

# ---- Colour ------------------------------------------------------------------

# Client values: #0F8B79 (teal) and #191144 (indigo). Everything else derives
# from them or is a neutral tinted towards the indigo. No white anywhere.
# Surfaces are lifted by tone and FW_SHADOW$card, not borders.
#
# brand_teal fails AA as text, so it is non-text only; text and buttons use
# teal_text. dev/check_contrast.R checks every pair.
FW_COLOURS = list(
  # Text
  ink             = "#191144",  # client value. Body text and headings
  ink_muted       = "#4a4468",  # secondary text, captions, help

  # Brand
  brand_indigo    = "#191144",  # client value. Page-title band and footer
  brand_teal      = "#0f8b79",  # client value. Accents and markers, non-text only
  teal_text       = "#0c7565",  # links and primary buttons: the teal that passes AA
  teal_hover      = "#0a6152",  # hover for links and buttons, a step darker
  teal_tint       = "#c9e7e3",  # chips, progress track, tinted panels
  teal_wash       = "#e4f1ee",  # faintest teal: notices, subtle washes
  teal_light      = "#7fc5bd",  # links and accents on the indigo ground

  # Surfaces
  page            = "#e9ebf3",  # the page ground
  surface         = "#f7f8fc",  # cards, tables, inputs, navbar, popups, report sheet
  sunken          = "#eef0f7",  # table header rows, disabled fields, skeleton base

  # Lines
  border          = "#d9dcea",  # hairlines, dividers, chart grid. Decorative
  border_input    = "#6b6486",  # input edges: interactive, needs 3:1

  # Text on the indigo ground
  on_indigo       = "#f7f8fc",
  on_indigo_muted = "#c9c4e3",

  # Welcome map keys, sampled from www/img/home/map-*.png. Non-text only.
  map_now         = "#54a1cb",
  map_next        = "#eebf54",
  # The same hues darkened for the map caption. map_next_text passes 3:1 only
  # (large bold text), so don't use it for body text.
  map_now_text    = "#2e6f9a",
  map_next_text   = "#b07b00"
)

# ---- Type --------------------------------------------------------------------

# Ubuntu, self-hosted from www/fonts/. Ubuntu Mono for numerals only (.fw-num).
#
# 1rem floor (client): nothing that stays on the page goes below size_min.
# Exemptions: size_popup (transient overlays), size_fine, size_credit and
# size_photo_credit. floor_px is the same floor for plotly.
FW_TYPE = list(
  font_body = paste0('"Ubuntu", -apple-system, BlinkMacSystemFont, "Segoe UI", ',
                     'Roboto, "Helvetica Neue", Arial, sans-serif'),
  font_mono = paste0('"Ubuntu Mono", ui-monospace, SFMono-Regular, "SF Mono", ',
                     'Menlo, Consolas, "Liberation Mono", monospace'),
  # plotly can't take the full stack.
  font_plot = "Ubuntu, system-ui, sans-serif",
  # Word question list: readers won't have Ubuntu.
  font_docx = "Calibri",

  size_h1      = "2.4rem",
  size_h2      = "1.9rem",
  size_h3      = "1.55rem",
  # Navigation tabs: its own step, larger than body.
  size_nav     = "1.3rem",
  size_lead    = "1.3rem",
  size_body    = "1.2rem",
  size_caption = "1.15rem",    # subtitles and notes
  size_min     = "1.05rem",    # the floor. Do not lower.
  size_popup   = "0.9rem",     # exemption: transient overlays only

  # Exemption (client): small print that qualifies the page. Only the Welcome
  # footnote, the Mercator note under maps, the footer licence line and the
  # PDF's "What this report covers" table. Ask the client before adding more.
  size_fine    = "0.95rem",

  # Exemption (client): footer credits and photo credits on paper. Must stay
  # legible, since the credit is a licence obligation.
  size_credit  = "0.8rem",

  # Exemption (client): the Wikimedia credit under on-page photographs.
  size_photo_credit = "0.6rem",

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

# ---- Print -------------------------------------------------------------------

# PDF type in points. 1rem = 16px = 12pt, so size_min (1.05rem) is 12.6pt. Body
# text and chart labels sit on that floor; charts are drawn at printed size.
fw_rem_pt = function(rem) 12 * as.numeric(sub("rem$", "", rem))

# Printed map colours (client): land and water drawn as themselves, muted so the
# outcome dots stay the data. `frame` is the border, ticks and scale bar.
FW_MAP_PRINT = list(
  land    = "#dbe6d4",   # pale green
  water   = "#cfe0ea",   # pale blue - the sea, and every lake
  outline = "#9aa89a",   # coastlines and lake edges
  frame   = "#4a4f53"    # the border, the ticks and the scale bar
)

FW_PRINT = list(
  floor   = fw_rem_pt(FW_TYPE$size_min),
  title   = 24,
  h2      = 17,
  h3      = 14,
  stat    = 20,     # the five numbers in the summary strip
  leading = 0.62    # Typst's gap between lines, in em
)

# ---- Space -------------------------------------------------------------------

# An 8px scale. The content column is a share of the viewport, not a pixel cap.
FW_SPACE = list(
  s1 = "0.5rem", s2 = "1rem", s3 = "1.5rem", s4 = "2rem",
  s5 = "3rem",   s6 = "4rem",
  container_width = "95%"
)

# ---- Radius ------------------------------------------------------------------

# Controls and containers get different radii on purpose.
FW_RADIUS = list(
  input = "8px",
  card  = "12px",
  round = "999px"
)

# ---- Shadow ------------------------------------------------------------------

# `card` is a resting card; `raised` is for things that float (popups, map card).
FW_SHADOW = list(
  card   = "0 1px 2px rgba(25, 17, 68, 0.05), 0 6px 20px rgba(25, 17, 68, 0.08)",
  raised = "0 12px 32px rgba(25, 17, 68, 0.18)"
)

# ---- Motion ------------------------------------------------------------------

# Motion only answers a user action. No page-load or scroll motion, no count-ups.
FW_MOTION = list(
  fast   = "150ms",
  base   = "200ms",
  easing = "cubic-bezier(0.22, 0.61, 0.36, 1)"
)

# ---- Breakpoints -------------------------------------------------------------

# `xl_max` is the top of the band where the navbar brand and nav items compete
# for one row.
FW_BREAKPOINTS = list(
  xs = "480px", sm = "600px", md = "768px", lg = "900px",
  xl = "992px", xl_max = "1099px"
)

# Transparent plotly backgrounds, so a chart takes its surface's colour.
FW_TRANSPARENT = "rgba(0,0,0,0)"

# ---- Helpers -----------------------------------------------------------------

#' A token as a CSS rgba() string, for partial opacity in R (plotly, inline styles)
fw_rgba = function(hex, alpha) {
  rgb = grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], format(alpha))
}

#' Every token above as a named list of Sass variables
#'
#' FW_COLOURS$ink -> `fw-ink`, FW_TYPE$size_h1 -> `fw-size-h1`,
#' FW_SPACE$s1 -> `fw-space-1`. Values are pasted verbatim. The Wong palette
#' (R/config.R) is added as `fw-data-*`, read at call time.
fw_sass_variables = function() {
  flatten = function(prefix, x) {
    stats::setNames(as.list(unname(unlist(x))),
                    paste0(prefix, gsub("_", "-", names(x))))
  }
  vars = c(
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
    vars = c(vars, flatten("fw-data-", as.list(FW_PALETTE)))
  }
  # `s1` was flattened as `fw-space-s1`; Sass wants `fw-space-1`.
  names(vars) = sub("^fw-space-s", "fw-space-", names(vars))
  vars
}
