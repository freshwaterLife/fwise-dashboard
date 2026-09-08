# theme.R
# The bslib theme. Bootstrap variables are set here so that stock components
# (buttons, form controls, the navbar) already look like FWISE before any of our
# own CSS in www/scss/ loads. That ordering matters: overriding Bootstrap after
# the fact leads to specificity fights, which the brief warns about.
#
# Colour values are duplicated between here and _tokens.scss because Sass
# variables cannot cross the boundary. If you change a colour, change it in BOTH
# and re-run `Rscript dev/check_contrast.R`.

library(bslib)

# Kept in sync with www/scss/_tokens.scss. Sampled from the logo files.
FW_COLOURS <- list(
  abyss      = "#0a2e29",
  deep       = "#0d574c",
  primary    = "#108978",
  shallow    = "#1c9484",
  shoal      = "#c7ede8",
  line_input = "#65948d",
  silt       = "#f7f4ef",
  paper      = "#ffffff",
  ink_muted  = "#4a6a64"
)

fw_theme <- function() {
  bs_theme(
    version = 5,

    # Type. Ubuntu is registered as a local font so bslib serves the woff2 files
    # from www/fonts/ rather than reaching for a CDN.
    base_font = font_collection(
      font_face(
        family = "Ubuntu",
        src = c("url('fonts/ubuntu-400.woff2') format('woff2')"),
        weight = 400, display = "swap"
      ),
      "system-ui", "sans-serif"
    ),
    code_font = font_collection("Ubuntu Mono", "monospace"),
    heading_font = font_collection("Ubuntu", "system-ui", "sans-serif"),

    base_font_size = "1rem",

    # Colour. `primary` drives Bootstrap's buttons, links and focus states, so it
    # is set to the accessible teal rather than the true brand teal. See the
    # contrast note in _tokens.scss.
    bg      = FW_COLOURS$silt,
    fg      = FW_COLOURS$abyss,
    primary = FW_COLOURS$deep,
    secondary = FW_COLOURS$ink_muted,
    success = "#009E73",  # Wong palette, so a success state matches the charts
    info    = FW_COLOURS$shallow,
    warning = "#E69F00",
    danger  = "#D55E00",
    light   = FW_COLOURS$shoal,
    dark    = FW_COLOURS$abyss,

    # Shape. Radius varies by hierarchy elsewhere; this is the control default.
    "border-radius"    = "4px",
    "border-radius-lg" = "10px",
    "border-radius-sm" = "3px",
    "border-color"     = FW_COLOURS$shoal,

    # Surfaces are defined by hairlines and background contrast, so Bootstrap's
    # shadows are switched off. The two places a shadow IS wanted (the map popup
    # and the sticky form footer) set it themselves in _components.scss.
    "box-shadow"    = "none",
    "box-shadow-sm" = "none",
    "box-shadow-lg" = "none",

    "body-color"    = FW_COLOURS$abyss,
    "link-color"    = FW_COLOURS$deep,
    "link-hover-color" = FW_COLOURS$primary,
    "headings-color"   = FW_COLOURS$deep,
    "headings-font-weight" = "500",

    "navbar-light-color"        = FW_COLOURS$abyss,
    "navbar-light-hover-color"  = FW_COLOURS$deep,
    "navbar-light-active-color" = FW_COLOURS$deep,

    "card-border-color"  = FW_COLOURS$shoal,
    "card-cap-bg"        = FW_COLOURS$paper,
    "card-bg"            = FW_COLOURS$paper,

    "input-border-color" = FW_COLOURS$line_input,
    "input-focus-border-color" = FW_COLOURS$deep,

    # Bootstrap's own focus ring. Our :focus-visible rule handles the general
    # case; matching the colour here stops the two disagreeing on controls that
    # Bootstrap styles directly.
    "focus-ring-color" = FW_COLOURS$shallow,
    "focus-ring-width" = "3px"
  )
}
