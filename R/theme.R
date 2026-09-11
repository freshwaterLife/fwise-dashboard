# theme.R
# The bslib theme. Bootstrap variables are set here so that stock components
# (buttons, form controls, the navbar) already look like FWISE before any of our
# own CSS in www/scss/ loads. That ordering matters: overriding Bootstrap after
# the fact leads to specificity fights.
#
# THERE IS NO PALETTE IN THIS FILE. Every value below is read from R/brand.R
# (FW_COLOURS, FW_TYPE, FW_RADIUS) or R/config.R (FW_PALETTE). Bootstrap's Sass
# is compiled by bslib separately from our own stylesheet and cannot see the
# variables fw_compile_css() injects there, so this file maps the same tokens
# onto Bootstrap's variable names. To change a colour, a size or a radius, edit
# R/brand.R; nothing here is a value of its own.

library(bslib)

fw_theme <- function() {
  bs_theme(
    version = 5,

    # Type. Ubuntu is registered as a local font so bslib serves the woff2 files
    # from www/fonts/ rather than reaching for a CDN.
    base_font = font_collection(
      font_face(
        family = "Ubuntu",
        src = c("url('fonts/ubuntu-400.woff2') format('woff2')"),
        weight = FW_TYPE$weight_regular, display = "swap"
      ),
      "system-ui", "sans-serif"
    ),
    code_font = font_collection("Ubuntu Mono", "monospace"),
    heading_font = font_collection("Ubuntu", "system-ui", "sans-serif"),

    # Bootstrap sizes its own controls off this, so it has to be the body size
    # our stylesheet uses or prose and form controls drift apart.
    base_font_size = FW_TYPE$size_body,

    # THE 1rem TEXT FLOOR, applied to Bootstrap's own components.
    #
    # Our SCSS can only hold the floor on markup we write. Bootstrap sizes a
    # dozen of its own pieces off separate variables that default to 0.875em or
    # smaller - form help text, validation feedback, small buttons and inputs,
    # badges, tooltips, legends - and every one of those renders somewhere in
    # this app. Setting the variables is what makes the floor actually hold;
    # chasing the same values back out with CSS overrides would mean a
    # specificity fight per component.
    #
    # rem and not em deliberately: an em here would compound against whatever
    # the parent happens to be and quietly drop back under the floor inside a
    # small container.
    "font-size-sm"            = FW_TYPE$size_min,
    "small-font-size"         = FW_TYPE$size_min,
    "sub-sup-font-size"       = FW_TYPE$size_min,
    "form-text-font-size"     = FW_TYPE$size_min,
    "form-label-font-size"    = FW_TYPE$size_min,
    "form-feedback-font-size" = FW_TYPE$size_min,
    "input-font-size-sm"      = FW_TYPE$size_min,
    "btn-font-size-sm"        = FW_TYPE$size_min,
    # bslib sets its own --bs-btn-font-size of .9375rem, which is 15px and under
    # the floor. Buttons are set at body size so a call to action is never
    # smaller than the sentence that introduced it.
    "btn-font-size"           = FW_TYPE$size_body,
    "input-btn-font-size"     = FW_TYPE$size_body,
    "badge-font-size"         = FW_TYPE$size_min,
    # THE POP-UP EXEMPTION, granted by the client: transient overlay text may sit
    # under the floor. Tooltips and popovers only. Everything that stays on the
    # page is still at the floor.
    "tooltip-font-size"       = FW_TYPE$size_popup,
    "popover-font-size"       = FW_TYPE$size_popup,
    "dropdown-font-size"      = FW_TYPE$size_min,
    "legend-font-size"        = FW_TYPE$size_min,
    "nav-link-font-size"      = FW_TYPE$size_min,

    # Colour. `primary` drives Bootstrap's buttons, links and focus states, so it
    # is set to the teal that passes AA as text rather than the brand teal. See
    # the contrast note in R/brand.R.
    bg        = FW_COLOURS$page,
    fg        = FW_COLOURS$ink,
    primary   = FW_COLOURS$teal_text,
    secondary = FW_COLOURS$ink_muted,
    # The Wong palette, so a success or danger state matches the charts.
    success   = unname(FW_PALETTE["successful"]),
    info      = FW_COLOURS$teal_hover,
    warning   = unname(FW_PALETTE["failed"]),
    danger    = unname(FW_PALETTE["vermilion"]),
    light     = FW_COLOURS$teal_tint,
    dark      = FW_COLOURS$brand_indigo,

    # Shape. Radius varies by hierarchy elsewhere; this is the control default.
    "border-radius"    = FW_RADIUS$input,
    "border-radius-lg" = FW_RADIUS$card,
    "border-radius-sm" = FW_RADIUS$input,
    "border-color"     = FW_COLOURS$border,

    # Surfaces are defined by tone and by our own shadow tokens, so Bootstrap's
    # shadows are switched off. The places a shadow IS wanted set it themselves
    # in _components.scss from $fw-shadow-*.
    "box-shadow"    = "none",
    "box-shadow-sm" = "none",
    "box-shadow-lg" = "none",

    "body-color"       = FW_COLOURS$ink,
    "link-color"       = FW_COLOURS$teal_text,
    "link-hover-color" = FW_COLOURS$ink,
    # Headings are set in the ink colour; teal is kept for accents and fills.
    "headings-color"       = FW_COLOURS$ink,
    "headings-font-weight" = as.character(FW_TYPE$weight_medium),

    "navbar-light-color"        = FW_COLOURS$ink,
    "navbar-light-hover-color"  = FW_COLOURS$teal_text,
    "navbar-light-active-color" = FW_COLOURS$teal_text,

    "card-border-color" = FW_COLOURS$border,
    "card-cap-bg"       = FW_COLOURS$surface,
    "card-bg"           = FW_COLOURS$surface,

    # FIELDS TAKE THE SURFACE COLOUR, NOT THE PAGE COLOUR. Bootstrap defaults
    # every form control's background to $body-bg. Left alone, an unchecked
    # checkbox was a page-coloured square with a pale border, which did not read
    # as something you could tick. Fields are always the lightest thing on the
    # page.
    "input-bg"                 = FW_COLOURS$surface,
    "form-check-input-bg"      = FW_COLOURS$surface,
    "input-border-color"       = FW_COLOURS$border_input,
    "form-check-input-border"  = paste0("1px solid ", FW_COLOURS$border_input),
    "input-focus-border-color" = FW_COLOURS$teal_text,

    # Bootstrap's own focus ring. Our :focus-visible rule handles the general
    # case; matching the colour here stops the two disagreeing on controls that
    # Bootstrap styles directly. The brand teal rather than the hover teal: the
    # lighter one misses the 3:1 floor a focus ring has to meet on the page
    # ground. Verified by dev/check_contrast.R.
    "focus-ring-color" = FW_COLOURS$brand_teal,
    "focus-ring-width" = "3px"
  )
}
