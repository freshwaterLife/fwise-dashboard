# theme.R
# The bslib theme: maps the R/brand.R tokens onto Bootstrap's variables, so
# stock components look right without CSS overrides. bslib compiles Bootstrap
# separately and can't see our Sass variables. No values of its own here.

library(bslib)

fw_theme = function() {
  bs_theme(
    version = 5,

    # Type. @font-face is in main.scss; a font_face() here 404s from bslib's
    # font.css.
    base_font = font_collection("Ubuntu", "system-ui", "sans-serif"),
    code_font = font_collection("Ubuntu Mono", "monospace"),
    heading_font = font_collection("Ubuntu", "system-ui", "sans-serif"),

    # Must match our body size.
    base_font_size = FW_TYPE$size_body,

    # The 1rem floor for Bootstrap's small-text variables (help text,
    # validation, -sm controls, badges). rem, not em, so it can't compound.
    # Page small print takes size_caption; already-small components size_min.
    "font-size-sm"            = FW_TYPE$size_min,
    "small-font-size"         = FW_TYPE$size_caption,
    "sub-sup-font-size"       = FW_TYPE$size_min,
    "form-text-font-size"     = FW_TYPE$size_caption,
    "form-label-font-size"    = FW_TYPE$size_caption,
    "form-feedback-font-size" = FW_TYPE$size_caption,
    "input-font-size-sm"      = FW_TYPE$size_min,
    "btn-font-size-sm"        = FW_TYPE$size_min,
    # bslib's default button size (15px) is under the floor.
    "btn-font-size"           = FW_TYPE$size_body,
    "input-btn-font-size"     = FW_TYPE$size_body,
    "badge-font-size"         = FW_TYPE$size_min,
    # The pop-up exemption (client): tooltips and popovers only.
    "tooltip-font-size"       = FW_TYPE$size_popup,
    "popover-font-size"       = FW_TYPE$size_popup,
    "dropdown-font-size"      = FW_TYPE$size_caption,
    "legend-font-size"        = FW_TYPE$size_caption,
    # Nav tabs larger than body (client).
    "nav-link-font-size"      = FW_TYPE$size_nav,

    # Colour. `primary` is teal_text, the teal that passes AA.
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

    # Bootstrap shadows off; ours come from $fw-shadow-*.
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

    # Modals default to white; use the surface tone (no white).
    "modal-content-bg"           = FW_COLOURS$surface,
    "modal-content-border-color" = FW_COLOURS$border,
    "modal-header-border-color"  = FW_COLOURS$border,
    "modal-footer-border-color"  = FW_COLOURS$border,
    # Indigo backdrop rather than black.
    "modal-backdrop-bg"          = FW_COLOURS$brand_indigo,
    "modal-backdrop-opacity"     = "0.5",

    # Fields take the surface colour, so they're the lightest thing on the page.
    "input-bg"                 = FW_COLOURS$surface,
    "form-check-input-bg"      = FW_COLOURS$surface,
    "input-border-color"       = FW_COLOURS$border_input,
    "form-check-input-border"  = paste0("1px solid ", FW_COLOURS$border_input),
    "input-focus-border-color" = FW_COLOURS$teal_text,

    # Bootstrap's focus ring, matching our :focus-visible. Brand teal meets
    # 3:1 (dev/check_contrast.R).
    "focus-ring-color" = FW_COLOURS$brand_teal,
    "focus-ring-width" = "3px"
  )
}
