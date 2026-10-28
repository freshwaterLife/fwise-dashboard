# check_contrast.R
# Verifies every text/background pair in the palette against WCAG AA.
#
# Run it after changing any colour in R/brand.R:
#     Rscript dev/check_contrast.R
#

relative_luminance = function(hex) {
  rgb = col2rgb(hex)[, 1] / 255
  lin = ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}

contrast_ratio = function(a, b) {
  la = relative_luminance(a); lb = relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

# The app's own palette (brand.R sources alone).
source("R/brand.R")
pal = unlist(FW_COLOURS)

# Pairs the interface uses; add new ones. 4.5 for text, 3.0 for meaningful
# non-text (focus ring, input edge, active marker).
pairs = list(
  list("Body text on page",         "ink",        "page",         4.5),
  list("Body text on surface",      "ink",        "surface",      4.5),
  list("Body text on sunken",       "ink",        "sunken",       4.5),
  list("Muted text on page",        "ink_muted",  "page",         4.5),
  list("Muted text on surface",     "ink_muted",  "surface",      4.5),
  list("Muted text on sunken",      "ink_muted",  "sunken",       4.5),  # Welcome picture placeholders
  list("Map caption blue on page",  "map_now_text",  "page",      4.5),
  list("Map caption amber on page", "map_next_text", "page",      3.0),  # 1.3rem bold: large text
  list("Link on page",              "teal_text",  "page",         4.5),
  list("Link on surface",           "teal_text",  "surface",      4.5),
  list("Link on sunken",            "teal_text",  "sunken",       4.5),
  list("Link on teal wash",         "teal_text",  "teal_wash",    4.5),
  list("Ink on teal tint",          "ink",        "teal_tint",    4.5),
  # Links in tinted cards use teal_hover (teal_text is 4.28:1 on teal_tint).
  list("Link on teal tint",         "teal_hover", "teal_tint",    4.5),
  list("Link hover on teal tint",   "ink",        "teal_tint",    4.5),
  list("Primary button label",      "surface",    "teal_text",    4.5),
  list("Primary button hover",      "surface",    "teal_hover",   4.5),
  list("Text on indigo",            "on_indigo",  "brand_indigo", 4.5),
  list("Muted text on indigo",      "on_indigo_muted", "brand_indigo", 4.5),
  list("Link on indigo",            "teal_light", "brand_indigo", 4.5),
  # brand_teal is non-text only, so checked at 3:1.
  list("Brand teal as non-text on page",    "brand_teal", "page",    3.0),
  list("Brand teal as non-text on surface", "brand_teal", "surface", 3.0),
  list("Brand teal as non-text on indigo",  "brand_teal", "brand_indigo", 3.0),
  # Input borders need 3:1 (WCAG 1.4.11). $fw-border is decorative, so absent.
  list("Input border on page",      "border_input", "page",    3.0),
  list("Input border on surface",   "border_input", "surface", 3.0)
)

cat(sprintf("%-26s %-10s %-10s %7s %6s  %s\n",
            "Pair", "Fore", "Back", "Ratio", "Need", "Result"))
cat(strrep("-", 78), "\n")

fails = 0
for (p in pairs) {
  ratio = contrast_ratio(pal[[p[[2]]]], pal[[p[[3]]]])
  pass = ratio >= p[[4]]
  if (!pass) fails = fails + 1
  cat(sprintf("%-26s %-10s %-10s %6.2f:1 %5.1f  %s\n",
              p[[1]], p[[2]], p[[3]], ratio, p[[4]],
              if (pass) "pass" else "FAIL"))
}

cat(strrep("-", 78), "\n")
if (fails > 0) {
  cat(fails, "pair(s) below the required ratio. Darken the foreground token.\n")
  quit(status = 1)
}
cat("All pairs meet WCAG AA.\n")
