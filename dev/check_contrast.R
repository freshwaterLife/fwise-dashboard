# check_contrast.R
# Verifies every text/background pair in the palette against WCAG AA.
#
# Run it after changing any colour in R/brand.R:
#     Rscript dev/check_contrast.R
#
# This lives in dev/ because it is a build-time check, not part of the app. The
# rest of dev/ is gitignored but this file is re-included, so it travels with
# the code; it is kept out of R/ so the deployment never loads it.

relative_luminance <- function(hex) {
  rgb <- col2rgb(hex)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}

contrast_ratio <- function(a, b) {
  la <- relative_luminance(a); lb <- relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

# THE APP'S OWN PALETTE, not a copy of it. R/brand.R has no dependencies, so
# it can be sourced here on its own.
source("R/brand.R")
pal <- unlist(FW_COLOURS)

# Pairs the interface actually uses. Add a row when you introduce a new one.
# 4.5 is the AA floor for text; 3.0 is the floor for a non-text element that
# carries meaning (a focus ring, an input edge, an active marker).
pairs <- list(
  list("Body text on page",         "ink",        "page",         4.5),
  list("Body text on surface",      "ink",        "surface",      4.5),
  list("Body text on sunken",       "ink",        "sunken",       4.5),
  list("Muted text on page",        "ink_muted",  "page",         4.5),
  list("Muted text on surface",     "ink_muted",  "surface",      4.5),
  list("Muted text on sunken",      "ink_muted",  "sunken",       4.5),  # Welcome picture placeholders
  list("Link on page",              "teal_text",  "page",         4.5),
  list("Link on surface",           "teal_text",  "surface",      4.5),
  list("Link on sunken",            "teal_text",  "sunken",       4.5),
  list("Link on teal wash",         "teal_text",  "teal_wash",    4.5),
  list("Ink on teal tint",          "ink",        "teal_tint",    4.5),
  list("Primary button label",      "surface",    "teal_text",    4.5),
  list("Primary button hover",      "surface",    "teal_hover",   4.5),
  list("Text on indigo",            "on_indigo",  "brand_indigo", 4.5),
  list("Muted text on indigo",      "on_indigo_muted", "brand_indigo", 4.5),
  list("Link on indigo",            "teal_light", "brand_indigo", 4.5),
  # $fw-brand-teal is the client's brand teal and does NOT meet 4.5:1 as text.
  # That is why text and button roles use teal_text. It is checked here at the
  # 3:1 non-text threshold, which is the standard it actually has to meet: the
  # active nav marker, the KPI rule, the focus ring.
  list("Brand teal as non-text on page",    "brand_teal", "page",    3.0),
  list("Brand teal as non-text on surface", "brand_teal", "surface", 3.0),
  list("Brand teal as non-text on indigo",  "brand_teal", "brand_indigo", 3.0),
  # Input borders are interactive component boundaries under WCAG 1.4.11 and
  # need 3:1. $fw-border is a decorative divider and does not, so it is
  # deliberately absent from this list.
  list("Input border on page",      "border_input", "page",    3.0),
  list("Input border on surface",   "border_input", "surface", 3.0)
)

cat(sprintf("%-26s %-10s %-10s %7s %6s  %s\n",
            "Pair", "Fore", "Back", "Ratio", "Need", "Result"))
cat(strrep("-", 78), "\n")

fails <- 0
for (p in pairs) {
  ratio <- contrast_ratio(pal[[p[[2]]]], pal[[p[[3]]]])
  pass <- ratio >= p[[4]]
  if (!pass) fails <- fails + 1
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
