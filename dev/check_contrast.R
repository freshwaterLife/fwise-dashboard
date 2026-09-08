# check_contrast.R
# Verifies every text/background pair in the palette against WCAG AA.
#
# Run it after changing any colour in www/scss/_tokens.scss:
#     Rscript dev/check_contrast.R
#
# This lives in dev/ because it is a build-time check, not part of the app.
# dev/ is gitignored, so if you want this in version control move it to R/ and
# it will still run. It is kept here deliberately so the client's deployment
# never has to load it.

relative_luminance <- function(hex) {
  rgb <- col2rgb(hex)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}

contrast_ratio <- function(a, b) {
  la <- relative_luminance(a); lb <- relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

pal <- c(
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

# Pairs the interface actually uses. Add a row when you introduce a new one.
pairs <- list(
  list("Body text on page",        "abyss",     "silt",    4.5),
  list("Body text on card",        "abyss",     "paper",   4.5),
  list("Muted text on page",       "ink_muted", "silt",    4.5),
  list("Muted text on card",       "ink_muted", "paper",   4.5),
  list("Heading on page",          "deep",      "silt",    4.5),
  list("Link on page",             "deep",      "silt",    4.5),
  # $fw-primary is the true sampled brand teal and does NOT meet 4.5:1 as text.
  # That is why text and button roles use $fw-deep instead. It is checked here at
  # the 3:1 non-text threshold, which is the standard it actually has to meet.
  list("Brand teal as non-text",   "primary",   "silt",    3.0),
  list("Primary button label",     "paper",     "deep",    4.5),
  list("Footer text on abyss",     "silt",      "abyss",   4.5),
  list("Footer muted on abyss",    "shoal",     "abyss",   4.5),
  list("Focus ring on page",       "shallow",   "silt",    3.0),
  list("Focus ring on card",       "shallow",   "paper",   3.0),
  # Input borders are interactive component boundaries under WCAG 1.4.11 and
  # need 3:1. The --fw-shoal hairline is a decorative divider and does not, so
  # it is deliberately absent from this list.
  list("Input border on page",     "line_input", "silt",   3.0),
  list("Input border on card",     "line_input", "paper",  3.0)
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
