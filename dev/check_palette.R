# check_palette.R
# Verifies the DATA palettes - the ones that encode a value in a chart. The
# interface palette is a different question and lives in dev/check_contrast.R.
#
#     Rscript dev/check_palette.R
#
# It computes, rather than eyeballs, four things about a categorical palette:
# that every colour sits in a usable lightness band, that none of them is so
# desaturated it reads as grey, that neighbouring colours stay apart under
# simulated colour-vision deficiency AND under normal vision, and that each one
# is visible against the surface it is drawn on.
#
# WHY IT EXISTS. The method palette was a single-hue ramp until the client said
# the segments were not distinguishable. They were right, and this script is how
# that was established rather than argued about: the old ramp scored dE 9.0 on
# its worst adjacent pair against a floor of 15. Run it before changing any
# colour in FW_METHOD_COLOURS or FW_PALETTE.
#
# PAIRLISTS. A stacked bar only ever butts a segment against its NEIGHBOURS, so
# "adjacent" is the pairlist it is judged on, and the order of FW_METHOD_COLOURS
# is part of the result. "all" is the harder test used for scatter plots and
# maps, where any two marks can end up side by side; seven categories cannot
# pass it, which is a property of seven categories and not of these seven
# colours.
#
# Method and thresholds are from Claude's data-visualisation skill; this is an R
# port of its validate_palette.js (checks 2-5), same Machado-Oliveira-Fernandes
# (2009) severity-1.0 simulation, verified to reproduce that script's published
# figures for its own reference palette.
#
# Lives in dev/ because it is a build-time check, not part of the app.

BAND <- list(light = c(0.43, 0.77), dark = c(0.48, 0.67))
CHROMA_FLOOR <- 0.10
CVD_TARGET <- 8.0; CVD_FLOOR <- 6.0
NORMAL_FLOOR <- 15.0
CONTRAST_MIN <- 3.0

MACHADO <- list(
  protan = matrix(c(0.152286, 1.052583, -0.204868,
                    0.114503, 0.786281,  0.099216,
                   -0.003882,-0.048116,  1.051998), 3, 3, byrow = TRUE),
  deutan = matrix(c(0.367322, 0.860646, -0.227968,
                    0.280085, 0.672501,  0.047413,
                   -0.011820, 0.042940,  0.968881), 3, 3, byrow = TRUE),
  tritan = matrix(c(1.255528,-0.076749, -0.178779,
                   -0.078411, 0.930809,  0.147602,
                    0.004733, 0.691367,  0.303900), 3, 3, byrow = TRUE))

hex2srgb <- function(h) { h <- sub("^#", "", trimws(h))
  as.numeric(strtoi(substring(h, c(1,3,5), c(2,4,6)), 16L)) / 255 }
s2lin <- function(c) ifelse(c <= 0.04045, c/12.92, ((c + 0.055)/1.055)^2.4)
lin   <- function(h) s2lin(hex2srgb(h))
relLum <- function(h) sum(c(0.2126, 0.7152, 0.0722) * lin(h))
contrast <- function(a, b) { l <- sort(c(relLum(a), relLum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05) }

oklabFromLin <- function(rgb) {
  l <- (0.4122214708*rgb[1] + 0.5363325363*rgb[2] + 0.0514459929*rgb[3])^(1/3)
  m <- (0.2119034982*rgb[1] + 0.6806995451*rgb[2] + 0.1073969566*rgb[3])^(1/3)
  s <- (0.0883024619*rgb[1] + 0.2817188376*rgb[2] + 0.6299787005*rgb[3])^(1/3)
  c(0.2104542553*l + 0.7936177850*m - 0.0040720468*s,
    1.9779984951*l - 2.4285922050*m + 0.4505937099*s,
    0.0259040371*l + 0.7827717662*m - 0.8086757660*s)
}
oklab <- function(h) oklabFromLin(lin(h))
oklch <- function(h) { v <- oklab(h); c(L = v[1], C = sqrt(v[2]^2 + v[3]^2)) }
simulate_cvd <- function(h, kind) pmin(1, pmax(0, as.numeric(MACHADO[[kind]] %*% lin(h))))
deltaE <- function(h1, h2, kind = NULL) {
  a <- oklabFromLin(if (is.null(kind)) lin(h1) else simulate_cvd(h1, kind))
  b <- oklabFromLin(if (is.null(kind)) lin(h2) else simulate_cvd(h2, kind))
  100 * sqrt(sum((a - b)^2))
}

validate <- function(palette, mode = "light", surface = NULL, pairs = "adjacent") {
  if (is.null(surface)) surface <- if (mode == "light") "#fcfcfb" else "#1a1a19"
  band <- BAND[[mode]]; n <- length(palette); ok <- TRUE

  L <- vapply(palette, function(c) oklch(c)[["L"]], 0)
  C <- vapply(palette, function(c) oklch(c)[["C"]], 0)
  off <- which(L < band[1] | L > band[2])
  if (length(off)) ok <- FALSE
  cat(sprintf("2. Lightness band   %s  %s\n", if (!length(off)) "PASS" else "FAIL",
    if (!length(off)) sprintf("all %d inside L %.2f-%.2f", n, band[1], band[2])
    else paste(sprintf("%s L=%.3f", palette[off], L[off]), collapse = ", ")))

  lowc <- which(C < CHROMA_FLOOR)
  if (length(lowc)) ok <- FALSE
  cat(sprintf("3. Chroma floor     %s  %s\n", if (!length(lowc)) "PASS" else "FAIL",
    if (!length(lowc)) sprintf("all %d >= %.2f", n, CHROMA_FLOOR)
    else paste(sprintf("%s C=%.3f", palette[lowc], C[lowc]), collapse = ", ")))

  pl <- if (identical(pairs, "all")) t(combn(n, 2)) else cbind(seq_len(n - 1), seq_len(n - 1) + 1)
  worst_cvd <- Inf; worst_cvd_pair <- NULL; worst_nrm <- Inf; worst_nrm_pair <- NULL
  for (k in seq_len(nrow(pl))) {
    i <- pl[k, 1]; j <- pl[k, 2]
    d <- min(deltaE(palette[i], palette[j], "protan"), deltaE(palette[i], palette[j], "deutan"))
    if (d < worst_cvd) { worst_cvd <- d; worst_cvd_pair <- c(i, j) }
    dn <- deltaE(palette[i], palette[j])
    if (dn < worst_nrm) { worst_nrm <- dn; worst_nrm_pair <- c(i, j) }
  }
  cvd_status <- if (worst_cvd >= CVD_TARGET) "PASS" else if (worst_cvd >= CVD_FLOOR) "WARN" else "FAIL"
  if (cvd_status == "FAIL") ok <- FALSE
  cat(sprintf("4. CVD separation   %s  worst %s pair %s/%s dE=%.1f (target %.0f, floor %.0f)\n",
    cvd_status, pairs, palette[worst_cvd_pair[1]], palette[worst_cvd_pair[2]], worst_cvd, CVD_TARGET, CVD_FLOOR))

  nrm_status <- if (worst_nrm >= NORMAL_FLOOR) "PASS" else "FAIL"
  if (nrm_status == "FAIL") ok <- FALSE
  cat(sprintf("4b. Normal vision   %s  worst pair %s/%s dE=%.1f (floor %.0f)\n",
    nrm_status, palette[worst_nrm_pair[1]], palette[worst_nrm_pair[2]], worst_nrm, NORMAL_FLOOR))

  cr <- vapply(palette, contrast, 0, b = surface)
  low <- which(cr < CONTRAST_MIN)
  cat(sprintf("5. Contrast vs %s %s  %s\n", surface, if (!length(low)) "PASS" else "WARN",
    if (!length(low)) sprintf("all >= %.1f:1 (min %.2f)", CONTRAST_MIN, min(cr))
    else paste(sprintf("%s %.2f:1", palette[low], cr[low]), collapse = ", ")))
  cat(if (ok) "=> no hard FAIL\n" else "=> HARD FAIL\n")
  invisible(ok)
}


# ---- The app's palettes ------------------------------------------------------

source("R/brand.R"); source("R/config.R")

cat("\n================ OUTCOME palette (Wong 2011) ================\n")
cat("Fixed by the associated paper. Checked, not tuned.\n\n")
validate(unname(FW_OUTCOME_COLOURS), "light", FW_COLOURS$surface)

# THE METHOD PALETTE USED TO BE CHECKED HERE. It coloured one chart, the
# methods-by-waterbody stack, which the client deleted on 23 Sept 2026 - it was
# the only figure in the app that ever encoded method as colour. Two of its
# seven fills had never passed the all-pairs test, and both faults were
# properties of that chart alone, so they went with it.
#
# FW_METHOD_COLOURS and FW_METHOD_LABEL_INK are gone from R/config.R; see the
# note where they were. Bring this section back with any chart that needs
# method-as-colour.

cat("\n================ In-segment label ink ================\n")
cat("The count drawn inside each segment, against that segment's own fill.\n")
cat("4.5:1, because the reader is expected to read a NUMBER off it.\n\n")
ink_ok <- TRUE
for (o in names(FW_OUTCOME_COLOURS)) {
  r <- contrast(FW_OUTCOME_LABEL_INK[[o]], FW_OUTCOME_COLOURS[[o]])
  cat(sprintf("  %-11s %s on %s  %5.2f:1  %s\n", o, FW_OUTCOME_LABEL_INK[[o]],
              FW_OUTCOME_COLOURS[[o]], r, if (r >= 4.5) "pass" else "FAIL"))
  if (r < 4.5) ink_ok <- FALSE
}
cat("\n================ The printed map ================\n")
cat("The PDF report's own green and blue, which are NOT the app's surfaces.\n")
cat("They sit UNDER the outcome dots drawn on them, so the test is that each\n")
cat("outcome colour still separates from both grounds.\n\n")
map_ok <- TRUE
for (g in c("land", "water")) {
  for (o in names(FW_OUTCOME_COLOURS)) {
    de <- deltaE(FW_OUTCOME_COLOURS[[o]], FW_MAP_PRINT[[g]])
    cat(sprintf("  %-11s on %-5s  %s vs %s  dE=%4.1f  %s\n", o, g,
                FW_OUTCOME_COLOURS[[o]], FW_MAP_PRINT[[g]], de,
                if (de >= 15) "pass" else "FAIL"))
    if (de < 15) map_ok <- FALSE
  }
}

cat("\n")
if (!map_ok) {
  cat("FAIL: an outcome dot does not separate from the map it sits on.\n")
  cat("Lighten FW_MAP_PRINT$land or $water in R/brand.R.\n")
  quit(status = 1)
}
if (!ink_ok) {
  cat("FAIL: a segment label is under 4.5:1 on its own fill.\n")
  cat("Flip that method's entry in FW_METHOD_LABEL_INK, or step the fill.\n")
  quit(status = 1)
}
cat("Every palette passes the pairlist that applies to it.\n")
