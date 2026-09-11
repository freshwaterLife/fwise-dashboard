# config.R
# Central place for paths, environment variables, the data palettes and the
# behaviour constants. If you need to change where data comes from, where
# submissions go, how many items a chart names or where an empty map points,
# change it here rather than hunting through the modules.
#
# Design values (colour, type, spacing, radius, shadow) are NOT here: they live
# in R/brand.R. This file reads FW_COLOURS from there, which is safe because
# Shiny sources R/ in byte order and brand.R comes first.

# ---- Paths -------------------------------------------------------------------

# THE DATA LIVES IN A SEPARATE REPOSITORY. fwise-data/ is a sibling of this
# directory, not a subdirectory of it, so that the client can publish new data by
# pushing to that repository without touching, rebuilding or redeploying the app.
#
# Nothing in here should ever assume the two are checked out together beyond the
# development default below. In production the data arrives over the GitHub API
# instead, and submissions are written back the same way. See fw_data_mode() in
# data_load.R and R/github.R.

FW_ROOT       <- getwd()
FW_DEV_DIR    <- file.path(FW_ROOT, "dev")

# The development default: the sibling checkout. Overridden by FWISE_DATA_SOURCE.
FW_DATA_DIR   <- normalizePath(file.path(FW_ROOT, "..", "fwise-data"),
                               mustWork = FALSE)
FW_SCHEMA_DIR <- file.path(FW_DATA_DIR, "schema")

# The raw flat export from the client. data_prep.R reads this and never writes to
# it. Update the filename when a newly cleaned export is dropped in.
FW_SOURCE_CSV <- file.path(FW_DATA_DIR, "fwise_2026-09-06.csv")

# ---- Environment -------------------------------------------------------------

# Small helper so a missing or empty environment variable behaves the same way.
fw_env <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) default else value
}

# WHERE THE DATA COMES FROM.
#
# THE DATA REPOSITORY IS A CONSTANT, NOT A SETTING. It is private, it is not
# going to move, and its name is not a secret. Making it something the client has
# to type into a deployment console would add a way to get the app wrong without
# adding anything you can do with it.
FW_DATA_REPO <- "freshwaterLife/fwise-data"
FW_DATA_REF  <- "main"

# THE ONE SECRET, and the switch. Set FWISE_DATA_TOKEN and the app reads the
# repository above over the GitHub API and writes submissions back to it. Leave
# it unset and everything stays on the local sibling checkout, making no network
# calls at all - a developer on a train gets the same app as a developer at a
# desk, and a GitHub outage does not stop local work.
#
# It is a fine-grained personal access token with Contents: Read and write,
# resource owner freshwaterLife, scoped to that one repository. IT EXPIRES, and
# when it does the app stops reading data as well as accepting submissions.
FWISE_DATA_TOKEN <- fw_env("FWISE_DATA_TOKEN", default = NULL)

# THE OVERRIDE, and it is not needed in production. Set it to point a test deploy
# at a fork, a branch or a local path without touching code. Three shapes, all
# resolved by fw_data_mode() in data_load.R:
#
#   unset            the token decides - API if it is set, ../fwise-data/ if not
#   /some/path       that directory, read as files
#   https://...      that base URL, read over plain HTTPS with NO credential
#
# Point it at the directory holding metadata.json and schema/, not at schema/
# itself. A trailing slash is tolerated.
FWISE_DATA_SOURCE <- fw_env("FWISE_DATA_SOURCE", default = NULL)

# ---- Data visualisation palette ----------------------------------------------

# Wong (2011) colourblind-safe palette. The associated academic paper uses this,
# so the app must match it for figures to stay consistent across the paper, the
# app and exported reports.
#
# These are DATA colours. Never use them as interface chrome, and never use the
# interface teals to encode data.
FW_PALETTE <- c(
  successful = "#009E73",
  failed     = "#E69F00",
  ongoing    = "#56B4E9",
  unknown    = "#CC79A7",
  neutral    = "#999999",
  emphasis   = "#0072B2",
  vermilion  = "#D55E00"
)

# Outcome values as they appear in the data, mapped to palette keys. Keeping the
# mapping explicit means a renamed outcome category fails loudly here rather than
# silently falling through to grey.
FW_OUTCOME_COLOURS <- c(
  "Successful" = unname(FW_PALETTE["successful"]),
  "Failed"     = unname(FW_PALETTE["failed"]),
  "Ongoing"    = unname(FW_PALETTE["ongoing"]),
  "Unknown"    = unname(FW_PALETTE["unknown"])
)

# The count that sits inside each outcome segment. ALL FOUR ARE INDIGO, and
# that is a measured result rather than a style choice: white numerals came to
# 2.25:1 on the Failed orange and 2.31:1 on the Ongoing blue, so the figure the
# reader is meant to read off the bar was the least legible thing on the page.
# None of the four Wong colours is dark enough to take white at 4.5:1; every one
# of them clears it with indigo.
#
# Kept as a named vector rather than one constant so that dev/check_palette.R
# can check them one by one, and so a future palette change fails loudly here.
FW_OUTCOME_LABEL_INK <- c(
  "Successful" = FW_COLOURS$ink,
  "Failed"     = FW_COLOURS$ink,
  "Ongoing"    = FW_COLOURS$ink,
  "Unknown"    = FW_COLOURS$ink
)

# Methods, for the one chart that segments by method rather than by outcome.
#
# SEVEN DISTINCT HUES. This was a single-hue sequential ramp - dark to light
# teal, ordered by how often each method appears. THE CLIENT REJECTED IT, and
# they were right: method is a nominal category, not a magnitude, and a ramp
# tells a reader the segments are ordered when they are not. Measured, the old
# ramp failed outright - its worst adjacent pair came to dE 9.0 against a floor
# of 15 under normal vision, so neighbouring segments genuinely were not
# separable.
#
# WHERE THESE VALUES COME FROM. Paul Tol's "muted" qualitative palette, which is
# designed for colour-vision deficiency, with each hue then stepped into the
# usable lightness band (OKLCH L 0.43-0.77) and lifted over the chroma floor
# (C >= 0.10) so that none of them reads as grey or vanishes against white. Tol's
# teal slot is deliberately NOT used: the brand teal is interface chrome and
# must never encode data.
#
# THE ORDER IS LOAD-BEARING, TWICE OVER. It is still frequency order, so the
# stack reads most-used first. It is ALSO the order that maximises separation
# between segments that physically touch: of all 5040 arrangements of these
# seven colours, this one gives the best worst-adjacent-pair distance. Reordering
# the entries re-colours the chart AND degrades it. Measured on this order:
#
#   adjacent pairs   worst dE 12.5 CVD / 23.0 normal   (floors 8 and 15)  PASS
#   all pairs        worst dE  2.7 CVD / 12.4 normal                      fails
#
# The all-pairs figure is expected and is not a defect to fix by re-picking
# colours: seven categories cannot be made pairwise-distinct at that floor by
# any palette. It only bites where two NON-neighbouring segments end up touching,
# which needs an intervening method to be absent from that waterbody. The white
# separator line and the in-segment counts in fw_chart_method_waterbody() are
# what carry that case, which is why both are mandatory rather than decoration.
#
# Keyed by method_id because those are fixed (ME01-ME07 in method.csv) and a
# renamed method must not silently re-colour the chart.
#
# Re-check with dev/check_palette.R after touching any value or the order.
FW_METHOD_COLOURS <- c(
  "ME07" = "#007da4",   # Rotenone            - blue
  "ME04" = "#a58a22",   # Netting / Trapping  - sand
  "ME02" = "#8e2a72",   # Draining            - wine
  "ME03" = "#3f9b3f",   # Electrofishing      - green
  "ME01" = "#8c4a1f",   # Antimycin-A         - brown
  "ME05" = "#534bb4",   # Other chemical      - indigo
  "ME06" = "#cf5f6f"    # Other mechanical    - rose
)

# The count that sits inside each segment, per method. Four of the seven fills
# are dark enough to take white; three are not, and white on the sand slot came
# to 3.36:1 - under the floor for text that the reader is expected to read a
# NUMBER off. Every pair below is >= 4.5:1 against its own fill.
FW_METHOD_LABEL_INK <- c(
  "ME07" = FW_COLOURS$surface,
  "ME04" = FW_COLOURS$ink,
  "ME02" = FW_COLOURS$surface,
  "ME03" = FW_COLOURS$ink,
  "ME01" = FW_COLOURS$surface,
  "ME05" = FW_COLOURS$surface,
  "ME06" = FW_COLOURS$ink
)

# ---- Constants ---------------------------------------------------------------

# Coordinates are shown and stored at six decimal places, roughly 0.1m. More
# precision than that is false confidence for a treated waterbody.
FW_COORD_DP <- 6

# ---- Behaviour ---------------------------------------------------------------
# Numbers that shape what the reader sees but are neither design tokens (those
# are in R/brand.R) nor data. Every consumer reads these; nothing restates them.

# "The ten most common." Every chart and tile grid that names the top n and
# gathers the rest into Other. The copy that says "ten" in words is filled from
# this as well, so changing it here changes the sentence under the chart.
FW_TOP_N <- 10L

# The country table in the HTML report. Longer than FW_TOP_N because a printed
# list is scanned rather than read off a bar.
FW_REPORT_COUNTRY_ROWS <- 15L

# How many attempts the HTML report lists. Everything: the reader scrolls, and
# the print rules repeat the header row across pages. The old Word export
# capped at 40 because Word could not repaginate a 900-row table.
FW_HTML_TABLE_ROWS <- Inf

# Page sizes offered under paged tables. The first element is the default. The
# report builder is read a screen at a time, so it starts smaller than the
# contacts directory.
FW_PLAN_PAGE_SIZES     <- c(10L, 20L, 50L, 100L)
FW_CONTACTS_PAGE_SIZES <- c(25L, 50L, 100L)

# The years a contributor may enter. Nothing before FW_YEAR_MIN is plausible,
# and an end year may run this many years past today for planned work.
FW_YEAR_MIN    <- 1500L
FW_YEAR_FUTURE <- 20L

# How many items of a multi-value cell (species, methods) the results table
# shows before "+n more". The export always carries the full list.
FW_TABLE_CELL_ITEMS <- 2L

# Above this many options a field's answer list is summarised in the question
# list downloads rather than printed in full. The country dropdown alone runs
# to about 200 entries and would bury the questions.
FW_QUESTION_OPTION_CAP <- 18L

# Maps.
FW_MAP <- list(
  # Where a map with nothing on it points: the whole world, centred a little
  # north of the equator, where most of the land is.
  empty_view = list(lng = 0, lat = 20, zoom = 2),
  # Attempt markers. The stroke colour is FW_COLOURS$surface.
  marker = list(radius = 6, weight = 1.5, opacity = 1, fill_opacity = 0.75),
  # The no-JavaScript fallback popup. Kept in step with .fw-map-card's width
  # in _components.scss.
  popup = list(max_width = 320, min_width = 260),
  legend_opacity = 0.85
)

# Charts.
FW_CHART <- list(
  # A chart's height grows with its number of rows, so eight methods are not
  # crushed into the space two would use: max(min, per_row * rows + pad).
  # `cumulative` is the one fixed-height chart.
  height = list(
    cumulative       = 360,
    method           = c(min = 250, per_row = 46, pad = 110),
    duration         = c(min = 270, per_row = 54, pad = 124),
    category         = c(min = 240, per_row = 34, pad = 120),
    method_waterbody = c(min = 240, per_row = 40, pad = 120)
  ),
  # A count is printed inside a segment only when the segment holds at least
  # this share of its bar, in percent; narrower than that and the hover carries
  # it. The rem floor means a label cannot be shrunk to fit.
  label_min_share = 9,
  # The rule between stacked segments, in px. 0 for the outcome charts: the
  # four Wong colours separate on their own and the rule read as aggressive.
  # The method chart keeps a hairline, because its adjacent-segment case under
  # colour-vision deficiency relies on one (see FW_METHOD_COLOURS above).
  separator_outcome = 0,
  separator_method  = 0.5,
  # The named ticks on the duration chart's log axis, in days. The labels are
  # fw_t("charts", "duration_ticks") and must stay the same length.
  duration_ticks = c(1, 7, 30, 365, 1825, 3650),
  # The dots on the duration chart, and the box under them.
  point    = list(size = 7, opacity = 0.75, stroke = 1),
  box_line = 1.5,
  # The step line on the cumulative chart.
  line = 1
)
