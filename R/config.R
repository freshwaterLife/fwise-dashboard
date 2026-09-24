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

# THE THREE FILES THAT ARE THE DATA. One wide table of attempts and two lookups
# it references by id. Everything else in fwise-data/ is provenance, a standard,
# or the submissions inbox. Relative to the data root, because in production the
# same names are fetched over the GitHub API - see fw_data_path().
FW_ATTEMPTS_FILE <- "attempts.csv"
FW_SPECIES_FILE  <- "species.csv"
FW_CONTACTS_FILE <- "contacts.csv"

FW_ATTEMPTS_CSV <- file.path(FW_DATA_DIR, FW_ATTEMPTS_FILE)
FW_SPECIES_CSV  <- file.path(FW_DATA_DIR, FW_SPECIES_FILE)
FW_CONTACTS_CSV <- file.path(FW_DATA_DIR, FW_CONTACTS_FILE)

# ---- Multi-value cells -------------------------------------------------------

# The delimiter for every multi-value cell, in the data and in every export.
# Chosen over a comma because references, site names and notes are full of
# commas, and over the source's underscore because species names contain them
# far less predictably than they contain spaces. A semicolon never occurs in a
# species or method name in the data.
FW_MULTI_SEP <- "; "

# The delimiter between per-method notes in attempts.csv, where each entry is
# "Method name: note". A different character from FW_MULTI_SEP on purpose: 32 of
# the method notes contain a semicolon and none contain a pipe.
FW_NOTES_SEP <- " | "

# ---- Methods -----------------------------------------------------------------

# THE METHOD VOCABULARY. Seven values, fixed by the paper, so they live in code
# rather than in a table that would only ever hold these rows. attempts.csv
# stores the NAME; the id exists so anything keying on a method keys on
# something a rename cannot silently move, and `class` drives the conditional
# chemical-detail section on the contribute form.
#
# A method that arrives from a submission and is not listed here is still
# loaded - it gets an id derived from its name and the class "other" - and
# dev/qa.R flags it for the reviewer. Add it here once it is accepted.
FW_METHODS <- data.frame(
  method_id    = c("ME01", "ME02", "ME03", "ME04", "ME05", "ME06", "ME07"),
  method_name  = c("Antimycin-A", "Draining", "Electrofishing", "Netting / Trapping",
                   "Other chemical", "Other mechanical", "Rotenone"),
  method_class = c("chemical", "mechanical", "mechanical", "mechanical",
                   "chemical", "mechanical", "chemical"),
  stringsAsFactors = FALSE
)

# The two methods that are not methods. "Other chemical" and "Other mechanical"
# are the remainder of the list rather than a treatment anyone chose, so every
# chart that ranks methods pins these to the bottom whatever their counts - the
# client's standard, and the same rule fw_chart_category() applies to its own
# "Other" bar. Held as ids, not names: renaming either above must not quietly
# unpin it. See fw_chart_method() in charts.R.
FW_METHOD_OTHER <- c("ME05", "ME06")

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
# Point it at the directory holding attempts.csv and metadata.json. A trailing
# slash is tolerated.
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

# FW_METHOD_COLOURS AND FW_METHOD_LABEL_INK USED TO SIT HERE: a seven-step
# categorical palette keyed by method_id, and the ink each fill could carry a
# number in. They existed for one chart, the methods-by-waterbody stack, which
# the client deleted on 23 Sept 2026 - it was the only figure in the app that
# ever encoded method as colour. Every other chart segments by OUTCOME, and
# FW_OUTCOME_COLOURS above is the palette for that.
#
# dev/check_palette.R lost its method sections with them. Two of those pairs
# had never passed, and both faults were properties of that chart alone.
#
# Bring them back only with a chart that needs method-as-colour, and re-run
# dev/check_palette.R against it before shipping.

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

# ---- Logo files ---------------------------------------------------------------
#
# THE MARK IS FWISE-SIMPLE NOW, at the client's request, and it replaced both the
# badge in the navbar and the full FWISE-LOGO-ALL-6 lockup everywhere else - the
# lockup files are no longer in www/img. The report and the Word question list
# both skip a logo that is not there rather than failing, so while they still
# named the lockup they were quietly printing with no mark at all.
#
# DOWNSCALED COPIES, NEVER THE ORIGINALS. FWISE-SIMPLE.png and FWISE-BADGE.png
# are 8334px client assets and are not to be edited or regenerated; the -600,
# -1200 and -256 files are copies made beside them (sips -Z) so a page does not
# pull a megabyte to draw a mark a few hundred pixels wide.
#
# `_web` paths are what the browser asks for (Shiny serves www/ at the root);
# `_file` paths are read from disk by R, for the documents that embed the mark.
# The badge is still in use - it is the loader and the busy spinner, where a
# square mark turns on its own centre and a wordmark would not.
#
# EXCEPT THE NAVBAR AND THE FOOTER, which serve the full-resolution original
# at the client's request (September 2026): the navbar mark is 7rem tall and
# the client wants it drawn from the uncompressed file. The footer carried the
# badge until the client asked for the long SIMPLE wordmark there too; the
# full-size FWISE-BADGE.png stays in www/img but nothing serves it.
# ---- The busy badge ----------------------------------------------------------

# HOW LONG SOMETHING MUST TAKE BEFORE A SPINNER APPEARS, in milliseconds.
#
# ONE NUMBER, TWO CONSUMERS. bslib's busyIndicatorOptions() in app.R takes it
# for every output on the site, and the map overlay in fw_client_script() takes
# it for the two leaflet maps, which redraw through a proxy and so are outside
# bslib's reach entirely. If the two ever disagreed a map would spin while the
# charts beside it sat quiet, or the other way round.
#
# 150ms rather than the 400 it was (client, 24 Sept 2026): they asked for a
# spinner wherever something takes a moment, and at 400 a redraw that took a
# third of a second showed nothing at all. Still long enough that a redraw
# finishing within a couple of frames never flashes a badge at anyone.
FW_SPINNER_DELAY_MS <- 150

FW_LOGO <- list(
  mark_web   = "img/FWISE-SIMPLE.png",
  mark_file  = "www/img/FWISE-SIMPLE-1200.png",
  badge_web  = "img/FWISE-BADGE-256.png",
  # THE REST OF THE FOOTER'S LOGOS, as files, for the two documents a reader
  # takes away (the PDF report and the attempts .html). The client asked for
  # every logo the site carries to travel with the report, collaborators
  # included. The -400 files are sips -Z copies of the originals beside them,
  # which are not to be edited; the footer itself still serves the originals.
  # Named, and in the footer's order, because the documents print them in it.
  wfa_file   = "www/img/wfa-logo-rect-dark-320.png",
  collab_files = c(
    fwl     = "www/img/collab/FRESHWATER_LIFE-400.png",
    ucsc    = "www/img/collab/UCSC-400.png",
    scripps = "www/img/collab/UCSD_SCRIPPS-400.png",
    issg    = "www/img/collab/ISSG_SSC_IUCN-400.png"
  )
)

# The Welcome page's pictures. WEB COPIES of the client's originals in
# resources/, which are print-sized (3508px species plates, 12600px maps) and
# not served. How the copies were made is at the top of R/mod_home.R. One
# beneficiary picture per success story, keyed as FW_COPY$home$stories is (A-Z
# by continent); NA is a picture the client has not supplied yet and draws as a
# placeholder.
#
# BENEFICIARIES ONLY (client, Sept 2026): the story cards no longer show the
# invasive species. Its greyscale plates are client artwork and are kept in
# resources/success-pic/, but they are no longer served.
#
# TWO SETS, AND THE DIFFERENCE IS THE LETTERING. `stories` is the small
# unlettered drawing in the tile strip; `stories_named` is the same beneficiary
# with its common name and binomial hand-lettered in, which is what the pop-up
# shows once a tile is clicked. Both are web copies - the -named ones are built
# by dev/build_success_named.R, NOT the print originals they are named after,
# which stay in resources/success-pic/.
#
# The two sets spell their filenames differently - underscores for the
# thumbnails, hyphens for the lettered plates. That is how the client sent them
# and how they already sit in www/; the build script preserves it on purpose.
FW_HOME_IMG <- list(
  map_now  = "img/home/map-now.png",
  map_next = "img/home/map-next.png",
  stories = c(
    africa        = "img/home/success/africa_fiery_redfin.png",
    asia          = "img/home/success/as_little_grebe.png",
    europe        = "img/home/success/eu_pearl_mussel.png",
    latin_america = "img/home/success/la_valchetta_frog.png",
    north_america = "img/home/success/na_apache_trout.png",
    oceania       = "img/home/success/oc_golden_galaxias.png"
  ),
  stories_named = c(
    africa        = "img/home/success/africa-fiery-redfin-named.png",
    asia          = "img/home/success/as-little-grebe-named.png",
    europe        = "img/home/success/eu-pearl-mussel-named.png",
    latin_america = "img/home/success/la-valchetta-frog-named.png",
    north_america = "img/home/success/na-apache-trout-named.png",
    oceania       = "img/home/success/oc-golden-galaxias-named.png"
  )
)

# The order the pictures sit in on the Welcome page, set by the client: two
# columns of three, filled DOWN the left column first. So Apache trout, Valcheta
# frog, fiery redfin on the left; little grebe, golden galaxias, pearl mussel on
# the right. The grid flows by column (.fw-home-species-grid), so this is also
# the tab order.
FW_HOME_ORDER <- c("north_america", "latin_america", "africa",
                   "asia", "oceania", "europe")

# The species photo grids on the report builder, which show FEWER than FW_TOP_N.
# A tile is a photograph the size of a playing card, so ten of them ran to two
# full rows and pushed the rest of the report below the fold. It was five, and
# the client has since cut it to three. Each role is now one full-width row and
# the tiles split that width between them (see .fw-species-tiles), so this
# number is also the number of columns - three is what keeps each photograph
# large enough to recognise the animal without the row running on. The charts that rank into a top-n-plus-Other still
# use FW_TOP_N - a bar costs a line, not a photograph.
#
# NOTHING IN THE COPY COUNTS THIS OUT IN WORDS ANY MORE. The notes under the two
# grids used to be filled from it; they are gone, so this number can move again
# without a sentence to keep in step with it.
FW_PLAN_SPECIES_N <- 3L

# The country table in the PDF report. Longer than FW_TOP_N because a printed
# list is scanned rather than read off a bar.
FW_REPORT_COUNTRY_ROWS <- 15L

# THE PDF REPORT. Rendered by Quarto (Typst engine) on the server - see the
# header of R/report_pdf.R.
#
# THE WARNING. The client asked for one when the PDF would be very large, and
# measured, "large" here is mostly LONG: the whole database comes to about
# 2 MB but 25 pages, 19 of them the contacts table. So the picker warns when
# the estimate reaches `warn_pages` OR `warn_mb`, whichever comes first -
# a size cap alone would never fire on this database.
#
# THE ESTIMATE is fw_pdf_size_estimate(), whose coefficients below were fitted
# to real renders on 21 Sept 2026: the whole database (237 contacts, 911 map
# dots, 5 photographs: 2.00 MB, 25 pages), Norway (2, 200, 6: 1.29 MB, 7) and
# a single attempt (2, 1, 4: 0.78 MB, 6). dev/value_test.R re-renders those
# three and fails if the estimate drifts more than 30% from either figure.
#
# `timeout_s` is how long a render may take before it is abandoned with an
# error, rather than leaving the reader waiting on a download that will not
# come. `image_timeout_s` is the same for each species photograph fetched from
# Wikimedia; one that does not arrive in time prints as the page's placeholder.
FW_PDF <- list(
  warn_mb = 5,
  warn_pages = 20,
  timeout_s = 120,
  image_timeout_s = 8,
  # bytes: the fixed part (fonts, logos, charts), then per species photograph,
  # per map dot (the map's PNG grows with its dots, up to about 300 of them,
  # after which they overlap and add nothing), and per contact row.
  est_base = 460000,
  est_per_image = 80000,
  est_per_point = 1750,
  est_point_cap = 300,
  est_per_contact = 2600,
  # How many contacts the report prints. SIX, at the client's request (23 Sept
  # 2026): the busiest handful to write to, not a directory - the whole
  # directory is the Networking page, and the spreadsheet in the same download
  # carries a contact on every row.
  contacts_n = 6,
  # Pages. Fixed now that the contacts table is capped at contacts_n: it was
  # the one block whose length ran with the selection, and a broad filter used
  # to push the report past twenty pages on that table alone.
  #
  # SIX, DOWN FROM SEVEN (24 Sept 2026). The five caveat blocks came out of the
  # closing section - the client is writing their own - and the methods
  # statement that replaced them is a short one, so every report lost about a
  # page. Measured across the three selections dev/value_test.R renders: 8, 6
  # and 5 pages, which 6 covers and 7 no longer did. RE-MEASURE WHEN THE REAL
  # CAVEATS ARRIVE; they will push it back up.
  est_pages_base = 6,
  # A4, in mm. The width is what every figure is drawn to.
  page_margin_mm = 18,
  text_width_mm = 174
)

# Page sizes offered under paged tables. The first element is the default.
#
# THE CONTACTS DIRECTORY on the Networking page. That page IS the directory, so
# a reader arrives there to browse a list and 25 rows is a list; ten would be a
# pager with a table attached.
FW_CONTACTS_PAGE_SIZES <- c(25L, 50L, 100L)

# THE CONTACTS BLOCK ON THE REPORT BUILDER, which is a different question and
# now has its own sizes rather than borrowing the directory's. It is the eighth
# block of a long report and the reader is still reading about their own
# situation, so it opens at ten - a glance at who is worth writing to, with the
# pager and the "see every contact" link below for anyone who wants the rest.
# The client asked for ten specifically.
FW_PLAN_CONTACTS_PAGE_SIZES <- c(10L, 25L, 50L, 100L)

# The years a contributor may enter. Nothing before FW_YEAR_MIN is plausible,
# and an end year may run this many years past today for planned work.
FW_YEAR_MIN    <- 1500L
FW_YEAR_FUTURE <- 20L

# Above this many options a field's answer list is summarised in the question
# list downloads rather than printed in full. The country dropdown alone runs
# to about 200 entries and would bury the questions.
FW_QUESTION_OPTION_CAP <- 18L

# Maps.
FW_MAP <- list(
  # Where a map with nothing on it points: the whole world, centred a little
  # north of the equator, where most of the land is.
  empty_view = list(lng = 0, lat = 20, zoom = 2),
  # HOW FAR OUT A MAP MAY GO. Zoomed out further, the world is shorter than
  # the map and grey bars show above and below it. At zoom 2 the world is
  # 1024 px tall, taller than any map's CSS height. Panning is held inside
  # max_lat so the poles cannot be dragged into view either; max_lng is wide
  # on purpose, so the map still wraps round the world. See fw_leaflet().
  min_zoom = 2L,
  max_lat = 85,
  max_lng = 100000,
  # The closest a fit to a selection may zoom. Below cluster$fine_zoom, so a
  # single-site selection still opens with its stack as one counted group and
  # with enough of the surrounding water to place it. See fw_fit_points().
  fit_max_zoom = 10L,
  # Attempt markers. The stroke colour is FW_COLOURS$surface.
  marker = list(radius = 6, weight = 1.5, opacity = 1, fill_opacity = 0.75),
  # The no-JavaScript fallback popup. Kept in step with .fw-map-card's width
  # in _components.scss.
  popup = list(max_width = 320, min_width = 260),
  legend_opacity = 0.85,
  # STACKED MARKERS. 96 of 911 located attempts share their exact coordinates
  # with another, and a dot drawn on top of a dot is the only one that can be
  # reached. So markers are grouped - but only where they genuinely overlap.
  # Below `fine_zoom` the cluster radius is 0, which Leaflet.markercluster
  # reads as "identical coordinates only", so the coarse view keeps its
  # coloured dots and a stack shows as one group. From `fine_zoom` up, markers
  # within `fine_radius` px of each other group as well. A click on a group
  # zooms to it, and fans it out once its members cannot be separated by
  # zooming.
  #
  # ONE LOOK FOR A GROUP: a counted ring, `icon_size` px across, wide enough to
  # carry its number at the 1rem floor. There used to be two - below `fine_zoom`
  # a group was drawn as a plain indigo dot the size of a marker, on the
  # reasoning that a counted ring at world zoom piled thirty of them over
  # Norway. The client asked for the number everywhere: an unlabelled dot is
  # indistinguishable from a single attempt, which is the one thing a group must
  # not look like. If the world view ever does read as too busy, the answer is
  # to draw the ring smaller at coarse zoom, not to take the number off it.
  cluster = list(fine_zoom = 13L, fine_radius = 12L, icon_size = 28L)
)

# Charts.
FW_CHART <- list(
  # A chart's height grows with its number of rows, so eight methods are not
  # crushed into the space two would use: max(min, per_row * rows + pad).
  # `cumulative` is the one fixed-height chart.
  height = list(
    cumulative       = 360,
    # Fixed. A ring does not grow with its number of categories the way a bar
    # chart does - it only gets more crowded, which is what the hover is for.
    # 300 rather than the 360 it shared with the outcome donut: there is no
    # second ring to sit level with any more, and with nothing printed on the
    # slices it needs less room to stay legible.
    donut            = 300,
    method           = c(min = 250, per_row = 46, pad = 110),
    duration         = c(min = 270, per_row = 54, pad = 124),
    category         = c(min = 240, per_row = 34, pad = 120)
  ),
  # A count is printed inside a segment only when the segment holds at least
  # this share of its bar, in percent; narrower than that and the hover carries
  # it. The rem floor means a label cannot be shrunk to fit. THE STACKED BARS
  # ONLY - the dashboard's donut used to honour this too and now prints nothing
  # on the ring at all, at the client's request. See fw_chart_donut().
  label_min_share = 9,
  # The hole in the two proportion donuts, as a fraction of the radius. Big
  # enough to carry the denominator in the middle, which is the whole reason
  # these are donuts rather than pies: a percentage with no visible n behind it
  # is the thing the rest of this file exists to avoid.
  donut_hole = 0.55,
  # WHERE THE RING STOPS AND ITS KEY STARTS, as fractions of the plot width.
  # The donut carries its key beside it rather than above - a seven-entry method
  # key stacked over a small circle takes more height than the chart does - and
  # plotly does not reserve room for a legend placed inside the paper, so the
  # pie's own domain has to be narrowed to make it. The gap between the two is
  # the breathing room between the ring and the swatches.
  #
  # The ring takes a little more of the width than it used to (0.58/0.62), which
  # it can afford now that nothing is printed on the slices: the key no longer
  # competes with text inside the circle.
  donut_domain_x = 0.62,
  donut_legend_x = 0.66,
  # The floor for the top margin a horizontal legend needs, in px. NOT the
  # value - fw_legend_margin() computes that from how many lines the key will
  # actually wrap onto. This was a flat 42, which is exactly one line at the
  # 16px type floor with nothing to spare, so any chart whose key wrapped put
  # its second line on top of the plot. See fw_plotly_style() in R/charts.R.
  legend_min_top = 46,
  # A legend line's height and the gap under the whole key, in px. Derived from
  # the type floor rather than typed twice.
  legend_line = 22,
  legend_pad  = 12,
  # Roughly how many px a legend entry takes per character at the type floor,
  # plus the swatch and the gap between entries. Used only to guess how many
  # entries fit on a line; plotly does the real layout.
  legend_char_px  = 8.2,
  legend_entry_px = 42,
  # The rule between stacked segments, in px. 0 for the outcome charts: the
  # four Wong colours separate on their own and the rule read as aggressive.
  #
  # separator_method WENT WITH THE METHODS-BY-WATERBODY CHART (client, 23 Sept
  # 2026). It was the hairline between two adjacent method fills, and that was
  # the only chart that ever stacked method against method.
  separator_outcome = 0,
  # The weight of the vertical rules on the stacked bar charts, in px. A
  # hairline: unlike the duration chart's dotted breaks below, these are solid
  # and are drawn OVER the bars (fw_bar_rules() in charts.R), so they need no
  # extra weight to be seen and would read as stripes if they had any.
  bar_grid = 1,
  # How far apart the success-rate axis's ticks sit, in percentage points. Set
  # rather than left to plotly, because the rules over the bars are drawn at
  # these same values and a rule that missed its label would be worse than no
  # rule at all. 20 is what plotly chose for 0-100 anyway.
  share_dtick = 20,
  # The FIXED named ticks on the duration chart's log axis, in days. The labels
  # are fw_t("charts", "duration_ticks") and must stay the same length.
  #
  # NOT THE WHOLE SET ANY MORE: fw_duration_ticks() drops the ones past the
  # selection's longest attempt and adds a seventh at that attempt, so the axis
  # labels reach the last dot. These are the breaks below it.
  duration_ticks = c(1, 7, 30, 365, 1825, 3650),
  # How close a fixed tick may come to that terminal one before it gives way,
  # in log10 days. 0.08 is about a fifth of the gap between two of the breaks
  # above, which is enough room for two labels not to overprint.
  duration_tick_gap = 0.08,
  # The weight of the dotted unit-break gridlines on that axis, in px. See the
  # xaxis comment in fw_chart_duration() for why a dotted line needs this much.
  duration_grid = 2,
  # duration_bands AND duration_band_alpha USED TO SIT HERE - the edges and the
  # tint of the shaded magnitude bands behind the duration chart. The client
  # replaced them with a dotted gridline on each unit break, which the axis
  # draws from duration_ticks above, so neither has a reader any more.
  # The breathing room either side of the duration chart's data, as a fraction
  # of the span it covers, with a floor in log10 units for a selection that
  # spans almost nothing. See fw_duration_range() - this axis sets its own
  # bounds because plotly's autorange for a box trace on a log scale does not.
  duration_pad = 0.04,
  duration_pad_min = 0.05,
  # The dots on the duration chart, and the box under them.
  point    = list(size = 7, opacity = 0.75, stroke = 1),
  box_line = 1.5,
  # The step line on the cumulative chart.
  line = 1,
  # The gap between an axis and its tick labels, in px. Made by invisible
  # outside ticks of this length - see fw_tick_gap(). The axis-line A/B test
  # that used to sit here ended with the client choosing bare axes (Sept 2026).
  tick_gap = 8,
  # The duration chart's dots are spread across their row rather than drawn on
  # one line, so a pile of identical durations shows as a column you can count.
  # `bin` is how close two durations have to be (in log10 days) to count as
  # the same place; `step` is the vertical gap between neighbours and `spread`
  # the furthest a dot may sit from its row's centre, both in rows.
  duration_swarm = list(bin = 0.035, step = 0.07, spread = 0.38),
  # PNG exports print at this density: plotly's scale is export px per screen
  # px, and a screen px is 1/96 inch, so dpi / 96 is the scale. Raise it here
  # if an export comes out soft.
  export_dpi = 300
)
