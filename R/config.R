# config.R
# Paths, environment variables, data palettes and behaviour constants.
# Design values (colour, type, space) live in R/brand.R, which sources first.

# ---- Paths -------------------------------------------------------------------

# The data is a separate repository (../fwise-data locally, the GitHub API in
# production) so the client can publish data without redeploying. Submissions
# go to a Google Sheet, not here. See fw_data_mode() and R/github.R.

FW_ROOT       = getwd()
FW_DEV_DIR    = file.path(FW_ROOT, "dev")

# Local default: the sibling checkout. Overridden by FWISE_DATA_SOURCE.
FW_DATA_DIR   = normalizePath(file.path(FW_ROOT, "..", "fwise-data"),
                               mustWork = FALSE)

# The three data files, relative to the data root (also fetched by these names
# over the API, see fw_data_path()).
FW_ATTEMPTS_FILE = "attempts.csv"
FW_SPECIES_FILE  = "species.csv"
FW_CONTACTS_FILE = "contacts.csv"

# Local paths, used by the dev scripts.
FW_ATTEMPTS_CSV = file.path(FW_DATA_DIR, FW_ATTEMPTS_FILE)
FW_SPECIES_CSV  = file.path(FW_DATA_DIR, FW_SPECIES_FILE)
FW_CONTACTS_CSV = file.path(FW_DATA_DIR, FW_CONTACTS_FILE)

# ---- Multi-value cells -------------------------------------------------------

# Separator for every multi-value cell, in the data and exports. Never occurs in
# a species or method name.
FW_MULTI_SEP = "; "

# Separator between per-method notes ("Method: note"). A pipe because some notes
# contain semicolons.
FW_NOTES_SEP = " | "

# ---- Methods -----------------------------------------------------------------

# The seven methods, fixed by the paper. attempts.csv stores the name; the id is
# a stable key, and `class` drives the chemical-detail section of the form. An
# unlisted method still loads (class "other") and dev/qa.R flags it.
FW_METHODS = data.frame(
  method_id    = c("ME01", "ME02", "ME03", "ME04", "ME05", "ME06", "ME07"),
  method_name  = c("Antimycin-A", "Draining", "Electrofishing", "Netting / Trapping",
                   "Other chemical", "Other mechanical", "Rotenone"),
  method_class = c("chemical", "mechanical", "mechanical", "mechanical",
                   "chemical", "mechanical", "chemical"),
  stringsAsFactors = FALSE
)

# "Other chemical" and "Other mechanical": always pinned to the bottom of method
# rankings. Ids, so a rename can't unpin them. See fw_chart_method().
FW_METHOD_OTHER = c("ME05", "ME06")

# ---- Environment -------------------------------------------------------------

# Treats a missing and an empty environment variable the same.
fw_env = function(name, default = NULL) {
  value = Sys.getenv(name, unset = "")
  if (identical(value, "")) default else value
}

# The data repository and branch. Constants, not settings: neither is secret
# or varies.
FW_DATA_REPO = "freshwaterLife/fwise-data"
FW_DATA_REF  = "main"

# Fine-grained GitHub token, Contents: Read on the repo above. Set = read over
# the API; unset = local sibling checkout, no network. It expires, and then the
# app stops reading data.
FWISE_DATA_TOKEN = fw_env("FWISE_DATA_TOKEN", default = NULL)

# Test-deploy override, not needed in production (see fw_data_mode()):
#   unset        the token decides
#   /some/path   read that directory
#   https://...  read that base URL, no credential
# Point it at the directory holding attempts.csv and metadata.json.
FWISE_DATA_SOURCE = fw_env("FWISE_DATA_SOURCE", default = NULL)

# Newsletter and feedback sheet. Both must be set, otherwise the forms write to
# dev/forms/ (see fw_forms_mode()).
#   FWISE_FORMS_SHEET_ID  the id in the sheet URL, between /d/ and /edit
#   GS4_SA_KEY_B64        service account JSON key, raw or base64
# Share the sheet with the key's client_email as an Editor.
FWISE_FORMS_SHEET_ID   = fw_env("FWISE_FORMS_SHEET_ID", default = NULL)
GS4_SA_KEY_B64 = fw_env("GS4_SA_KEY_B64", default = NULL)

# Submissions sheet (`submissions` and `new_species` tabs). Defaults to the
# forms sheet. See fw_inbox_mode().
FWISE_INBOX_SHEET_ID = fw_env("FWISE_INBOX_SHEET_ID", default = FWISE_FORMS_SHEET_ID)

# Usage log. See R/tracking.R and TRACKING.md.
#   FWISE_LOG_MODE      "sheet", "console" or "off" (default, so local runs
#                       never write to the live sheet)
#   FWISE_LOG_SHEET_ID  sheet with the `events` tab; defaults to the forms sheet
FWISE_LOG_MODE     = fw_env("FWISE_LOG_MODE", default = NULL)
FWISE_LOG_SHEET_ID = fw_env("FWISE_LOG_SHEET_ID", default = FWISE_FORMS_SHEET_ID)

# GoatCounter site code (the part before .goatcounter.com). Public.
FW_GOATCOUNTER_CODE = "fwise-stats"

# Written on every event row. Bump by hand when logging changes.
FW_APP_VERSION = "1.0.0"

# ---- Data visualisation palette ----------------------------------------------

# Wong (2011) colourblind-safe palette, matching the paper. Data colours only:
# never interface chrome, and interface teals never encode data.
FW_PALETTE = c(
  successful = "#009E73",
  failed     = "#E69F00",
  ongoing    = "#56B4E9",
  unknown    = "#CC79A7",
  neutral    = "#999999",
  vermilion  = "#D55E00"
)

# Outcome values mapped to palette keys. Explicit, so a renamed outcome fails
# here rather than falling through to grey.
FW_OUTCOME_COLOURS = c(
  "Successful" = unname(FW_PALETTE["successful"]),
  "Failed"     = unname(FW_PALETTE["failed"]),
  "Ongoing"    = unname(FW_PALETTE["ongoing"]),
  "Unknown"    = unname(FW_PALETTE["unknown"])
)

# Ink for the count inside each outcome segment. Indigo, because white fails AA
# on every Wong colour. Per outcome so dev/check_palette.R checks each.
FW_OUTCOME_LABEL_INK = c(
  "Successful" = FW_COLOURS$ink,
  "Failed"     = FW_COLOURS$ink,
  "Ongoing"    = FW_COLOURS$ink,
  "Unknown"    = FW_COLOURS$ink
)

# ---- Constants ---------------------------------------------------------------

# Coordinates are shown and stored to six decimal places (~0.1m).
FW_COORD_DP = 6

# ---- Behaviour ---------------------------------------------------------------
# Numbers that shape what the reader sees. Every consumer reads these.

# Top n named in charts and tile grids before the rest is "Other". The copy that
# says "ten" is filled from this too.
FW_TOP_N = 10L

# Newsletter and feedback: character limits per field, and the pause between
# two feedback sends in one session.
FW_FORM_LIMITS = list(name = 100L, organisation = 200L, email = 254L,
                       message = 2000L)
FW_FEEDBACK_COOLDOWN_S = 30

# How often the "records in review" count re-reads the inbox sheet, app-wide.
# See fw_review_count_live().
FW_REVIEW_REFRESH_S = 300

# Privacy page anchors a ?section= link may jump to. dev/plan_test.R checks
# each exists in content/privacy_and_data_terms.md.
FW_PRIVACY_SECTIONS = c("newsletter", "feedback", "submissions", "contacts",
                         "usage", "data-terms")

# ---- The busy badge ----------------------------------------------------------

# Delay before a spinner shows, in ms. Shared by bslib's busy indicator (app.R)
# and the map overlay in fw_client_script(), so the two stay in step.
FW_SPINNER_DELAY_MS = 150

# Welcome map reveal sway: start position and swing (% from the left), period,
# number of swings and initial delay. See fw_home_compare_script().
FW_HOME_COMPARE = list(start = 50L, swing = 40L, period_ms = 2600L,
                        cycles = 2L, delay_ms = 500L)

# Keepalive interval so the hosting proxy doesn't close an idle socket. See
# fw_client_script().
FW_KEEPALIVE_MS = 30000

# Short hash of every file that shapes the page. Sent in <meta name="fw-build">
# and again on connect; fw_client_script() reloads once if they differ, so a
# tab left open across a deploy picks up the new build.
FW_BUILD_ID = local({
  dirs  = file.path(FW_ROOT, c("R", "www/scss", "resources/js", "content"))
  files = sort(c(file.path(FW_ROOT, "app.R"),
                  list.files(dirs, full.names = TRUE)))
  files = files[file.exists(files)]
  substr(digest::digest(unname(tools::md5sum(files))), 1, 10)
})

# Public address, with trailing slash, for canonical/Open Graph URLs.
# www/robots.txt and www/sitemap.xml repeat it: update them if it moves.
FW_SITE_URL = "https://fwise.org/"

# Longest the loader may cover the page if Shiny never goes idle (no WebSocket,
# e.g. a crawler or a firewall). Without it they see only the badge.
FW_LOADER_TIMEOUT_S = 8

# ---- Logo files --------------------------------------------------------------

# `_web` paths are browser URLs (www/ is served at the root); `_file` paths are
# read from disk for the PDF, .html and Word documents. The -NNN files are
# downscaled copies (sips -Z) of the client originals. The navbar and footer
# serve the full-resolution SIMPLE mark at the client's request. The badge is
# the loader and busy spinner.
FW_LOGO = list(
  mark_web   = "img/FWISE-SIMPLE.png",
  mark_file  = "www/img/FWISE-SIMPLE-1200.png",
  badge_web  = "img/FWISE-BADGE-256.png",
  # Wordmark with the full name under it: report mastheads and link previews.
  explicit_file = "www/img/FWISE-EXPLICIT-2000.png",
  # The footer's other logos, in footer order, printed on the PDF and .html
  # reports. UNIL-400 is cropped of the original's wide transparent margin.
  wfa_file   = "www/img/wfa-logo-rect-dark-320.png",
  collab_files = c(
    fwl     = "www/img/collab/FRESHWATER_LIFE-400.png",
    ucsc    = "www/img/collab/UCSC-400.png",
    scripps = "www/img/collab/UCSD_SCRIPPS-400.png",
    unil    = "www/img/collab/UNIL-400.png",
    issg    = "www/img/collab/ISSG_SSC_IUCN-400.png",
    vetinst = "www/img/collab/VETINST-400.png"
  )
)

# Welcome page pictures: web copies of the client's print originals. One
# beneficiary per success story, keyed like FW_COPY$home$stories. `stories` is
# the plain tile drawing; `stories_named` has the names lettered in, for the
# pop-up. The two sets use different filename separators, as supplied.
FW_HOME_IMG = list(
  map_now  = "img/home/map-now.png",
  map_next = "img/home/map-next.png",
  stories = c(
    africa        = "img/home/success/africa_fiery_redfin.png",
    asia          = "img/home/success/as_little_grebe.png",
    europe        = "img/home/success/eu_pearl_mussel.png",
    north_america = "img/home/success/na_apache_trout.png",
    oceania       = "img/home/success/oc_golden_galaxias.png",
    latin_america = "img/home/success/la_valchetta_frog.png"
  ),
  stories_named = c(
    africa        = "img/home/success/africa-fiery-redfin-named.png",
    asia          = "img/home/success/as-little-grebe-named.png",
    europe        = "img/home/success/eu-pearl-mussel-named.png",
    north_america = "img/home/success/na-apache-trout-named.png",
    oceania       = "img/home/success/oc-golden-galaxias-named.png",
    latin_america = "img/home/success/la-valchetta-frog-named.png"
  )
)

# Welcome picture order (client): two columns of three, filled down the left
# column first. Also the tab order.
FW_HOME_ORDER = c("north_america", "latin_america", "africa",
                   "asia", "oceania", "europe")

# Species photo tiles per role on the Plan report. Also the number of columns,
# so photos stay large enough to recognise.
FW_PLAN_SPECIES_N = 3L

# Rows in the PDF's country table.
FW_REPORT_COUNTRY_ROWS = 15L

# The PDF report (see R/report_pdf.R). The picker warns at warn_pages or
# warn_mb, whichever comes first: the whole database is long rather than big.
# The est_* coefficients feed fw_pdf_size_estimate(); dev/value_test.R re-renders
# three selections and fails if the estimate drifts more than 30%.
FW_PDF = list(
  warn_mb = 5,
  warn_pages = 20,
  # Render timeout, and per-photo fetch timeout (a miss prints a placeholder).
  timeout_s = 120,
  image_timeout_s = 8,
  # Bytes: fixed part, per photo, per map dot (up to est_point_cap), per contact.
  est_base = 610000,
  est_per_image = 80000,
  est_per_point = 1750,
  est_point_cap = 300,
  est_per_contact = 2600,
  # Contacts printed (client): the busiest few, not a directory.
  contacts_n = 6,
  # Fixed pages. Re-measure with dev/value_test.R when the closing sections
  # change length.
  est_pages_base = 8,
  # A4, in mm. Figures are drawn to text_width_mm.
  page_margin_mm = 18,
  text_width_mm = 174
)

# Page sizes for paged tables. The first is the default.
#
# Connect with community: the page is the directory, so 25.
FW_CONTACTS_PAGE_SIZES = c(25L, 50L, 100L)

# Contacts block on the Plan report (client): ten, a glance at who to write to.
FW_PLAN_CONTACTS_PAGE_SIZES = c(10L, 25L, 50L, 100L)

# Record search on the Plan report: ten a page, no size choice.
FW_PLAN_RECORDS_PAGE_SIZE = 10L
# Search box debounce.
FW_PLAN_RECORDS_DEBOUNCE_MS = 300L

# Year bounds on the form; end year may run this far past today.
FW_YEAR_MIN    = 1500L
FW_YEAR_FUTURE = 20L

# Above this many options, the question-list download summarises a field's
# answers instead of listing them.
FW_QUESTION_OPTION_CAP = 18L

# Maps.
FW_MAP = list(
  # Empty map: the whole world (zoom 0 = the zoom-out limit).
  empty_view = list(lng = 0, lat = 20, zoom = 0),
  # Floor under the real zoom-out limit, which fw_map_fit.js sets per map so
  # one world fills its width. 0 so narrow maps can still show the whole band.
  min_zoom = 0L,
  # The band every map shows (80N to 60S), drawn at its Mercator aspect (see
  # fw_map_aspect()). Panning is held inside it and inside max_lng, so the
  # world doesn't wrap.
  lat_north = 80,
  lat_south = -60,
  max_lng = 180,
  # Tallest a map may be, as a share of the window. See .fw-map.
  max_height_vh = 70L,
  # Closest a fit to a selection may zoom. Below cluster$fine_zoom so a
  # single-site stack stays one counted group. See fw_fit_points().
  fit_max_zoom = 10L,
  # Zoom used when a record opens (client). Maps already closer stay put.
  record_zoom = 8L,
  # Attempt markers. The stroke colour is FW_COLOURS$surface.
  marker = list(radius = 6, weight = 1.5, opacity = 1, fill_opacity = 0.75),
  # No-JavaScript fallback popup. Keep in step with .fw-map-card's width.
  popup = list(max_width = 320, min_width = 260),
  legend_opacity = 0.85,
  # Stacked markers. Below fine_zoom the cluster radius is 0 (identical
  # coordinates only); from fine_zoom up, markers within fine_radius px group
  # too. A group is always a counted ring, icon_size px across, so it never
  # looks like a single attempt.
  cluster = list(fine_zoom = 13L, fine_radius = 12L, icon_size = 28L)
)

# Charts.
FW_CHART = list(
  # Bar charts grow with their rows: max(min, per_row * rows + pad).
  # `cumulative` is fixed.
  height = list(
    cumulative       = 360,
    method           = c(min = 250, per_row = 46, pad = 110),
    duration         = c(min = 270, per_row = 54, pad = 124),
    category         = c(min = 240, per_row = 34, pad = 120)
  ),
  # Minimum share of its bar (%) for a segment to print its count. Narrower
  # segments leave it to the hover, since labels can't shrink below 1rem.
  label_min_share = 9,
  # Floor for a horizontal legend's top margin, px. fw_legend_margin()
  # computes the real value from how many lines the key wraps onto.
  legend_min_top = 46,
  # Legend line height and the gap under the key, px.
  legend_line = 22,
  legend_pad  = 12,
  # Approximate px per legend character, and per entry for swatch and gap. Only
  # used to guess line wrapping; plotly does the real layout.
  legend_char_px  = 8.2,
  legend_entry_px = 42,
  # Rule between stacked outcome segments, px. 0: the colours separate alone.
  separator_outcome = 0,
  # Vertical rules on stacked bars, px. Drawn behind the bars (fw_bar_rules()).
  bar_grid = 1,
  # Success-rate axis tick spacing, in points. Fixed so the rules line up with
  # their labels.
  share_dtick = 20,
  # Named ticks on the duration chart's log axis, in days. Labels are
  # fw_t("charts", "duration_ticks") and must stay the same length.
  # fw_duration_ticks() drops ticks past the longest attempt and adds one there.
  duration_ticks = c(1, 7, 30, 365, 1825, 3650),
  # How close (log10 days) a fixed tick may sit to that last one before it's
  # dropped, so labels don't overprint.
  duration_tick_gap = 0.08,
  # Weight of the dotted unit-break gridlines on the duration axis, px.
  duration_grid = 2,
  # Padding either side of the duration data, as a share of its span, with a
  # floor in log10 units. Set by hand: plotly's log autorange for box traces is
  # wrong. See fw_duration_range().
  duration_pad = 0.04,
  duration_pad_min = 0.05,
  # Duration dots: see-through fill, solid outline.
  point    = list(size = 7, fill_alpha = 0.55, stroke = 1),
  # Invisible hover targets along each duration box, every `step` log10 days,
  # `size` px across, so the summary shows anywhere on the box.
  duration_hover = list(step = 0.04, size = 22),
  box_line = 1.5,
  # The step line on the cumulative chart.
  line = 1,
  # Gap between an axis and its tick labels, px (invisible outside ticks, see
  # fw_tick_gap()).
  tick_gap = 12,
  # Gap between tick labels and axis title, px. See fw_axis_title().
  title_gap = 14,
  # Beeswarm spread for duration dots: `bin` (log10 days) groups near-equal
  # values; `step` and `spread` are in rows.
  duration_swarm = list(bin = 0.035, step = 0.07, spread = 0.38),
  # PNG export density. plotly's scale = dpi / 96.
  export_dpi = 300
)
