# mod_home.R
# STUB. The landing page is not built.
#
# The data behind the map is not ready and the headline copy belongs to the
# client, so this page renders the standard stub. The full specification is
# recorded below, as comments, because it is settled and should not be
# redesigned later. Build against it, do not restart from scratch.
#
# ==============================================================================
# LANDING PAGE SPECIFICATION
# ==============================================================================
#
# THE JOB
# Make a first-time visitor understand, within about ten seconds, three things:
#   1. freshwater eradication is a proven, viable conservation action
#   2. it has been done many times, but NOT where freshwater biodiversity is
#      most at risk
#   3. they can act on it
#
# TWO CLIENT CONSTRAINTS THAT ARE NOT NEGOTIABLE
#   - NO single hero statistic is pushed at the user. Certain methods in this
#     field are socially sensitive, and the client wants users led to their own
#     caveated understanding rather than sold a headline number. Show the shape
#     of the evidence, not a slogan with a big number attached.
#   - The page order is fixed: headline, then KPIs, then map, then case studies.
#
# SECTION 1 - HERO
#   Full-bleed. $fw-silt background carrying the dot texture (fw_dots_divider()
#   uses the same motif; the hero wants the .fw-dots background class).
#   A bold headline. FW_COPY$home$title is a marked placeholder; the final
#   wording belongs to the client.
#   A supporting sentence of one or two lines beneath it.
#   Two actions: "Plan an eradication" primary, "Explore the data" secondary.
#   One screen height on desktop. Do NOT force that on mobile.
#
# SECTION 2 - KPI STRIP
#   Five figures, computed at runtime from FW_DATA via fw_headline_stats().
#   NEVER hardcoded. The function already returns exactly this set:
#     - attempts       total eradication attempts
#     - countries      countries with at least one attempt
#     - earliest_year  earliest start year in the data
#     - species        invasive species targeted
#     - successful     attempts recorded as successful
#   Proposed set, to be confirmed with the client.
#   Render with fw_kpi_stat(value, label, tooltip) inside fw_kpi_strip().
#   Large Ubuntu Mono figure, plain sentence-case label beneath.
#   STATIC. No count-up, no staggered reveal. Numbers appear at their value.
#
# SECTION 3 - THE GLOBAL PICTURE
#   Full-bleed Leaflet map. Two layers with a toggle between them:
#     a) country choropleth of invasive freshwater fish species per country
#        (the burden, ie the problem)
#     b) points for recorded eradication attempts (the response)
#   THE STORY IS THE MISMATCH BETWEEN THEM. That is the whole point of the
#   section, so the toggle should make the comparison easy rather than burying
#   one layer under the other.
#
#   The choropleth source is isolated behind fw_country_burden() in data_load.R,
#   so swapping it is a one-line change. NOTE: that function currently returns a
#   PLACEHOLDER derived from FWISE's own records, which is the response and not
#   the burden, so it does not yet tell the mismatch story. A real per-country
#   invasive fish file has to replace it before this section means anything.
#
#   Base map must be a muted, low-chroma tile set so the data layers carry all
#   the colour. Do NOT use the default OpenStreetMap tiles. CartoDB.PositronNoLabels
#   with a separate labels pane is the usual choice.
#   Colour the layers from FW_PALETTE only, never from the interface teals.
#   The map needs a text alternative (a short summary of what it shows).
#
# SECTION 4 - CASE STUDIES
#   Below the map. Expandable accordion sections grouped by continent.
#   The client will supply before-and-after content later.
#   Structure per entry: site, species, method, outcome, a short narrative, and
#   an image slot. Build three placeholder entries with that shape and mark them.
#
# SECTION 5 - CONTRIBUTE BAND
#   A single calm call to add an eradication attempt, linking to the Contribute
#   page. One paragraph, one button. Not a hard sell.
#
# ACCEPTED OVERLAP
#   The landing page and the Explore page will both show a map. That is a
#   deliberate client decision, so that a visitor who never navigates past the
#   landing page still gets the core message. Do not "fix" it by removing one.
#
# DOT MOTIF BUDGET
#   At most two uses on this page (the hero texture and one divider). The motif
#   appears nowhere else in the app.
# ==============================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("home", "title"), fw_t("home", "description")),
    tags$main(
      id = "fw-main",
      fw_section(fw_container(fw_stub_panel()))
    )
  )
}

mod_home_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing to do until the page is built. fw_headline_stats(data) is ready
    # and returns the five KPI figures when the strip goes in.
  })
}
