# mod_plan.R
# STUB. The planning page is not built.
#
# ==============================================================================
# PLAN AN ERADICATION - INTENDED STRUCTURE
# ==============================================================================
#
# THE LABEL. "Plan an eradication" replaces the internal name "report builder".
# That was a decision from the client wireframing call: the label has to signal
# that the user is entering a planning workflow, not browsing a database. Keep
# the label in R/copy.R so renaming stays a one-line edit.
#
# A GUIDED QUESTIONNAIRE, NOT A FILTER PANEL
#   This is the important structural point. The user answers questions about
#   THEIR site (waterbody type, size, target species, country or region,
#   constraints) and the app finds comparable attempts for them. A filter panel
#   would put the burden of knowing what to filter on the user, which is exactly
#   the knowledge they have come here without.
#   Reuse the stepped-wizard pieces from mod_contribute_steps.R rather than
#   writing a second wizard.
#
# OUTCOME IS DELIBERATELY EXCLUDED FROM THE QUESTIONS
#   Do not add an outcome filter here, even though Explore has one. If a user
#   planning an eradication can filter to successes only, they will, and they
#   will form false optimism about their own site from a biased subset. The
#   comparable set must include the failures. This is a considered decision, not
#   an oversight.
#
# OUTPUT: A TWO-PART PDF
#   1. a short report, the part a user actually reads and shares
#   2. a longer appendix carrying the full comparable-attempt table and the
#      caveats
#   Splitting them is what lets the short report stay short without hiding the
#   evidence.
#   OUT OF SCOPE for this build: the report generation engine, the Quarto or
#   R Markdown templates, and the PDF output itself. None of it exists yet.
#
# CHARTS
#   Wong palette from FW_PALETTE only, so the figures match the paper, the app
#   and the exported report.
# ==============================================================================

mod_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("plan", "title"), fw_t("plan", "description")),
    tags$main(
      id = "fw-main",
      fw_section(fw_container(fw_stub_panel()))
    )
  )
}

mod_plan_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing to do until the page is built.
  })
}
