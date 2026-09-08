# mod_about.R
# STUB. The about page is not built.
#
# ==============================================================================
# ABOUT - INTENDED STRUCTURE
# ==============================================================================
#
# Mostly client-supplied prose, so the work here is structure and typography
# rather than logic. Set it in .fw-prose so the measure is capped at 68
# characters; this is the most text-heavy page in the app.
#
# SECTIONS, IN ORDER
#   1. THE DATABASE
#      What FWISE covers and what it does not. Animals, not plants. Freshwater.
#      Eradication as complete and permanent removal (Genovesi 2005), and the
#      point that an attempt can be recorded as successful without formal proof
#      of absence, with the QA team recording that distinction separately.
#      State the record count from the data rather than typing a number:
#      nrow(data$attempt).
#
#   2. THE METHOD
#      How records were gathered, how they are reviewed, and the known gaps.
#      The gaps matter as much as the coverage and the client is explicit about
#      wanting them stated rather than glossed.
#
#   3. THE TEAM AND PARTNERS
#      Freshwater Life and contributing partners. Logos if supplied.
#
#   4. HOW TO CITE
#      The paper citation and the Zenodo DOI. Give a copyable citation block,
#      set in the mono face.
#
#   5. THE DATA LICENCE
#      Currently a placeholder in FW_COPY$footer$licence. Confirm with the
#      client before launch: it appears in the footer on every page.
#
#   6. LINKS
#      The paper, Zenodo, the GitHub repository.
#
# All of the above is client copy. Do not invent claims about the data, the
# organisation's positions, or the partners.
# ==============================================================================

mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("about", "title"), fw_t("about", "description")),
    tags$main(
      id = "fw-main",
      fw_section(fw_container(fw_stub_panel()))
    )
  )
}

mod_about_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Nothing to do until the page is built.
  })
}
