# mod_contribute_steps.R
# Section builders for the Add a record form. Copy is in R/copy_contribute.R;
# spec ambiguities are marked TODO(alex), simplest reading implemented.

library(shiny)
library(bslib)

# The sections, in page order. `id` keys the validators and progress rail;
# `conditional` sections show only when their predicate holds.
FW_STEPS = list(
  # Contributor and consent first (client).
  list(id = "contributor", title_key = "contributor"),
  list(id = "site",        title_key = "site"),
  list(id = "waterbody",   title_key = "waterbody"),
  list(id = "invasive",    title_key = "invasive"),
  list(id = "timeline",    title_key = "timeline"),
  list(id = "benefit",     title_key = "benefit"),
  list(id = "methods",     title_key = "methods"),
  # Shown only when a chemical method is selected.
  list(id = "chemical",    title_key = "chemical", conditional = TRUE),
  list(id = "outcome",     title_key = "outcome"),
  list(id = "other",       title_key = "other"),
  list(id = "review",      title_key = "review")
)

# ---- Copy ------------------------------------------------------------------
# Short helpers into R/copy_contribute.R.

fw_lab  = function(key) fw_t("contribute", "fields", key)
fw_help = function(key) fw_t("contribute", "help", key)
fw_tip  = function(key) fw_t("contribute", "tips", key)
fw_ph   = function(key) fw_t("contribute", "placeholder", key)

# ---- Small builders ----------------------------------------------------------

fw_step_intro = function(key) {
  p(class = "fw-lead", fw_t("contribute", "blurb", key))
}

# A numeric input labelled by fw_field(); Shiny's own label is visually hidden.
fw_num_input = function(id, value = NA, min = NA, max = NA, step = NA,
                         placeholder = NULL) {
  div(
    class = "fw-bare-label",
    numericInput(id, label = NULL, value = value, min = min, max = max,
                 step = step, width = "100%")
  )
}

fw_text_input = function(id, placeholder = NULL) {
  textInput(id, label = NULL, width = "100%", placeholder = placeholder)
}

fw_area_input = function(id, rows = 3, placeholder = NULL) {
  textAreaInput(id, label = NULL, width = "100%", rows = rows,
                placeholder = placeholder)
}

#' A dropdown that starts genuinely empty
#'
#' Gotcha: selectize auto-selects the first choice without a leading blank.
fw_select = function(id, choices, multiple = FALSE, selected = NULL) {
  if (!multiple) {
    choices = c(stats::setNames("", fw_ph("select")), choices)
    if (is.null(selected)) selected = ""
  }
  selectizeInput(id, label = NULL, choices = choices, selected = selected,
                 multiple = multiple, width = "100%",
                 options = list(placeholder = fw_ph("select")))
}

# An "Other (specify)" box, shown by conditionalPanel.
fw_other_panel = function(ns, watch_id, target_id, label, multiple = FALSE) {
  condition = if (multiple) {
    sprintf("input['%s'] && input['%s'].indexOf('%s') > -1",
            ns(watch_id), ns(watch_id), FW_OTHER)
  } else {
    sprintf("input['%s'] === '%s'", ns(watch_id), FW_OTHER)
  }
  # Gotcha: conditionalPanel() calls ns(""), so pass NS(NULL), not NULL.
  conditionalPanel(
    condition = condition,
    ns = shiny::NS(NULL),
    fw_field(fw_text_input(ns(target_id)), label, input_id = ns(target_id))
  )
}

# ---- Steps -------------------------------------------------------------------

fw_step_site_ui = function(ns, choices) {
  tagList(
    fw_step_intro("site"),
    fw_field(fw_text_input(ns("site_name")), fw_lab("site_name"), required = TRUE,
             tooltip = fw_tip("site_name"), input_id = ns("site_name")),
    # Country: full ISO 3166-1 list, recorded countries first. Region: that
    # country's ISO 3166-2 subdivisions. See fw_load_iso().
    fw_field(fw_select(ns("country"), choices$country), fw_lab("country"), required = TRUE,
             tooltip = fw_tip("country"), input_id = ns("country")),
    fw_other_panel(ns, "country", "country_other",
                   fw_lab("country_other")),
    # Filled from the chosen country; free text otherwise.
    fw_field(fw_select(ns("region"), character(0)), fw_lab("region"),
             help = fw_help("region"),
             input_id = ns("region")),
    div(
      class = "fw-field",
      div(class = "fw-field__label-row",
          tags$label(class = "form-label", fw_lab("location"),
                     tags$span(class = "fw-required-mark", `aria-hidden` = "true", "*"),
                     tags$span(class = "fw-visually-hidden", fw_t("a11y", "required"))),
          fw_info(fw_tip("location"), tolower(fw_lab("location")))),
      div(class = "fw-field__help", fw_help("location")),
      # Same shape as every map, so the whole world shows.
      fw_map_output(ns("picker"), class = "fw-map-picker"),
      div(
        class = "fw-coord-row",
        fw_field(fw_num_input(ns("latitude"), min = -90, max = 90, step = 0.000001),
                 fw_lab("latitude"), input_id = ns("latitude")),
        fw_field(fw_num_input(ns("longitude"), min = -180, max = 180, step = 0.000001),
                 fw_lab("longitude"), input_id = ns("longitude"))
      )
    )
  )
}

fw_step_waterbody_ui = function(ns, choices) {
  tagList(
    fw_step_intro("waterbody"),

    # Regime first; the type list narrows to it (server side).
    fw_field(radioButtons(ns("water_regime"), label = NULL,
                          choices = choices$water_regime, selected = character(0),
                          inline = TRUE),
             fw_lab("water_regime"), required = TRUE,
             tooltip = fw_tip("water_regime"), input_id = ns("water_regime")),

    fw_field(fw_select(ns("waterbody_type"), choices$waterbody_by_regime[[FW_ALL]]),
             fw_lab("waterbody_type"), required = TRUE,
             tooltip = fw_tip("waterbody"), input_id = ns("waterbody_type"),
             help = fw_help("waterbody_type")),
    fw_other_panel(ns, "waterbody_type", "waterbody_type_other",
                   fw_lab("waterbody_other")),

    div(
      class = "fw-coord-row",
      fw_field(fw_num_input(ns("area_treated"), min = 0),
               fw_lab("area_treated"), tooltip = fw_tip("area"),
               input_id = ns("area_treated")),
      # Required once a value is entered (in the validator).
      fw_field(fw_select(ns("area_unit"), choices$area_unit), fw_lab("area_unit"),
               help = fw_help("area_unit"),
               input_id = ns("area_unit"))
    ),
    fw_field(fw_area_input(ns("area_notes"), placeholder = fw_ph("area_notes")),
             fw_lab("area_notes"), tooltip = fw_tip("area_notes"),
             input_id = ns("area_notes")),

    fw_field(fw_num_input(ns("depth_m"), min = 0), fw_lab("depth_m"),
             tooltip = fw_tip("depth"), input_id = ns("depth_m")),
    fw_field(fw_area_input(ns("depth_notes"), rows = 2), fw_lab("depth_notes"),
             input_id = ns("depth_notes")),

    fw_field(fw_num_input(ns("volume_m3"), min = 0), fw_lab("volume_m3"),
             tooltip = fw_tip("volume"), input_id = ns("volume_m3")),
    fw_field(fw_area_input(ns("volume_notes"), rows = 2), fw_lab("volume_notes"),
             input_id = ns("volume_notes")),

    # Flow only for flowing water.
    conditionalPanel(
      condition = sprintf("input['%s'] === 'Lotic'", ns("water_regime")),
      ns = shiny::NS(NULL),
      fw_field(fw_num_input(ns("max_flow_m3s"), min = 0), fw_lab("max_flow"),
               tooltip = fw_tip("max_flow"), input_id = ns("max_flow_m3s"))
    ),

    fw_field(fw_num_input(ns("water_temp_c")), fw_lab("water_temp"),
             tooltip = fw_tip("water_temp"), input_id = ns("water_temp_c")),
    fw_field(fw_area_input(ns("water_temp_notes"), rows = 2),
             fw_lab("water_temp_notes"), input_id = ns("water_temp_notes"))
  )
}

fw_step_invasive_ui = function(ns, choices) {
  tagList(
    fw_step_intro("invasive"),
    # Each target pairs an animal group with its species; "Add another target"
    # repeats it. Family is never asked. Row 1 is rendered here: insertUI into
    # an unpainted container left this empty or doubled.
    div(id = ns("target_rows"), fw_target_row(ns, 1L, choices)),
    actionButton(ns("add_target"), fw_t("contribute", "add_target"),
                 class = "btn btn-outline-primary btn-sm")
  )
}

fw_step_timeline_ui = function(ns, choices) {
  this_year = as.integer(format(Sys.Date(), "%Y"))
  tagList(
    fw_step_intro("timeline"),
    fw_field(fw_num_input(ns("invasion_year"), min = FW_YEAR_MIN, max = this_year),
             fw_lab("invasion_year"), tooltip = fw_tip("invasion_year"),
             input_id = ns("invasion_year")),
    fw_field(fw_num_input(ns("start_year"), min = FW_YEAR_MIN, max = this_year),
             fw_lab("start_year"), required = TRUE,
             tooltip = fw_tip("start_year"), input_id = ns("start_year")),
    fw_field(fw_num_input(ns("end_year"), min = FW_YEAR_MIN, max = this_year + FW_YEAR_FUTURE),
             fw_lab("end_year"),
             help = fw_help("end_year"),
             tooltip = fw_tip("end_year"), input_id = ns("end_year")),
    fw_field(fw_num_input(ns("duration_days"), min = 0),
             fw_lab("duration"), tooltip = fw_tip("duration"),
             input_id = ns("duration_days")),
    fw_field(fw_select(ns("driver"), choices$driver),
             fw_lab("driver"),
             required = TRUE, tooltip = fw_tip("driver"), input_id = ns("driver")),
    fw_other_panel(ns, "driver", "driver_other", fw_lab("driver_other"))
  )
}

fw_step_benefit_ui = function(ns, choices) {
  tagList(
    fw_step_intro("benefit"),
    # Same group/species pairs as the targets, row 1 rendered here.
    div(id = ns("benefit_rows"), fw_beneficiary_row(ns, 1L, choices)),
    actionButton(ns("add_beneficiary"), fw_t("contribute", "add_beneficiary"),
                 class = "btn btn-outline-primary btn-sm")
  )
}

fw_step_methods_ui = function(ns, choices) {
  tagList(
    fw_step_intro("methods"),
    tags$h3(fw_lab("methods_heading")),
    # Row 1 inline, for the same reason as the targets above.
    div(id = ns("method_rows"), fw_method_row(ns, 1L, choices)),
    actionButton(ns("add_method"), fw_t("contribute", "add_method"),
                 class = "btn btn-outline-primary btn-sm"),

    tags$hr(),
    fw_field(fw_area_input(ns("method_description"), rows = 4),
             fw_lab("method_description"),
             tooltip = fw_tip("method_desc"), input_id = ns("method_description")),
    # TODO(alex): spec asks for a number "with a free-text fallback for
    # ranges". Implemented as one text box, so "20 to 30" passes to QA.
    fw_field(fw_text_input(ns("labour_person_days")),
             fw_lab("labour"), tooltip = fw_tip("labour"),
             input_id = ns("labour_person_days")),
    fw_field(fw_num_input(ns("cost_estimate"), min = 0),
             fw_lab("cost"),
             help = fw_help("cost"),
             tooltip = fw_tip("cost"), input_id = ns("cost_estimate")),
    fw_field(fw_area_input(ns("cost_notes"), rows = 2), fw_lab("cost_notes"),
             input_id = ns("cost_notes"))
  )
}

fw_step_chemical_ui = function(ns, choices) {
  tagList(
    fw_step_intro("chemical"),
    # Basis first: a concentration is meaningless without it.
    fw_field(radioButtons(ns("target_ingredient_basis"), label = NULL,
                          choices = c("Active", "Product"), selected = character(0),
                          inline = TRUE),
             fw_lab("ingredient_basis"), tooltip = fw_tip("ingredient_basis"),
             input_id = ns("target_ingredient_basis")),
    fw_field(fw_text_input(ns("toxin_conc_target")),
             fw_lab("toxin_conc"),
             tooltip = fw_tip("toxin_conc"), input_id = ns("toxin_conc_target")),
    fw_field(fw_area_input(ns("conc_target_notes"), rows = 3),
             fw_lab("conc_target_notes"), input_id = ns("conc_target_notes")),
    # Measured value plus notes.
    fw_field(fw_text_input(ns("toxin_conc_measured")),
             fw_lab("toxin_conc_measured"),
             tooltip = fw_tip("toxin_conc_measured"),
             input_id = ns("toxin_conc_measured")),
    fw_field(fw_area_input(ns("conc_measured_notes"), rows = 3),
             fw_lab("conc_measured_notes"),
             input_id = ns("conc_measured_notes")),
    fw_field(fw_select(ns("neutralising_agent"), choices$neutralising_agent),
             fw_lab("neutralising"), tooltip = fw_tip("neutralising"),
             input_id = ns("neutralising_agent")),
    fw_other_panel(ns, "neutralising_agent", "neutralising_agent_other",
                   fw_lab("neutralising_other")),
    fw_field(fw_area_input(ns("neutralising_notes"), rows = 2),
             fw_lab("neutralising_notes"), input_id = ns("neutralising_notes"))
  )
}

fw_step_outcome_ui = function(ns, choices) {
  tagList(
    fw_step_intro("outcome"),
    fw_field(fw_select(ns("outcome"), choices$outcome), fw_lab("outcome"),
             required = TRUE, tooltip = fw_tip("outcome"), input_id = ns("outcome")),
    # One box, per the spec; split downstream if needed.
    fw_field(fw_area_input(ns("verification"), rows = 4),
             fw_lab("verification"),
             tooltip = fw_tip("verification"), input_id = ns("verification")),
    fw_field(fw_area_input(ns("reference"), rows = 3),
             fw_lab("reference"),
             help = fw_help("reference"), tooltip = fw_tip("reference"),
             input_id = ns("reference")),
    fw_field(fw_text_input(ns("source")), fw_lab("source"),
             help = fw_help("source"),
             tooltip = fw_tip("source"), input_id = ns("source"))
  )
}

fw_step_contributor_ui = function(ns, choices) {
  tagList(
    fw_step_intro("contributor"),
    # One contact; submissions write the secondary as NA.
    tags$h3(fw_lab("contact_heading")),
    fw_field(fw_text_input(ns("primary_contact_name")), fw_lab("contact_name"), required = TRUE,
             tooltip = fw_tip("contact"), input_id = ns("primary_contact_name")),
    fw_field(fw_text_input(ns("primary_contact_email")), fw_lab("contact_email"), required = TRUE,
             input_id = ns("primary_contact_email")),
    fw_field(fw_text_input(ns("primary_contact_org")), fw_lab("contact_org"),
             input_id = ns("primary_contact_org")),

    # ---- Consent: two boxes ----------------------------------------------------
    # Data use is required (all_valid() and fw_check()). Display is opt-in and
    # must never be required: publication consent has to be free. Unticked, the
    # contact is stored private and redacted everywhere. Both are fw_field()s
    # so the question lists include them.
    tags$h3(fw_t("contribute", "consent", "heading")),
    p(fw_t("contribute", "consent", "statement")),
    fw_field(checkboxInput(ns("consent_data_use"),
                           fw_t("contribute", "consent", "agree_yes"), value = FALSE),
             fw_t("contribute", "consent", "agree_label"), required = TRUE,
             input_id = ns("consent_data_use")),
    # Terms open in a new tab, so the form isn't lost.
    p(fw_new_tab_link(fw_privacy_href("submissions"),
                      fw_t("contribute", "consent", "terms_link_label"))),
    fw_field(checkboxInput(ns("contact_public"), fw_lab("contact_public_yes"), value = FALSE),
             fw_lab("contact_public"), help = fw_help("contact_public"),
             input_id = ns("contact_public"))
  )
}

fw_step_other_ui = function(ns, choices) {
  tagList(
    fw_step_intro("other"),
    fw_field(fw_area_input(ns("notes_for_fwise"), rows = 6), fw_lab("notes"),
             tooltip = fw_tip("notes"), input_id = ns("notes_for_fwise"))
  )
}

fw_step_review_ui = function(ns, choices) {
  tagList(
    fw_step_intro("review"),
    # Rendered on demand, not per keystroke.
    actionButton(ns("check_answers"), fw_t("contribute", "check_action"),
                 class = "btn btn-outline-primary"),
    uiOutput(ns("review_summary"))
  )
}

# Dispatch table, so the module does not carry a long if/else chain.
FW_STEP_UI = list(
  site        = fw_step_site_ui,
  waterbody   = fw_step_waterbody_ui,
  invasive    = fw_step_invasive_ui,
  timeline    = fw_step_timeline_ui,
  benefit     = fw_step_benefit_ui,
  methods     = fw_step_methods_ui,
  chemical    = fw_step_chemical_ui,
  outcome     = fw_step_outcome_ui,
  contributor = fw_step_contributor_ui,
  other       = fw_step_other_ui,
  review      = fw_step_review_ui
)

# ---- Repeatable rows ---------------------------------------------------------

#' The species picker used by targets and by beneficiaries
#'
#' Client-side (the list is ~15KB). Revisit if it reaches thousands.
#' `create = TRUE` lets a contributor add a new species.
fw_species_picker = function(input_id, species) {
  selectizeInput(
    input_id, label = NULL,
    # Leading blank, or selectize auto-picks the first species. As fw_select().
    choices = c("", species), selected = "",
    width = "100%",
    options = list(
      placeholder = fw_ph("species_search"),
      create = TRUE,
      createOnBlur = TRUE,
      persist = FALSE,
      maxOptions = 1000
    )
  )
}

# ---- Repeatable rows ---------------------------------------------------------

#' One row: an animal group and its species. Used for targets and
#' beneficiaries
#'
#' Inputs carry the row index, so removing one row leaves the rest. A target is
#' required; a beneficiary isn't.
FW_ROW_KINDS = list(
  target = list(
    heading      = "target_heading",
    taxa_label   = "target_taxa",
    taxa_other   = "target_taxa_other",
    species_label = "species",
    taxa_choices = "invasive_taxa",
    taxa_tip     = "invasive_taxa",
    species_tip  = "species",
    required     = TRUE
  ),
  benefit = list(
    heading      = "benefit_heading",
    taxa_label   = "benefit_taxa",
    taxa_other   = "benefit_taxa_other",
    species_label = "species",
    taxa_choices = "beneficiary_taxa",
    taxa_tip     = "benefit_taxa",
    species_tip  = "benefit_sp",
    required     = FALSE
  )
)

fw_pair_row = function(ns, index, choices, kind = "target") {
  cfg   = FW_ROW_KINDS[[kind]]
  first = index == 1
  taxa_id    = paste0(kind, "_taxa_", index)
  species_id = paste0(kind, "_species_", index)

  div(
    id = ns(paste0(kind, "_row_", index)),
    class = "fw-repeat-row fw-repeat-row--stacked",
    div(
      div(class = "fw-repeat-row__heading", paste(fw_lab(cfg$heading), index)),
      fw_field(fw_select(ns(taxa_id), choices[[cfg$taxa_choices]]),
               fw_lab(cfg$taxa_label), required = first && cfg$required,
               tooltip = fw_tip(cfg$taxa_tip), input_id = ns(taxa_id)),
      fw_other_panel(ns, taxa_id, paste0(kind, "_taxa_other_", index),
                     fw_lab(cfg$taxa_other)),
      fw_field(fw_species_picker(ns(species_id), choices$species_by_taxa[[FW_ALL]]),
               fw_lab(cfg$species_label), required = first && cfg$required,
               tooltip = fw_tip(cfg$species_tip), input_id = ns(species_id),
               help = fw_help("species"))
    ),
    div(
      class = "fw-repeat-row__remove",
      if (!first) {
        # Pass "Remove" as `label`; positionally it lands in `width`.
        actionButton(ns(paste0("remove_", kind, "_", index)),
                     label = fw_lab("remove"),
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}

# Thin wrappers, for readability at the call sites.
fw_target_row      = function(ns, index, choices) fw_pair_row(ns, index, choices, "target")
fw_beneficiary_row = function(ns, index, choices) fw_pair_row(ns, index, choices, "benefit")

fw_method_row = function(ns, index, choices) {
  row_id = paste0("method_row_", index)
  first = index == 1

  div(
    id = ns(row_id),
    class = "fw-repeat-row",
    div(
      fw_field(fw_select(ns(paste0("method_", index)), choices$method),
               if (first) fw_lab("method_main") else fw_fill(fw_lab("method_n"), n = index),
               required = first, tooltip = fw_tip("method"),
               input_id = ns(paste0("method_", index))),
      fw_other_panel(ns, paste0("method_", index), paste0("method_other_", index),
                     fw_lab("method_other")),
      fw_field(fw_area_input(ns(paste0("method_notes_", index)), rows = 2),
               fw_lab("method_notes"), tooltip = fw_tip("method_notes"),
               input_id = ns(paste0("method_notes_", index)))
    ),
    div(
      class = "fw-repeat-row__remove",
      if (!first) {
        # `label =`, as in fw_pair_row().
        actionButton(ns(paste0("remove_method_", index)), label = fw_lab("remove"),
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}
