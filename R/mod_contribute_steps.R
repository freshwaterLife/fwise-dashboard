# mod_contribute_steps.R
# Step UI builders for the contribute form, kept out of the main module so
# neither file grows unreadable.
#
# Field labels, prompts and tooltips come from the FWISE Upload Form field
# specification and live in R/copy_contribute.R. Where that document was
# ambiguous there is a TODO(alex) next to the field naming the ambiguity, and
# the simplest reading is implemented.

library(shiny)
library(bslib)

# The step registry. Order here is the order of the wizard, and `id` is what the
# validator and the progress indicator key on. `conditional` marks a step that is
# skipped unless its predicate returns TRUE.
FW_STEPS <- list(
  list(id = "site",        title_key = "site"),
  list(id = "waterbody",   title_key = "waterbody"),
  list(id = "invasive",    title_key = "invasive"),
  list(id = "timeline",    title_key = "timeline"),
  list(id = "benefit",     title_key = "benefit"),
  list(id = "methods",     title_key = "methods"),
  # Shown only when a chemical method has been selected. See fw_step_visible().
  list(id = "chemical",    title_key = "chemical", conditional = TRUE),
  list(id = "outcome",     title_key = "outcome"),
  list(id = "contributor", title_key = "contributor"),
  list(id = "other",       title_key = "other"),
  list(id = "review",      title_key = "review")
)

# ---- Copy ------------------------------------------------------------------
# Every label, help line, placeholder and tooltip on the form lives in
# R/copy_contribute.R. These four are the short spellings the builders use.

fw_lab  <- function(key) fw_t("contribute", "fields", key)
fw_help <- function(key) fw_t("contribute", "help", key)
fw_tip  <- function(key) fw_t("contribute", "tips", key)
fw_ph   <- function(key) fw_t("contribute", "placeholder", key)

# ---- Small builders ----------------------------------------------------------

fw_step_intro <- function(key) {
  p(class = "fw-lead", fw_t("contribute", "blurb", key))
}

# A numeric input without its own label, so fw_field() owns the labelling. Shiny
# always renders a label element, so it is hidden from view but left in the
# accessibility tree pointing at the same text.
fw_num_input <- function(id, value = NA, min = NA, max = NA, step = NA,
                         placeholder = NULL) {
  div(
    class = "fw-bare-label",
    numericInput(id, label = NULL, value = value, min = min, max = max,
                 step = step, width = "100%")
  )
}

fw_text_input <- function(id, placeholder = NULL) {
  textInput(id, label = NULL, width = "100%", placeholder = placeholder)
}

fw_area_input <- function(id, rows = 3, placeholder = NULL) {
  textAreaInput(id, label = NULL, width = "100%", rows = rows,
                placeholder = placeholder)
}

#' A dropdown that starts genuinely empty
#'
#' A selectize with choices and no blank first entry auto-selects the first one,
#' which would silently default every submission's country to Argentina. The
#' blank option plus an explicit empty selection is what makes "not answered"
#' distinguishable from "answered with the first item".
fw_select <- function(id, choices, multiple = FALSE, selected = NULL) {
  if (!multiple) {
    choices <- c(stats::setNames("", fw_ph("select")), choices)
    if (is.null(selected)) selected <- ""
  }
  selectizeInput(id, label = NULL, choices = choices, selected = selected,
                 multiple = multiple, width = "100%",
                 options = list(placeholder = fw_ph("select")))
}

# An "Other (specify)" free-text box, revealed by conditionalPanel because the
# condition is a plain input value and needs no server round trip.
fw_other_panel <- function(ns, watch_id, target_id, label, multiple = FALSE) {
  condition <- if (multiple) {
    sprintf("input['%s'] && input['%s'].indexOf('%s') > -1",
            ns(watch_id), ns(watch_id), FW_OTHER)
  } else {
    sprintf("input['%s'] === '%s'", ns(watch_id), FW_OTHER)
  }
  # NOTE: conditionalPanel() calls ns("") internally to write its
  # data-ns-prefix, so `ns = NULL` is not "no namespace" - it throws
  # 'could not find function "ns"' the moment the panel renders. The condition
  # above is already namespaced by hand, so the prefix must be empty, and
  # NS(NULL) is the function that produces an empty one.
  conditionalPanel(
    condition = condition,
    ns = shiny::NS(NULL),
    fw_field(fw_text_input(ns(target_id)), label, input_id = ns(target_id))
  )
}

# ---- Steps -------------------------------------------------------------------

fw_step_site_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("site"),
    fw_field(fw_text_input(ns("site_name")), fw_lab("site_name"), required = TRUE,
             tooltip = fw_tip("site_name"), input_id = ns("site_name")),
    # THE TWO-LEVEL ISO PICKER the specification asked for. Country is the full
    # ISO 3166-1 list with the countries FWISE already holds records for at the
    # top; region is that country's ISO 3166-2 subdivisions, filled in by the
    # server once a country is chosen. Both lists are built offline by
    # dev/build_iso_lookups.R and read from disk - see fw_load_iso().
    fw_field(fw_select(ns("country"), choices$country), fw_lab("country"), required = TRUE,
             tooltip = fw_tip("country"), input_id = ns("country")),
    fw_other_panel(ns, "country", "country_other",
                   fw_lab("country_other")),
    # Starts empty and is populated from the chosen country. A country with no
    # subdivisions in the standard, or one typed in by hand, leaves this as free
    # text - see the observer in mod_contribute.R.
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
      div(class = "fw-map-picker",
          leaflet::leafletOutput(ns("picker"), height = 320)),
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

fw_step_waterbody_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("waterbody"),

    # Regime comes FIRST, and the type list below narrows to match it. Asking
    # for the type first offered a Lentic contributor "River" and a Lotic one
    # "Lake", which is the sort of thing that makes a form feel like it is not
    # listening. The narrowing happens in the server, on water_regime.
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
      # The unit becomes required once a value is entered. Enforced in the
      # validator rather than here, so the message appears next to the field.
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

    # Flow is a property of flowing water. Asking a contributor for the maximum
    # flow of a pond is asking them to leave a field blank and wonder whether
    # they have missed something. conditionalPanel rather than a server-side
    # toggle, so it costs no round trip.
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

fw_step_invasive_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("invasive"),
    # ONE TARGET AT A TIME. This used to ask for every animal group up front and
    # then, separately, for a list of species, which left a contributor who had
    # answered "Fish, Crayfish" with an unlabelled species box and no way to say
    # which was which. A target pairs the group with its species, and "Add
    # another target" repeats the pair. The fish family question is gone
    # entirely: it is filled in from the species on the way to the sheet.
    # Row 1 is rendered HERE rather than inserted by the server. insertUI on a
    # container that the page has not painted yet was why this section could
    # arrive empty, or with two copies of row 1 in it.
    div(id = ns("target_rows"), fw_target_row(ns, 1L, choices)),
    actionButton(ns("add_target"), fw_t("contribute", "add_target"),
                 class = "btn btn-outline-primary btn-sm")
  )
}

fw_step_timeline_ui <- function(ns, choices) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
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

fw_step_benefit_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("benefit"),
    # ONE BENEFICIARY AT A TIME, pairing the group with its species, exactly as
    # the invasive step does. The section-wide group multi-select that used to
    # sit here left a contributor who answered "Fish, Invertebrate" with
    # unlabelled species boxes and no way to say which was which.
    #
    # Row 1 is rendered HERE rather than inserted by the server: insertUI on a
    # container the page has not painted yet is what used to make a section
    # arrive empty or doubled.
    div(id = ns("benefit_rows"), fw_beneficiary_row(ns, 1L, choices)),
    actionButton(ns("add_beneficiary"), fw_t("contribute", "add_beneficiary"),
                 class = "btn btn-outline-primary btn-sm")
  )
}

fw_step_methods_ui <- function(ns, choices) {
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
    # TODO(alex): the specification asks for a numeric box "with a free-text
    # fallback for ranges". The simplest reading is implemented: one text box,
    # validated as a number only when it parses as one, so "20 to 30" is accepted
    # and passed through to QA.
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

fw_step_chemical_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("chemical"),
    # THE BASIS COMES FIRST because the concentration means nothing without it:
    # a figure for the commercial product is the active-ingredient figure
    # divided by the product's strength, so the two are not comparable. Every
    # one of the 325 chemical records in the database carries this answer.
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
    # The measured value is a field in the database (86 records hold one), so
    # the form asks for it beside the notes the specification listed.
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

fw_step_outcome_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("outcome"),
    fw_field(fw_select(ns("outcome"), choices$outcome), fw_lab("outcome"),
             required = TRUE, tooltip = fw_tip("outcome"), input_id = ns("outcome")),
    # The specification is explicit that verification stays as one box on the
    # form and is split downstream if needed.
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

fw_step_contributor_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("contributor"),
    tags$h3(fw_lab("primary_heading")),
    fw_field(fw_text_input(ns("primary_contact_name")), fw_lab("contact_name"), required = TRUE,
             tooltip = fw_tip("contact"), input_id = ns("primary_contact_name")),
    fw_field(fw_text_input(ns("primary_contact_email")), fw_lab("contact_email"), required = TRUE,
             input_id = ns("primary_contact_email")),
    fw_field(fw_text_input(ns("primary_contact_org")), fw_lab("contact_org"),
             input_id = ns("primary_contact_org")),

    tags$h3(fw_lab("secondary_heading")),
    p(class = "fw-caption", fw_lab("secondary_note")),
    fw_field(fw_text_input(ns("secondary_contact_name")), fw_lab("contact_name"),
             input_id = ns("secondary_contact_name")),
    fw_field(fw_text_input(ns("secondary_contact_email")), fw_lab("contact_email"),
             input_id = ns("secondary_contact_email")),
    fw_field(fw_text_input(ns("secondary_contact_org")), fw_lab("contact_org"),
             input_id = ns("secondary_contact_org"))
  )
}

fw_step_other_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("other"),
    fw_field(fw_area_input(ns("notes_for_fwise"), rows = 6), fw_lab("notes"),
             tooltip = fw_tip("notes"), input_id = ns("notes_for_fwise"))
  )
}

fw_step_review_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("review"),
    # ON DEMAND, not live. Rebuilding this table on every keystroke means a
    # server render per character typed anywhere on the page, which is exactly
    # the lag a single scrolling form is meant to avoid. Everything it shows is
    # visible further up the page anyway; this is a last read-through.
    actionButton(ns("check_answers"), fw_t("contribute", "check_action"),
                 class = "btn btn-outline-primary"),
    uiOutput(ns("review_summary"))
  )
}

# Dispatch table, so the module does not carry a long if/else chain.
FW_STEP_UI <- list(
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
#' CLIENT-SIDE, deliberately. It used to be a server-side selectize on the
#' grounds that 390 species was too many to ship to the browser. 390 labels is
#' about 15KB - less than one of the fonts - and the server-side version was the
#' reason search did not work at all: updateSelectizeInput(server = TRUE) silently
#' does nothing when the element it names is not in the DOM yet, which is exactly
#' what happened while the form re-rendered each step. Searching in the browser is
#' instant and cannot get out of step with the DOM. Revisit only if the species
#' list reaches several thousand.
#'
#' `create = TRUE` is what lets a contributor add a species we do not hold.
fw_species_picker <- function(input_id, species) {
  selectizeInput(
    input_id, label = NULL,
    # The LEADING BLANK IS LOAD-BEARING. A selectize handed choices with no
    # empty first entry selects the first one, so every submission that never
    # touched this field would arrive naming whichever species happens to sort
    # first. With the blank, "not answered" stays distinguishable from
    # "answered with the first item". Same reasoning as fw_select().
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

#' ONE row builder for both roles: a group of animal, and the species in it
#'
#' Invasive targets and beneficiaries are the SAME question asked about two
#' sides of the same eradication, so they are the same code. They used to differ:
#' a target paired its group with its species, while beneficiaries had one
#' group multi-select for the whole section and separate unlabelled species
#' boxes. That left a contributor who answered "Fish, Invertebrate" with two
#' species boxes and no way to say which was which - exactly the problem that
#' had already been fixed on the invasive side.
#'
#' Each row's inputs are namespaced with the row index, so removing row 2 does
#' not disturb rows 1 and 3. The species list is narrowed to the group by the
#' server; the fish family is never asked for, it is derived on submission.
#'
#' The only real difference is that a target is REQUIRED and a beneficiary is
#' not. Every eradication has something it was aimed at; plenty of real records
#' never recorded what it was meant to help.
FW_ROW_KINDS <- list(
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

fw_pair_row <- function(ns, index, choices, kind = "target") {
  cfg   <- FW_ROW_KINDS[[kind]]
  first <- index == 1
  taxa_id    <- paste0(kind, "_taxa_", index)
  species_id <- paste0(kind, "_species_", index)

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
        # "Remove" is the LABEL. With label = NULL and the word passed
        # positionally it landed in actionButton's `width`, which threw
        # '"Remove" is not a valid CSS unit' the moment a second row rendered.
        actionButton(ns(paste0("remove_", kind, "_", index)),
                     label = fw_lab("remove"),
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}

# Kept as thin names because the step builders and questions_text.R read better
# saying what they are building than passing a string.
fw_target_row      <- function(ns, index, choices) fw_pair_row(ns, index, choices, "target")
fw_beneficiary_row <- function(ns, index, choices) fw_pair_row(ns, index, choices, "benefit")

fw_method_row <- function(ns, index, choices) {
  row_id <- paste0("method_row_", index)
  first <- index == 1

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
        # See the note in fw_pair_row(): the word is the label, not a
        # positional argument that falls through to `icon`.
        actionButton(ns(paste0("remove_method_", index)), label = fw_lab("remove"),
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}
