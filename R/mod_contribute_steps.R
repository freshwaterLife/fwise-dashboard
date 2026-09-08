# mod_contribute_steps.R
# Step UI builders for the contribute form, kept out of the main module so
# neither file grows unreadable.
#
# Field labels, prompts and tooltips come from the FWISE Upload Form field
# specification. Where that document was ambiguous there is a TODO(alex) next to
# the field naming the ambiguity, and the simplest reading is implemented.

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

# ---- Tooltips ----------------------------------------------------------------
# Kept together so the client can review the whole set in one place.

FW_TIPS <- list(
  site_name    = "A descriptive name for the site or waterbody, as you would refer to it in a report.",
  country      = "The country the site sits in. Pick the region as well where the site is in a territory recorded separately, such as Hawaii or the Galapagos Islands.",
  location     = "Click the map to drop a pin, or type coordinates directly. Either way the two boxes stay in step with the map.",
  waterbody    = "The kind of waterbody treated. Choose the closest match, or Other (specify) if none fits.",
  water_regime = "Still water is lentic, such as a lake or pond. Flowing water is lotic, such as a river or stream.",
  area         = "The size of the area treated. Hectares are usual for still water, kilometres for flowing water.",
  area_notes   = "Anything that qualifies the figure, for example five golf-course ponds.",
  depth        = "Average or estimated depth in metres.",
  volume       = "Estimated volume in cubic metres. An exact figure is preferred, but an estimate is far better than nothing.",
  max_flow     = "Maximum flow in cubic metres per second.",
  water_temp   = "Water temperature in degrees Celsius. Use the notes box if it varied, for example if the surface and the bottom of a lake differed.",
  invasive_taxa = "What kind of animal this target is. One group per target: if the eradication went after more than one, add another target below.",
  species      = "Common and scientific name, for example Common carp (Cyprinus carpio). Search by either. If your species is not listed, type it in and our review team will add it.",
  invasion_year = "The year the invasion happened, if it is known.",
  start_year   = "The year the eradication began.",
  end_year     = "The year the attempt was confirmed ended. Leave it blank if the work is ongoing.",
  duration     = "Estimated total duration of the intervention, in days.",
  driver       = "The main reason the eradication was carried out.",
  benefit_taxa = "What kind of species the eradication was meant to help.",
  benefit_sp   = "Entries such as Zooplankton or Cottidae spp. are fine. Our review team tidies these up.",
  method       = "The main eradication method used. Add further methods below if more than one was used.",
  method_notes = "Any detail on how the method was applied.",
  method_desc  = "A fuller description of the approach at this site.",
  labour       = "Effort required, in person-days. If you only have a range, put it in and we will work with it.",
  cost         = "We are not looking for a full breakdown, but an estimate helps build a picture of costs so funders and practitioners can benchmark interventions.",
  toxin_conc   = "Target concentration of the chemical in mg/L. A text box rather than a number, because the value can vary over a treatment.",
  neutralising = "The neutralising agent used, if any.",
  outcome      = "Eradication means the complete and permanent removal of the population (Genovesi 2005). You can record an attempt as successful without formal proof of absence; our review team records that distinction separately, so answer as you see it.",
  verification = "How the outcome was verified, and any notes on it.",
  reference    = "A link or citation for the underlying evidence. A DOI or URL is ideal.",
  source       = "Where this record came from. This is for our internal provenance and is not shown publicly.",
  contact      = "We need a primary contact so the review team can follow up on your submission.",
  notes        = "Anything else you would like the FWISE team to know."
)

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
    choices <- c(stats::setNames("", "Select..."), choices)
    if (is.null(selected)) selected <- ""
  }
  selectizeInput(id, label = NULL, choices = choices, selected = selected,
                 multiple = multiple, width = "100%",
                 options = list(placeholder = "Select..."))
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
    fw_field(fw_text_input(ns("site_name")), "Site name", required = TRUE,
             tooltip = FW_TIPS$site_name, input_id = ns("site_name")),
    # TODO(alex): the specification asks for "a searchable dropdown of Sovereign
    # ISO and Location ISO". The simplest reading is implemented: a country
    # dropdown from data/lookup_country.csv plus a free-text region box. If the
    # client wants a true two-level ISO picker, the lookup table already carries
    # iso3 and region and can drive it.
    fw_field(fw_select(ns("country"), choices$country), "Country", required = TRUE,
             tooltip = FW_TIPS$country, input_id = ns("country")),
    fw_field(fw_text_input(ns("region")), "Region or state",
             help = "Optional. Use it where the country alone is not specific enough.",
             input_id = ns("region")),

    div(
      class = "fw-field",
      div(class = "fw-field__label-row",
          tags$label(class = "form-label", "Location on map",
                     tags$span(class = "fw-required-mark", `aria-hidden` = "true", "*"),
                     tags$span(class = "fw-visually-hidden", " (required)")),
          fw_info(FW_TIPS$location, "location on map")),
      div(class = "fw-field__help",
          "Click the map to place your site, or type coordinates below."),
      div(class = "fw-map-picker",
          leaflet::leafletOutput(ns("picker"), height = 320)),
      div(
        class = "fw-coord-row",
        fw_field(fw_num_input(ns("latitude"), min = -90, max = 90, step = 0.000001),
                 "Latitude", input_id = ns("latitude")),
        fw_field(fw_num_input(ns("longitude"), min = -180, max = 180, step = 0.000001),
                 "Longitude", input_id = ns("longitude"))
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
             "Is it still water or flowing water?", required = TRUE,
             tooltip = FW_TIPS$water_regime, input_id = ns("water_regime")),

    fw_field(fw_select(ns("waterbody_type"), choices$waterbody_by_regime[[FW_ALL]]),
             "What kind of waterbody is it?", required = TRUE,
             tooltip = FW_TIPS$waterbody, input_id = ns("waterbody_type"),
             help = "The list narrows once you have answered still or flowing."),
    fw_other_panel(ns, "waterbody_type", "waterbody_type_other",
                   "Please describe the waterbody type"),

    div(
      class = "fw-coord-row",
      fw_field(fw_num_input(ns("area_treated"), min = 0),
               "Size of the area treated", tooltip = FW_TIPS$area,
               input_id = ns("area_treated")),
      # The unit becomes required once a value is entered. Enforced in the
      # validator rather than here, so the message appears next to the field.
      fw_field(fw_select(ns("area_unit"), choices$area_unit), "Unit",
               help = "Hectares for still water, kilometres for flowing water.",
               input_id = ns("area_unit"))
    ),
    fw_field(fw_area_input(ns("area_notes"), placeholder = "e.g. five golf-course ponds"),
             "Anything else about the area treated?", tooltip = FW_TIPS$area_notes,
             input_id = ns("area_notes")),

    fw_field(fw_num_input(ns("depth_m"), min = 0), "Average or estimated depth (m)",
             tooltip = FW_TIPS$depth, input_id = ns("depth_m")),
    fw_field(fw_area_input(ns("depth_notes"), rows = 2), "Notes on depth",
             input_id = ns("depth_notes")),

    fw_field(fw_num_input(ns("volume_m3"), min = 0), "Estimated volume (m3)",
             tooltip = FW_TIPS$volume, input_id = ns("volume_m3")),
    fw_field(fw_area_input(ns("volume_notes"), rows = 2), "Notes on volume",
             input_id = ns("volume_notes")),

    # Flow is a property of flowing water. Asking a contributor for the maximum
    # flow of a pond is asking them to leave a field blank and wonder whether
    # they have missed something. conditionalPanel rather than a server-side
    # toggle, so it costs no round trip.
    conditionalPanel(
      condition = sprintf("input['%s'] === 'Lotic'", ns("water_regime")),
      ns = shiny::NS(NULL),
      fw_field(fw_num_input(ns("max_flow_m3s"), min = 0), "Maximum flow (m3/s)",
               tooltip = FW_TIPS$max_flow, input_id = ns("max_flow_m3s"))
    ),

    fw_field(fw_num_input(ns("water_temp_c")), "Water temperature (degrees Celsius)",
             tooltip = FW_TIPS$water_temp, input_id = ns("water_temp_c")),
    fw_field(fw_area_input(ns("water_temp_notes"), rows = 2),
             "Notes on water temperature", input_id = ns("water_temp_notes"))
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
    fw_field(fw_num_input(ns("invasion_year"), min = 1500, max = this_year),
             "When did the invasion happen? (year)", tooltip = FW_TIPS$invasion_year,
             input_id = ns("invasion_year")),
    fw_field(fw_num_input(ns("start_year"), min = 1500, max = this_year),
             "Year the eradication began", required = TRUE,
             tooltip = FW_TIPS$start_year, input_id = ns("start_year")),
    fw_field(fw_num_input(ns("end_year"), min = 1500, max = this_year + 20),
             "Year the attempt was confirmed ended",
             help = "Leave blank if the work is ongoing.",
             tooltip = FW_TIPS$end_year, input_id = ns("end_year")),
    fw_field(fw_num_input(ns("duration_days"), min = 0),
             "Estimated total duration (days)", tooltip = FW_TIPS$duration,
             input_id = ns("duration_days")),
    fw_field(fw_select(ns("driver"), choices$driver),
             "What was the primary driver for the eradication attempt?",
             required = TRUE, tooltip = FW_TIPS$driver, input_id = ns("driver")),
    fw_other_panel(ns, "driver", "driver_other", "Please describe the driver")
  )
}

fw_step_benefit_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("benefit"),
    fw_field(fw_select(ns("beneficiary_taxa"), choices$beneficiary_taxa, multiple = TRUE),
             "What species benefited from the eradication?",
             tooltip = FW_TIPS$benefit_taxa, input_id = ns("beneficiary_taxa")),
    fw_other_panel(ns, "beneficiary_taxa", "beneficiary_taxa_other",
                   "Please describe the group that benefited", multiple = TRUE),

    tags$h3("Beneficiary species"),
    div(id = ns("benefit_rows")),
    actionButton(ns("add_beneficiary"), fw_t("contribute", "add_beneficiary"),
                 class = "btn btn-outline-primary btn-sm")
  )
}

fw_step_methods_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("methods"),
    tags$h3("Methods used"),
    # Row 1 inline, for the same reason as the targets above.
    div(id = ns("method_rows"), fw_method_row(ns, 1L, choices)),
    actionButton(ns("add_method"), fw_t("contribute", "add_method"),
                 class = "btn btn-outline-primary btn-sm"),

    tags$hr(),
    fw_field(fw_area_input(ns("method_description"), rows = 4),
             "A fuller description of the approach at this site",
             tooltip = FW_TIPS$method_desc, input_id = ns("method_description")),
    # TODO(alex): the specification asks for a numeric box "with a free-text
    # fallback for ranges". The simplest reading is implemented: one text box,
    # validated as a number only when it parses as one, so "20 to 30" is accepted
    # and passed through to QA.
    fw_field(fw_text_input(ns("labour_person_days")),
             "Effort required (person-days)", tooltip = FW_TIPS$labour,
             input_id = ns("labour_person_days")),
    fw_field(fw_num_input(ns("cost_estimate"), min = 0),
             "Estimated cost of the eradication attempt",
             help = paste("Include staff time and materials. Any currency is fine,",
                          "just say which in the notes."),
             tooltip = FW_TIPS$cost, input_id = ns("cost_estimate")),
    fw_field(fw_area_input(ns("cost_notes"), rows = 2), "Notes on cost",
             input_id = ns("cost_notes"))
  )
}

fw_step_chemical_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("chemical"),
    # TODO(alex): the specification lists a "Measured concentration notes" field
    # but no measured concentration value. Implemented as specified, notes only.
    fw_field(fw_text_input(ns("toxin_conc_target")),
             "Target concentration of the chemical (mg/L)",
             tooltip = FW_TIPS$toxin_conc, input_id = ns("toxin_conc_target")),
    fw_field(fw_area_input(ns("conc_target_notes"), rows = 3),
             "Notes on the target concentration", input_id = ns("conc_target_notes")),
    fw_field(fw_area_input(ns("conc_measured_notes"), rows = 3),
             "Notes on the measured concentration, if different",
             input_id = ns("conc_measured_notes")),
    fw_field(fw_select(ns("neutralising_agent"), choices$neutralising_agent),
             "Neutralising agent used, if any", tooltip = FW_TIPS$neutralising,
             input_id = ns("neutralising_agent")),
    fw_other_panel(ns, "neutralising_agent", "neutralising_agent_other",
                   "Please name the neutralising agent"),
    fw_field(fw_area_input(ns("neutralising_notes"), rows = 2),
             "Notes on the neutralising agent", input_id = ns("neutralising_notes"))
  )
}

fw_step_outcome_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("outcome"),
    fw_field(fw_select(ns("outcome"), choices$outcome), "What was the outcome?",
             required = TRUE, tooltip = FW_TIPS$outcome, input_id = ns("outcome")),
    # The specification is explicit that verification stays as one box on the
    # form and is split downstream if needed.
    fw_field(fw_area_input(ns("verification"), rows = 4),
             "How was the outcome verified? Method and any notes",
             tooltip = FW_TIPS$verification, input_id = ns("verification")),
    fw_field(fw_area_input(ns("reference"), rows = 3),
             "Link or citation for the underlying evidence",
             help = "A DOI or URL is ideal.", tooltip = FW_TIPS$reference,
             input_id = ns("reference")),
    fw_field(fw_text_input(ns("source")), "Where did this record come from?",
             help = "For our internal provenance. Not shown publicly.",
             tooltip = FW_TIPS$source, input_id = ns("source"))
  )
}

fw_step_contributor_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("contributor"),
    tags$h3("Primary contact"),
    fw_field(fw_text_input(ns("primary_contact_name")), "Name", required = TRUE,
             tooltip = FW_TIPS$contact, input_id = ns("primary_contact_name")),
    fw_field(fw_text_input(ns("primary_contact_email")), "Email", required = TRUE,
             input_id = ns("primary_contact_email")),
    fw_field(fw_text_input(ns("primary_contact_org")), "Organisation",
             input_id = ns("primary_contact_org")),

    tags$h3("Secondary contact"),
    p(class = "fw-caption", "Optional."),
    fw_field(fw_text_input(ns("secondary_contact_name")), "Name",
             input_id = ns("secondary_contact_name")),
    fw_field(fw_text_input(ns("secondary_contact_email")), "Email",
             input_id = ns("secondary_contact_email")),
    fw_field(fw_text_input(ns("secondary_contact_org")), "Organisation",
             input_id = ns("secondary_contact_org"))
  )
}

fw_step_other_ui <- function(ns, choices) {
  tagList(
    fw_step_intro("other"),
    fw_field(fw_area_input(ns("notes_for_fwise"), rows = 6), "Notes for FWISE",
             tooltip = FW_TIPS$notes, input_id = ns("notes_for_fwise"))
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
      placeholder = "Search by common or scientific name",
      create = TRUE,
      createOnBlur = TRUE,
      persist = FALSE,
      maxOptions = 1000
    )
  )
}

# ---- Repeatable rows ---------------------------------------------------------

#' One target: the group of animal, and the species within it
#'
#' Each row's inputs are namespaced with the row index, so removing row 2 does
#' not disturb rows 1 and 3. The species list is narrowed to the group by the
#' server; the family is never asked for, it is derived on submission.
fw_target_row <- function(ns, index, choices) {
  first <- index == 1
  taxa_id    <- paste0("target_taxa_", index)
  species_id <- paste0("target_species_", index)

  div(
    id = ns(paste0("target_row_", index)),
    class = "fw-repeat-row fw-repeat-row--stacked",
    div(
      div(class = "fw-repeat-row__heading", paste("Target", index)),
      fw_field(fw_select(ns(taxa_id), choices$invasive_taxa),
               "What kind of animal was targeted?", required = first,
               tooltip = FW_TIPS$invasive_taxa, input_id = ns(taxa_id)),
      fw_other_panel(ns, taxa_id, paste0("target_taxa_other_", index),
                     "Please describe the group targeted"),
      fw_field(fw_species_picker(ns(species_id), choices$species_by_taxa[[FW_ALL]]),
               "Which species?", required = first,
               tooltip = FW_TIPS$species, input_id = ns(species_id),
               help = paste("The list narrows to the group above. If your species",
                            "is not there, type it in and our review team will",
                            "add it."))
    ),
    div(
      class = "fw-repeat-row__remove",
      if (!first) {
        actionButton(ns(paste0("remove_target_", index)),
                     label = "Remove",
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}

#' One beneficiary species row
fw_species_row <- function(ns, index, choices, kind = "benefit") {
  row_id <- paste0(kind, "_row_", index)
  input_id <- paste0(kind, "_species_", index)

  div(
    id = ns(row_id),
    class = "fw-repeat-row",
    div(
      fw_field(
        fw_species_picker(ns(input_id), choices$species_by_taxa[[FW_ALL]]),
        label = paste("Beneficiary species", index),
        required = FALSE,
        tooltip = FW_TIPS$benefit_sp,
        input_id = ns(input_id)
      )
    ),
    div(
      class = "fw-repeat-row__remove",
      # "Remove" is the LABEL. With label = NULL and the word passed
      # positionally it landed in actionButton's `width`, which threw
      # '"Remove" is not a valid CSS unit' the moment a second row rendered.
      actionButton(ns(paste0("remove_", kind, "_", index)),
                   label = "Remove",
                   class = "btn btn-outline-primary btn-sm",
                   `aria-label` = paste(fw_t("contribute", "remove_row"), index))
    )
  )
}

fw_method_row <- function(ns, index, choices) {
  row_id <- paste0("method_row_", index)
  first <- index == 1

  div(
    id = ns(row_id),
    class = "fw-repeat-row",
    div(
      fw_field(fw_select(ns(paste0("method_", index)), choices$method),
               if (first) "Main eradication method" else paste("Method", index),
               required = first, tooltip = FW_TIPS$method,
               input_id = ns(paste0("method_", index))),
      fw_other_panel(ns, paste0("method_", index), paste0("method_other_", index),
                     "Please name the method"),
      fw_field(fw_area_input(ns(paste0("method_notes_", index)), rows = 2),
               "Notes on how this method was applied", tooltip = FW_TIPS$method_notes,
               input_id = ns(paste0("method_notes_", index)))
    ),
    div(
      class = "fw-repeat-row__remove",
      if (!first) {
        # See the note in fw_species_row(): the word is the label, not a
        # positional argument that falls through to `icon`.
        actionButton(ns(paste0("remove_method_", index)), label = "Remove",
                     class = "btn btn-outline-primary btn-sm",
                     `aria-label` = paste(fw_t("contribute", "remove_row"), index))
      }
    )
  )
}
