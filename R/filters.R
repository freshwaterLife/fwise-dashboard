# filters.R
# ONE filter engine, shared by the report builder and the dashboard.
#
# WHY THIS FILE EXISTS. The filter set used to be enumerated independently in
# four places - the clear-all name vector, the state snapshot, the zero-result
# hints and the export's "filters applied" sheet - so adding or removing a
# filter meant four edits and any one of them could be missed. Everything here
# is driven from FW_FILTERS instead. Add a filter to that list and it appears in
# the state, the clear-all, the hints and the workbook with no second edit.
#
# THE TWO PAGES DIFFER ON PURPOSE. The dashboard offers every filter. The report
# builder drops `outcome`, and that is a deliberate design decision rather than
# an oversight: see the note in mod_plan.R. Both pages therefore call
# fw_filter_ids() with a `drop` argument rather than keeping their own list.
#
# ANY-OF, NOT ONE-OF. Species, beneficiaries and methods live in bridge tables,
# so an attempt can carry several of each. Selecting "Rotenone" and "Draining"
# must match an attempt that used EITHER, not one that used both, and must not
# return the same attempt twice. That is why those filters resolve through a set
# of attempt ids rather than through a column comparison.
#
# NO SIZE FILTER. Treated size is recorded in hectares for still water and
# kilometers for flowing water, which are different quantities that cannot share
# a slider, and 173 of 914 attempts have no size at all. Size is still in the
# table and the export; it is deliberately not filterable.
#
# EVERY OPTION LIST COMES FROM THE APPROVED DATA, because fw_load_data() has
# already filtered the tables it is handed. Nothing here needs to think about
# approval - see fw_filter_approved() in data_load.R.

library(shiny)
library(dplyr)

# ---- The registry ------------------------------------------------------------

# Each entry: the copy key for its label, the control kind, and how it narrows
# the attempt table. `column` filters attempt directly; `bridge` resolves through
# a bridge table to a set of attempt ids.
#
# ORDER MATTERS TWICE: it is the order the controls are drawn in, and the order
# fw_filter_zero_hints() suggests relaxing them - narrowest choice first, since
# that is the one that most often emptied the result.
FW_FILTERS <- list(
  species = list(
    copy = "species", kind = "multi",
    bridge = "species", role = "invasive", match = "label",
    help = "any_note"
  ),
  beneficiary = list(
    copy = "beneficiary", kind = "multi",
    bridge = "species", role = "beneficiary", match = "label",
    help = "any_note"
  ),
  method = list(
    copy = "method", kind = "multi",
    bridge = "method", match = "method_name",
    help = "any_note"
  ),
  taxa = list(
    copy = "taxa", kind = "multi",
    bridge = "species", role = "invasive", match = "taxa"
  ),
  waterbody = list(copy = "waterbody", kind = "multi", column = "waterbody_type"),
  country   = list(copy = "country",   kind = "multi", column = "country"),
  # `labels` names a function that turns stored values into the wording the
  # picker showed, so the export's Filters sheet records the reader's selection
  # in the words they actually saw. Held as a NAME rather than the function
  # itself: this list is built when the file is sourced, and Shiny's source
  # order is not a thing to depend on.
  regime    = list(copy = "regime",    kind = "multi", column = "water_regime",
                   labels = "fw_regime_label"),
  outcome   = list(copy = "outcome",   kind = "multi", column = "outcome"),
  continent = list(copy = "continent", kind = "multi", column = "continent"),
  years     = list(copy = "years",     kind = "range")
)

#' The filter ids in the order they are drawn
#'
#' @param drop ids this page does not offer. The report builder passes
#'   "outcome"; see mod_plan.R for why.
fw_filter_ids <- function(drop = character(0)) {
  setdiff(names(FW_FILTERS), drop)
}

# The controls read better grouped by what they are about than in the order the
# zero-hints want, so the UI walks this instead of names(FW_FILTERS).
FW_FILTER_ORDER <- c("continent", "country", "regime", "waterbody",
                     "taxa", "species", "method", "beneficiary",
                     "outcome", "years")

fw_filter_draw_order <- function(drop = character(0)) {
  intersect(FW_FILTER_ORDER, fw_filter_ids(drop))
}

fw_filter_label <- function(id) fw_t("filters", FW_FILTERS[[id]]$copy)

# ---- Choices -----------------------------------------------------------------

#' Every filter's options, built once at startup
#'
#' Multi-value lists are ordered by how often each value actually appears, so
#' the answers a user is likely to want sit near the top rather than buried
#' alphabetically in a 390-entry list.
fw_filter_choices <- function(data) {
  species <- fw_species_label(data$species)
  sp_labels <- setNames(species$label, species$species_id)

  by_freq <- function(ids, labels) {
    tab <- sort(table(ids), decreasing = TRUE)
    unname(labels[match(names(tab), names(labels))])
  }

  role_ids <- function(role_name) {
    filter(data$attempt_species, role == role_name)
  }
  inv <- role_ids("invasive")
  ben <- role_ids("beneficiary")

  taxa_for <- function(bridge) {
    sort(unique(species$taxa[species$species_id %in% bridge$species_id &
                               !is.na(species$taxa)]))
  }

  list(
    continent   = sort(unique(data$attempt$continent)),
    country     = sort(unique(data$attempt$country)),
    # Shown as "Still water" / "Flowing water"; the value submitted is still
    # Lentic / Lotic. See fw_regime_choices() in data_load.R.
    regime      = fw_regime_choices(
      sort(unique(data$attempt$water_regime[!is.na(data$attempt$water_regime)]))),
    waterbody   = sort(unique(data$attempt$waterbody_type[!is.na(data$attempt$waterbody_type)])),
    taxa        = taxa_for(inv),
    species     = by_freq(inv$species_id, sp_labels),
    beneficiary = by_freq(ben$species_id, sp_labels),
    method      = data$method$method_name[order(match(
      data$method$method_id,
      names(sort(table(data$attempt_method$method_id), decreasing = TRUE))
    ))],
    # FIXED, not derived. These four are the analysis categories the paper uses
    # and the interface must show all of them even if a slice contains none.
    outcome     = c("Successful", "Failed", "Ongoing", "Unknown"),
    year_min    = min(data$attempt$start_year, na.rm = TRUE),
    year_max    = max(data$attempt$start_year, na.rm = TRUE),
    n_no_year   = sum(is.na(data$attempt$start_year))
  )
}

# ---- State -------------------------------------------------------------------

#' Read the filter inputs into a plain list
#'
#' On the report builder this is snapshotted at the moment Build is pressed, so
#' the results and the export describe the same selection even if the user then
#' changes a control. On the dashboard it is read live.
fw_filter_state <- function(input, ids = fw_filter_ids()) {
  g <- function(nm) {
    v <- input[[nm]]
    if (is.null(v)) character(0) else v[nzchar(v)]
  }
  out <- list()
  for (id in ids) {
    if (identical(FW_FILTERS[[id]]$kind, "range")) next
    out[[id]] <- g(id)
  }
  if ("years" %in% ids) {
    out$year_from <- input$years[1]
    out$year_to <- input$years[2]
    out$include_no_year <- isTRUE(input$include_no_year)
  }
  out$.ids <- ids
  out
}

#' Reset every control this page offers
fw_filter_clear <- function(session, ids, ch) {
  for (id in ids) {
    if (identical(FW_FILTERS[[id]]$kind, "multi")) {
      updateSelectizeInput(session, id, selected = character(0))
    }
  }
  if ("years" %in% ids) {
    updateSliderInput(session, "years", value = c(ch$year_min, ch$year_max))
    updateCheckboxInput(session, "include_no_year", value = TRUE)
  }
}

# ---- Applying ----------------------------------------------------------------

#' Apply the filters, returning the matching attempt rows
fw_filter_apply <- function(data, f) {
  a <- data$attempt
  ids <- f$.ids %||% fw_filter_ids()

  species <- NULL  # built once, only if a species-side filter is in play

  for (id in ids) {
    spec <- FW_FILTERS[[id]]
    vals <- f[[id]]
    if (identical(spec$kind, "range") || !length(vals)) next

    if (!is.null(spec$column)) {
      a <- a[a[[spec$column]] %in% vals, ]
      next
    }

    # ANY-OF over a bridge table.
    if (identical(spec$bridge, "species")) {
      if (is.null(species)) species <- fw_species_label(data$species)
      # Role is part of the question, not a detail: filtering on an invasive
      # species means "attempts against this species", never "attempts that
      # happened to benefit it".
      hit <- data$attempt_species |>
        filter(role == spec$role) |>
        left_join(select(species, species_id, label, taxa), by = "species_id") |>
        filter(.data[[spec$match]] %in% vals)
    } else {
      hit <- data$attempt_method |>
        left_join(select(data$method, method_id, method_name), by = "method_id") |>
        filter(.data[[spec$match]] %in% vals)
    }
    a <- a[a$attempt_id %in% unique(hit$attempt_id), ]
  }

  if (!"years" %in% ids) return(a)

  # The year range, and the deliberate decision about undated attempts.
  #
  # A missing bound means "no bound", not "nothing matches". The slider lives in
  # a renderUI, so its value is NULL until the browser paints it, and a NULL
  # silently collapsing the comparison to logical(0) would empty the whole
  # selection rather than leave it alone.
  from <- if (length(f$year_from)) f$year_from else -Inf
  to   <- if (length(f$year_to))   f$year_to   else  Inf
  in_range <- !is.na(a$start_year) & a$start_year >= from & a$start_year <= to
  a[in_range | (is.na(a$start_year) & isTRUE(f$include_no_year)), ]
}

#' Which filter to suggest relaxing when nothing matches
#'
#' Reported in FW_FILTERS order, which is narrowest choice first. Naming the
#' filter is the difference between a dead end and a next step.
fw_filter_zero_hints <- function(data, f) {
  ids <- f$.ids %||% fw_filter_ids()
  hints <- character(0)

  # Drop one filter at a time and see which one alone unblocks the result.
  for (id in ids) {
    if (identical(FW_FILTERS[[id]]$kind, "range") || !length(f[[id]])) next
    relaxed <- f
    relaxed[[id]] <- character(0)
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints <- c(hints, fw_filter_label(id))
    }
  }

  # The year range is the one filter that is always set, so it is only worth
  # suggesting when widening it actually helps.
  if (!length(hints) && "years" %in% ids) {
    relaxed <- f
    relaxed$year_from <- -Inf
    relaxed$year_to <- Inf
    relaxed$include_no_year <- TRUE
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints <- fw_filter_label("years")
    }
  }
  hints
}

# ---- Describing --------------------------------------------------------------

#' The applied filters as label/value pairs, for the export's Filters sheet
#'
#' Driven from the same registry as everything else, so a filter cannot be
#' offered on the page and then be missing from the spreadsheet that is supposed
#' to record what the reader selected.
fw_filter_summary <- function(f) {
  ids <- f$.ids %||% fw_filter_ids()
  rows <- list()
  for (id in ids) {
    if (identical(FW_FILTERS[[id]]$kind, "range")) next
    vals <- f[[id]]
    lab <- FW_FILTERS[[id]]$labels
    if (!is.null(lab) && length(vals)) vals <- match.fun(lab)(vals)
    rows[[length(rows) + 1L]] <- list(
      setting = fw_filter_label(id),
      value = if (length(vals)) paste(vals, collapse = FW_MULTI_SEP) else "All"
    )
  }
  if ("years" %in% ids) {
    rows[[length(rows) + 1L]] <- list(
      setting = fw_filter_label("years"),
      value = paste0(f$year_from %||% "-", " to ", f$year_to %||% "-")
    )
    rows[[length(rows) + 1L]] <- list(
      setting = fw_t("filters", "no_year"),
      value = if (isTRUE(f$include_no_year)) "Yes" else "No"
    )
  }
  rows
}
