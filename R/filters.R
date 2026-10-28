# filters.R
# One filter engine for Explore and Plan, driven by FW_FILTERS: state, apply,
# clear, zero-result hints and the export's Filters sheet all read it. Each page
# picks its filters with fw_filter_ids(drop = ...).
#
# Species, beneficiary and method filters are any-of, resolved through bridge
# tables to attempt ids.
#
# Size has one log-scaled slider per unit (ha for still water, km for flowing),
# showing only the units the regime selection allows; each attempt is matched
# against its own unit. Size and years have "include unrecorded" boxes, on by
# default, so missing values aren't silently dropped.
#
# Options come from approved data only (fw_filter_approved()).

library(shiny)
library(dplyr)

# ---- The registry ------------------------------------------------------------

# Each entry: label and tooltip copy keys, control kind, and how it narrows.
# `column` filters the attempt table; `bridge` goes through a bridge table.
# List order is the order fw_filter_zero_hints() suggests relaxing (narrowest
# first); draw order is FW_FILTER_ORDER.
FW_FILTERS = list(
  species = list(
    copy = "species", kind = "multi", tip = "tip_species",
    bridge = "species", role = "invasive", match = "label"
  ),
  beneficiary = list(
    copy = "beneficiary", kind = "multi", tip = "tip_beneficiary",
    bridge = "species", role = "beneficiary", match = "label"
  ),
  # `taxa` for the protected side.
  taxa_beneficiary = list(
    copy = "taxa_beneficiary", kind = "multi", tip = "tip_taxa_beneficiary",
    bridge = "species", role = "beneficiary", match = "taxa"
  ),
  method = list(
    copy = "method", kind = "multi", tip = "tip_method",
    bridge = "method", match = "method_name"
  ),
  taxa = list(
    copy = "taxa", kind = "multi", tip = "tip_taxa",
    bridge = "species", role = "invasive", match = "taxa"
  ),
  # Fish family, per side, shown only while Fish is picked (`when`). Only fish
  # have a family. Ignored while hidden, since Shiny keeps hidden inputs' values.
  family = list(
    copy = "family", kind = "multi", tip = "tip_family",
    bridge = "species", role = "invasive", match = "family",
    when = list(input = "taxa", value = "Fish")
  ),
  family_beneficiary = list(
    copy = "family_beneficiary", kind = "multi", tip = "tip_family_beneficiary",
    bridge = "species", role = "beneficiary", match = "family",
    when = list(input = "taxa_beneficiary", value = "Fish")
  ),
  country   = list(copy = "country",   kind = "multi", tip = "tip_country",
                   column = "country"),
  # `labels` names the function that turns stored values into picker wording,
  # for the Filters sheet. A name, not the function: source order varies.
  regime    = list(copy = "regime",    kind = "multi", tip = "tip_regime",
                   column = "water_regime", labels = "fw_regime_label"),
  outcome   = list(copy = "outcome",   kind = "multi", tip = "tip_outcome",
                   column = "outcome"),
  continent = list(copy = "continent", kind = "multi",
                   column = "continent"),
  # Two sliders (one per unit) plus the include-unrecorded box.
  size      = list(copy = "size",      kind = "size",  tip = "tip_size"),
  years     = list(copy = "years",     kind = "range", tip = "tip_years")
)

#' A filter's tooltip text, with its data-dependent placeholders filled in
#'
#' Fills {min} and {n}.
fw_filter_tip = function(id, ch = NULL) {
  spec = FW_FILTERS[[id]]
  if (is.null(spec$tip)) return(NULL)
  txt = fw_t("filters", spec$tip)
  if (!is.null(ch)) {
    txt = fw_fill(txt, min = ch$year_min, n = ch$n_no_year)
  }
  txt
}

#' The filter ids in the order they are drawn
#'
#' @param drop ids this page doesn't offer
fw_filter_ids = function(drop = character(0)) {
  setdiff(names(FW_FILTERS), drop)
}

# Reading order: place, water, invasive side, protected side (same pattern),
# then ranges. Used by the UI and the Filters sheet.
FW_FILTER_ORDER = c("continent", "country", "regime",
                     "taxa", "family", "species", "method",
                     "taxa_beneficiary", "family_beneficiary", "beneficiary",
                     "outcome", "size", "years")

#' The order the filter PANEL draws its controls in
#'
#' FW_FILTER_ORDER with the sliders moved last, so the grid stays even.
#' Drawing only.
fw_filter_draw_order = function(drop = character(0)) {
  ids = intersect(FW_FILTER_ORDER, fw_filter_ids(drop))
  is_range = vapply(ids, function(id) FW_FILTERS[[id]]$kind %in% c("range", "size"),
                     logical(1))
  c(ids[!is_range], ids[is_range])
}

fw_filter_label = function(id) fw_t("filters", FW_FILTERS[[id]]$copy)

# ---- Size, the one filter with two units -------------------------------------

# Unit codes, in slider order.
FW_SIZE_UNITS = c("ha", "km")

# The unit each stored regime is measured in.
FW_REGIME_UNITS = list(Lentic = "ha", Lotic = "km")

#' The units worth offering for a regime selection
#'
#' Still water offers ha, flowing water km, neither or both offers both. An
#' attempt whose unit disagrees with its regime isn't size-filtered while its
#' unit's slider is hidden. fw_filter_state() must drop hidden sliders too, or
#' Shiny's kept values filter invisibly.
#'
#' @param regime the reader's `regime` selection, possibly empty
fw_size_units = function(regime = character(0)) {
  if (!length(regime)) return(FW_SIZE_UNITS)
  out = intersect(FW_SIZE_UNITS,
                   unlist(FW_REGIME_UNITS[regime], use.names = FALSE))
  # Never hide both sliders.
  if (!length(out)) FW_SIZE_UNITS else out
}

#' The real-unit bounds of each size slider, and how many rows have no size
#'
fw_size_bounds = function(a) {
  one = function(unit) {
    v = a$area_treated[!is.na(a$area_treated) & a$area_treated > 0 &
                          !is.na(a$area_unit) & a$area_unit == unit]
    if (!length(v)) return(NULL)
    range(v)
  }
  out = lapply(FW_SIZE_UNITS, one)
  names(out) = FW_SIZE_UNITS
  out$n_no_size = sum(is.na(a$area_treated))
  out
}

# Sliders work in log10(1 + x), matching in real units. The only conversions.
# 1 + x so both sliders start at 0.
fw_size_log = function(x) log10(1 + x)
fw_size_unlog = function(x) 10^x - 1

# Positions on a size slider. Step = range / this, so both ends sit on the grid.
FW_SIZE_POSITIONS = 1000

#' The next round number at or above x: 1, 2 or 5 times a power of ten
#'
#' The top of each size slider, e.g. 500,000 rather than 237,500.
fw_nice_ceiling = function(x) {
  if (!length(x) || is.na(x) || x <= 0) return(1)
  k = 10^floor(log10(x))
  for (m in c(1, 2, 5, 10)) if (m * k >= x * (1 - 1e-9)) return(m * k)
}

#' A size slider's ends, in slider units: 0 up to a round number above the max
#'
#' From the whole database, so the scale doesn't shrink with the selection.
fw_size_log_range = function(bounds) {
  if (is.null(bounds)) return(NULL)
  c(0, fw_size_log(fw_nice_ceiling(bounds[2])))
}

#' The step a size slider moves in. See FW_SIZE_POSITIONS.
fw_size_log_step = function(r) diff(r) / FW_SIZE_POSITIONS

#' A size printed for a human, at a sensible number of digits
#'
#' Fewer decimals for larger sizes, for the slider readout and Filters sheet.
fw_size_label = function(x) {
  if (!length(x) || is.na(x)) return(fw_t("common", "empty_value"))
  # The slider's left end, which floating point brings back as 1e-16 or so.
  if (abs(x) < 1e-9) return(fw_fmt_num(0))
  digits = if (x >= 100) 0 else if (x >= 10) 1 else if (x >= 1) 2 else 4
  fw_fmt_num(round(x, digits))
}

#' Which attempts a size selection keeps
#'
#' Each attempt is compared against its own unit's slider. A unit with no
#' slider in the state doesn't constrain.
#'
#' @return a logical vector along `a`
fw_size_match = function(a, f) {
  keep = rep(TRUE, nrow(a))
  for (unit in FW_SIZE_UNITS) {
    b = f[[paste0("size_", unit)]]
    if (length(b) != 2 || anyNA(b)) next
    # Slack for floating-point round trips at the ends.
    lo = fw_size_unlog(b[1]) * (1 - 1e-9) - 1e-12
    hi = fw_size_unlog(b[2]) * (1 + 1e-9) + 1e-12
    is_unit = !is.na(a$area_unit) & a$area_unit == unit & !is.na(a$area_treated)
    keep[is_unit] = a$area_treated[is_unit] >= lo & a$area_treated[is_unit] <= hi
  }
  # Unmeasured attempts follow the checkbox alone. Assignment, not `|`: keep
  # starts all-TRUE, so an OR would never drop them.
  keep[is.na(a$area_treated)] = isTRUE(f$include_no_size)
  keep
}

# ---- Choices -----------------------------------------------------------------

#' Every filter's options, built once at startup
#'
#' Species and methods are ordered most frequent first.
fw_filter_choices = function(data) {
  species = fw_species_label(data$species)
  sp_labels = setNames(species$label, species$species_id)

  by_freq = function(ids, labels) {
    tab = sort(table(ids), decreasing = TRUE)
    unname(labels[match(names(tab), names(labels))])
  }

  role_ids = function(role_name) {
    filter(data$attempt_species, role == role_name)
  }
  inv = role_ids("invasive")
  ben = role_ids("beneficiary")

  taxa_for = function(bridge) {
    sort(unique(species$taxa[species$species_id %in% bridge$species_id &
                               !is.na(species$taxa)]))
  }
  family_for = function(bridge) {
    sort(unique(species$family[species$species_id %in% bridge$species_id &
                                 !is.na(species$family)]))
  }

  list(
    continent   = sort(unique(data$attempt$continent)),
    country     = sort(unique(data$attempt$country)),
    # Shown as Still/Flowing water, submitted as Lentic/Lotic.
    regime      = fw_regime_choices(
      sort(unique(data$attempt$water_regime[!is.na(data$attempt$water_regime)]))),
    taxa        = taxa_for(inv),
    species     = by_freq(inv$species_id, sp_labels),
    beneficiary = by_freq(ben$species_id, sp_labels),
    taxa_beneficiary = taxa_for(ben),
    family      = family_for(inv),
    family_beneficiary = family_for(ben),
    method      = data$method$method_name[order(match(
      data$method$method_id,
      names(sort(table(data$attempt_method$method_id), decreasing = TRUE))
    ))],
    # Fixed: the paper's four categories, always all shown.
    outcome     = c("Successful", "Failed", "Ongoing", "Unknown"),
    year_min    = min(data$attempt$start_year, na.rm = TRUE),
    year_max    = max(data$attempt$start_year, na.rm = TRUE),
    n_no_year   = sum(is.na(data$attempt$start_year)),
    # Real-unit bounds per unit and the no-size count. See fw_size_bounds().
    size        = fw_size_bounds(data$attempt)
  )
}

# ---- State -------------------------------------------------------------------

#' Read the filter inputs into a plain list
#'
#' Snapshotted on Build on Plan; read live on Explore.
#' @param ch the choice lists. Optional: used to record each slider's full
#'   range, so the summary can print "All" for an untouched slider.
fw_filter_state = function(input, ids = fw_filter_ids(), ch = NULL) {
  g = function(nm) {
    v = input[[nm]]
    if (is.null(v)) character(0) else v[nzchar(v)]
  }
  out = list()
  for (id in ids) {
    if (FW_FILTERS[[id]]$kind %in% c("range", "size")) next
    out[[id]] = g(id)
    when = FW_FILTERS[[id]]$when
    if (!is.null(when) && !when$value %in% g(when$input)) out[[id]] = character(0)
  }
  if ("size" %in% ids) {
    # Only the sliders currently shown: Shiny keeps hidden sliders' values.
    # An absent unit means "not constrained" to fw_size_match().
    offered = fw_size_units(input$regime %||% character(0))
    for (unit in FW_SIZE_UNITS) {
      if (!unit %in% offered) next
      out[[paste0("size_", unit)]] = input[[paste0("size_", unit)]]
      if (!is.null(ch)) {
        out[[paste0("size_full_", unit)]] = fw_size_log_range(ch$size[[unit]])
      }
    }
    out$include_no_size = isTRUE(input$include_no_size)
  }
  if ("years" %in% ids) {
    out$year_from = input$years[1]
    out$year_to = input$years[2]
    out$include_no_year = isTRUE(input$include_no_year)
    # The slider's full range, so a summary can tell untouched from chosen.
    if (!is.null(ch)) out$year_full = c(ch$year_min, ch$year_max)
  }
  out$.ids = ids
  out
}

#' Reset every control this page offers
fw_filter_clear = function(session, ids, ch) {
  for (id in ids) {
    if (identical(FW_FILTERS[[id]]$kind, "multi")) {
      updateSelectizeInput(session, id, selected = character(0))
    }
  }
  if ("size" %in% ids) {
    for (unit in FW_SIZE_UNITS) {
      r = fw_size_log_range(ch$size[[unit]])
      if (!is.null(r)) updateSliderInput(session, paste0("size_", unit), value = r)
    }
    updateCheckboxInput(session, "include_no_size", value = TRUE)
  }
  if ("years" %in% ids) {
    updateSliderInput(session, "years", value = c(ch$year_min, ch$year_max))
    updateCheckboxInput(session, "include_no_year", value = TRUE)
  }
}

#' The state a page's controls are in straight after fw_filter_clear()
#'
#' Lets Clear rebuild the default report in the same flush. dev/plan_test.R
#' checks it matches fw_filter_state().
fw_filter_defaults = function(ids, ch) {
  input = list(include_no_size = TRUE, include_no_year = TRUE)
  if ("size" %in% ids) {
    for (unit in FW_SIZE_UNITS) {
      input[[paste0("size_", unit)]] = fw_size_log_range(ch$size[[unit]])
    }
  }
  if ("years" %in% ids) input$years = c(ch$year_min, ch$year_max)
  fw_filter_state(input, ids, ch = ch)
}

# ---- Linked species ----------------------------------------------------------

#' The species labels a picker may offer, given the kind of animal and family
#'
#' Empty taxa or family means no narrowing. Keeps the order of `all`.
#'
#' @param sp species labels with their taxa and family, from fw_species_label()
#' @param taxa,family what the reader chose, possibly empty
#' @param all the picker's full list of labels
fw_species_allowed = function(sp, taxa, family, all) {
  keep = rep(TRUE, nrow(sp))
  if (length(taxa)) keep = keep & sp$taxa %in% taxa
  if (length(family)) keep = keep & sp$family %in% family
  all[all %in% sp$label[keep]]
}

# The protected-side pickers, narrowed by every other filter.
FW_PROTECTED_FILTERS = c("taxa_beneficiary", "family_beneficiary", "beneficiary")

#' What the protected pickers may offer, given every other filter
#'
#' Only what the attempts matching every other filter protected; species
#' also follow the protected taxa and family. Pure; fw_link_species_filters()
#' wires it up.
#'
#' @param data the loaded tables
#' @param sp fw_species_label(data$species)
#' @param attempt_ids the attempts matching every non-protected filter
#' @param taxa,family the reader's protected kind of animal and family
#' @param ch fw_filter_choices(data), whose order each list keeps
#' @return list(taxa_beneficiary, family_beneficiary, beneficiary)
fw_protected_allowed = function(data, sp, attempt_ids, taxa, family, ch) {
  ben_ids = unique(data$attempt_species$species_id[
    data$attempt_species$role == "beneficiary" &
      data$attempt_species$attempt_id %in% attempt_ids])
  here = sp[sp$species_id %in% ben_ids, ]
  list(
    taxa_beneficiary   = ch$taxa_beneficiary[ch$taxa_beneficiary %in% here$taxa],
    family_beneficiary = ch$family_beneficiary[ch$family_beneficiary %in% here$family],
    beneficiary        = fw_species_allowed(here, taxa, family, ch$beneficiary)
  )
}

#' Wire the species pickers to the filters that should narrow them
#'
#' Invasive: family follows taxa; species follows both. Protected: see
#' fw_protected_allowed(). Doesn't loop: each observer writes only to pickers it
#' doesn't read, and an unchanged write invalidates nothing. Pickers not on the
#' page are skipped. A hidden family doesn't narrow.
#'
#' @param ids the filter ids the page draws
fw_link_species_filters = function(input, session, data, choices, ids) {
  sp = fw_species_label(data$species)
  picked = function(x) if (length(x)) as.character(x) else character(0)
  shown = function(id) {
    if (!id %in% ids) return(character(0))
    when = FW_FILTERS[[id]]$when
    if (!is.null(when) && !when$value %in% picked(input[[when$input]])) {
      return(character(0))
    }
    picked(input[[id]])
  }
  update = function(id, allowed) {
    shiny::updateSelectizeInput(session, id, choices = allowed,
                                selected = intersect(picked(input[[id]]), allowed))
  }

  # ---- Invasive: within its own side ----
  if ("family" %in% ids) {
    shiny::observeEvent(picked(input$taxa), {
      fam = sp$family[!is.na(sp$family) &
                         (!length(picked(input$taxa)) | sp$taxa %in% picked(input$taxa))]
      update("family", choices$family[choices$family %in% fam])
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }
  if ("species" %in% ids) {
    shiny::observeEvent(list(picked(input$taxa), shown("family")), {
      update("species", fw_species_allowed(sp, picked(input$taxa), shown("family"),
                                           choices$species))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }

  # ---- Protected: by everything else ----
  others = setdiff(ids, FW_PROTECTED_FILTERS)
  if (!length(intersect(ids, FW_PROTECTED_FILTERS))) return(invisible(NULL))
  # Debounced: sliders send a value per pixel of a drag.
  relevant = shiny::debounce(shiny::reactive({
    f = fw_filter_state(input, others, ch = choices)
    fw_filter_apply(data, f)$attempt_id
  }), 300)
  shiny::observeEvent(list(relevant(), shown("taxa_beneficiary"),
                           shown("family_beneficiary")), {
    allowed = fw_protected_allowed(data, sp, relevant(),
                                    shown("taxa_beneficiary"),
                                    shown("family_beneficiary"), choices)
    for (id in intersect(FW_PROTECTED_FILTERS, ids)) update(id, allowed[[id]])
  }, ignoreInit = TRUE)
  invisible(NULL)
}

# ---- Linked geography --------------------------------------------------------

#' The values one geography picker may still offer, given the other
#'
#' Each picker offers only what's possible given the other, so "Europe and
#' Australia" can't be asked for. Pure; fw_link_geo_filters() wires it up.
#'
#' @param geo distinct continent/country pairs from the attempt table
#' @param from the column the reader has chosen in
#' @param to the column being narrowed
#' @param keep what they chose, or empty for "no choice"
#' @param all what `to` offers when nothing is chosen
#' @return the allowed values of `to`, sorted
fw_geo_allowed = function(geo, from, to, keep, all) {
  # No choice means no narrowing: restore the full list.
  if (!length(keep)) return(all)
  sort(unique(geo[[to]][geo[[from]] %in% keep]))
}

#' Wire the two geography pickers together for one page
#'
#' Both directions. Used by Explore and Plan. Doesn't loop: each observer
#' writes only to the other picker, and an unchanged value doesn't invalidate.
#'
#' @param input,session the calling module's own input and session
#' @param data the loaded data, for its attempt table
#' @param choices fw_filter_choices(data), for the full list each picker
#'   returns to when the other is cleared
#' @return invisibly, the two observers
fw_link_geo_filters = function(input, session, data, choices) {
  geo = unique(data$attempt[, c("continent", "country")])
  geo = geo[!is.na(geo$continent) & !is.na(geo$country), ]

  # NULL and character(0) must compare equal, or the observers ping-pong.
  picked = function(x) if (length(x)) as.character(x) else character(0)

  a = shiny::observeEvent(picked(input$continent), {
    allowed = fw_geo_allowed(geo, "continent", "country",
                              picked(input$continent), choices$country)
    shiny::updateSelectizeInput(session, "country", choices = allowed,
                                selected = intersect(picked(input$country),
                                                     allowed))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  b = shiny::observeEvent(picked(input$country), {
    allowed = fw_geo_allowed(geo, "country", "continent",
                              picked(input$country), choices$continent)
    shiny::updateSelectizeInput(session, "continent", choices = allowed,
                                selected = intersect(picked(input$continent),
                                                     allowed))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  invisible(list(a, b))
}

#' Draw a filter only while its `when` condition holds
#'
#' conditionalPanel's display:none removes the grid cell, leaving no gap. A
#' filter with no `when` is returned unchanged.
#'
#' @param ns the calling module's namespace function
#' @param id the filter id in FW_FILTERS
#' @param control the already-built control for that filter
fw_filter_when_panel = function(ns, id, control) {
  when = FW_FILTERS[[id]]$when
  if (is.null(when)) return(control)
  shiny::conditionalPanel(
    sprintf("(input['%s'] || []).indexOf('%s') > -1", ns(when$input), when$value),
    control
  )
}

#' Empty a `when` filter once its condition stops holding
#'
#' So it doesn't come back pre-set when Fish is picked again.
#'
#' @param input,session the calling module's own input and session
#' @param ids the filter ids the page draws
fw_filter_when_observers = function(input, session, ids) {
  for (fid in intersect(ids, names(Filter(function(x) !is.null(x$when), FW_FILTERS)))) {
    local({
      id = fid
      when = FW_FILTERS[[id]]$when
      shiny::observeEvent(input[[when$input]], ignoreNULL = FALSE, {
        if (!when$value %in% (input[[when$input]] %||% character(0)) && length(input[[id]])) {
          shiny::updateSelectizeInput(session, id, selected = character(0))
        }
      })
    })
  }
  invisible(NULL)
}

# ---- Applying ----------------------------------------------------------------

#' Apply the filters, returning the matching attempt rows
fw_filter_apply = function(data, f) {
  a = data$attempt
  ids = f$.ids %||% fw_filter_ids()

  species = NULL  # built once, only if a species-side filter is in play

  for (id in ids) {
    spec = FW_FILTERS[[id]]
    vals = f[[id]]
    if (spec$kind %in% c("range", "size") || !length(vals)) next

    if (!is.null(spec$column)) {
      a = a[a[[spec$column]] %in% vals, ]
      next
    }

    # ANY-OF over a bridge table.
    if (identical(spec$bridge, "species")) {
      if (is.null(species)) species = fw_species_label(data$species)
      # Role matters: an invasive species filter means attempts against it.
      hit = data$attempt_species |>
        filter(role == spec$role) |>
        left_join(select(species, species_id, label, taxa, family), by = "species_id") |>
        filter(.data[[spec$match]] %in% vals)
    } else {
      hit = data$attempt_method |>
        left_join(select(data$method, method_id, method_name), by = "method_id") |>
        filter(.data[[spec$match]] %in% vals)
    }
    a = a[a$attempt_id %in% unique(hit$attempt_id), ]
  }

  # Size before years, since the year block returns.
  if ("size" %in% ids) a = a[fw_size_match(a, f), ]

  if (!"years" %in% ids) return(a)

  # A missing bound means no bound (the slider is NULL until it renders).
  from = if (length(f$year_from)) f$year_from else -Inf
  to   = if (length(f$year_to))   f$year_to   else  Inf
  in_range = !is.na(a$start_year) & a$start_year >= from & a$start_year <= to
  a[in_range | (is.na(a$start_year) & isTRUE(f$include_no_year)), ]
}

#' Which filter to suggest relaxing when nothing matches
#'
#' In FW_FILTERS order, narrowest first.
fw_filter_zero_hints = function(data, f) {
  ids = f$.ids %||% fw_filter_ids()
  hints = character(0)

  # Drop one filter at a time and see which one alone unblocks the result.
  for (id in ids) {
    if (FW_FILTERS[[id]]$kind %in% c("range", "size") || !length(f[[id]])) next
    relaxed = f
    relaxed[[id]] = character(0)
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints = c(hints, fw_filter_label(id))
    }
  }

  # Size and years are always set, so test widening each separately.
  if (!length(hints) && "size" %in% ids) {
    relaxed = f
    for (unit in FW_SIZE_UNITS) relaxed[[paste0("size_", unit)]] = NULL
    relaxed$include_no_size = TRUE
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints = fw_filter_label("size")
    }
  }
  if (!length(hints) && "years" %in% ids) {
    relaxed = f
    relaxed$year_from = -Inf
    relaxed$year_to = Inf
    relaxed$include_no_year = TRUE
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints = fw_filter_label("years")
    }
  }
  hints
}

# ---- Describing --------------------------------------------------------------

#' The applied filters as label/value pairs, for the export's Filters sheet
#'
fw_filter_summary = function(f) {
  ids = f$.ids %||% fw_filter_ids()
  rows = list()
  for (id in ids) {
    if (FW_FILTERS[[id]]$kind %in% c("range", "size")) next
    vals = f[[id]]
    lab = FW_FILTERS[[id]]$labels
    if (!is.null(lab) && length(vals)) vals = match.fun(lab)(vals)
    rows[[length(rows) + 1L]] = list(
      setting = fw_filter_label(id),
      value = if (length(vals)) paste(vals, collapse = FW_MULTI_SEP)
              else fw_t("export", "filter_all")
    )
  }
  if ("size" %in% ids) {
    # In real units, not log10.
    for (unit in FW_SIZE_UNITS) {
      b = f[[paste0("size_", unit)]]
      full = f[[paste0("size_full_", unit)]]
      # An untouched slider prints "All", not its rounded ends.
      untouched = !length(b) || (length(full) == 2 && isTRUE(all.equal(b, full)))
      rows[[length(rows) + 1L]] = list(
        setting = paste0(fw_filter_label("size"), " (", fw_t("filters", paste0("unit_", unit)), ")"),
        value = if (!untouched && length(b) == 2 && !anyNA(b)) {
          paste0(fw_size_label(fw_size_unlog(b[1])), fw_t("export", "range_sep"),
                 fw_size_label(fw_size_unlog(b[2])))
        } else fw_t("export", "filter_all")
      )
    }
    rows[[length(rows) + 1L]] = list(
      setting = fw_t("filters", "no_size"),
      value = if (isTRUE(f$include_no_size)) fw_t("export", "filter_yes")
              else fw_t("export", "filter_no")
    )
  }
  if ("years" %in% ids) {
    rows[[length(rows) + 1L]] = list(
      setting = fw_filter_label("years"),
      # An untouched slider is flagged so the PDF can skip it.
      untouched = length(f$year_full) == 2 &&
        isTRUE(all.equal(c(f$year_from, f$year_to), as.numeric(f$year_full))),
      value = paste0(f$year_from %||% fw_t("export", "range_missing"),
                     fw_t("export", "range_sep"),
                     f$year_to %||% fw_t("export", "range_missing"))
    )
    rows[[length(rows) + 1L]] = list(
      setting = fw_t("filters", "no_year"),
      value = if (isTRUE(f$include_no_year)) fw_t("export", "filter_yes")
              else fw_t("export", "filter_no")
    )
  }
  rows
}
