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
# THE SIZE FILTER IS UNIT-AWARE, AND THAT IS WHY IT IS NOT AN ORDINARY RANGE.
# Treated size is recorded in hectares for still water and kilometres for flowing
# water. Those are different quantities and cannot share one slider, so the
# control offers one slider per unit and shows only the unit the reader's regime
# selection is measured in - see fw_size_units() below, and the note there about
# why the hidden slider has to be dropped from the state as well as from the UI.
# An attempt is matched against the slider for ITS OWN unit and is untouched by
# the other one.
#
# It is also LOG SCALED. Hectares run from 0.0014 to 237,500 with a median of
# 3.4, so on a linear slider every value a reader might want sits inside the
# first pixel. The slider's positions are log10 and the comparison is done on
# real values; the chosen bounds are printed underneath in real units.
#
# 173 of 914 attempts have no size at all, so it carries an "include unrecorded"
# checkbox defaulting to TRUE, for the same reason the year range does: a silent
# drop is indistinguishable from "none matched".
#
# EVERY OPTION LIST COMES FROM THE APPROVED DATA, because fw_load_data() has
# already filtered the tables it is handed. Nothing here needs to think about
# approval - see fw_filter_approved() in data_load.R.

library(shiny)
library(dplyr)

# ---- The registry ------------------------------------------------------------

# Each entry: the copy key for its label, the copy key for its tooltip, the
# control kind, and how it narrows the attempt table. `column` filters attempt
# directly; `bridge` resolves through a bridge table to a set of attempt ids.
#
# EVERY FILTER CARRIES A `tip`. The guidance used to be a line of prose printed
# under four of the nine controls and nothing at all under the other five, which
# made the panel read as a form to be filled in and left half of it unexplained.
# It is the same guidance; it is now asked for rather than issued. Naming the
# key HERE rather than at either page's call site is what keeps the report
# builder and the dashboard from drifting apart - both draw from this list.
#
# ORDER MATTERS TWICE: it is the order the controls are drawn in, and the order
# fw_filter_zero_hints() suggests relaxing them - narrowest choice first, since
# that is the one that most often emptied the result.
FW_FILTERS <- list(
  species = list(
    copy = "species", kind = "multi", tip = "tip_species",
    bridge = "species", role = "invasive", match = "label"
  ),
  beneficiary = list(
    copy = "beneficiary", kind = "multi", tip = "tip_beneficiary",
    bridge = "species", role = "beneficiary", match = "label"
  ),
  # The beneficiary side's answer to `taxa`. Same bridge, other role: "attempts
  # meant to help a fish" rather than "attempts against one".
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
  # FISH FAMILY, one per side, and each only while Fish is picked in its own
  # kind-of-animal filter (`when`). Only fish carry a family in species.csv, so
  # the question does not exist until the reader has said "fish". Hidden, it is
  # not applied either: fw_filter_state() reads `when` and drops the value,
  # because Shiny keeps the last value of an input the reader can no longer see.
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
  # NO TIP (client, 23 Sept 2026). Its tooltip only restated the label, and
  # fw_filter_tip() returns NULL for a filter without one, which fw_field()
  # draws as no (i) button at all. Same for `continent` below.
  waterbody = list(copy = "waterbody", kind = "multi",
                   column = "waterbody_type"),
  country   = list(copy = "country",   kind = "multi", tip = "tip_country",
                   column = "country"),
  # `labels` names a function that turns stored values into the wording the
  # picker showed, so the export's Filters sheet records the reader's selection
  # in the words they actually saw. Held as a NAME rather than the function
  # itself: this list is built when the file is sourced, and Shiny's source
  # order is not a thing to depend on.
  regime    = list(copy = "regime",    kind = "multi", tip = "tip_regime",
                   column = "water_regime", labels = "fw_regime_label"),
  outcome   = list(copy = "outcome",   kind = "multi", tip = "tip_outcome",
                   column = "outcome"),
  continent = list(copy = "continent", kind = "multi",
                   column = "continent"),
  # kind = "size" rather than "range": two sliders in one cell, each in its own
  # unit, plus the include-unrecorded checkbox. The engine branches on this in
  # state, apply, clear and summary.
  size      = list(copy = "size",      kind = "size",  tip = "tip_size"),
  years     = list(copy = "years",     kind = "range", tip = "tip_years")
)

#' A filter's tooltip text, with its data-dependent placeholders filled in
#'
#' {min} and {n} are the two the copy uses. Substituted here rather than at each
#' call site so the report builder and the dashboard cannot fill them in
#' differently - or one of them forget to.
fw_filter_tip <- function(id, ch = NULL) {
  spec <- FW_FILTERS[[id]]
  if (is.null(spec$tip)) return(NULL)
  txt <- fw_t("filters", spec$tip)
  if (!is.null(ch)) {
    txt <- fw_fill(txt, min = ch$year_min, n = ch$n_no_year)
  }
  txt
}

#' The filter ids in the order they are drawn
#'
#' @param drop ids this page does not offer. The report builder passes
#'   "outcome"; see mod_plan.R for why.
fw_filter_ids <- function(drop = character(0)) {
  setdiff(names(FW_FILTERS), drop)
}

# The controls read better grouped by what they are about than in the order the
# zero-hints want, so the UI walks this instead of names(FW_FILTERS). The
# protected side runs kind, family, species - the same way round as the invasive
# side, so the two read as a pair (client, Sept 2026 user testing).
FW_FILTER_ORDER <- c("continent", "country", "regime", "waterbody",
                     "taxa", "family", "species", "method",
                     "taxa_beneficiary", "family_beneficiary", "beneficiary",
                     "outcome", "size", "years")

#' The order the filter PANEL draws its controls in
#'
#' FW_FILTER_ORDER is the reading order - place, then water, then species, then
#' the ranges - and it is what the export's Filters sheet follows. The panel is
#' a grid, and in a grid the two sliders have to come last whatever else moves:
#' a slider is twice the height of a picker and needs room for its handles and
#' its readout, so one sitting mid-grid leaves a ragged hole beside it and drags
#' the row below out of line. Pickers first, ranges after, and the grid stays
#' even.
#'
#' The order is only ever about drawing. The matching, the summary and the sheet
#' all read FW_FILTER_ORDER directly and are untouched by this.
fw_filter_draw_order <- function(drop = character(0)) {
  ids <- intersect(FW_FILTER_ORDER, fw_filter_ids(drop))
  is_range <- vapply(ids, function(id) FW_FILTERS[[id]]$kind %in% c("range", "size"),
                     logical(1))
  c(ids[!is_range], ids[is_range])
}

fw_filter_label <- function(id) fw_t("filters", FW_FILTERS[[id]]$copy)

# ---- Size, the one filter with two units -------------------------------------

# The stored unit codes, and the order the sliders are drawn in. Labels are copy.
FW_SIZE_UNITS <- c("ha", "km")

# Which unit each stored regime is measured in. "Lentic" and "Lotic" are the
# STORED vocabulary, not labels - see FW_REGIME_LABELS in data_load.R, which is
# the only place they are turned into words a reader sees.
FW_REGIME_UNITS <- list(Lentic = "ha", Lotic = "km")

#' The units worth offering for a regime selection
#'
#' STILL WATER IS AN AREA, FLOWING WATER IS A LENGTH. Choosing "still water"
#' offers hectares, "flowing water" offers kilometres, and choosing neither or
#' both offers both. That is the client's rule and it is what a reader expects
#' the control to do.
#'
#' The data does not fully agree with it yet:
#'
#'     regime    ha    km   (none)
#'     Lentic   497     6       81
#'     Lotic     34   203       87
#'
#' Forty attempts carry a unit that disagrees with their regime. Under this map
#' those forty are NOT filtered by size - choosing still water hides the
#' kilometre slider, and the six kilometre-measured Lentic attempts pass the
#' size filter untouched rather than being judged against a slider the reader
#' cannot see. That is the honest reading of a hidden control, and it is the
#' half of this that is load-bearing:
#'
#'   THE HIDDEN UNIT'S SLIDER MUST NOT GO ON FILTERING. Shiny keeps the value of
#'   an input whose UI has been removed, so without the matching drop in
#'   fw_filter_state() the removed slider would keep its last bounds and narrow
#'   the result invisibly. That was measured, not theorised: it silently dropped
#'   those six. See the size block there.
#'
#' As the client's cleaning lands and the mismatches go, the exception goes with
#' them and no code changes.
#'
#' @param regime the reader's `regime` selection, possibly empty
fw_size_units <- function(regime = character(0)) {
  if (!length(regime)) return(FW_SIZE_UNITS)
  out <- intersect(FW_SIZE_UNITS,
                   unlist(FW_REGIME_UNITS[regime], use.names = FALSE))
  # A regime with no unit of its own must not hide both sliders, which would
  # read as "size cannot be filtered here" rather than as an answer.
  if (!length(out)) FW_SIZE_UNITS else out
}

#' The real-unit bounds of each size slider, and how many rows have no size
#'
#' Bounds come from the data rather than a typed constant, so the client's
#' ongoing cleaning flows through without a code change.
fw_size_bounds <- function(a) {
  one <- function(unit) {
    v <- a$area_treated[!is.na(a$area_treated) & a$area_treated > 0 &
                          !is.na(a$area_unit) & a$area_unit == unit]
    if (!length(v)) return(NULL)
    range(v)
  }
  out <- lapply(FW_SIZE_UNITS, one)
  names(out) <- FW_SIZE_UNITS
  out$n_no_size <- sum(is.na(a$area_treated))
  out
}

# The sliders work in log10 and the matching works in real units. These two are
# the only places that conversion happens, so the control and the comparison
# cannot drift apart.
#
# A zero or negative area has no logarithm. There are none in the data, and
# fw_size_bounds() excludes them from the bounds, but a value that cannot be
# placed on the slider must not be silently dropped by it either - see the
# is.na() guard in fw_size_match().
fw_size_log <- function(x) log10(x)
fw_size_unlog <- function(x) 10^x

# The step the log sliders move in, and the rounding used when a bound is
# printed back in real units. A tenth of a decade is fine enough to land on a
# meaningful figure and coarse enough that the handle does not feel stuck.
FW_SIZE_LOG_STEP <- 0.1

#' Widen a log range out to whole steps
#'
#' So the slider ends sit on round numbers and the reader can always reach the
#' true minimum and maximum, which a truncated range would leave just inside.
fw_size_log_range <- function(bounds) {
  if (is.null(bounds)) return(NULL)
  lo <- floor(fw_size_log(bounds[1]) / FW_SIZE_LOG_STEP) * FW_SIZE_LOG_STEP
  hi <- ceiling(fw_size_log(bounds[2]) / FW_SIZE_LOG_STEP) * FW_SIZE_LOG_STEP
  c(lo, hi)
}

#' A size printed for a human, at a sensible number of digits
#'
#' A log slider lands on values like 3.1622776601683795, and the readout under
#' it and the export's Filters sheet both have to show a figure a reader can
#' repeat. Small sizes keep their decimals because 0.0014 ha rounded to a whole
#' number is 0; large ones lose them because 237,500 ha is not measured to the
#' hectare.
fw_size_label <- function(x) {
  if (!length(x) || is.na(x)) return(fw_t("common", "empty_value"))
  digits <- if (x >= 100) 0 else if (x >= 10) 1 else if (x >= 1) 2 else 4
  fw_fmt_num(round(x, digits))
}

#' Which attempts a size selection keeps
#'
#' EACH ATTEMPT IS COMPARED AGAINST ITS OWN UNIT and is untouched by the other
#' slider. A unit whose slider is not on the page does not constrain anything -
#' the regime filter has already removed those rows, and a bound that is not
#' being shown must not silently narrow the result.
#'
#' @return a logical vector along `a`
fw_size_match <- function(a, f) {
  keep <- rep(TRUE, nrow(a))
  for (unit in FW_SIZE_UNITS) {
    b <- f[[paste0("size_", unit)]]
    if (length(b) != 2 || anyNA(b)) next
    lo <- fw_size_unlog(b[1])
    hi <- fw_size_unlog(b[2])
    is_unit <- !is.na(a$area_unit) & a$area_unit == unit & !is.na(a$area_treated)
    keep[is_unit] <- a$area_treated[is_unit] >= lo & a$area_treated[is_unit] <= hi
  }
  # An attempt with no size recorded is not out of range, it is UNMEASURED, and
  # whether it travels is the checkbox's decision alone.
  #
  # ASSIGNMENT, NOT `keep | ...`. An OR can only ever add rows back: `keep`
  # starts all-TRUE, so the unmeasured rows were already TRUE and unticking the
  # box removed nothing at all.
  keep[is.na(a$area_treated)] <- isTRUE(f$include_no_size)
  keep
}

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
  family_for <- function(bridge) {
    sort(unique(species$family[species$species_id %in% bridge$species_id &
                                 !is.na(species$family)]))
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
    taxa_beneficiary = taxa_for(ben),
    family      = family_for(inv),
    family_beneficiary = family_for(ben),
    method      = data$method$method_name[order(match(
      data$method$method_id,
      names(sort(table(data$attempt_method$method_id), decreasing = TRUE))
    ))],
    # FIXED, not derived. These four are the analysis categories the paper uses
    # and the interface must show all of them even if a slice contains none.
    outcome     = c("Successful", "Failed", "Ongoing", "Unknown"),
    year_min    = min(data$attempt$start_year, na.rm = TRUE),
    year_max    = max(data$attempt$start_year, na.rm = TRUE),
    n_no_year   = sum(is.na(data$attempt$start_year)),
    # Real-unit bounds per unit, plus how many rows carry no size at all. The
    # control converts to log10; the matching does not. See fw_size_bounds().
    size        = fw_size_bounds(data$attempt)
  )
}

# ---- State -------------------------------------------------------------------

#' Read the filter inputs into a plain list
#'
#' On the report builder this is snapshotted at the moment Build is pressed, so
#' the results and the export describe the same selection even if the user then
#' changes a control. On the dashboard it is read live.
#' @param ch the choice lists. Optional, and only the size filter uses it: it
#'   records each slider's FULL range alongside the reader's setting, so the
#'   summary can tell "they widened it to everything" from "they never touched
#'   it" and print "All" rather than a pair of bounds nobody chose.
fw_filter_state <- function(input, ids = fw_filter_ids(), ch = NULL) {
  g <- function(nm) {
    v <- input[[nm]]
    if (is.null(v)) character(0) else v[nzchar(v)]
  }
  out <- list()
  for (id in ids) {
    if (FW_FILTERS[[id]]$kind %in% c("range", "size")) next
    out[[id]] <- g(id)
    when <- FW_FILTERS[[id]]$when
    if (!is.null(when) && !when$value %in% g(when$input)) out[[id]] <- character(0)
  }
  if ("size" %in% ids) {
    # ONLY THE UNITS THE READER CAN CURRENTLY SEE, and this is the control that
    # makes the regime-driven size cell safe. Shiny KEEPS the value of an input
    # whose UI has been removed, so a slider hidden by a regime change goes on
    # reporting the last bounds it was given - and fw_size_match() would go on
    # applying them to rows the reader can no longer see a control for. Reading
    # input$regime here, from the same input list the rest of the state comes
    # from, means the snapshot can only ever contain sliders that were on the
    # page when Build was pressed.
    #
    # NULL for a unit that is not offered, which is a real state rather than a
    # missing value: fw_size_match() reads it as "this unit is not constrained".
    offered <- fw_size_units(input$regime %||% character(0))
    for (unit in FW_SIZE_UNITS) {
      if (!unit %in% offered) next
      out[[paste0("size_", unit)]] <- input[[paste0("size_", unit)]]
      if (!is.null(ch)) {
        out[[paste0("size_full_", unit)]] <- fw_size_log_range(ch$size[[unit]])
      }
    }
    out$include_no_size <- isTRUE(input$include_no_size)
  }
  if ("years" %in% ids) {
    out$year_from <- input$years[1]
    out$year_to <- input$years[2]
    out$include_no_year <- isTRUE(input$include_no_year)
    # THE SLIDER'S OWN ENDS, carried alongside the reader's, so anything
    # summarising this selection can tell "1934 to 2025 because I chose it"
    # from "1934 to 2025 because I did not touch the slider". The size filter
    # has kept its equivalent (size_full_*) for the same reason since it had
    # one; the years row had no way to make that distinction until the PDF's
    # filters table started hiding untouched filters (client, 23 Sept 2026).
    if (!is.null(ch)) out$year_full <- c(ch$year_min, ch$year_max)
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
  if ("size" %in% ids) {
    for (unit in FW_SIZE_UNITS) {
      r <- fw_size_log_range(ch$size[[unit]])
      if (!is.null(r)) updateSliderInput(session, paste0("size_", unit), value = r)
    }
    updateCheckboxInput(session, "include_no_size", value = TRUE)
  }
  if ("years" %in% ids) {
    updateSliderInput(session, "years", value = c(ch$year_min, ch$year_max))
    updateCheckboxInput(session, "include_no_year", value = TRUE)
  }
}

# ---- Linked geography --------------------------------------------------------

#' The values one geography picker may still offer, given the other
#'
#' CONTINENT AND COUNTRY NARROW WITH AND, like every other filter, so nothing
#' stops a reader asking for Europe and Australia and getting the correct answer
#' of nothing at all. A blank result with no explanation is indistinguishable
#' from a broken page, and that is what the client reported. Rather than explain
#' the contradiction after the fact, each picker offers only what is still
#' possible given the other, and the contradiction cannot be expressed.
#'
#' Pure, and separate from the observers that call it, because the interesting
#' part is this arithmetic rather than the plumbing - see mod_explore.R.
#'
#' @param geo distinct continent/country pairs, from the attempt table. NOT from
#'   the ISO lookup: the pickers have only ever offered values that actually
#'   appear in the data, and a list that offered more would narrow to nothing.
#' @param from the column the reader has chosen in
#' @param to the column being narrowed
#' @param keep what they chose, or empty for "no choice"
#' @param all what `to` offers when nothing is chosen
#' @return the allowed values of `to`, sorted
fw_geo_allowed <- function(geo, from, to, keep, all) {
  # NO CHOICE MEANS NO NARROWING, not "nothing matches". An empty selection is
  # the reader clearing the box, and it has to restore the full list rather than
  # leave the other picker holding whatever the last choice left behind.
  if (!length(keep)) return(all)
  sort(unique(geo[[to]][geo[[from]] %in% keep]))
}

#' Wire the two geography pickers together for one page
#'
#' THIS USED TO LIVE IN mod_explore.R AND ONLY THERE, which was the bug the
#' client reported next: the dashboard's pickers narrowed each other and the
#' report builder's did not, so the contradiction fw_geo_allowed() exists to
#' make unreachable was still reachable one page over. A page that offers a
#' continent filter and a country filter gets both halves or neither, so the
#' observers live here beside the arithmetic and both modules call this.
#'
#' WHY THIS DOES NOT LOOP. Each observer writes only to the OTHER control, and
#' Shiny does not invalidate a reactive when a value is set to something
#' identical to what it already held. A write that changes nothing therefore
#' stops there. A write that DOES change something - a selected country falling
#' outside a newly chosen continent, which is the only case - runs one more
#' round and then stops, because by then both are consistent.
#'
#' BOTH WAYS ROUND, because a reader who knows their country should not have to
#' know its continent first: picking Australia narrows the continent list to
#' Oceania rather than leaving Europe selectable beside it.
#'
#' The pairs come from the attempt table rather than the ISO lookup on purpose.
#' The country picker has only ever offered countries that actually appear in
#' the data (see fw_filter_choices()), and the continent filter has to agree
#' with it or the narrowing would offer empty options.
#'
#' @param input,session the calling module's own input and session
#' @param data the loaded data, for its attempt table
#' @param choices fw_filter_choices(data), for the full list each picker
#'   returns to when the other is cleared
#' @return invisibly, the two observers
fw_link_geo_filters <- function(input, session, data, choices) {
  geo <- unique(data$attempt[, c("continent", "country")])
  geo <- geo[!is.na(geo$continent) & !is.na(geo$country), ]

  # Both selections come back as NULL when the reader empties the box and as
  # character(0) from fw_filter_clear(), and those must not read as different
  # states or the two observers would trade writes forever.
  picked <- function(x) if (length(x)) as.character(x) else character(0)

  a <- shiny::observeEvent(picked(input$continent), {
    allowed <- fw_geo_allowed(geo, "continent", "country",
                              picked(input$continent), choices$country)
    shiny::updateSelectizeInput(session, "country", choices = allowed,
                                selected = intersect(picked(input$country),
                                                     allowed))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  b <- shiny::observeEvent(picked(input$country), {
    allowed <- fw_geo_allowed(geo, "country", "continent",
                              picked(input$country), choices$continent)
    shiny::updateSelectizeInput(session, "continent", choices = allowed,
                                selected = intersect(picked(input$continent),
                                                     allowed))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  invisible(list(a, b))
}

#' Draw a filter only while its `when` condition holds
#'
#' The fish family pair: each is shown only while its kind-of-animal filter
#' includes Fish. conditionalPanel sets display:none, which takes the cell out
#' of a grid rather than leaving a gap. A filter with no `when` is returned
#' as it is. Shared by Explore and Plan (Plan gained the pair on 24 Sept 2026),
#' so the two pages hide and show it by one rule.
#'
#' @param ns the calling module's namespace function
#' @param id the filter id in FW_FILTERS
#' @param control the already-built control for that filter
fw_filter_when_panel <- function(ns, id, control) {
  when <- FW_FILTERS[[id]]$when
  if (is.null(when)) return(control)
  shiny::conditionalPanel(
    sprintf("(input['%s'] || []).indexOf('%s') > -1", ns(when$input), when$value),
    control
  )
}

#' Empty a `when` filter once its condition stops holding
#'
#' A fish family filter hidden by deselecting Fish is emptied as well, so it
#' does not come back already set when Fish is picked again.
#' fw_filter_state() already ignores it while hidden; this is about what the
#' reader sees on the way back.
#'
#' @param input,session the calling module's own input and session
#' @param ids the filter ids the page draws
fw_filter_when_observers <- function(input, session, ids) {
  for (fid in intersect(ids, names(Filter(function(x) !is.null(x$when), FW_FILTERS)))) {
    local({
      id <- fid
      when <- FW_FILTERS[[id]]$when
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
fw_filter_apply <- function(data, f) {
  a <- data$attempt
  ids <- f$.ids %||% fw_filter_ids()

  species <- NULL  # built once, only if a species-side filter is in play

  for (id in ids) {
    spec <- FW_FILTERS[[id]]
    vals <- f[[id]]
    if (spec$kind %in% c("range", "size") || !length(vals)) next

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
        left_join(select(species, species_id, label, taxa, family), by = "species_id") |>
        filter(.data[[spec$match]] %in% vals)
    } else {
      hit <- data$attempt_method |>
        left_join(select(data$method, method_id, method_name), by = "method_id") |>
        filter(.data[[spec$match]] %in% vals)
    }
    a <- a[a$attempt_id %in% unique(hit$attempt_id), ]
  }

  # Size, before the year range, because both are bounds rather than choices and
  # the year block below returns.
  if ("size" %in% ids) a <- a[fw_size_match(a, f), ]

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
    if (FW_FILTERS[[id]]$kind %in% c("range", "size") || !length(f[[id]])) next
    relaxed <- f
    relaxed[[id]] <- character(0)
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints <- c(hints, fw_filter_label(id))
    }
  }

  # The year range and the size sliders are the two filters that are ALWAYS set,
  # so neither can be found by dropping it above - a relaxed copy would still
  # carry the reader's bounds. They are only worth suggesting when widening them
  # actually helps, and they are checked one at a time so the hint names the one
  # that is in the way rather than both.
  if (!length(hints) && "size" %in% ids) {
    relaxed <- f
    for (unit in FW_SIZE_UNITS) relaxed[[paste0("size_", unit)]] <- NULL
    relaxed$include_no_size <- TRUE
    if (nrow(fw_filter_apply(data, relaxed)) > 0) {
      hints <- fw_filter_label("size")
    }
  }
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
    if (FW_FILTERS[[id]]$kind %in% c("range", "size")) next
    vals <- f[[id]]
    lab <- FW_FILTERS[[id]]$labels
    if (!is.null(lab) && length(vals)) vals <- match.fun(lab)(vals)
    rows[[length(rows) + 1L]] <- list(
      setting = fw_filter_label(id),
      value = if (length(vals)) paste(vals, collapse = FW_MULTI_SEP)
              else fw_t("export", "filter_all")
    )
  }
  if ("size" %in% ids) {
    # Recorded in REAL units, not in the log10 the slider works in. A sheet
    # saying "2.2 to 4.6" six months later is worse than no row at all.
    for (unit in FW_SIZE_UNITS) {
      b <- f[[paste0("size_", unit)]]
      full <- f[[paste0("size_full_", unit)]]
      # A slider left where it started is not a filter. It reads as one on the
      # sheet, though - and worse, it reads as a bound the reader chose, because
      # the slider's ends are widened out to whole log steps and so do not match
      # the real minimum and maximum in the data.
      untouched <- !length(b) || (length(full) == 2 && isTRUE(all.equal(b, full)))
      rows[[length(rows) + 1L]] <- list(
        setting = paste0(fw_filter_label("size"), " (", fw_t("filters", paste0("unit_", unit)), ")"),
        value = if (!untouched && length(b) == 2 && !anyNA(b)) {
          paste0(fw_size_label(fw_size_unlog(b[1])), fw_t("export", "range_sep"),
                 fw_size_label(fw_size_unlog(b[2])))
        } else fw_t("export", "filter_all")
      )
    }
    rows[[length(rows) + 1L]] <- list(
      setting = fw_t("filters", "no_size"),
      value = if (isTRUE(f$include_no_size)) fw_t("export", "filter_yes")
              else fw_t("export", "filter_no")
    )
  }
  if ("years" %in% ids) {
    rows[[length(rows) + 1L]] <- list(
      setting = fw_filter_label("years"),
      # A slider left at its ends is not a filter - see the size block above.
      # NA rather than FALSE when year_full is absent, so a caller that cannot
      # tell errs towards printing the row.
      untouched = length(f$year_full) == 2 &&
        isTRUE(all.equal(c(f$year_from, f$year_to), as.numeric(f$year_full))),
      value = paste0(f$year_from %||% fw_t("export", "range_missing"),
                     fw_t("export", "range_sep"),
                     f$year_to %||% fw_t("export", "range_missing"))
    )
    rows[[length(rows) + 1L]] <- list(
      setting = fw_t("filters", "no_year"),
      value = if (isTRUE(f$include_no_year)) fw_t("export", "filter_yes")
              else fw_t("export", "filter_no")
    )
  }
  rows
}
