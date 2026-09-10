# mod_plan_results.R
# The results area of the report builder: summary, outcome bars, map, table and
# caveats. The plotly figures live in R/charts.R because the dashboard draws the
# same ones, and the map in R/maps.R for the same reason.

library(shiny)
library(dplyr)

# ---- Summary -----------------------------------------------------------------

#' The headline counts for a selection
#'
#' Counts, not rates. There is deliberately no success percentage here: the
#' page's job is to show what has happened, and a single number at the top
#' invites the reader to stop there.
fw_plan_summary <- function(data, sel) {
  ids <- sel$attempt_id
  inv <- data$attempt_species |>
    filter(attempt_id %in% ids, role == "invasive")
  me <- data$attempt_method |> filter(attempt_id %in% ids)

  years <- sel$start_year[!is.na(sel$start_year)]
  list(
    attempts  = nrow(sel),
    countries = n_distinct(sel$country),
    species   = n_distinct(inv$species_id),
    methods   = n_distinct(me$method_id),
    year_span = if (length(years)) paste0(min(years), "-", max(years)) else "-"
  )
}

fw_plan_summary_ui <- function(s) {
  item <- function(value, label) div(
    class = "fw-summary-strip__item",
    span(class = "fw-summary-strip__value", value),
    span(class = "fw-summary-strip__label", label)
  )
  div(
    class = "fw-summary-strip", role = "status",
    item(fw_fmt_num(s$attempts),  fw_t("plan", "r_attempts")),
    item(fw_fmt_num(s$countries), fw_t("plan", "r_countries")),
    item(fw_fmt_num(s$species),   fw_t("plan", "r_species")),
    item(fw_fmt_num(s$methods),   fw_t("plan", "r_methods")),
    item(s$year_span,             fw_t("plan", "r_years"))
  )
}

#' Outcome counts as labelled bars
#'
#' The label and the count are TEXT; the bar and its colour are decoration. A
#' reader who cannot separate the colours loses nothing. All four levels are
#' always drawn, at zero if need be - see fw_outcome_counts() in charts.R.
fw_outcome_bars_ui <- function(sel) {
  o <- fw_outcome_counts(sel)
  total <- sum(o$n)
  div(
    class = "fw-outcome-bars",
    lapply(seq_len(nrow(o)), function(i) {
      n <- o$n[i]
      pc <- if (total > 0) 100 * n / total else 0
      div(
        class = "fw-outcome-bars__row",
        span(class = "fw-outcome-bars__label", o$outcome[i]),
        div(class = "fw-outcome-bars__track",
            div(class = "fw-outcome-bars__fill",
                style = sprintf("width:%.1f%%;background:%s;", pc,
                                FW_OUTCOME_COLOURS[[o$outcome[i]]]))),
        span(class = "fw-outcome-bars__value",
             fw_fmt_num(n), " ", sprintf("(%.0f%%)", pc))
      )
    })
  )
}

# ---- Map ---------------------------------------------------------------------

#' The report builder's map
#'
#' Basemaps, markers and popups all come from R/maps.R, so this map and the
#' dashboard's are the same map with a different selection in it.
fw_plan_map <- function(data, sel) {
  leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
    fw_add_basemaps() |>
    fw_add_attempt_markers(data, sel)
}

# ---- Table -------------------------------------------------------------------

# Page sizes offered under the results table. First element is the default.
# Deliberately its own vector rather than FW_CONTACTS_PAGE_SIZES: a report is
# read a screen at a time, so it starts smaller than the contacts directory.
FW_PLAN_PAGE_SIZES <- c(10L, 20L, 50L, 100L)

#' One page of the matching attempts
#'
#' Hand-built rather than DT: the table is read as text and exported as a
#' spreadsheet, and a datatable would add a second sorting and paging model
#' beside the one the page already has.
#'
#' The contact column is what turns a row into a next step - the whole point of
#' the networking side of FWISE - so it travels with the attempt here as well as
#' in the export.
fw_plan_table <- function(export, page = 1L, per_page = FW_PLAN_PAGE_SIZES[1]) {
  cols <- c(site_name = "Site", country = "Country", start_year = "Began",
            invasive_species = "Invasive species", methods = "Methods",
            outcome = "Outcome", primary_contact_name = "Contact")
  have <- cols[names(cols) %in% names(export)]

  # Multi-value cells are TRUNCATED HERE, not in the export. An attempt against
  # nine species turns one table row into a fifteen-line block, which pushes the
  # contact column off the side and makes the page unreadable. The full list is
  # one download away and is stated as such under the table.
  trim <- function(x, keep = 2L) {
    vapply(x, function(v) {
      if (is.na(v) || !nzchar(as.character(v))) return(NA_character_)
      parts <- trimws(strsplit(as.character(v), FW_MULTI_SEP, fixed = TRUE)[[1]])
      if (length(parts) <= keep) return(paste(parts, collapse = ", "))
      paste0(paste(parts[seq_len(keep)], collapse = ", "),
             " +", length(parts) - keep, " more")
    }, character(1), USE.NAMES = FALSE)
  }
  for (col in intersect(c("invasive_species", "methods"), names(export))) {
    export[[col]] <- trim(export[[col]])
  }

  from <- (page - 1L) * per_page + 1L
  to <- min(nrow(export), page * per_page)
  rows <- if (from > nrow(export)) export[0, ] else export[seq(from, to), ]

  tags$table(
    class = "fw-table",
    tags$thead(tags$tr(lapply(unname(have), function(h) tags$th(scope = "col", h)))),
    tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
      tags$tr(lapply(names(have), function(c) {
        v <- rows[[c]][i]
        tags$td(if (is.na(v) || !nzchar(as.character(v))) "-" else as.character(v))
      }))
    }))
  )
}

#' How many pages a selection needs
fw_plan_pages <- function(n_rows, per_page) {
  max(1L, as.integer(ceiling(n_rows / per_page)))
}

# ---- Caveats -----------------------------------------------------------------

#' The caveats panel, always visible beside the results
#'
#' Not a collapsed accordion and not a footnote. The same text goes into the
#' export, so the two cannot say different things.
fw_plan_caveats_ui <- function(data) {
  # Parsed by fw_caveat_blocks() next to fw_caveats(), so the panel never has to
  # know how many blocks there are. Add or remove one there and this reflows.
  blocks <- fw_caveat_blocks(data)

  div(
    class = "fw-caveats",
    h2(fw_t("plan", "caveats_heading")),
    # The blocks sit in their own grid wrapper rather than directly in the
    # panel, so the heading above stays full width and only the blocks column
    # up. See .fw-caveats__grid.
    div(
      class = "fw-caveats__grid",
      lapply(blocks, function(b) {
        div(
          class = "fw-caveats__block",
          h3(fw_caveat_title(b$heading)),
          lapply(b$body, function(x) p(x))
        )
      })
    )
  )
}

# The export sheet wants shouting headings; a web page does not.
fw_caveat_title <- function(x) {
  paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
}
