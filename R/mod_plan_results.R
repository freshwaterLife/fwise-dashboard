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
    year_span = if (length(years)) paste0(min(years), "-", max(years))
                else fw_t("common", "empty_value")
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

# ---- Species tiles -----------------------------------------------------------

#' The top species for a role, as photographs
#'
#' A GRID OF PICTURES RATHER THAN A BAR CHART, and that is the point of it. The
#' rest of this page is counts; this is the row where a reader recognises the
#' animal they are actually dealing with. The count and the outcome split are
#' still there, so nothing is traded away for the photograph.
#'
#' Every photograph carries its credit and a linked licence. That is a condition
#' of using them, not decoration - fw_species_figure() builds both, and returns
#' a placeholder rather than a bare image when a species has no licensed
#' photograph. See the header of R/species_images.R.
#'
#' live = FALSE always. A grid of these is built at once and none of them may
#' reach Wikimedia while the page is rendering.
#'
#' @param limit how many tiles. The page passes FW_PLAN_SPECIES_N (five), which
#'   is deliberately fewer than the FW_TOP_N the ranked bar charts use: a tile
#'   is a photograph rather than a line, so ten of them ran to two full rows and
#'   pushed the rest of the report below the fold.
#'
#' @param role_name "invasive" or "beneficiary"
#' @return NULL when the selection has none of that role, so the calling block
#'   disappears rather than standing over an empty grid. 107 of 914 attempts
#'   record no beneficiary at all.
fw_species_tiles_ui <- function(data, sel, role_name, limit = FW_TOP_N) {
  top <- fw_species_top_n(data, sel, role_name, limit)
  if (!nrow(top)) return(NULL)

  div(
    class = "fw-species-tiles",
    lapply(seq_len(nrow(top)), function(i) {
      row <- top[i, ]
      counts <- vapply(FW_OUTCOME_LEVELS, function(o) as.integer(row[[o]]), 1L)
      total <- sum(counts)

      div(
        class = "fw-species-tile",
        div(
          class = "fw-species-tile__figure",
          HTML(fw_species_figure_for(data$species, row$species_id, row$label))
        ),
        div(
          class = "fw-species-tile__body",
          span(class = "fw-species-tile__name", row$label),
          span(class = "fw-species-tile__count",
               fw_fmt_num(row$n), " ",
               fw_t("plan", if (row$n == 1) "r_tile_attempt" else "r_tile_attempts")),
          # The outcome split as a single bar. Decoration: the numbers are in
          # the title attribute and in the Outcomes block above, so a reader who
          # cannot separate the colours has lost nothing.
          div(
            class = "fw-species-tile__bar",
            title = paste(paste0(FW_OUTCOME_LEVELS, ": ", counts),
                          collapse = ", "),
            lapply(FW_OUTCOME_LEVELS, function(o) {
              n <- counts[[o]]
              if (n == 0L) return(NULL)
              div(
                class = "fw-species-tile__seg",
                style = sprintf("width:%.2f%%;background:%s;",
                                100 * n / total, FW_OUTCOME_COLOURS[[o]])
              )
            })
          )
        )
      )
    })
  )
}

# ---- Map ---------------------------------------------------------------------

#' The report builder's map
#'
#' Basemaps, markers and popups all come from R/maps.R, so this map and the
#' dashboard's are the same map with a different selection in it.
#'
#' @param detail,detail_input passed to fw_add_attempt_markers(): the page
#'   fetches each record on click, the HTML report carries them all.
fw_plan_map <- function(data, sel, detail = c("embed", "lazy"), detail_input = NULL) {
  leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
    fw_add_basemaps() |>
    fw_add_attempt_markers(data, sel, detail = detail, detail_input = detail_input)
}

# ---- Table -------------------------------------------------------------------

# Page sizes come from FW_PLAN_PAGE_SIZES in R/config.R.

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
  cols <- c(site_name = fw_t("plan", "col_site"),
            country = fw_t("plan", "col_country"),
            start_year = fw_t("plan", "col_began"),
            invasive_species = fw_t("plan", "col_species"),
            methods = fw_t("plan", "col_methods"),
            outcome = fw_t("plan", "col_outcome"),
            primary_contact_name = fw_t("plan", "col_contact"))
  have <- cols[names(cols) %in% names(export)]

  # Multi-value cells are TRUNCATED HERE, not in the export. An attempt against
  # nine species turns one table row into a fifteen-line block, which pushes the
  # contact column off the side and makes the page unreadable. The full list is
  # one download away and is stated as such under the table.
  trim <- function(x, keep = FW_TABLE_CELL_ITEMS) {
    vapply(x, function(v) {
      if (is.na(v) || !nzchar(as.character(v))) return(NA_character_)
      parts <- trimws(strsplit(as.character(v), FW_MULTI_SEP, fixed = TRUE)[[1]])
      if (length(parts) <= keep) return(paste(parts, collapse = ", "))
      paste0(paste(parts[seq_len(keep)], collapse = ", "),
             fw_fill(fw_t("plan", "more_suffix"), n = length(parts) - keep))
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
        tags$td(if (is.na(v) || !nzchar(as.character(v))) fw_t("common", "empty_value")
                else as.character(v))
      }))
    }))
  )
}

#' How many pages a selection needs
fw_plan_pages <- function(n_rows, per_page) {
  max(1L, as.integer(ceiling(n_rows / per_page)))
}

# ---- Potential relevant contacts ---------------------------------------------

#' The people attached to the attempts in this selection
#'
#' REDACTION IS NOT DONE HERE, and must not be. fw_contacts_summary() replaces
#' the address of any contact flagged not-public with NA before the data reaches
#' the session, so this function - and the page built from it - has no code path
#' that can see one. Do not reach past it to data$contact.
#'
#' RELEVANCE IS THE SELECTION ITSELF, not a second idea of "region". A contact
#' is relevant if they are attached to an attempt the reader actually built, so
#' the table cannot disagree with the filters above it. `attempt_ids` is the
#' list column fw_contacts_summary() already derives from both contact slots.
#'
#' @return one row per contact, most involved in THIS selection first, with
#'   attempt_count replaced by the count within the selection.
fw_plan_contacts <- function(data, sel) {
  contacts <- fw_contacts_summary(data)
  ids <- sel$attempt_id

  here <- vapply(contacts$attempt_ids, function(x) length(intersect(x, ids)),
                 integer(1))
  out <- contacts[here > 0, , drop = FALSE]
  # The count the reader is shown is the count IN FRONT OF THEM. Carrying the
  # whole-database figure through would say 40 beside a selection of three.
  out$attempt_count <- here[here > 0]
  out[order(-out$attempt_count, out$contact_name), , drop = FALSE]
}

#' One page of the relevant contacts
#'
#' Hand-built, like every other table in the app, and the mailto comes from
#' fw_contact_action() in mod_networking.R rather than a second copy of it: the
#' address is assembled in JavaScript at click time so a scraper reading the
#' served markup does not harvest it in one pass, and a contact with no public
#' address gets an empty cell rather than a badge advertising a hidden one.
fw_plan_contacts_ui <- function(contacts, page = 1L,
                                per_page = FW_CONTACTS_PAGE_SIZES[1]) {
  if (!nrow(contacts)) return(p(fw_t("plan", "r_contacts_none")))

  from <- (page - 1L) * per_page + 1L
  to <- min(nrow(contacts), page * per_page)
  rows <- if (from > nrow(contacts)) contacts[0, ] else contacts[seq(from, to), ]

  tags$table(
    class = "fw-table",
    tags$thead(tags$tr(
      tags$th(scope = "col", fw_t("plan", "col_contact_name")),
      tags$th(scope = "col", fw_t("plan", "col_contact_org")),
      tags$th(scope = "col", fw_t("networking", "col_continent")),
      tags$th(scope = "col", fw_t("networking", "col_country")),
      tags$th(scope = "col", class = "fw-col-num", fw_t("plan", "col_contact_n")),
      tags$th(scope = "col", fw_t("plan", "col_contact_email"))
    )),
    tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      tags$tr(
        tags$td(r$contact_name),
        tags$td(r$organisation %|na|% fw_t("networking", "no_organisation")),
        tags$td(r$continent_label),
        tags$td(r$country_label),
        tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
        tags$td(fw_contact_action(r$contact_email, r$contact_name))
      )
    }))
  )
}

# ---- Caveats -----------------------------------------------------------------
#
# THE PANEL MOVED TO THE ABOUT PAGE. fw_caveats_ui() and fw_caveat_title() now
# live in R/ui_helpers.R, because About, the HTML report and this page's
# downloads all draw them and none of the three owns the other two. The text
# itself has always come from fw_caveat_blocks() in R/export.R and still does.
