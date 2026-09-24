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
  sp <- data$attempt_species |> filter(attempt_id %in% ids)

  # SPECIES PROTECTED COUNTS SUCCESSFUL ATTEMPTS ONLY (client, 23 Sept 2026),
  # which is what fw_headline_stats()$protected has always done for the Welcome
  # page. A species on an attempt that failed was not protected by it, so it is
  # not counted here - and this is the only figure in the strip that narrows to
  # one outcome, which is why it is computed from its own id set rather than
  # from `sp`.
  won <- sel$attempt_id[sel$outcome %in% "Successful"]
  protected <- n_distinct(sp$species_id[sp$role == "beneficiary" &
                                          sp$attempt_id %in% won])

  years <- sel$start_year[!is.na(sel$start_year)]
  list(
    attempts  = nrow(sel),
    countries = n_distinct(sel$country),
    species   = n_distinct(sp$species_id[sp$role == "invasive"]),
    # Species protected, in place of the methods count (client, 21 Sept 2026).
    # Shown as a floor (">X"): beneficiaries are under-recorded.
    beneficiaries = protected,
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
  # EVERY LABEL INFLECTS WITH ITS FIGURE (client, 23 Sept 2026: one country was
  # labelled "countries"). Two of the four do not change form in English, and
  # they still go through fw_plural() so that adding a form later is a copy
  # edit rather than a code change. "Year range" is a range whatever it spans.
  lab <- function(n, key) fw_plural(n, fw_t("plan", paste0(key, "_one")),
                                    fw_t("plan", key))
  div(
    class = "fw-summary-strip", role = "status",
    item(fw_fmt_num(s$attempts),  lab(s$attempts, "r_attempts")),
    item(fw_fmt_num(s$countries), lab(s$countries, "r_countries")),
    item(fw_fmt_num(s$species),   lab(s$species, "r_species")),
    item(paste0(">", fw_fmt_num(s$beneficiaries)),
         lab(s$beneficiaries, "r_beneficiaries")),
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

#' The heading over one role's tiles
#'
#' "Top three invasive species targeted (of 41 total)", and for the protected
#' side "(of >12 total)" because beneficiaries are under-recorded. The number
#' word is the tiles actually shown, so a selection with two species says "Top
#' two". Shared by the page and the PDF report so the two cannot disagree.
#'
#' @return NULL when the selection has no species in that role
fw_species_top_title <- function(data, sel, role_name, limit = FW_PLAN_SPECIES_N) {
  total <- dplyr::n_distinct(fw_species_rows(data, sel, role_name)$species_id)
  if (!total) return(NULL)
  key <- if (role_name == "invasive") "r_species_top_inv" else "r_species_top_ben"
  fw_fill(fw_t("plan", key), n_word = fw_num_word(min(limit, total)),
          total = fw_fmt_num(total))
}

#' The top species for a role, as photographs
#'
#' A GRID OF PICTURES RATHER THAN A BAR CHART.
fw_species_tiles_ui <- function(data, sel, role_name, limit = FW_TOP_N) {
  top <- fw_species_top_n(data, sel, role_name, limit)
  if (!nrow(top)) return(NULL)

  # THE COLUMN COUNT IS THE TILE COUNT.
  div(
    class = "fw-species-tiles",
    style = sprintf("--fw-tiles:%d;", nrow(top)),
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
          # The outcome split as a single bar, EACH SEGMENT HOVERABLE (client,
          # 21 Sept 2026): a popover gives its outcome and share, "Successful:
          # 25% (3 of 12)". The same popovers as the (i) buttons, initialised by
          # fw_popover_script() when renderUI adds them, and focusable so a
          # keyboard reaches them too. The bar's aria-label carries the whole
          # split for a screen reader in one go.
          div(
            class = "fw-species-tile__bar",
            role = "img",
            `aria-label` = paste(paste0(FW_OUTCOME_LEVELS, ": ", counts),
                                 collapse = ", "),
            lapply(FW_OUTCOME_LEVELS, function(o) {
              n <- counts[[o]]
              if (n == 0L) return(NULL)
              div(
                class = "fw-species-tile__seg",
                tabindex = "0",
                `data-bs-toggle` = "popover",
                `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "top",
                `data-bs-content` = fw_fill(fw_t("plan", "r_tile_seg"),
                                            outcome = o,
                                            pc = sprintf("%.0f", 100 * n / total),
                                            n = fw_fmt_num(n),
                                            total = fw_fmt_num(total)),
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
#'   fetches each record on click; "embed" carries every record inside its
#'   marker, for a map with no server behind it.
fw_plan_map <- function(data, sel, detail = c("embed", "lazy"), detail_input = NULL) {
  fw_leaflet() |>
    fw_add_basemaps() |>
    fw_add_attempt_markers(data, sel, detail = detail, detail_input = detail_input)
}

# ---- Table -------------------------------------------------------------------
#
# fw_plan_table() USED TO LIVE HERE - one page of the matching attempts, hand
# built rather than DT so the page did not carry a second sorting and paging
# model beside its own. The client removed the table from the report builder,
# and the old HTML report's copy of it went at the same time, so it had no callers
# left.
#
# fw_plan_pages() below is still used - the contacts block pages the same way.
#
# THE RECORD-BY-RECORD READING ARRIVED as its own download (21 Sept 2026): every
# attempt written out in full, one card after another - see R/report_records.R.
# Not a table of truncated cells, which is what the CSV already does better.

#' How many pages a selection needs
fw_plan_pages <- function(n_rows, per_page) {
  max(1L, as.integer(ceiling(n_rows / per_page)))
}

# ---- Potential relevant contacts ---------------------------------------------

#' The people attached to the attempts in this selection
#'
#' REDACTION IS NOT DONE HERE.
#'
#' RELEVANCE IS IN THE SELECTION.
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

fw_plan_contacts_ui <- function(contacts, page = 1L,
                                per_page = FW_PLAN_CONTACTS_PAGE_SIZES[1]) {
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
        tags$td(r$contact_name %|na|% ""),
        tags$td(r$organisation %|na|% fw_t("networking", "no_organisation")),
        tags$td(r$continent_label),
        tags$td(r$country_label),
        tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
        tags$td(fw_contact_action(r$contact_email, fw_contact_who(r)))
      )
    }))
  )
}

