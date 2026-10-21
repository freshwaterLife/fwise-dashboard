# mod_plan_results.R
# Plan's results pieces: summary, outcome bars, species tiles, record search
# and contacts. Charts are in charts.R, maps in maps.R.

library(shiny)
library(dplyr)

# ---- Summary -----------------------------------------------------------------

#' The headline counts for a selection
#'
#' Counts only, no success percentage.
fw_plan_summary = function(data, sel) {
  ids = sel$attempt_id
  sp = data$attempt_species |> filter(attempt_id %in% ids)

  # Species protected counts successful attempts only (client), as on Home.
  won = sel$attempt_id[sel$outcome %in% "Successful"]
  protected = n_distinct(sp$species_id[sp$role == "beneficiary" &
                                          sp$attempt_id %in% won])

  years = sel$start_year[!is.na(sel$start_year)]
  list(
    attempts  = nrow(sel),
    countries = n_distinct(sel$country),
    species   = n_distinct(sp$species_id[sp$role == "invasive"]),
    # Shown as ">X": beneficiaries are under-recorded.
    beneficiaries = protected,
    year_span = if (length(years)) paste0(min(years), "-", max(years))
                else fw_t("common", "empty_value")
  )
}

fw_plan_summary_ui = function(s) {
  item = function(value, label) div(
    class = "fw-summary-strip__item",
    span(class = "fw-summary-strip__value", value),
    span(class = "fw-summary-strip__label", label)
  )
  # Labels go through fw_plural() to match their figure.
  lab = function(n, key) fw_plural(n, fw_t("plan", paste0(key, "_one")),
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
#' Label and count are text; colour is decoration. All four always drawn.
fw_outcome_bars_ui = function(sel) {
  o = fw_outcome_counts(sel)
  total = sum(o$n)
  div(
    class = "fw-outcome-bars",
    lapply(seq_len(nrow(o)), function(i) {
      n = o$n[i]
      pc = if (total > 0) 100 * n / total else 0
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
#' "Top three invasive species targeted (of 41 total)"; ">" on the protected
#' side. Shared with the PDF. Totals count species inside the role's filters.
#'
#' @param f the filter state, or NULL for every species
#' @return NULL when the selection has no species in that role, or when the
#'   reader picked species for it and the row is not drawn
fw_species_top_title = function(data, sel, role_name, limit = FW_PLAN_SPECIES_N,
                                 f = NULL) {
  if (!fw_species_row_shown(role_name, f)) return(NULL)
  total = dplyr::n_distinct(fw_species_rows(data, sel, role_name, f)$species_id)
  if (!total) return(NULL)
  key = if (role_name == "invasive") "r_species_top_inv" else "r_species_top_ben"
  fw_fill(fw_t("plan", key), n_word = fw_num_word(min(limit, total)),
          total = fw_fmt_num(total))
}

#' The top species for a role, as photographs
#'
#' Each tile's bar is the success rate (Successful vs Failed); the count is
#' every attempt. See fw_tile_rate().
fw_species_tiles_ui = function(data, sel, role_name, limit = FW_TOP_N, f = NULL) {
  top = fw_species_top_n(data, sel, role_name, limit, f)
  if (!nrow(top)) return(NULL)

  # One column per tile.
  div(
    class = "fw-species-tiles",
    style = sprintf("--fw-tiles:%d;", nrow(top)),
    lapply(seq_len(nrow(top)), function(i) {
      row = top[i, ]
      counts = fw_tile_rate(row)
      total = sum(counts)

      div(
        class = "fw-species-tile",
        div(
          class = "fw-species-tile__figure",
          HTML(fw_species_figure_for(data$species, row$species_id, row$label))
        ),
        div(
          class = "fw-species-tile__body",
          # Red List code on the protected side.
          span(class = "fw-species-tile__name", row$shown),
          span(class = "fw-species-tile__count",
               fw_fmt_num(row$n), " ",
               fw_t("plan", if (row$n == 1) "r_tile_attempt" else "r_tile_attempts")),
          # Each segment has a focusable popover ("Successful: 75% (3 of 4)");
          # the aria-label gives the whole split. No finished attempts: an
          # empty track and a line saying so.
          if (total == 0L) {
            span(class = "fw-species-tile__none", fw_t("plan", "r_tile_none"))
          } else div(
            class = "fw-species-tile__bar",
            role = "img",
            `aria-label` = paste(paste0(names(counts), ": ", counts),
                                 collapse = ", "),
            lapply(names(counts), function(o) {
              n = counts[[o]]
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

#' A species tile's success-rate counts: Successful and Failed, named
#'
#' @param row one row of fw_species_top_n()
fw_tile_rate = function(row) {
  vapply(FW_RATE_LEVELS, function(o) as.integer(row[[o]]), 1L)
}

# ---- Map ---------------------------------------------------------------------

#' The report builder's map
#'
#' @param detail,detail_input passed to fw_add_attempt_markers()
fw_plan_map = function(data, sel, detail = c("embed", "lazy", "anchor"),
                        detail_input = NULL) {
  fw_leaflet() |>
    fw_add_basemaps() |>
    fw_add_attempt_markers(data, sel, detail = detail, detail_input = detail_input)
}

# ---- Paging ------------------------------------------------------------------

#' How many pages a selection needs
fw_plan_pages = function(n_rows, per_page) {
  max(1L, as.integer(ceiling(n_rows / per_page)))
}

# ---- The record search -------------------------------------------------------
#
# Ten rows a page; a click opens the row's full card. Same matching as the
# Detailed report (fw_record_search()).

#' The rows of a report's export frame that match what was typed
#'
#' @param haystack fw_record_search() of each export row, lower case
#' @param q what was typed. Blank matches every row.
#' @return row indices, in the export's order
fw_plan_records_find = function(haystack, q) {
  q = tolower(trimws(q %||% ""))
  if (!nzchar(q)) return(seq_along(haystack))
  which(grepl(q, haystack, fixed = TRUE))
}

#' The search strings of an export frame, one per row. Built once per report.
fw_plan_records_haystack = function(export) {
  vapply(seq_len(nrow(export)), function(i) {
    fw_record_search(as.list(export[i, , drop = FALSE]))
  }, character(1))
}

#' One page of matching records, as a table whose rows open their cards
#'
#' Two rows per attempt: the summary and a hidden full card
#' (fw_record_card()), opened in the browser by fw_plan_records_script().
#'
#' @param export fw_export_frame() of the report
#' @param rows the matching row indices (fw_plan_records_find())
#' @param species data$species, for the protected species' Red List codes
fw_plan_records_ui = function(export, rows, page = 1L,
                               per_page = FW_PLAN_RECORDS_PAGE_SIZE,
                               species = NULL) {
  if (!length(rows)) return(p(class = "fw-caption", fw_t("plan", "r_records_none")))
  from = (page - 1L) * per_page + 1L
  to = min(length(rows), page * per_page)
  shown = if (from > length(rows)) integer(0) else rows[seq(from, to)]

  copy = fw_record_copy(species)
  lab = copy$labels
  n_cols = 6L
  # Column classes set widths; phones show only site and outcome.
  cls = function(k) paste0("fw-records__c-", k)
  body = lapply(shown, function(i) {
    r = as.list(export[i, , drop = FALSE])
    detail_id = paste0("fw-records-", r$attempt_id)
    site = if (fw_record_blank(r$site_name)) copy$none else r$site_name
    tagList(
      tags$tr(
        class = "fw-records__row", tabindex = "0", role = "button",
        `aria-expanded` = "false", `aria-controls` = detail_id,
        tags$td(class = cls("id"),
                tags$span(class = "fw-records__chev", `aria-hidden` = "true",
                          HTML("&#9656;")),
                tags$span(class = "fw-rec-id", r$attempt_id)),
        tags$td(class = cls("site"), site),
        tags$td(class = cls("country"), r$country %|na|% copy$none),
        tags$td(class = cls("species"), r$invasive_species %|na|% copy$none),
        # As fw_popup_years().
        tags$td(class = paste("fw-num", cls("years")),
                fw_popup_years(r$start_year, r$end_year) %|na|% copy$none),
        tags$td(class = cls("outcome"), HTML(fw_record_outcome(r$outcome, copy$none)))
      ),
      tags$tr(
        class = "fw-records__detail", id = detail_id, hidden = NA,
        tags$td(colspan = n_cols,
                HTML(fw_record_card(r, copy, top = FALSE, id = FALSE)))
      )
    )
  })

  # Fixed layout, so an opened card can't widen the table.
  tags$table(
    class = "fw-table fw-records__table",
    tags$thead(tags$tr(
      tags$th(scope = "col", class = cls("id"), lab[["attempt_id"]]),
      tags$th(scope = "col", class = cls("site"), lab[["site_name"]]),
      tags$th(scope = "col", class = cls("country"), lab[["country"]]),
      tags$th(scope = "col", class = cls("species"), lab[["invasive_species"]]),
      tags$th(scope = "col", class = cls("years"), fw_t("plan", "col_record_years")),
      tags$th(scope = "col", class = cls("outcome"), lab[["outcome"]])
    )),
    tags$tbody(body)
  )
}

#' The record search's one script: a row opens and closes its card
#'
#' Delegated, so it survives re-renders. Enter and Space work as click.
fw_plan_records_script = function() {
  tags$script(HTML("
    (function () {
      if (window.fwRecordsWired) return;
      window.fwRecordsWired = true;
      function toggle(row) {
        var detail = document.getElementById(row.getAttribute('aria-controls'));
        if (!detail) return;
        var open = row.getAttribute('aria-expanded') !== 'true';
        row.setAttribute('aria-expanded', open ? 'true' : 'false');
        detail.hidden = !open;
      }
      document.addEventListener('click', function (e) {
        var row = e.target.closest && e.target.closest('.fw-records__row');
        if (!row) return;
        // A link inside the row still works as a link.
        if (e.target.closest('a')) return;
        toggle(row);
      });
      document.addEventListener('keydown', function (e) {
        if (e.key !== 'Enter' && e.key !== ' ') return;
        var row = e.target.classList && e.target.classList.contains('fw-records__row') && e.target;
        if (!row) return;
        e.preventDefault();
        toggle(row);
      });
    })();"))
}

# ---- Potential relevant contacts ---------------------------------------------

#' The people attached to the attempts in this selection
#'
#' From fw_contacts_summary(), so already redacted.
#'
#' @return one row per contact, most involved in THIS selection first, with
#'   attempt_count replaced by the count within the selection.
fw_plan_contacts = function(data, sel) {
  contacts = fw_contacts_summary(data)
  ids = sel$attempt_id

  here = vapply(contacts$attempt_ids, function(x) length(intersect(x, ids)),
                 integer(1))
  out = contacts[here > 0, , drop = FALSE]
  # Count within the selection, not the whole database.
  out$attempt_count = here[here > 0]
  out[order(-out$attempt_count, out$contact_name), , drop = FALSE]
}

#' One page of the relevant contacts
#'
#' @param reveal FALSE in the downloaded report, which writes each address out
#'   rather than behind a button. See fw_contact_action().
fw_plan_contacts_ui = function(contacts, page = 1L,
                                per_page = FW_PLAN_CONTACTS_PAGE_SIZES[1],
                                reveal = TRUE) {
  if (!nrow(contacts)) return(p(fw_t("plan", "r_contacts_none")))

  from = (page - 1L) * per_page + 1L
  to = min(nrow(contacts), page * per_page)
  rows = if (from > nrow(contacts)) contacts[0, ] else contacts[seq(from, to), ]

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
      r = rows[i, ]
      tags$tr(
        tags$td(r$contact_name %|na|% ""),
        tags$td(r$organisation %|na|% fw_t("networking", "no_organisation")),
        tags$td(r$continent_label),
        tags$td(r$country_label),
        tags$td(class = "fw-col-num", fw_fmt_num(r$attempt_count)),
        tags$td(fw_contact_action(r$contact_email, fw_contact_who(r), reveal = reveal,
                                  contact_id = r$contact_id))
      )
    }))
  )
}

