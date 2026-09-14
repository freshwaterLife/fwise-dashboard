# mod_explore_table.R
# The dashboard's table of attempts. One row per attempt, opening the same
# record panel the map's markers open.
#
# A TABLE, NOT A GRID OF CARDS, and the difference is the second species. A card
# carried one photograph - the lead invasive - and the reader scanning for "what
# was this about" got half the pairing. A row carries both: the species the
# attempt targeted and the species it was meant to help, side by side, which is
# the comparison the database exists to record.
#
# HAND-BUILT, like every other table in the app. The reasoning is in
# mod_plan_results.R: the table is read as text and exported as a spreadsheet,
# and a datatable would bring a second sorting and paging model alongside the
# one the page already owns. The sort control above this table is the page's.
#
# EVERY PHOTOGRAPH CARRIES ITS CREDIT. That is a condition of using these images
# rather than decoration - see the header of R/species_images.R - so the caption
# is rendered in the cell rather than hidden in a title attribute. It is what
# makes the rows tall, and it is why the text columns here are kept to the four
# a reader actually scans: everything else is one click away in the record.
#
# fw_species_figure() DRAWS THE CELLS, unchanged. The size is a class on the
# wrapper (.fw-explore-table__thumb), the same way the record cards and the
# species tiles each re-scale the same figure for their own context.

library(shiny)

#' One page of attempts, as a table
#'
#' @param rec rows from fw_attempt_records(), which carries the species id and
#'   name lists each row needs
#' @param figures the figure cache for this page, from fw_map_figure_cache().
#'   Keyed by species id; a species with no cached photograph is absent and
#'   falls through to fw_species_figure()'s placeholder.
#' @param detail_input the input a row click writes the attempt id into. The
#'   SAME input the map's markers use, so there is one record panel and
#'   previous/next steps through the list the reader is looking at.
#' @param locate_input the input the "show on map" link writes to, or NULL
fw_explore_table <- function(rec, figures = list(), detail_input,
                             locate_input = NULL) {
  if (!nrow(rec)) return(p(fw_t("common", "no_results")))

  set_input <- function(id, value) {
    sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'});",
            id, gsub("'", "\\\\'", value))
  }

  # The lead species of a role, as a credited figure. Lead rather than all of
  # them: the row is a summary and the record panel behind it carries every
  # species as a carousel. NA ids fall through to the placeholder, so the
  # fallback lives here rather than at each cell.
  thumb <- function(ids, labels, none_label) {
    lead <- fw_popup_parts(ids)[1]
    label <- fw_popup_parts(labels)[1]
    if (is.na(lead)) {
      return(div(class = "fw-explore-table__thumb fw-explore-table__thumb--none",
                 span(class = "fw-caption", none_label)))
    }
    fig <- if (lead %in% names(figures)) unname(figures[lead]) else NULL
    div(
      class = "fw-explore-table__thumb",
      HTML(fig %||% fw_species_figure(NULL, label)),
      span(class = "fw-explore-table__species", label %|na|% none_label)
    )
  }

  head <- function(key) tags$th(scope = "col", fw_t("explore", key))

  tags$table(
    class = "fw-table fw-explore-table",
    tags$thead(tags$tr(
      head("col_invasive"),
      head("col_beneficiary"),
      head("col_site"),
      head("col_country"),
      head("col_began"),
      head("col_outcome"),
      tags$th(scope = "col",
              tags$span(class = "fw-visually-hidden", fw_t("explore", "col_open")))
    )),
    tags$tbody(lapply(seq_len(nrow(rec)), function(i) {
      row <- rec[i, ]
      located <- !is.na(row$latitude) && !is.na(row$longitude)
      tags$tr(
        `data-fw-id` = row$attempt_id,
        tags$td(thumb(row$inv_ids, row$inv_names, fw_t("common", "empty_value"))),
        tags$td(thumb(row$ben_ids, row$ben_names, fw_t("explore", "no_beneficiary"))),
        # THE WHOLE CELL IS THE BUTTON, not a link inside it, so the row has one
        # focus stop and one obvious target rather than a name that happens to
        # be clickable.
        tags$td(tags$button(
          type = "button", class = "fw-explore-table__open",
          onclick = set_input(detail_input, row$attempt_id),
          row$site_name %|na|% fw_t("species", "unnamed_site")
        )),
        tags$td(row$country %|na|% fw_t("common", "empty_value")),
        tags$td(class = "fw-col-num",
                fw_popup_years(row$start_year, row$end_year) %|na|%
                  fw_t("common", "empty_value")),
        tags$td(fw_outcome_pill(row$outcome)),
        tags$td(if (located && !is.null(locate_input)) {
          tags$a(
            href = "#", class = "fw-explore-table__locate",
            onclick = paste(set_input(locate_input, row$attempt_id), "return false;"),
            fw_t("explore", "show_on_map")
          )
        })
      )
    }))
  )
}

#' An outcome as a labelled dot
#'
#' COLOUR IS NOT THE ENCODING - the word is. The dot is decoration that lets a
#' reader scanning a column of forty rows find the successes without reading
#' each one, and a reader who cannot separate the colours has lost nothing,
#' which is the rule every chart in the app follows.
fw_outcome_pill <- function(outcome) {
  value <- if (is.na(outcome)) "Unknown" else outcome
  tags$span(
    class = "fw-outcome-pill",
    tags$span(class = "fw-outcome-pill__dot",
              style = sprintf("background:%s;", FW_OUTCOME_COLOURS[[value]])),
    value
  )
}
