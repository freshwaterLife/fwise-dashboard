# mod_plan_results.R
# The results area of the report builder: summary, map, charts, table.
#
# COLOUR IS NEVER THE ONLY ENCODING. Colourblind safety is a stated client
# requirement, so every chart that uses the Wong palette also carries the number
# or the label in text, and the map labels each marker with its outcome. A reader
# who cannot separate the greens from the oranges loses nothing.
#
# ALL FOUR OUTCOMES STAY VISIBLE. Successful, Failed, Ongoing and Unknown are
# never collapsed into a success rate. Failure teaches as much as success and
# ongoing attempts show where the next results will come from, so a binary would
# throw away half of what the database is for.
#
# ROTENONE IS NOT A HEADLINE. It is 544 of 914 attempts and is socially
# sensitive. Nothing here foregrounds its success rate as a hero statistic; it
# appears inside the method comparison alongside every other method, reached by
# the user's own filtering, with the confounding stated next to the chart.

library(shiny)
library(dplyr)

# Every chart shares this, so a chart cannot drift into a different look.
FW_PLOT_FONT <- list(family = "Ubuntu, system-ui, sans-serif", size = 13,
                     color = "#0a2e29")

#' Strip plotly's chrome down to what the design system uses
#'
#' Height is NOT set here. plotly deprecated width/height in layout(), so each
#' chart passes it to plot_ly() instead - which is also where a chart that needs
#' to grow with its number of rows can compute it.
fw_plotly_style <- function(p) {
  plotly::layout(
    p,
    font = FW_PLOT_FONT,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    margin = list(l = 8, r = 8, t = 8, b = 40),
    hoverlabel = list(font = FW_PLOT_FONT),
    legend = list(orientation = "h", y = -0.18, x = 0)
  ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

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

#' Outcome counts, always all four, in the paper's order
#'
#' A level with no rows still appears, at zero. Dropping it would let a slice
#' silently look unanimous when it is only small.
fw_plan_outcomes <- function(sel) {
  lv <- c("Successful", "Failed", "Ongoing", "Unknown")
  o <- factor(ifelse(is.na(sel$outcome), "Unknown", sel$outcome), levels = lv)
  tibble(outcome = lv, n = as.integer(table(o)))
}

fw_plan_outcome_ui <- function(sel) {
  o <- fw_plan_outcomes(sel)
  total <- sum(o$n)
  div(
    class = "fw-outcome-bars",
    lapply(seq_len(nrow(o)), function(i) {
      n <- o$n[i]
      pc <- if (total > 0) 100 * n / total else 0
      div(
        class = "fw-outcome-bars__row",
        # The label and the count are text. The bar and its colour are the
        # decoration, not the information.
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

fw_plan_map <- function(sel) {
  pts <- sel[!is.na(sel$latitude) & !is.na(sel$longitude), ]
  m <- leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE)) |>
    # Muted, low-chroma base so the data carries the colour. Esri rather than
    # CartoDB because Carto now watermarks keyless tiles.
    leaflet::addProviderTiles("Esri.WorldGrayCanvas",
                              options = leaflet::providerTileOptions(noWrap = FALSE))
  if (nrow(pts) == 0) return(leaflet::setView(m, 0, 20, zoom = 2))

  outcome <- ifelse(is.na(pts$outcome), "Unknown", pts$outcome)
  leaflet::addCircleMarkers(
    m, lng = pts$longitude, lat = pts$latitude,
    radius = 6, weight = 1.5, opacity = 1, fillOpacity = 0.75,
    color = "#ffffff",
    fillColor = unname(FW_OUTCOME_COLOURS[outcome]),
    # The outcome is in the label as words. Colour is reinforcement, not the
    # only carrier.
    label = lapply(seq_len(nrow(pts)), function(i) {
      htmltools::HTML(paste0(
        "<strong>", htmltools::htmlEscape(pts$site_name[i] %|na|% "Unnamed site"),
        "</strong><br>", htmltools::htmlEscape(pts$country[i]),
        "<br>Outcome: ", outcome[i],
        if (!is.na(pts$start_year[i])) paste0("<br>Began ", pts$start_year[i]) else ""
      ))
    })
  ) |>
    leaflet::addLegend(
      position = "bottomright", colors = unname(FW_OUTCOME_COLOURS),
      labels = names(FW_OUTCOME_COLOURS), opacity = 0.85, title = "Outcome"
    ) |>
    leaflet::fitBounds(min(pts$longitude), min(pts$latitude),
                       max(pts$longitude), max(pts$latitude))
}

# ---- Outcome by method -------------------------------------------------------

#' Outcome mix within each method
#'
#' Proportional bars, with the ATTEMPT COUNT printed against each method. The
#' count is what stops a method with three attempts reading as comparable to one
#' with five hundred - there is no suppression threshold, so the number has to be
#' visible for the reader to make that judgement themselves.
fw_plan_method_chart <- function(data, sel) {
  me <- data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    distinct(attempt_id, method_name) |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome))
  if (nrow(me) == 0) return(NULL)

  totals <- me |> count(method_name, name = "total") |> arrange(total)
  d <- me |>
    count(method_name, outcome, name = "n") |>
    left_join(totals, by = "method_name") |>
    mutate(share = 100 * n / total,
           method_label = paste0(method_name, "  (", total, ")"))

  order_lv <- paste0(totals$method_name, "  (", totals$total, ")")
  # Grows with the number of methods, so eight methods are not crushed into the
  # space two would use.
  p <- plotly::plot_ly(height = max(220, 46 * nrow(totals) + 90))
  for (o in c("Successful", "Failed", "Ongoing", "Unknown")) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(method_label, levels = order_lv), x = ~share, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = "#ffffff", width = 1)),
      # The count inside the segment. Colour alone never carries the value.
      text = ~ifelse(share >= 9, as.character(n), ""),
      textposition = "inside", insidetextfont = list(color = "#ffffff"),
      hovertemplate = paste0("%{y}<br>", o, ": %{text} of %{customdata}<extra></extra>"),
      customdata = ~total
    )
  }
  fw_plotly_style(p) |>
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "Share of attempts (%)", range = c(0, 100),
                   ticksuffix = "%", zeroline = FALSE, gridcolor = "#e7e2da"),
      yaxis = list(title = "", automargin = TRUE)
    )
}

# ---- Cumulative over time ----------------------------------------------------

fw_plan_cumulative_chart <- function(sel) {
  y <- sel$start_year[!is.na(sel$start_year)]
  if (!length(y)) return(NULL)
  d <- tibble(year = y) |>
    count(year, name = "n") |>
    arrange(year) |>
    mutate(cumulative = cumsum(n))

  plotly::plot_ly(
    d, x = ~year, y = ~cumulative, type = "scatter", mode = "lines", height = 300,
    line = list(color = unname(FW_PALETTE["emphasis"]), width = 2.5, shape = "hv"),
    fill = "tozeroy", fillcolor = "rgba(0,114,178,0.12)",
    hovertemplate = "By %{x}: %{y} attempts<extra></extra>"
  ) |>
    fw_plotly_style() |>
    plotly::layout(
      xaxis = list(title = "Year the attempt began", gridcolor = "#e7e2da",
                   zeroline = FALSE),
      yaxis = list(title = "Attempts to date", gridcolor = "#e7e2da",
                   zeroline = FALSE, rangemode = "tozero"),
      showlegend = FALSE
    )
}

# ---- Table -------------------------------------------------------------------

FW_PLAN_TABLE_ROWS <- 25L

fw_plan_table <- function(export) {
  cols <- c(site_name = "Site", country = "Country", start_year = "Began",
            invasive_species = "Invasive species", methods = "Methods",
            outcome = "Outcome")
  have <- cols[names(cols) %in% names(export)]
  head_n <- head(export, FW_PLAN_TABLE_ROWS)

  tags$table(
    class = "fw-table",
    tags$thead(tags$tr(lapply(unname(have), function(h) tags$th(scope = "col", h)))),
    tags$tbody(lapply(seq_len(nrow(head_n)), function(i) {
      tags$tr(lapply(names(have), function(c) {
        v <- head_n[[c]][i]
        tags$td(if (is.na(v) || !nzchar(as.character(v))) "-" else as.character(v))
      }))
    }))
  )
}

# ---- Caveats -----------------------------------------------------------------

#' The caveats panel, always visible beside the results
#'
#' Not a collapsed accordion and not a footnote. The same text goes into the
#' export, so the two cannot say different things.
fw_plan_caveats_ui <- function(data) {
  lines <- fw_caveats(data)
  # fw_caveats() returns headings, paragraphs and blanks as one vector. A heading
  # is the all-caps line; everything else is body text under it.
  blocks <- list(); current <- NULL
  for (ln in lines) {
    if (!nzchar(ln)) next
    if (ln == toupper(ln)) {
      if (!is.null(current)) blocks <- c(blocks, list(current))
      current <- list(heading = ln, body = character(0))
    } else if (!is.null(current)) {
      current$body <- c(current$body, ln)
    }
  }
  if (!is.null(current)) blocks <- c(blocks, list(current))

  div(
    class = "fw-caveats",
    h2(fw_t("plan", "caveats_heading")),
    lapply(blocks, function(b) {
      div(
        class = "fw-caveats__block",
        h3(fw_caveat_title(b$heading)),
        lapply(b$body, function(x) p(x))
      )
    })
  )
}

# The export sheet wants shouting headings; a web page does not.
fw_caveat_title <- function(x) {
  paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
}
