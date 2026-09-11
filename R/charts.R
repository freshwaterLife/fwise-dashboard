# charts.R
# Every plotly figure in the app, shared by the report builder and the
# dashboard. Keeping them here rather than beside one page is what stops the
# same quantity being drawn two different ways on two different pages.
#
# COLOUR IS NEVER THE ONLY ENCODING. Colourblind safety is a stated client
# requirement, so every chart that uses the Wong palette also carries the number
# or the label in text. A reader who cannot separate the greens from the oranges
# loses nothing.
#
# ALL FOUR OUTCOMES STAY VISIBLE. Successful, Failed, Ongoing and Unknown are
# never collapsed into a success rate. The client's metrics framework proposed a
# headline "% successful" figure; it was declined, because failure teaches as
# much as success and ongoing attempts show where the next results will come
# from. A single rate throws away half of what the database is for, and on
# socially sensitive methods it reads as advocacy.
#
# ROTENONE IS NOT A HEADLINE. It is 544 of 914 attempts and is socially
# sensitive. Nothing here foregrounds its success rate as a hero statistic; it
# appears inside the method comparison alongside every other method, reached by
# the user's own filtering, with the confounding stated next to the chart.

library(dplyr)

# Every chart shares this, so a chart cannot drift into a different look.
FW_PLOT_FONT <- list(family = "Ubuntu, system-ui, sans-serif", size = 14,
                     color = "#0a2e29")

FW_OUTCOME_LEVELS <- c("Successful", "Failed", "Ongoing", "Unknown")

FW_GRID_COLOUR <- "#e7e2da"

#' Strip plotly's chrome down to what the design system uses
#'
#' Height is NOT set here. plotly deprecated width/height in layout(), so each
#' chart passes it to plot_ly() instead - which is also where a chart that needs
#' to grow with its number of rows can compute it.
#' @param legend whether this chart needs a key at all
fw_plotly_style <- function(p, legend = TRUE) {
  plotly::layout(
    p,
    font = FW_PLOT_FONT,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    # THE LEGEND SITS ABOVE THE PLOT, not below it. Underneath, plotly places it
    # in paper coordinates at a fixed offset and it lands on top of the x-axis
    # title, which is where the units are - so the reader loses the label that
    # says what they are looking at. Above, it has the margin to itself.
    margin = list(l = 8, r = 8, t = if (legend) 42 else 8, b = 52),
    hoverlabel = list(font = FW_PLOT_FONT),
    showlegend = legend,
    # traceorder IS NOT REDUNDANT. plotly.js flips its default to "reversed" as
    # soon as a chart has stacked bars or a filled area, which is every chart
    # here except the box plot - so the key read Unknown first and Successful
    # last while the traces were added Successful first. Pinning it makes the
    # key agree with FW_OUTCOME_LEVELS, which is the one source of that order.
    legend = list(orientation = "h", y = 1, yanchor = "bottom", x = 0,
                  traceorder = "normal")
  ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

#' Outcome as a factor with every level present, missing read as Unknown
#'
#' A level with no rows still appears, at zero. Dropping it would let a slice
#' silently look unanimous when it is only small.
fw_outcome_factor <- function(x) {
  factor(ifelse(is.na(x), "Unknown", x), levels = FW_OUTCOME_LEVELS)
}

fw_outcome_counts <- function(sel) {
  tibble(outcome = FW_OUTCOME_LEVELS,
         n = as.integer(table(fw_outcome_factor(sel$outcome))))
}

# ---- Cumulative over time ----------------------------------------------------

#' Cumulative attempts by start year, stacked by outcome
#'
#' Stacked rather than a single line, at the client's request: the growth of the
#' record and the mix of what came of it are the same question, and a single
#' line answers only half of it. Step interpolation ("hv") because an attempt
#' joins the total on its start year rather than easing in across the gap.
fw_chart_cumulative <- function(sel) {
  y <- sel[!is.na(sel$start_year), c("start_year", "outcome")]
  if (!nrow(y)) return(NULL)

  years <- seq(min(y$start_year), max(y$start_year))
  d <- y |>
    mutate(outcome = fw_outcome_factor(outcome)) |>
    count(start_year, outcome, name = "n") |>
    tidyr::complete(start_year = years, outcome = FW_OUTCOME_LEVELS,
                    fill = list(n = 0L)) |>
    arrange(start_year) |>
    group_by(outcome) |>
    mutate(cumulative = cumsum(n)) |>
    ungroup()

  p <- plotly::plot_ly(height = 360)
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    p <- plotly::add_trace(
      p, data = dd, x = ~start_year, y = ~cumulative,
      type = "scatter", mode = "lines", name = o,
      # "hv" because an attempt joins the total on its start year rather than
      # easing in across the gap. The line is the same colour as its fill, so
      # the steps read as one band rather than as an outlined shape.
      stackgroup = "one",
      line = list(shape = "hv", width = 1,
                  color = unname(FW_OUTCOME_COLOURS[[o]])),
      fillcolor = unname(FW_OUTCOME_COLOURS[[o]]),
      hovertemplate = paste0("By %{x}<br>", o, ": %{y}<extra></extra>")
    )
  }
  fw_plotly_style(p) |>
    plotly::layout(
      xaxis = list(title = "Year the attempt began", gridcolor = FW_GRID_COLOUR,
                   zeroline = FALSE),
      yaxis = list(title = "Attempts to date", gridcolor = FW_GRID_COLOUR,
                   zeroline = FALSE, rangemode = "tozero")
    )
}

# ---- Outcome by method -------------------------------------------------------

#' Outcome mix within each method
#'
#' Two modes over one chart rather than two charts, because the sidebar leaves a
#' narrow column and a pair side by side would crush both. "share" answers "how
#' often does this work", "count" answers "how much evidence is there".
#'
#' Either way the ATTEMPT COUNT is printed against each method. The count is what
#' stops a method with three attempts reading as comparable to one with five
#' hundred - there is no suppression threshold, so the number has to be visible
#' for the reader to make that judgement themselves.
#'
#' COUNT IS THE DEFAULT. It used to be share. A 100% stacked bar answers "how
#' often did this work" before the reader has been told how much evidence is
#' behind it, and a method with three attempts looks exactly as authoritative as
#' one with five hundred. Absolute counts first, share on request.
#'
#' @param mode "count" for absolute stacked, "share" for 100% stacked
fw_chart_method <- function(data, sel, mode = c("count", "share")) {
  mode <- match.arg(mode)
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
           value = if (mode == "share") share else n,
           method_label = paste0(method_name, "  (", total, ")"))

  order_lv <- paste0(totals$method_name, "  (", totals$total, ")")
  # Grows with the number of methods, so eight methods are not crushed into the
  # space two would use.
  p <- plotly::plot_ly(height = max(250, 46 * nrow(totals) + 110))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(method_label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = "#ffffff", width = 1)),
      # The count inside the segment. Colour alone never carries the value.
      text = ~ifelse(share >= 9, as.character(n), ""),
      textposition = "inside", insidetextfont = list(color = "#ffffff"),
      hovertemplate = paste0("%{y}<br>", o,
                             ": %{text} of %{customdata}<extra></extra>"),
      customdata = ~total
    )
  }

  x_axis <- if (mode == "share") {
    list(title = "Share of attempts (%)", range = c(0, 100), ticksuffix = "%",
         zeroline = FALSE, gridcolor = FW_GRID_COLOUR)
  } else {
    list(title = "Attempts", zeroline = FALSE, gridcolor = FW_GRID_COLOUR)
  }

  fw_plotly_style(p) |>
    plotly::layout(
      barmode = "stack",
      # Stops plotly shrinking the in-bar counts to illegibility on a narrow
      # segment; below the floor it hides them instead, which is honest.
      uniformtext = list(minsize = 10, mode = "hide"),
      xaxis = x_axis,
      yaxis = list(title = "", automargin = TRUE)
    )
}

# ---- How long attempts take --------------------------------------------------

#' Time to completion by method, on a log scale
#'
#' A LOG AXIS IS THE POINT, not a convenience. Recorded durations run from a
#' single day to twenty-eight years, so a linear axis puts almost every attempt
#' in the leftmost pixel. The axis is annotated in words at both ends because a
#' log scale is exactly the thing a reader misreads without being told.
#'
#' Every attempt is drawn as a point on top of its box, coloured by outcome, so
#' the reader sees the actual spread rather than a summary of it - and a method
#' with four durations cannot masquerade as a distribution.
fw_chart_duration <- function(data, sel) {
  d <- data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    distinct(attempt_id, method_name) |>
    left_join(select(sel, attempt_id, duration_days, outcome), by = "attempt_id") |>
    filter(!is.na(duration_days), duration_days > 0) |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome))
  if (nrow(d) < 2) return(NULL)

  # The n WITH A DURATION, not the n with the method. Half the database has no
  # duration recorded, so labelling with the method's full total would overstate
  # what this chart is drawn from.
  totals <- d |> count(method_name, name = "n") |> arrange(n)
  d <- d |>
    left_join(totals, by = "method_name") |>
    mutate(method_label = paste0(method_name, "  (", n, ")"))
  order_lv <- paste0(totals$method_name, "  (", totals$n, ")")

  p <- plotly::plot_ly(height = max(270, 54 * nrow(totals) + 124))
  p <- plotly::add_trace(
    p, data = d, type = "box", orientation = "h",
    x = ~duration_days, y = ~factor(method_label, levels = order_lv),
    name = "", showlegend = FALSE, hoverinfo = "x",
    fillcolor = "rgba(199,237,232,0.45)",
    line = list(color = "#0d574c", width = 1.5),
    boxpoints = FALSE
  )
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "scatter", mode = "markers",
      x = ~duration_days, y = ~factor(method_label, levels = order_lv),
      name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]), size = 7,
                    opacity = 0.75,
                    line = list(color = "#ffffff", width = 1)),
      hovertemplate = paste0("%{y}<br>", o,
                             ": %{x:,.0f} days<extra></extra>")
    )
  }

  fw_plotly_style(p) |>
    plotly::layout(
      boxmode = "group",
      xaxis = list(
        title = "Days from start to finish  ← days   ·   years →",
        type = "log", gridcolor = FW_GRID_COLOUR, zeroline = FALSE,
        # Named ticks, because 10^3 means nothing to a practitioner deciding
        # whether they can commit a season or a decade.
        tickmode = "array",
        tickvals = c(1, 7, 30, 365, 1825, 3650),
        ticktext = c("1 day", "1 week", "1 month", "1 year", "5 years",
                     "10 years")
      ),
      yaxis = list(title = "", automargin = TRUE)
    )
}

# ---- Waterbody, driver and species -------------------------------------------

#' A horizontal bar of counts by category, stacked by outcome
#'
#' The workhorse behind the waterbody, reason and species charts. One function
#' rather than three near-identical ones, so they cannot drift into three
#' different looks for the same shape of question.
#'
#' @param d      a frame with `category` and `outcome`
#' @param title  the x-axis label
#' @param limit  keep the top n categories and gather the rest into "Other"
fw_chart_category <- function(d, title, limit = NA_integer_) {
  if (!nrow(d)) return(NULL)
  d$outcome <- as.character(fw_outcome_factor(d$outcome))

  totals <- d |> count(category, name = "total") |> arrange(desc(total))
  if (!is.na(limit) && nrow(totals) > limit) {
    keep <- totals$category[seq_len(limit)]
    # "Other" is a real bar, not a dropped remainder. A reader has to be able to
    # see how much of the picture the named categories actually cover.
    d$category <- ifelse(d$category %in% keep, d$category, FW_OTHER_LABEL)
    totals <- d |> count(category, name = "total")
  }
  # Ascending, because plotly draws the first category at the bottom.
  totals <- totals |>
    mutate(is_other = category == FW_OTHER_LABEL) |>
    arrange(desc(is_other), total)

  d <- d |>
    count(category, outcome, name = "n") |>
    left_join(select(totals, category, total), by = "category") |>
    mutate(label = paste0(category, "  (", total, ")"))
  order_lv <- paste0(totals$category, "  (", totals$total, ")")

  p <- plotly::plot_ly(height = max(240, 34 * nrow(totals) + 120))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~n, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = "#ffffff", width = 1)),
      hovertemplate = paste0("%{y}<br>", o, ": %{x}<extra></extra>")
    )
  }
  fw_plotly_style(p) |>
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = title, zeroline = FALSE, gridcolor = FW_GRID_COLOUR),
      yaxis = list(title = "", automargin = TRUE)
    )
}

FW_OTHER_LABEL <- "Other"

#' Attempts by kind of waterbody
#'
#' The specific type rather than the still/flowing split: "Lake" and "Pond"
#' behave differently enough that collapsing them loses the useful part, and the
#' regime is one filter away in the sidebar.
fw_chart_waterbody <- function(sel) {
  d <- sel |>
    filter(!is.na(waterbody_type)) |>
    transmute(category = waterbody_type, outcome)
  fw_chart_category(d, "Attempts", limit = 10L)
}

#' Why the eradications were carried out
fw_chart_driver <- function(sel) {
  d <- sel |>
    filter(!is.na(driver)) |>
    transmute(category = driver, outcome)
  fw_chart_category(d, "Attempts")
}

#' One row per (attempt, species) for a role, labelled and with its outcome
#'
#' DEDUPED PER ATTEMPT. An attempt listing a species twice must count once, or a
#' messily recorded row quietly inflates its species up the ranking.
#'
#' Pulled out of fw_chart_species() so the photo tiles on the report builder
#' count the same way the bar chart does. Two joins that are meant to agree and
#' are written twice are two joins that will eventually disagree.
#'
#' @param role_name "invasive" or "beneficiary"
fw_species_rows <- function(data, sel, role_name = c("invasive", "beneficiary")) {
  role_name <- match.arg(role_name)
  species <- fw_species_label(data$species)
  data$attempt_species |>
    filter(role == role_name, attempt_id %in% sel$attempt_id) |>
    distinct(attempt_id, species_id) |>
    left_join(select(species, species_id, label), by = "species_id") |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    filter(!is.na(label))
}

#' The top n species for a role, with their outcome split
#'
#' NO "OTHER" ROW HERE, unlike fw_chart_category(). This feeds a grid of
#' photographs, and there is no photograph of "the other 84 species" - the tail
#' is reported as a count in the block's note instead. Returns species_id too,
#' because that is what the image cache is keyed on.
#'
#' @return a tibble of species_id, label, n and one column per outcome, ordered
#'   by n descending; zero rows if the role has none in this selection.
fw_species_top_n <- function(data, sel, role_name, limit = 10L) {
  d <- fw_species_rows(data, sel, role_name)
  if (!nrow(d)) return(d[0, ])
  d$outcome <- as.character(fw_outcome_factor(d$outcome))

  totals <- d |> count(species_id, label, name = "n") |> arrange(desc(n), label)
  keep <- head(totals, limit)

  splits <- d |>
    filter(species_id %in% keep$species_id) |>
    count(species_id, outcome, name = "n_outcome")

  keep |>
    left_join(
      splits |>
        tidyr::pivot_wider(names_from = outcome, values_from = n_outcome,
                           values_fill = 0L),
      by = "species_id"
    ) |>
    # A level with no rows in this selection still needs its column, because the
    # tile bar always draws all four segments.
    (\(x) {
      for (o in FW_OUTCOME_LEVELS) if (is.null(x[[o]])) x[[o]] <- 0L
      x
    })()
}

#' The species most often targeted, or most often said to have benefited
#'
#' @param role_name "invasive" or "beneficiary"
fw_chart_species <- function(data, sel, role_name = c("invasive", "beneficiary")) {
  d <- fw_species_rows(data, sel, role_name) |>
    transmute(category = label, outcome)
  fw_chart_category(d, "Attempts", limit = 10L)
}

# ---- Methods against waterbody -----------------------------------------------

#' Which methods get used in which kind of water
#'
#' THE ONE CHART SEGMENTED BY METHOD RATHER THAN OUTCOME. It answers a question
#' the outcome charts cannot: standing at a lake, what have people actually
#' reached for? "How the methods compare" says how each method fared overall,
#' which is not the same thing - draining a pond and draining a river are one
#' method and two different propositions.
#'
#' Colour therefore comes from FW_METHOD_COLOURS, a sequential ramp that is
#' deliberately nothing like the outcome palette. See the note in config.R.
#'
#' Counted once per (attempt, method): an attempt using rotenone twice is one
#' use of rotenone.
#'
#' @param mode "count" for absolute stacked, "share" for 100% stacked
fw_chart_method_waterbody <- function(data, sel, mode = c("count", "share")) {
  mode <- match.arg(mode)
  d <- data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    distinct(attempt_id, method_id) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    left_join(select(sel, attempt_id, waterbody_type), by = "attempt_id") |>
    filter(!is.na(waterbody_type), !is.na(method_name))
  if (!nrow(d)) return(NULL)

  # Same contract as fw_chart_category(): keep the top ten kinds of water and
  # gather the rest into a real bar rather than dropping them, so the reader can
  # see how much of the picture the named ones cover.
  wb <- d |> count(waterbody_type, name = "total") |> arrange(desc(total))
  if (nrow(wb) > 10L) {
    keep <- wb$waterbody_type[seq_len(10L)]
    d$waterbody_type <- ifelse(d$waterbody_type %in% keep, d$waterbody_type,
                               FW_OTHER_LABEL)
  }

  totals <- d |>
    count(waterbody_type, name = "total") |>
    mutate(is_other = waterbody_type == FW_OTHER_LABEL) |>
    arrange(desc(is_other), total)

  dd <- d |>
    count(waterbody_type, method_id, method_name, name = "n") |>
    left_join(select(totals, waterbody_type, total), by = "waterbody_type") |>
    mutate(share = 100 * n / total,
           value = if (mode == "share") share else n,
           label = paste0(waterbody_type, "  (", total, ")"))
  order_lv <- paste0(totals$waterbody_type, "  (", totals$total, ")")

  # Methods are added in ramp order, so the key reads dark to light rather than
  # in whatever order the selection happened to produce.
  method_ids <- intersect(names(FW_METHOD_COLOURS), unique(dd$method_id))

  p <- plotly::plot_ly(height = max(240, 40 * nrow(totals) + 120))
  for (m in method_ids) {
    seg <- dd[dd$method_id == m, ]
    if (!nrow(seg)) next
    p <- plotly::add_trace(
      p, data = seg, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~value,
      name = seg$method_name[1],
      marker = list(color = unname(FW_METHOD_COLOURS[[m]]),
                    line = list(color = "#ffffff", width = 1)),
      text = ~ifelse(share >= 9, as.character(n), ""),
      textposition = "inside", insidetextfont = list(color = "#ffffff"),
      hovertemplate = paste0("%{y}<br>", seg$method_name[1],
                             ": %{text} of %{customdata}<extra></extra>"),
      customdata = ~total
    )
  }

  x_axis <- if (mode == "share") {
    list(title = "Share of uses (%)", range = c(0, 100), ticksuffix = "%",
         zeroline = FALSE, gridcolor = FW_GRID_COLOUR)
  } else {
    list(title = "Times used", zeroline = FALSE, gridcolor = FW_GRID_COLOUR)
  }

  fw_plotly_style(p) |>
    plotly::layout(
      barmode = "stack",
      uniformtext = list(minsize = 10, mode = "hide"),
      xaxis = x_axis,
      yaxis = list(title = "", automargin = TRUE)
    )
}
