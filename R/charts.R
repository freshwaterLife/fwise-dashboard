# charts.R
# Every plotly figure, shared by Plan and Explore so a quantity is drawn one way.
#
# Colour is never the only encoding: every chart also carries numbers or labels
# as text (client requirement, colourblind safety).
# Counts show all four outcomes. A success rate is Successful / (Successful +
# Failed) only; see fw_outcome_levels(). No headline "% successful" figure.
# Rotenone is never a headline statistic (socially sensitive).

library(dplyr)

# Shared chart font: the 1rem floor in px (plotly takes px) and the body ink.
# A function so it reads the tokens after every file is sourced.
fw_plot_font = function() {
  list(family = FW_TYPE$font_plot, size = FW_TYPE$floor_px,
       color = FW_COLOURS$ink)
}

#' A chart's height from its row count, per FW_CHART$height
fw_chart_height = function(chart, rows) {
  h = FW_CHART$height[[chart]]
  max(h[["min"]], h[["per_row"]] * rows + h[["pad"]])
}

#' Top margin a horizontal legend needs, in px
#'
#' Guesses how many lines the key wraps onto so plotly reserves room for it.
#' Rounds up: too much space is cheap, overlapping the bars is not.
#'
#' @param labels the legend entries, or NULL when the chart has no key
#' @param width plot width to assume, in px (the narrow end, where wrapping bites)
fw_legend_margin = function(labels = NULL, width = 640) {
  if (!length(labels)) return(FW_CHART$legend_min_top)
  entry = nchar(labels) * FW_CHART$legend_char_px + FW_CHART$legend_entry_px
  # Pack entries onto lines the way a flow layout would.
  lines = 1L
  used = 0
  for (w in entry) {
    if (used > 0 && used + w > width) { lines = lines + 1L; used = 0 }
    used = used + w
  }
  max(FW_CHART$legend_min_top,
      lines * FW_CHART$legend_line + FW_CHART$legend_pad)
}

FW_OUTCOME_LEVELS = c("Successful", "Failed", "Ongoing", "Unknown")

# The two outcomes a success rate is made of.
FW_RATE_LEVELS = c("Successful", "Failed")

#' The outcomes a chart draws: all four for counts, two for a success rate
fw_outcome_levels = function(mode = c("count", "share")) {
  if (match.arg(mode) == "share") FW_RATE_LEVELS else FW_OUTCOME_LEVELS
}

#' The x axis shared by every horizontal stacked bar
#'
#' Share mode has no gridlines of its own (fw_bar_rules() draws them) and a fixed
#' dtick so the rules line up with the labels. Count mode keeps plotly's grid.
fw_bar_x_axis = function(mode = c("count", "share")) {
  mode = match.arg(mode)
  if (mode == "share") {
    list(title = fw_axis_title(fw_t("charts", "x_share")), range = c(0, 100), ticksuffix = "%",
         zeroline = FALSE, showgrid = FALSE,
         tick0 = 0, dtick = FW_CHART$share_dtick)
  } else {
    # The zero rule is a shape (fw_bar_rules()), so plotly's zeroline is off.
    list(title = fw_axis_title(fw_t("charts", "x_attempts")), zeroline = FALSE,
         gridcolor = FW_COLOURS$border, gridwidth = FW_CHART$bar_grid)
  }
}

#' Vertical rules drawn behind a stacked bar chart
#'
#' Behind the bars at the client's request, so on the 100% view they show only
#' between rows. Shapes rather than gridlines so they stay on the share dtick.
#' Gotcha: xaxis.layer = "above traces" does not lift gridlines over bars; only
#' a shape with layer = "above" can.
#'
#' @return plotly shapes spanning the plot height (yref "paper")
fw_bar_rules = function(mode = c("count", "share")) {
  mode = match.arg(mode)
  at = if (mode == "share") seq(0, 100, FW_CHART$share_dtick) else 0
  lapply(at, function(v) {
    list(type = "line", layer = "below", xref = "x", yref = "paper",
         x0 = v, x1 = v, y0 = 0, y1 = 1,
         line = list(color = FW_COLOURS$border, width = FW_CHART$bar_grid))
  })
}

#' A stacked segment's hover text
#'
#' Count view: "Failed: 3". Share view: "Successful: 75% (3 of 4)", using the
#' species tiles' template (plan$r_tile_seg) so the two match. Built from the
#' counts, not %{x}, which on a 100% stack prints "33.33333".
#'
#' @param outcome the trace's outcome
#' @param n,total,share each segment's count, its bar total, and n/total in %
#' @param mode "count" or "share", as the chart
fw_hover_counts = function(outcome, n, total, share, mode = c("count", "share")) {
  mode = match.arg(mode)
  if (mode == "count") {
    tpl = fw_t("plan", "r_bar_count_seg")
    return(vapply(n, function(x) fw_fill(tpl, outcome = outcome, n = fw_fmt_num(x)),
                  character(1)))
  }
  tpl = fw_t("plan", "r_tile_seg")
  # pmax guards an empty category from printing NA.
  pc = sprintf("%.0f", pmax(0, round(share)))
  vapply(seq_along(n), function(i) {
    fw_fill(tpl, outcome = outcome, pc = pc[i],
            n = fw_fmt_num(n[i]), total = fw_fmt_num(total[i]))
  }, character(1))
}

#' An axis title with FW_CHART$title_gap between it and the tick labels
#'
#' Every axis title should go through here: a plain string would drop the gap.
fw_axis_title = function(text) {
  list(text = text, standoff = FW_CHART$title_gap)
}

#' Gap between an axis and its tick labels
#'
#' Invisible outside ticks, because the bundled plotly.js (2.25) predates
#' ticklabelstandoff (2.29).
#'
#' @param axis an axis list to add the gap to
fw_tick_gap = function(axis = list()) {
  modifyList(axis, list(ticks = "outside", ticklen = FW_CHART$tick_gap,
                        tickcolor = FW_TRANSPARENT))
}

#' Shared plotly styling: font, transparent ground, legend, locked axes
#'
#' Height is set per chart in plot_ly(), not here.
#' @param legend whether this chart needs a key at all
#' @param legend_labels the key's entries, used to size the top margin (see
#'   fw_legend_margin())
#' @param filename PNG download name, without extension or time stamp
fw_plotly_style = function(p, legend = TRUE, legend_labels = NULL,
                            filename = "fwise-chart") {
  legend_layout = list(orientation = "h", yref = "container", y = 1,
                       yanchor = "top", x = 0,
                       traceorder = "normal", font = fw_plot_font())

  plotly::layout(
    p,
    font = fw_plot_font(),
    # Transparent, so a chart takes the surface it sits on.
    paper_bgcolor = FW_TRANSPARENT,
    plot_bgcolor  = FW_TRANSPARENT,
    margin = list(l = 8, r = 8,
                  t = if (legend) fw_legend_margin(legend_labels) else 8,
                  b = 52),
    hoverlabel = list(font = fw_plot_font()),
    modebar = list(
      bgcolor = FW_TRANSPARENT,
      color = FW_COLOURS$ink_muted,
      activecolor = FW_COLOURS$teal_text
    ),
    showlegend = legend,
    legend = modifyList(legend_layout, list(itemclick = FALSE, itemdoubleclick = FALSE)),
    dragmode = FALSE,
    xaxis = fw_tick_gap(list(fixedrange = TRUE)),
    yaxis = fw_tick_gap(list(fixedrange = TRUE))
  ) |>
    plotly::config(
      responsive = TRUE,
      displaylogo = FALSE,
      scrollZoom = FALSE,
      doubleClick = FALSE,
      showAxisDragHandles = FALSE,
      modeBarButtons = list(list(fw_png_button(filename)))
    )
}

#' The camera button, naming the PNG with the time of the click
#'
#' UTC YYYYMMDD-HHMMSS, matching fw_file_stamp(). Plain JS so it also works in
#' a saved file with no server.
fw_png_button = function(filename) {
  list(
    name  = "toImage",
    title = fw_t("charts", "save_png"),
    icon  = htmlwidgets::JS("Plotly.Icons.camera"),
    click = htmlwidgets::JS(sprintf(paste0(
      "function(gd) {",
      "  var t = new Date().toISOString().slice(0, 19).replace(/[-:]/g, '').replace('T', '-');",
      "  Plotly.downloadImage(gd, {format: 'png', scale: %s,",
      "    width: gd._fullLayout.width, height: gd._fullLayout.height,",
      "    filename: %s + '_' + t});",
      "}"), FW_CHART$export_dpi / 96, jsonlite::toJSON(filename, auto_unbox = TRUE)))
  )
}

#' A chart, or a sentence saying there is nothing to draw
#'
#' Avoids a blank slot that looks like a failed load. The PDF skips NULL charts
#' instead.
fw_chart_or_empty = function(p) {
  shiny::validate(shiny::need(!is.null(p), fw_t("charts", "empty")))
  p
}

#' Outcome as a factor with every level present; missing is Unknown
#'
#' Empty levels stay at zero so a small slice doesn't look unanimous.
fw_outcome_factor = function(x) {
  factor(ifelse(is.na(x), "Unknown", x), levels = FW_OUTCOME_LEVELS)
}

fw_outcome_counts = function(sel) {
  tibble(outcome = FW_OUTCOME_LEVELS,
         n = as.integer(table(fw_outcome_factor(sel$outcome))))
}

# ---- Cumulative over time ----------------------------------------------------

#' Cumulative attempts by start year, stacked by outcome

fw_chart_cumulative = function(sel) {
  y = sel[!is.na(sel$start_year), c("start_year", "outcome")]
  if (!nrow(y)) return(NULL)
  this_year = as.integer(format(Sys.Date(), "%Y"))
  last_year = max(max(y$start_year), this_year)
  years = seq(min(y$start_year), last_year)
  d = y |>
    mutate(outcome = fw_outcome_factor(outcome)) |>
    count(start_year, outcome, name = "n") |>
    tidyr::complete(start_year = years, outcome = FW_OUTCOME_LEVELS,
                    fill = list(n = 0L)) |>
    arrange(start_year) |>
    group_by(outcome) |>
    mutate(cumulative = cumsum(n)) |>
    ungroup()

  p = plotly::plot_ly(height = FW_CHART$height$cumulative)
  for (o in FW_OUTCOME_LEVELS) {
    dd = d[d$outcome == o, ]
    p = plotly::add_trace(
      p, data = dd, x = ~start_year, y = ~cumulative,
      type = "scatter", mode = "lines", name = o,
      stackgroup = "one",
      line = list(shape = "linear", width = FW_CHART$line,
                  color = unname(FW_OUTCOME_COLOURS[[o]])),
      fillcolor = unname(FW_OUTCOME_COLOURS[[o]]),
      hovertemplate = paste0(o, ": %{y}<extra></extra>")
    )
  }
  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = "fwise-cumulative-attempts") |>
    plotly::layout(
      hovermode = "x unified",
      xaxis = list(title = fw_axis_title(fw_t("charts", "x_year")), gridcolor = FW_COLOURS$border,
                   zeroline = FALSE,
                   # The axis ends at the present, not at the last record.
                   range = c(min(years), last_year)),
      yaxis = list(title = fw_axis_title(fw_t("charts", "y_cumulative")), gridcolor = FW_COLOURS$border,
                   zeroline = FALSE, rangemode = "tozero")
    )
}



# ---- Outcome by method -------------------------------------------------------

#' Outcome mix within each method
#'
#' "share" answers how often it works, "count" how much evidence there is.
#' Single-method attempts only (fw_single_method_ids()), so no method is
#' credited with another's result. In share mode the bar total is finished
#' attempts only.
fw_method_data = function(data, sel, mode = c("count", "share")) {
  mode = match.arg(mode)
  one = fw_single_method_ids(data, sel)
  me = data$attempt_method |>
    filter(attempt_id %in% one) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    distinct(attempt_id, method_name, method_id) |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome)) |>
    filter(outcome %in% fw_outcome_levels(mode))
  if (nrow(me) == 0) return(NULL)

  totals = me |>
    count(method_name, method_id, name = "total") |>
    mutate(is_other = method_id %in% FW_METHOD_OTHER) |>
    arrange(desc(is_other), total) |>
    select(method_name, total)
  d = me |>
    count(method_name, outcome, name = "n") |>
    left_join(totals, by = "method_name") |>
    mutate(share = 100 * n / total,
           value = if (mode == "share") share else n,
           method_label = paste0(method_name, "  (", total, ")"))

  list(d = d, order_lv = paste0(totals$method_name, "  (", totals$total, ")"),
       mode = mode)
}

#' Attempts that recorded exactly one method
#'
#' The method and duration charts draw only these: outcome and duration belong
#' to the attempt, and there are no per-method outcomes or dates.
#'
#' @return attempt ids from `sel`
fw_single_method_ids = function(data, sel) {
  am = data$attempt_method[data$attempt_method$attempt_id %in% sel$attempt_id, ]
  am = unique(am[, c("attempt_id", "method_id")])
  n = table(am$attempt_id)
  names(n)[n == 1L]
}

#' How many attempts the method charts leave out for recording several methods
fw_n_multi_method = function(data, sel) {
  with_method = unique(data$attempt_method$attempt_id[
    data$attempt_method$attempt_id %in% sel$attempt_id])
  length(with_method) - length(fw_single_method_ids(data, sel))
}

fw_chart_method = function(data, sel, mode = c("count", "share")) {
  mode = match.arg(mode)
  md = fw_method_data(data, sel, mode)
  if (is.null(md)) return(NULL)
  d = md$d
  order_lv = md$order_lv
  font = fw_plot_font()
  p = plotly::plot_ly(height = fw_chart_height("method", length(order_lv)))
  levels = fw_outcome_levels(mode)
  for (o in levels) {
    dd = d[d$outcome == o, ]
    if (!nrow(dd)) next
    # Computed now, not as a ~formula: a formula is evaluated after the loop,
    # when `o` is the last outcome.
    hover = fw_hover_counts(o, dd$n, dd$total, dd$share, mode)
    p = plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(method_label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      # Indigo, not white (see FW_OUTCOME_LABEL_INK).
      insidetextfont = list(color = unname(FW_OUTCOME_LABEL_INK[[o]]),
                            family = font$family, size = font$size),
      # The bar name, then the sentence from fw_hover_counts().
      hovertemplate = "%{y}<br>%{customdata}<extra></extra>",
      customdata = hover
    )
  }

  fw_plotly_style(p, legend_labels = levels,
                  filename = paste0("fwise-methods-", mode)) |>
    plotly::layout(
      barmode = "stack",
      uniformtext = list(minsize = FW_TYPE$floor_px, mode = "hide"),
      shapes = fw_bar_rules(mode),
      xaxis = fw_bar_x_axis(mode),
      yaxis = list(title = "", automargin = TRUE)
    )
}

# ---- How long attempts take --------------------------------------------------

# Time to completion by method on a log axis (durations run from a day to
# decades). Every attempt is a dot over its method's box.

#' The attempts the duration chart draws
#'
#' Single-method (a multi-method duration is the whole campaign, not one
#' method's), successful (the chart is for planning how long success takes),
#' and with a positive duration.
#'
#' @return the rows of `sel` this chart draws, which the caption counts
fw_duration_sel = function(data, sel) {
  sel |>
    filter(attempt_id %in% fw_single_method_ids(data, sel),
           outcome %in% "Successful",
           !is.na(duration_days), duration_days > 0)
}

#' The duration chart's x range, in log10 (what a log axis range wants)
#'
#' Set by hand: plotly's autorange for box traces on a log axis is badly wrong.
#' The pad is a share of the span.
fw_duration_range = function(days) {
  lx = log10(range(days))
  pad = max(FW_CHART$duration_pad_min, diff(lx) * FW_CHART$duration_pad)
  c(lx[1] - pad, lx[2] + pad)
}

#' Named ticks for a selection, ending at its longest attempt
#'
#' Fixed breaks past the maximum are dropped, a final tick is added at the
#' maximum (labelled like the map card), and a fixed tick too close to it gives
#' way so labels don't overprint.
#'
#' @param days every duration in the selection, all > 0
#' @return list(vals = tick positions in days, text = labels). Days, not log10:
#'   plotly logs tickvals itself.
fw_duration_ticks = function(days) {
  mx = max(days)
  vals = FW_CHART$duration_ticks
  text = fw_t("charts", "duration_ticks")

  keep = vals <= mx
  vals = vals[keep]
  text = text[keep]

  # The longest attempt already sits on a named break - nothing to add.
  if (length(vals) && isTRUE(all.equal(vals[length(vals)], mx))) {
    return(list(vals = vals, text = text))
  }

  # Same units as the map card and the box hover.
  label = fw_popup_duration(mx)

  crowded = length(vals) > 0 &&
    (log10(mx) - log10(vals[length(vals)])) < FW_CHART$duration_tick_gap
  if (crowded) {
    vals = vals[-length(vals)]
    text = text[-length(text)]
  }

  list(vals = c(vals, mx), text = c(text, label))
}

#' Vertical offsets that spread duration dots across their row (a cheap beeswarm)
#'
#' Dots within FW_CHART$duration_swarm$bin (log10 days) fan out 0, +1, -1, +2...
#' steps, squeezed to fit the row. Deterministic.
#'
#' @param days durations in days, all > 0.
#' @param row the row each dot belongs to.
#' @return an offset per dot, in rows, within +/- FW_CHART$duration_swarm$spread.
fw_duration_swarm = function(days, row) {
  cfg = FW_CHART$duration_swarm
  bin = floor(log10(days) / cfg$bin)
  out = numeric(length(days))
  for (idx in split(seq_along(days), list(row, bin), drop = TRUE)) {
    m = length(idx)
    if (m < 2) next
    half = ceiling((m - 1) / 2)
    step = min(cfg$step, cfg$spread / half)
    k = seq_len(m) - 1L
    out[idx] = step * ceiling(k / 2) * ifelse(k %% 2 == 1, 1, -1)
  }
  out
}

#' The duration chart's rows, one per dot, with row and offset
#'
#' Shared by the plotly chart and the PDF version.
#'
#' @return list(d = one row per attempt with row and y_dot, order_lv = the row
#'   labels bottom to top), or NULL when fewer than two dots would be drawn
fw_duration_data = function(data, sel) {
  sel = fw_duration_sel(data, sel)
  if (!nrow(sel)) return(NULL)
  d = data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    distinct(attempt_id, method_name, method_id) |>
    left_join(select(sel, attempt_id, duration_days, outcome), by = "attempt_id") |>
    filter(!is.na(duration_days), duration_days > 0) |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome))
  if (nrow(d) < 2) return(NULL)

  # Labels count attempts with a duration. The two "Other" methods sort to the
  # bottom, as on every method chart; the rest by count.
  totals = d |>
    count(method_name, method_id, name = "n") |>
    mutate(is_other = method_id %in% FW_METHOD_OTHER) |>
    arrange(desc(is_other), n)
  d = d |>
    left_join(select(totals, method_name, n), by = "method_name") |>
    mutate(method_label = paste0(method_name, "  (", n, ")"))
  order_lv = paste0(totals$method_name, "  (", totals$n, ")")
  # A numeric row per method so dots can sit either side of centre. Names come
  # back as tick text in fw_chart_duration().
  d = d |> arrange(method_label, duration_days, attempt_id)
  d$row = match(d$method_label, order_lv)
  d$y_dot = d$row + fw_duration_swarm(d$duration_days, d$row)

  list(d = d, order_lv = order_lv)
}

#' Box statistics per duration row, and the hover text describing them
#'
#' Type 7 quartiles, 1.5 x IQR fences (what plotly and ggplot draw). Lengths are
#' formatted by fw_popup_duration(), like the map card.
#'
#' @param d fw_duration_data()$d
#' @return one row per method row: row, q1, median, q3, lowerfence, upperfence,
#'   hover
fw_duration_box_stats = function(d) {
  rows = sort(unique(d$row))
  out = lapply(rows, function(r) {
    x = d$duration_days[d$row == r]
    q = unname(stats::quantile(x, c(0.25, 0.5, 0.75), type = 7))
    iqr = q[3] - q[1]
    data.frame(row = r, label = d$method_label[d$row == r][1], n = length(x),
               q1 = q[1], median = q[2], q3 = q[3],
               lowerfence = min(x[x >= q[1] - 1.5 * iqr]),
               upperfence = max(x[x <= q[3] + 1.5 * iqr]),
               shortest = min(x), longest = max(x))
  })
  st = do.call(rbind, out)
  fmt = function(v) vapply(v, fw_popup_duration, character(1))
  tpl = fw_t("charts", "duration_hover")
  st$hover = vapply(seq_len(nrow(st)), function(i) {
    fw_fill(tpl, method = sub("  \\([0-9]+\\)$", "", st$label[i]),
            n = fw_fmt_num(st$n[i]), median = fmt(st$median[i]),
            q1 = fmt(st$q1[i]), q3 = fmt(st$q3[i]),
            shortest = fmt(st$shortest[i]), longest = fmt(st$longest[i]))
  }, character(1))
  st
}

fw_chart_duration = function(data, sel) {
  dd = fw_duration_data(data, sel)
  if (is.null(dd)) return(NULL)
  d = dd$d
  order_lv = dd$order_lv

  ticks = fw_duration_ticks(d$duration_days)

  # Box from precomputed stats: plotly's own box hover prints raw days. The
  # readable hover comes from invisible markers below.
  st = fw_duration_box_stats(d)
  p = plotly::plot_ly(height = fw_chart_height("duration", length(order_lv)))
  p = plotly::add_trace(
    p, type = "box", orientation = "h",
    y = st$row, q1 = st$q1, median = st$median, q3 = st$q3,
    lowerfence = st$lowerfence, upperfence = st$upperfence,
    width = 2 * FW_CHART$duration_swarm$spread,
    name = "", showlegend = FALSE, hoverinfo = "skip",
    # Brand colours: the box is chrome, the dots are the data.
    fillcolor = fw_rgba(FW_COLOURS$teal_tint, 0.45),
    line = list(color = FW_COLOURS$teal_text, width = FW_CHART$box_line)
  )
  # Dots: no hover (they're picture, not controls) and one colour, since all
  # are successful.
  p = plotly::add_trace(
    p, data = d, type = "scatter", mode = "markers",
    x = ~duration_days, y = ~y_dot,
    name = "", showlegend = FALSE, hoverinfo = "skip",
    marker = list(color = fw_rgba(unname(FW_OUTCOME_COLOURS[["Successful"]]),
                                  FW_CHART$point$fill_alpha),
                  size = FW_CHART$point$size,
                  line = list(color = FW_COLOURS$teal_hover,
                              width = FW_CHART$point$stroke))
  )
  # Box hover: invisible markers along each row, drawn last so they're on top.
  hc = FW_CHART$duration_hover
  hv = do.call(rbind, lapply(seq_len(nrow(st)), function(i) {
    lx = log10(c(st$shortest[i], st$longest[i]))
    n = max(3L, ceiling(diff(lx) / hc$step) + 1L)
    data.frame(x = 10^seq(lx[1], lx[2], length.out = n),
               row = st$row[i], hover = st$hover[i])
  }))
  p = plotly::add_trace(
    p, type = "scatter", mode = "markers", x = hv$x, y = hv$row,
    name = "", showlegend = FALSE,
    marker = list(size = hc$size, opacity = 0),
    hovertemplate = "%{customdata}<extra></extra>", customdata = hv$hover,
    # Site pop-up colours; an invisible trace would otherwise get plotly's green.
    hoverlabel = list(bgcolor = FW_COLOURS$surface,
                      bordercolor = FW_COLOURS$teal_text,
                      align = "left", font = fw_plot_font())
  )

  fw_plotly_style(p, legend = FALSE, filename = "fwise-duration") |>
    plotly::layout(
      boxmode = "group",
      # Right margin for the last tick's label.
      margin = list(l = 8, r = 44, t = 8, b = 52),
      xaxis = list(
        # No title: the named ticks say what it measures.
        title = "",
        type = "log", zeroline = FALSE,
        # Dotted ink gridline on each unit break. tickmode "array" puts a
        # gridline at each tickval only.
        showgrid = TRUE, griddash = "dot",
        gridcolor = FW_COLOURS$ink, gridwidth = FW_CHART$duration_grid,
        # Explicit, see fw_duration_range().
        range = fw_duration_range(d$duration_days),
        # Gotcha: on a log axis the range is log10 but tickvals are raw days
        # (plotly logs them itself). Don't wrap these in log10().
        tickmode = "array",
        tickvals = ticks$vals,
        ticktext = ticks$text
      ),
      # No horizontal gridlines: this axis is method names.
      yaxis = list(title = "", automargin = TRUE, showgrid = FALSE,
                   zeroline = FALSE, tickmode = "array",
                   tickvals = seq_along(order_lv), ticktext = order_lv,
                   range = c(0.5, length(order_lv) + 0.5))
    )
}

# ---- Waterbody and species ---------------------------------------------------

#' A horizontal bar of counts by category, stacked by outcome
#'
#' Shared by the waterbody and species charts.
#'
#' @param d      a frame with `category` and `outcome`
#' @param limit  keep the top n categories and gather the rest into "Other"
#' @param filename PNG export name; the mode is appended
#' @param mode "count" or "share" (100% stacked, within each category's own
#'   total, like fw_chart_method())
#'
#' fw_category_data() counts and is shared with the PDF (R/charts_static.R);
#' fw_chart_category() draws.
fw_category_data = function(d, limit = NA_integer_, mode = c("count", "share")) {
  mode = match.arg(mode)
  if (!nrow(d)) return(NULL)
  d$outcome = as.character(fw_outcome_factor(d$outcome))
  d = d[d$outcome %in% fw_outcome_levels(mode), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  other = fw_t("charts", "other")

  totals = d |> count(category, name = "total") |> arrange(desc(total))
  if (!is.na(limit) && nrow(totals) > limit) {
    keep = totals$category[seq_len(limit)]
    # "Other" is a real bar, so the reader sees how much the named ones cover.
    d$category = ifelse(d$category %in% keep, d$category, other)
    totals = d |> count(category, name = "total")
  }
  # Ascending, because plotly draws the first category at the bottom.
  totals = totals |>
    mutate(is_other = category == other) |>
    arrange(desc(is_other), total)

  d = d |>
    count(category, outcome, name = "n") |>
    left_join(select(totals, category, total), by = "category") |>
    mutate(share = 100 * n / total,
           value = if (mode == "share") share else n,
           label = paste0(category, "  (", total, ")"))
  order_lv = paste0(totals$category, "  (", totals$total, ")")

  list(d = d, order_lv = order_lv)
}

fw_chart_category = function(d, limit = NA_integer_,
                              filename = "fwise-categories",
                              mode = c("count", "share")) {
  mode = match.arg(mode)
  cd = fw_category_data(d, limit, mode)
  if (is.null(cd)) return(NULL)
  d = cd$d
  order_lv = cd$order_lv
  font = fw_plot_font()

  p = plotly::plot_ly(height = fw_chart_height("category", length(order_lv)))
  levels = fw_outcome_levels(mode)
  for (o in levels) {
    dd = d[d$outcome == o, ]
    if (!nrow(dd)) next
    # Eager, not a ~formula - see fw_chart_method().
    hover = fw_hover_counts(o, dd$n, dd$total, dd$share, mode)
    p = plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      insidetextfont = list(color = unname(FW_OUTCOME_LABEL_INK[[o]]),
                            family = font$family, size = font$size),
      hovertemplate = "%{y}<br>%{customdata}<extra></extra>",
      customdata = hover
    )
  }

  fw_plotly_style(p, legend_labels = levels,
                  filename = paste0(filename, "-", mode)) |>
    plotly::layout(
      barmode = "stack",
      # A segment too narrow for the 1rem floor hides its number; the hover
      # still has it.
      uniformtext = list(minsize = FW_TYPE$floor_px, mode = "hide"),
      shapes = fw_bar_rules(mode),
      xaxis = fw_bar_x_axis(mode),
      yaxis = list(title = "", automargin = TRUE)
    )
}

#' Attempts in still and flowing water (lentic/lotic)
#'
#' Labelled as the regime filter is (fw_regime_label()). Hidden when one regime
#' is filtered (fw_show_waterbody()).
#'
#' @param mode "count" or "share"; bar labels keep their totals in both
fw_chart_waterbody = function(sel, mode = c("count", "share")) {
  mode = match.arg(mode)
  fw_chart_category(fw_waterbody_rows(sel), filename = "fwise-waterbody", mode = mode)
}

#' The still/flowing chart's input: one row per attempt with a regime
fw_waterbody_rows = function(sel) {
  sel |>
    filter(!is.na(water_regime)) |>
    transmute(category = fw_regime_label(water_regime), outcome)
}

#' Whether the still/flowing chart is shown: not when exactly one regime is picked
fw_show_waterbody = function(filters) {
  length(filters$regime %||% character(0)) != 1L
}

#' One row per (attempt, species) for a role, with label and outcome
#'
#' Deduped per attempt. With `f`, only species matching that role's animal and
#' family filters count (fw_species_in_filter()), so a family filter doesn't
#' rank species from outside it.
#'
#' @param role_name "invasive" or "beneficiary"
#' @param f a filter state (fw_filter_state()), or NULL for every species
fw_species_rows = function(data, sel, role_name = c("invasive", "beneficiary"),
                            f = NULL) {
  role_name = match.arg(role_name)
  species = fw_species_in_filter(fw_species_label(data$species), role_name, f)
  # `shown` is the tile text: protected species carry their Red List code.
  species = if (role_name == "beneficiary") fw_species_shown(species) else
    mutate(species, shown = label)
  data$attempt_species |>
    filter(role == role_name, attempt_id %in% sel$attempt_id) |>
    distinct(attempt_id, species_id) |>
    inner_join(select(species, species_id, label, shown), by = "species_id") |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    filter(!is.na(label))
}

# Each role's kind-of-animal, family and species filters, by FW_FILTERS id.
FW_SPECIES_ROLE_FILTERS = list(
  invasive    = c(taxa = "taxa", family = "family", species = "species"),
  beneficiary = c(taxa = "taxa_beneficiary", family = "family_beneficiary",
                  species = "beneficiary")
)

#' The species a role's animal and family filters allow
#'
#' Same rule as the pickers (fw_species_allowed()). Species picks aren't applied
#' here: that role's tiles are hidden instead (fw_species_row_shown()).
fw_species_in_filter = function(species, role_name, f = NULL) {
  if (is.null(f)) return(species)
  ids = FW_SPECIES_ROLE_FILTERS[[role_name]]
  keep = fw_species_allowed(species, f[[ids[["taxa"]]]] %||% character(0),
                             f[[ids[["family"]]]] %||% character(0), species$label)
  species[species$label %in% keep, , drop = FALSE]
}

#' Whether a role's top-species tiles are shown: not when species are picked
#' for that role (they'd echo the pick back)
fw_species_row_shown = function(role_name, f = NULL) {
  if (is.null(f)) return(TRUE)
  !length(f[[FW_SPECIES_ROLE_FILTERS[[role_name]][["species"]]]] %||% character(0))
}

#' The top n species for a role, with their outcome split
#'
#' No "Other" row: this feeds a photo grid. species_id is kept for the image
#' cache.
#'
#' @return species_id, label, shown, n and one column per outcome, by n
#'   descending; zero rows if the role has none in this selection.
fw_species_top_n = function(data, sel, role_name, limit = FW_TOP_N, f = NULL) {
  d = fw_species_rows(data, sel, role_name, f)
  if (!nrow(d)) return(d[0, ])
  d$outcome = as.character(fw_outcome_factor(d$outcome))

  totals = d |> count(species_id, label, shown, name = "n") |> arrange(desc(n), label)
  keep = head(totals, limit)

  splits = d |>
    filter(species_id %in% keep$species_id) |>
    count(species_id, outcome, name = "n_outcome")

  keep |>
    left_join(
      splits |>
        tidyr::pivot_wider(names_from = outcome, values_from = n_outcome,
                           values_fill = 0L),
      by = "species_id"
    ) |>
    # Every outcome needs a column: the tile bar draws all four.
    (\(x) {
      for (o in FW_OUTCOME_LEVELS) if (is.null(x[[o]])) x[[o]] = 0L
      x
    })()
}
