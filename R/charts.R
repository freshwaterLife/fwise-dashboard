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
#
# A chart's axis ticks, legend and hover labels are text like any other text
# in the app, and the client's 1rem floor covers them. plotly takes pixels
# rather than rem, so FW_TYPE$floor_px is that floor in the unit plotly
# understands. The ink is the same colour as the prose around the chart.
#
# A function rather than a constant: this file sorts before R/brand.R would
# have been read if it were not first, and a function reads the tokens when it
# is called, which is after every file has been sourced.
fw_plot_font <- function() {
  list(family = FW_TYPE$font_plot, size = FW_TYPE$floor_px,
       color = FW_COLOURS$ink)
}

#' A chart's height from its row count, per FW_CHART$height
fw_chart_height <- function(chart, rows) {
  h <- FW_CHART$height[[chart]]
  max(h[["min"]], h[["per_row"]] * rows + h[["pad"]])
}

#' How much top margin a horizontal legend needs, in px
#'
#' THIS USED TO BE A FLAT 42 AND THAT IS THE BUG. 42px is exactly one line at
#' the 16px type floor with nothing to spare, so any chart whose key wrapped -
#' seven method names, or four outcomes in a narrow column - put its second line
#' on top of the plot area and over the top of the bars.
#'
#' plotly does the real layout; this only has to guess how many lines the key
#' will take so the margin is reserved before it does. Guessing high is cheap
#' (a little white space) and guessing low is not (an unreadable chart), so the
#' estimate rounds up and never returns less than FW_CHART$legend_min_top.
#'
#' @param labels the legend entries, or NULL when the chart has no key
#' @param width the plot width to assume, in px. Charts here are fluid, so this
#'   is the narrow end of the range rather than the average: the overlap only
#'   ever happened in the narrow case.
fw_legend_margin <- function(labels = NULL, width = 640) {
  if (!length(labels)) return(FW_CHART$legend_min_top)
  entry <- nchar(labels) * FW_CHART$legend_char_px + FW_CHART$legend_entry_px
  # Pack entries onto lines the way a flow layout would.
  lines <- 1L
  used <- 0
  for (w in entry) {
    if (used > 0 && used + w > width) { lines <- lines + 1L; used <- 0 }
    used <- used + w
  }
  max(FW_CHART$legend_min_top,
      lines * FW_CHART$legend_line + FW_CHART$legend_pad)
}

FW_OUTCOME_LEVELS <- c("Successful", "Failed", "Ongoing", "Unknown")

#' Room between an axis and its tick labels
#'
#' INVISIBLE TICKS, NOT A STANDOFF. plotly.js has ticklabelstandoff from 2.29,
#' and the plotly R package here bundles 2.25, so the gap is made the way it
#' was before that existed: outside ticks as long as the gap, drawn in no
#' colour. The labels sit off the axis and there is still no mark on it - the
#' client ended the axis-line A/B test (Sept 2026) in favour of bare axes.
#'
#' @param axis an axis list to add the gap to
fw_tick_gap <- function(axis = list()) {
  modifyList(axis, list(ticks = "outside", ticklen = FW_CHART$tick_gap,
                        tickcolor = FW_TRANSPARENT))
}

#' Strip plotly's chrome down to what the design system uses
#'
#' Height is NOT set here. plotly deprecated width/height in layout(), so each
#' chart passes it to plot_ly() instead - which is also where a chart that needs
#' to grow with its number of rows can compute it.
#' @param legend whether this chart needs a key at all
#' @param legend_labels the entries that will appear in the key. Passed so the
#'   top margin can be sized to the number of LINES they wrap onto rather than
#'   assuming one - see fw_legend_margin(). A chart that omits this gets the
#'   floor, which is one line.
#' @param legend_side "top" for every chart with an x-axis, "right" for the
#'   donuts. See the note on the side legend below.
#' @param filename what a downloaded PNG of this chart is called, without the
#'   extension. Every chart should pass its own: a reader who exports four of
#'   these wants four distinguishable files, not newplot (1..4).
fw_plotly_style <- function(p, legend = TRUE, legend_labels = NULL,
                            legend_side = c("top", "right"),
                            filename = "fwise-chart") {
  legend_side <- match.arg(legend_side)
  side <- identical(legend_side, "right")

  legend_layout <- if (side) {
    list(orientation = "v", x = FW_CHART$donut_legend_x, xanchor = "left",
         y = 0.5, yanchor = "middle",
         traceorder = "normal", font = fw_plot_font())
  } else {
    list(orientation = "h", yref = "container", y = 1, yanchor = "top", x = 0,
         traceorder = "normal", font = fw_plot_font())
  }

  plotly::layout(
    p,
    font = fw_plot_font(),
    # Transparent, so a chart takes the surface it sits on.
    paper_bgcolor = FW_TRANSPARENT,
    plot_bgcolor  = FW_TRANSPARENT,
    margin = if (side) {
      list(l = 8, r = 8, t = 8, b = 8)
    } else {
      list(l = 8, r = 8,
           t = if (legend) fw_legend_margin(legend_labels) else 8,
           b = 52)
    },
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
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(format = "png",
                                  scale = FW_CHART$export_dpi / 96,
                                  filename = filename)
    )
}

#' A chart, or a sentence saying there is none
#'
#' Every plot returns NULL when the selection gives it nothing to
#' draw, and renderPlotly(NULL) leaves a blank space under the block's heading -
#' which reads as a chart that failed to load. Shiny's validation message lands
#' in the output's own slot, so this needs no second output and no second id.
#' The PDF report does not use it: fw_typ_figure() skips a NULL chart,
#' heading and all.
fw_chart_or_empty <- function(p) {
  shiny::validate(shiny::need(!is.null(p), fw_t("charts", "empty")))
  p
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

fw_chart_cumulative <- function(sel) {
  y <- sel[!is.na(sel$start_year), c("start_year", "outcome")]
  if (!nrow(y)) return(NULL)
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  last_year <- max(max(y$start_year), this_year)
  years <- seq(min(y$start_year), last_year)
  d <- y |>
    mutate(outcome = fw_outcome_factor(outcome)) |>
    count(start_year, outcome, name = "n") |>
    tidyr::complete(start_year = years, outcome = FW_OUTCOME_LEVELS,
                    fill = list(n = 0L)) |>
    arrange(start_year) |>
    group_by(outcome) |>
    mutate(cumulative = cumsum(n)) |>
    ungroup()

  p <- plotly::plot_ly(height = FW_CHART$height$cumulative)
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    p <- plotly::add_trace(
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
      xaxis = list(title = fw_t("charts", "x_year"), gridcolor = FW_COLOURS$border,
                   zeroline = FALSE,
                   # Explicit, so the axis ends at the present rather than at
                   # whatever the data happens to reach.
                   range = c(min(years), last_year)),
      yaxis = list(title = fw_t("charts", "y_cumulative"), gridcolor = FW_COLOURS$border,
                   zeroline = FALSE, rangemode = "tozero")
    )
}



# ---- Outcome by method -------------------------------------------------------

#' Outcome mix within each method
#'
#' Two modes over one chart rather than two charts, because the sidebar leaves a
#' narrow column and a pair side by side would crush both. "share" answers "how
#' often does this work", "count" answers "how much evidence is there".
fw_method_data <- function(data, sel, mode = c("count", "share")) {
  mode <- match.arg(mode)
  me <- data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    left_join(select(data$method, method_id, method_name), by = "method_id") |>
    distinct(attempt_id, method_name, method_id) |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome))
  if (nrow(me) == 0) return(NULL)

  totals <- me |>
    count(method_name, method_id, name = "total") |>
    mutate(is_other = method_id %in% FW_METHOD_OTHER) |>
    arrange(desc(is_other), total) |>
    select(method_name, total)
  d <- me |>
    count(method_name, outcome, name = "n") |>
    left_join(totals, by = "method_name") |>
    mutate(share = 100 * n / total,
           value = if (mode == "share") share else n,
           method_label = paste0(method_name, "  (", total, ")"))

  list(d = d, order_lv = paste0(totals$method_name, "  (", totals$total, ")"),
       mode = mode)
}

fw_chart_method <- function(data, sel, mode = c("count", "share")) {
  mode <- match.arg(mode)
  md <- fw_method_data(data, sel, mode)
  if (is.null(md)) return(NULL)
  d <- md$d
  order_lv <- md$order_lv
  # Grows with the number of methods, so eight methods are not crushed into the
  # space two would use.
  font <- fw_plot_font()
  p <- plotly::plot_ly(height = fw_chart_height("method", length(order_lv)))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(method_label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      # Indigo, not white. See FW_OUTCOME_LABEL_INK in config.R: none of the
      # four Wong fills is dark enough to carry white numerals.
      insidetextfont = list(color = unname(FW_OUTCOME_LABEL_INK[[o]]),
                            family = font$family, size = font$size),
      hovertemplate = paste0("%{y}<br>", o, ": %{customdata}<extra></extra>"),
      customdata = ~paste0(n, fw_t("charts", "hover_of"), total)
    )
  }

  x_axis <- if (mode == "share") {
    list(title = fw_t("charts", "x_share"), range = c(0, 100), ticksuffix = "%",
         zeroline = FALSE, gridcolor = FW_COLOURS$border)
  } else {
    list(title = fw_t("charts", "x_attempts"), zeroline = FALSE,
         gridcolor = FW_COLOURS$border)
  }

  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = paste0("fwise-methods-", mode)) |>
    plotly::layout(
      barmode = "stack",
      uniformtext = list(minsize = FW_TYPE$floor_px, mode = "hide"),
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
#'
#' SINGLE-METHOD ATTEMPTS ONLY. See fw_duration_sel() for why.

#' The attempts this chart is allowed to draw
#'
#' ONE RECORDED METHOD, plus a usable duration. The second condition is obvious;
#' the first is the one that needs saying.
#'
#' duration_days is a property of the ATTEMPT, not of a method within it, and
#' this chart puts it against a method. For an attempt that used one method that
#' is the same statement. For an attempt that ran rotenone in 1994 and was still
#' netting in 2021, it is not: the ten thousand days it contributes is how long
#' the CAMPAIGN ran, and drawing it against both methods says each of them took
#' twenty-eight years. That outlier is what the client saw on the call.
#'
#' The cost is small - the large majority of attempts with a duration record a
#' single method - and the caption under the chart says what it was.
#'
#' THE HONEST ALTERNATIVE IS PER-METHOD DATES, which the database does not hold.
#' If it ever does, this filter is what should be removed first.
#'
#' @return the rows of `sel` this chart draws, which is what the caption counts
fw_duration_sel <- function(data, sel) {
  one <- data$attempt_method |>
    filter(attempt_id %in% sel$attempt_id) |>
    distinct(attempt_id, method_id) |>
    count(attempt_id, name = "n_methods") |>
    filter(n_methods == 1L)
  sel |>
    filter(attempt_id %in% one$attempt_id,
           !is.na(duration_days), duration_days > 0)
}

# fw_duration_bands() USED TO LIVE HERE - alternating tinted rects behind the
# duration chart, one per order of magnitude, drawn to make the compression of a
# log axis visible. The client asked for a plain background with a dotted line on
# each unit break instead, which the axis draws itself (see the gridlines in
# fw_chart_duration()), so the shapes and their two FW_CHART entries are gone.
#
# THE WARNING IT CARRIED IS WORTH KEEPING AND HAS MOVED to the xaxis comment in
# fw_chart_duration(): on one log axis, layout.xaxis.range is in log10 and
# tickvals and shape coordinates are in data units. Every edge here once went
# through log10() on the reasoning that a log axis takes log coordinates, and the
# first band landed at ten to the zeroth of a day - about six thousand pixels off
# the left of the canvas, tinting everything left of the data. If shapes ever
# come back to this chart, they take raw days.

#' The x bounds of the duration chart, as the log10 values a log axis wants
#'
#' AN EXPLICIT RANGE, BECAUSE THE AUTOMATIC ONE IS UNUSABLE HERE. plotly pads
#' the autorange of a BOX trace to leave room for the boxes, and on a log axis it
#' does that arithmetic in the wrong space: seven boxes over durations of one day
#' to twenty years came out as a range of 10^-67.5 to 10^8.1. The data then
#' occupied about a twentieth of the width at the right-hand edge, all six named
#' ticks piled up on top of one another under it, and the rest of the chart was
#' one large empty panel. That is what the client saw.
#'
#' The pad is a fraction of the span rather than a fixed number of decades, so a
#' selection spanning one order of magnitude is not given four.
fw_duration_range <- function(days) {
  lx <- log10(range(days))
  pad <- max(FW_CHART$duration_pad_min, diff(lx) * FW_CHART$duration_pad)
  c(lx[1] - pad, lx[2] + pad)
}

#' The named ticks for a selection, reaching as far as its longest attempt
#'
#' THE AXIS HAS TO LABEL THE LAST DOT (client, 23 Sept 2026). The six fixed
#' breaks in FW_CHART$duration_ticks stop at ten years, so a selection holding a
#' twenty-seven-year attempt drew dots well past the final label and left the
#' reader with nothing to measure them against. A seventh tick is added at the
#' longest duration in the selection, named in whichever unit reads plainly at
#' that length.
#'
#' A FIXED TICK TOO CLOSE TO IT IS DROPPED. "10 years" and "11 years" a few
#' pixels apart overprint each other and say nothing the second does not, so a
#' fixed tick within FW_CHART$duration_tick_gap of the terminal one (in log10
#' space, the space the axis is actually spaced in) gives way to it. The
#' terminal tick always wins, because it is the one carrying new information.
#'
#' Ticks past the maximum are dropped outright - they would sit in the padding
#' beyond the data, labelling empty axis.
#'
#' @param days every duration in the selection, all > 0
#' @return list(vals = tick positions in DAYS, text = their labels). Days, not
#'   log10: plotly logs tickvals itself. See the long note at the call site.
fw_duration_ticks <- function(days) {
  mx <- max(days)
  vals <- FW_CHART$duration_ticks
  text <- fw_t("charts", "duration_ticks")

  keep <- vals <= mx
  vals <- vals[keep]
  text <- text[keep]

  # The longest attempt already sits on a named break - nothing to add.
  if (length(vals) && isTRUE(all.equal(vals[length(vals)], mx))) {
    return(list(vals = vals, text = text))
  }

  # Name it in the largest unit that leaves a whole number above one, so a
  # nine-month attempt is "9 months" rather than "0.7 years".
  label <- if (mx >= 365) {
    fw_fill(fw_t("charts", "duration_max_years"), n = round(mx / 365))
  } else if (mx >= 30) {
    fw_fill(fw_t("charts", "duration_max_months"), n = round(mx / 30))
  } else {
    fw_fill(fw_t("charts", "duration_max_days"), n = round(mx))
  }

  crowded <- length(vals) > 0 &&
    (log10(mx) - log10(vals[length(vals)])) < FW_CHART$duration_tick_gap
  if (crowded) {
    vals <- vals[-length(vals)]
    text <- text[-length(text)]
  }

  list(vals = c(vals, mx), text = c(text, label))
}

#' Vertical offsets that spread the duration chart's dots across their row
#'
#' A BEESWARM, CHEAPLY. Every dot used to sit on its method's centre line, so
#' attempts with the same duration - and a great many were entered as exactly
#' a year - were one dot drawn on top of another, and a row of forty looked like
#' a row of six. Dots that land within FW_CHART$duration_swarm$bin of each
#' other (log10 days) now fan out from the centre, 0, +1, -1, +2, -2 steps, so
#' a pile of equal durations becomes a column whose height is its count. A pile
#' too tall for the row is squeezed to fit rather than allowed into the next.
#' Deterministic: the same selection always draws the same picture.
#'
#' @param days durations in days, all > 0.
#' @param row the row each dot belongs to.
#' @return an offset per dot, in rows, within +/- FW_CHART$duration_swarm$spread.
fw_duration_swarm <- function(days, row) {
  cfg <- FW_CHART$duration_swarm
  bin <- floor(log10(days) / cfg$bin)
  out <- numeric(length(days))
  for (idx in split(seq_along(days), list(row, bin), drop = TRUE)) {
    m <- length(idx)
    if (m < 2) next
    half <- ceiling((m - 1) / 2)
    step <- min(cfg$step, cfg$spread / half)
    k <- seq_len(m) - 1L
    out[idx] <- step * ceiling(k / 2) * ifelse(k %% 2 == 1, 1, -1)
  }
  out
}

#' The rows the duration chart draws, one per dot, with its row and offset
#'
#' Shared by the plotly chart and the PDF's static twin, so the swarm is the
#' same picture in both. See fw_method_data() for why this is split out.
#'
#' @return list(d = one row per attempt with row and y_dot, order_lv = the row
#'   labels bottom to top), or NULL when fewer than two dots would be drawn
fw_duration_data <- function(data, sel) {
  sel <- fw_duration_sel(data, sel)
  if (!nrow(sel)) return(NULL)
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
  # A NUMERIC ROW PER METHOD, not a category axis: a category can only be hit
  # dead centre, and the dots need to sit either side of it. The names come
  # back as the y axis's tick text in fw_chart_duration().
  d <- d |> arrange(method_label, duration_days, attempt_id)
  d$row <- match(d$method_label, order_lv)
  d$y_dot <- d$row + fw_duration_swarm(d$duration_days, d$row)

  list(d = d, order_lv = order_lv)
}

fw_chart_duration <- function(data, sel) {
  dd <- fw_duration_data(data, sel)
  if (is.null(dd)) return(NULL)
  d <- dd$d
  order_lv <- dd$order_lv

  ticks <- fw_duration_ticks(d$duration_days)

  p <- plotly::plot_ly(height = fw_chart_height("duration", length(order_lv)))
  p <- plotly::add_trace(
    p, data = d, type = "box", orientation = "h",
    x = ~duration_days, y = ~row, width = 2 * FW_CHART$duration_swarm$spread,
    name = "", showlegend = FALSE, hoverinfo = "x",
    # Interface colours, deliberately. The box is chrome rather than data - the
    # outcome markers on top of it carry the encoding - so it is the only chart
    # element drawn from the brand palette rather than the data palette.
    fillcolor = fw_rgba(FW_COLOURS$teal_tint, 0.45),
    line = list(color = FW_COLOURS$teal_text, width = FW_CHART$box_line),
    boxpoints = FALSE
  )
  # THE DOTS ARE PICTURE, NOT CONTROLS (Sept 2026 user testing): no hover,
  # so nothing on them invites a click. The box under them still answers a
  # hover with its median and quartiles.
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "scatter", mode = "markers",
      x = ~duration_days, y = ~y_dot,
      name = o, hoverinfo = "skip",
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    size = FW_CHART$point$size,
                    opacity = FW_CHART$point$opacity,
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$point$stroke))
    )
  }

  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = "fwise-duration") |>
    plotly::layout(
      boxmode = "group",
      # ROOM FOR THE TERMINAL TICK'S LABEL, which sits at the longest attempt
      # in the selection and so at the right-hand end of the axis. The shared
      # style leaves 8px there, which "20 years" centred on that tick overruns.
      margin = list(l = 8, r = 44, t = fw_legend_margin(FW_OUTCOME_LEVELS), b = 52),
      xaxis = list(
        # NO TITLE (client, 23 Sept 2026). The named ticks below say what the
        # axis measures, and the title under them said it a second time.
        title = "",
        type = "log", zeroline = FALSE,
        # A DOTTED LINE ON EACH UNIT BREAK, ON A PLAIN GROUND, and that is the
        # client's instruction. This chart used to carry alternating tinted
        # bands behind it - see the note where fw_duration_bands() was - and the
        # breaks are now drawn by the axis's own gridlines instead: dotted, one
        # per named tick, nothing else behind the data.
        #
        # THE GRIDLINES LAND ON THE BREAKS BECAUSE THE TICKS DO. tickmode is
        # "array" below, so plotly draws a gridline at each of tickvals and
        # nowhere else - a day, a week, a month, a year, five years, ten - which
        # is exactly the set of unit breaks asked for. Leave tickmode alone and
        # the axis reverts to decades and the breaks stop being units.
        #
        # INK, NOT THE BORDER GREY, and heavier than a hairline. They were
        # $fw-border at 1px and the client could not see them - a dotted line
        # is mostly gaps, so it needs more weight and contrast than a solid rule
        # to read as a line at all. FW_COLOURS$ink is the off-black the text is
        # set in, so the breaks read as part of the axis rather than as a
        # second colour.
        showgrid = TRUE, griddash = "dot",
        gridcolor = FW_COLOURS$ink, gridwidth = FW_CHART$duration_grid,
        # EXPLICIT, and not a preference - see fw_duration_range() for what
        # plotly's own autorange does to a horizontal box trace on a log axis.
        range = fw_duration_range(d$duration_days),
        # Named ticks, because 10^3 means nothing to a practitioner deciding
        # whether they can commit a season or a decade.
        #
        # RAW DAYS HERE, NOT log10 - and this is the asymmetry that catches
        # everyone who edits this chart, including whoever reads this next. On
        # a log axis plotly wants layout COORDINATES in log10 (the range above
        # goes through log10(), and so did every shape back when this chart had
        # any) and tickvals in DATA units, which it logs itself. Wrapping these
        # in log10() looks like the consistent thing to do and is not: the ticks
        # come out log-logged, bunched into the left tenth of the axis, and
        # "1 day" disappears entirely because log10(0) is -Inf. The gridlines
        # ride on these values, so getting them wrong loses the unit breaks too.
        tickmode = "array",
        # THE SELECTION'S OWN TICKS, not the fixed six: the last one lands on
        # the longest attempt drawn, so no dot sits past the final label. See
        # fw_duration_ticks() - and note it returns DAYS, per the note above.
        tickvals = ticks$vals,
        ticktext = ticks$text
      ),
      # NO HORIZONTAL RULES. "Plain background" means the vertical unit breaks
      # and nothing else; a y gridline here would be a line through the middle
      # of every box rather than a reference of any kind, since this axis is
      # method names.
      yaxis = list(title = "", automargin = TRUE, showgrid = FALSE,
                   zeroline = FALSE, tickmode = "array",
                   tickvals = seq_along(order_lv), ticktext = order_lv,
                   range = c(0.5, length(order_lv) + 0.5))
    )
}

# ---- Waterbody and species ---------------------------------------------------

#' A horizontal bar of counts by category, stacked by outcome
#'
#' The workhorse behind the waterbody, reason and species charts. One function
#' rather than three near-identical ones, so they cannot drift into three
#' different looks for the same shape of question.
#'
#' @param d      a frame with `category` and `outcome`
#' @param limit  keep the top n categories and gather the rest into "Other"
#' @param filename what a PNG export is called. Each caller passes its own,
#'   because this one builder draws waterbodies and species. The mode
#'   is appended to it, so the two views do not export over each other.
#' @param mode "count" for absolute stacked, "share" for 100% stacked. The
#'   denominator is the CATEGORY's own total, so share answers "within this kind
#'   of thing, how did it go" - the same question, and the same arithmetic, as
#'   the mode on fw_chart_method(). It is not each category's share of the
#'   selection.
#'
#' fw_category_data() is the counting half, shared with the PDF's static twin
#' (fw_gg_category() in R/charts_static.R); fw_chart_category() draws it.
fw_category_data <- function(d, limit = NA_integer_, mode = c("count", "share")) {
  mode <- match.arg(mode)
  if (!nrow(d)) return(NULL)
  d$outcome <- as.character(fw_outcome_factor(d$outcome))
  other <- fw_t("charts", "other")

  totals <- d |> count(category, name = "total") |> arrange(desc(total))
  if (!is.na(limit) && nrow(totals) > limit) {
    keep <- totals$category[seq_len(limit)]
    # "Other" is a real bar, not a dropped remainder. A reader has to be able to
    # see how much of the picture the named categories actually cover.
    d$category <- ifelse(d$category %in% keep, d$category, other)
    totals <- d |> count(category, name = "total")
  }
  # Ascending, because plotly draws the first category at the bottom.
  totals <- totals |>
    mutate(is_other = category == other) |>
    arrange(desc(is_other), total)

  d <- d |>
    count(category, outcome, name = "n") |>
    left_join(select(totals, category, total), by = "category") |>
    mutate(share = 100 * n / total,
           value = if (mode == "share") share else n,
           label = paste0(category, "  (", total, ")"))
  order_lv <- paste0(totals$category, "  (", totals$total, ")")

  list(d = d, order_lv = order_lv)
}

fw_chart_category <- function(d, limit = NA_integer_,
                              filename = "fwise-categories",
                              mode = c("count", "share")) {
  mode <- match.arg(mode)
  cd <- fw_category_data(d, limit, mode)
  if (is.null(cd)) return(NULL)
  d <- cd$d
  order_lv <- cd$order_lv
  font <- fw_plot_font()

  p <- plotly::plot_ly(height = fw_chart_height("category", length(order_lv)))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      # The value inside the segment, following the mode, as fw_chart_method()
      # does and for the same reasons. This chart had none in either mode until
      # the client caught its 100% view with no numbers on it (21 Sept 2026).
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      insidetextfont = list(color = unname(FW_OUTCOME_LABEL_INK[[o]]),
                            family = font$family, size = font$size),
      # ITS OWN COPY OF THE COUNT, not %{x}. The hover used to read the drawn
      # value, which is right up until the bar is a 100% stack and the reader is
      # told "Successful: 33.33333". Same fix, and the same reasoning, as
      # fw_chart_method(): the hover carries the raw count in BOTH modes, so a
      # share never hides how much evidence is behind it.
      hovertemplate = paste0("%{y}<br>", o, ": %{customdata}<extra></extra>"),
      customdata = ~paste0(n, fw_t("charts", "hover_of"), total)
    )
  }

  x_axis <- if (mode == "share") {
    list(title = fw_t("charts", "x_share"), range = c(0, 100), ticksuffix = "%",
         zeroline = FALSE, gridcolor = FW_COLOURS$border)
  } else {
    list(title = fw_t("charts", "x_attempts"), zeroline = FALSE,
         gridcolor = FW_COLOURS$border)
  }

  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = paste0(filename, "-", mode)) |>
    plotly::layout(
      barmode = "stack",
      # The 1rem floor: a segment too narrow for it shows no number rather
      # than a shrunken one. The hover still has it.
      uniformtext = list(minsize = FW_TYPE$floor_px, mode = "hide"),
      xaxis = x_axis,
      yaxis = list(title = "", automargin = TRUE)
    )
}

#' Attempts by kind of waterbody
#'
#' The specific type rather than the still/flowing split: "Lake" and "Pond"
#' behave differently enough that collapsing them loses the useful part, and the
#' regime is one filter away in the sidebar.
#'
#' @param mode "count" for attempts, "share" for the outcome mix in each kind of
#'   water as a 100% bar. The segments carry counts or percentages to match;
#'   the bar labels keep their totals in both modes, so the evidence behind a
#'   share is never off the chart.
fw_chart_waterbody <- function(sel, mode = c("count", "share")) {
  mode <- match.arg(mode)
  fw_chart_category(fw_waterbody_rows(sel), limit = FW_TOP_N,
                    filename = "fwise-waterbody-types", mode = mode)
}

#' The kind-of-water chart's input: one row per attempt with a waterbody
fw_waterbody_rows <- function(sel) {
  sel |>
    filter(!is.na(waterbody_type)) |>
    transmute(category = waterbody_type, outcome)
}

#' One row per (attempt, species) for a role, labelled and with its outcome
#'
#' DEDUPED PER ATTEMPT. An attempt listing a species twice must count once, or a
#' messily recorded row quietly inflates its species up the ranking.
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
fw_species_top_n <- function(data, sel, role_name, limit = FW_TOP_N) {
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
