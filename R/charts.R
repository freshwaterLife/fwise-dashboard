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

  # THE DONUTS PUT THEIR KEY BESIDE THEM, and the bar charts do not, because
  # the two have different things above and below the plot. A bar chart has an
  # axis and an axis title on both sides and nowhere sideways to go without
  # squeezing the bars; a donut is a circle in a rectangle with half its width
  # already empty, so the key costs it nothing and a seven-entry key above a
  # small circle takes more vertical room than the chart. The pie's own domain
  # is narrowed to match in fw_chart_donut() - plotly does not reserve the
  # space for a legend at x > 1 on its own.
  side <- identical(legend_side, "right")

  legend_layout <- if (side) {
    list(orientation = "v", x = FW_CHART$donut_legend_x, xanchor = "left",
         y = 0.5, yanchor = "middle",
         traceorder = "normal", font = fw_plot_font())
  } else {
    list(orientation = "h", y = 1, yanchor = "bottom", x = 0,
         traceorder = "normal", font = fw_plot_font())
  }

  plotly::layout(
    p,
    font = fw_plot_font(),
    # Transparent, so a chart takes the surface it sits on.
    paper_bgcolor = FW_TRANSPARENT,
    plot_bgcolor  = FW_TRANSPARENT,
    # THE LEGEND SITS ABOVE THE PLOT, not below it. Underneath, plotly places it
    # in paper coordinates at a fixed offset and it lands on top of the x-axis
    # title, which is where the units are - so the reader loses the label that
    # says what they are looking at. Above, it has the margin to itself - but
    # only as much of it as was reserved, which is why the top margin is
    # computed rather than fixed.
    #
    # A SIDE LEGEND NEEDS NEITHER. It is inside the paper, beside a pie that has
    # been narrowed to leave room for it, so there is no top margin to reserve
    # and no axis title underneath to clear.
    margin = if (side) {
      list(l = 8, r = 8, t = 8, b = 8)
    } else {
      list(l = 8, r = 8,
           t = if (legend) fw_legend_margin(legend_labels) else 8,
           b = 52)
    },
    hoverlabel = list(font = fw_plot_font()),
    # THE CAMERA ICON'S COLOUR HAS TO BE SET HERE, and a CSS rule will not do
    # it. plotly picks the modebar's colour from paper_bgcolor, and ours is
    # transparent (FW_TRANSPARENT) - which it reads as a dark ground and answers
    # with a near-white icon at 30% opacity. On our light surface that is an
    # empty white pill with nothing visible in it, which is what the client was
    # looking at when they reported the download button as not visible. The
    # colour is written onto the path as an inline attribute, so it beats
    # anything .modebar-btn can say from the stylesheet - hence the colours live
    # here and _components.scss only gives the bar its surface.
    modebar = list(
      bgcolor = FW_TRANSPARENT,
      color = FW_COLOURS$ink_muted,
      activecolor = FW_COLOURS$teal_text
    ),
    showlegend = legend,
    # traceorder IS NOT REDUNDANT. plotly.js flips its default to "reversed" as
    # soon as a chart has stacked bars or a filled area, which is every chart
    # here except the box plot - so the key read Unknown first and Successful
    # last while the traces were added Successful first. Pinning it makes the
    # key agree with FW_OUTCOME_LEVELS, which is the one source of that order.
    #
    # font IS NOT REDUNDANT EITHER. plotly.js does not reliably inherit
    # layout.font into legend entries, so the key was rendering below the
    # client's 1rem floor while every other label on the chart honoured it.
    legend = legend_layout
  ) |>
    # DOWNLOADABLE, AND NOTHING ELSE. The modebar used to be off entirely
    # (displayModeBar = FALSE), which also took away the one button on it worth
    # having: the client asked for every plot to be saveable as a PNG the way
    # plotly does it by default. So the bar comes back with the camera and the
    # camera alone - zoom, pan, lasso and select are removed rather than left to
    # be discovered, because none of these charts is a canvas the reader is
    # meant to navigate, and a half-zoomed axis is a way to misread one.
    #
    # scale = 2 because the paper is transparent and the type is at the 1rem
    # floor: a 1x export of this is soft the moment it lands in a slide.
    #
    # This travels into the downloaded HTML report too - the report embeds these
    # same widgets (see fw_html_figure() in R/report_html.R) - so its charts
    # become saveable as well. The print block in _report_frame.scss takes the
    # bar off the page, which matters more now that it is always on screen.
    plotly::config(
      responsive = TRUE,
      displaylogo = FALSE,
      # AN ALLOW LIST, NOT A DENY LIST. modeBarButtonsToRemove was the obvious
      # way to write this and it does not hold: plotly adds buttons of its own
      # accord depending on the chart - setting hovermode on the cumulative
      # chart brings in a hover toggle that is not either of the
      # hoverClosest/hoverCompare pair and does not come off by name - so a deny
      # list quietly grows a button every time a chart option changes. Naming
      # the one button we want is the only version that stays true.
      #
      # The nesting is plotly's: the outer list is groups, the inner is the
      # buttons in a group. One of each.
      modeBarButtons = list(list("toImage")),
      # ALWAYS ON, NOT ON HOVER. plotly's default is displayModeBar = "hover",
      # which fades the one button we keep to nothing until the pointer is over
      # the chart - so on a page of charts the way to save a PNG was invisible
      # until you happened to find it, and on touch there is no hover to find it
      # with. The client reported the button as not visible enough; this is the
      # half of the fix that makes it present at all. The rest of it - a surface
      # and a border, so it reads as a control over a transparent chart - is in
      # .modebar-group in _components.scss.
      displayModeBar = TRUE,
      toImageButtonOptions = list(format = "png", scale = 2,
                                  filename = filename)
    )
}

#' A chart, or a sentence saying there is none
#'
#' Every builder below returns NULL when the selection gives it nothing to
#' draw, and renderPlotly(NULL) leaves a blank space under the block's heading -
#' which reads as a chart that failed to load. Shiny's validation message lands
#' in the output's own slot, so this needs no second output and no second id.
#' The HTML report does not use it: fw_html_figure() skips a NULL widget,
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
#'
#' Stacked rather than a single line, at the client's request: the growth of the
#' record and the mix of what came of it are the same question, and a single
#' line answers only half of it.
#'
#' LIVES ON THE DASHBOARD, NOT THE REPORT BUILDER. It answers how the database
#' has grown, which is a question about the record rather than about a reader's
#' own situation, and on a narrow selection it was actively misleading.
#'
#' SMOOTHED, at the client's request. It was step interpolation ("hv") on the
#' reasoning that an attempt joins the total on its start year rather than
#' easing in across the gap - true, but over ninety mostly-sparse years it drew
#' a staircase that read as noise. "linear" and NOT "spline": the series is
#' cumulative and therefore never decreases, and a spline overshoots between
#' knots, so it would draw a band dipping below a total the record had already
#' reached.
#'
#' THE AXIS RUNS TO THIS YEAR, not to the last year with an attempt in it. The
#' chart answers "how has the record grown", and an axis that stops at the most
#' recent attempt quietly redraws itself every time one lands - and, worse,
#' leaves a reader to assume the last point is the present. The completion grid
#' is extended to the current year with it, so the bands carry flat to the right
#' edge: cumsum() over zero-count years is the honest reading, because the total
#' genuinely has not changed since the last recorded attempt.
#'
#' HOVER IS UNIFIED, at the client's request: one box listing all four outcomes
#' at the year under the pointer, rather than whichever single band happens to
#' be nearest. Set on THIS chart and not in fw_plotly_style(), which is shared
#' with the donuts and the horizontal bars where an x-unified hover is wrong.
fw_chart_cumulative <- function(sel) {
  y <- sel[!is.na(sel$start_year), c("start_year", "outcome")]
  if (!nrow(y)) return(NULL)

  # max() of the two, not the current year outright: a selection can hold a
  # start year in the future (FW_YEAR_FUTURE allows a planned attempt), and
  # truncating the axis would cut a band off mid-flight.
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
      # The line is the same colour as its fill, so the band reads as one shape
      # rather than as an outlined one.
      stackgroup = "one",
      line = list(shape = "linear", width = FW_CHART$line,
                  color = unname(FW_OUTCOME_COLOURS[[o]])),
      fillcolor = unname(FW_OUTCOME_COLOURS[[o]]),
      # NO YEAR IN THE TEMPLATE. Unified hover prints the x value once in its
      # own header; repeating it on all four rows is what it looked like before.
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

# ---- The proportion donuts, both of which are gone ---------------------------
#
# THERE WERE TWO RINGS HERE and the client removed them one at a time.
#
# The outcome donut went first - "what came of these attempts", one slice per
# level of FW_OUTCOME_LEVELS. The outcome split is already the segmentation of
# every stacked bar in the app and the colour of every marker on the map, so the
# ring was a fourth telling of it and the one that carried the least.
#
# The method donut followed, for the same reason and with the same argument made
# out loud: the stacked bars of outcome-by-method say everything the ring said
# about the method mix AND say what happened to each method, so the ring was the
# weaker of two tellings. fw_chart_method() is what the dashboard draws in its
# place.
#
# WHAT THE METHOD RING COUNTED IS NOT WHAT REPLACED IT COUNTS. The ring's
# denominator was USES - an attempt using three methods put three slices on it -
# and fw_chart_method() counts ATTEMPTS, once under each of its methods. The
# totals differ, the copy under the chart says which is which, and anyone
# comparing a screenshot of the old ring to the new bars needs to know that.
#
# None of the counts are lost: fw_outcome_counts() still feeds the report
# builder's summary (R/mod_plan_results.R) and the dashboard's summary strip.
# fw_chart_donut() was generic - a data.frame of label and n - so if a ring is
# ever wanted again it is a small function, not a recovery job.

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
    distinct(attempt_id, method_name, method_id) |>
    left_join(select(sel, attempt_id, outcome), by = "attempt_id") |>
    mutate(outcome = ifelse(is.na(outcome), "Unknown", outcome))
  if (nrow(me) == 0) return(NULL)

  # THE TWO "OTHER" METHODS SIT AT THE BOTTOM, whatever their counts. This is
  # the client's standard and it is the same rule fw_chart_category() applies to
  # its own "Other" bar: an "other" bucket is not a method, it is the remainder
  # of a list, so ranking it against real methods invites a reader to compare
  # the two. Everything else is still ordered by frequency, ascending, because
  # plotly draws the first category at the bottom.
  #
  # ON method_id, NOT THE DISPLAY NAME. FW_METHODS in config.R is what makes
  # ME05/ME06 "Other chemical"/"Other mechanical", and renaming either there
  # must not quietly unpin it here.
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

  order_lv <- paste0(totals$method_name, "  (", totals$total, ")")
  # Grows with the number of methods, so eight methods are not crushed into the
  # space two would use.
  font <- fw_plot_font()
  p <- plotly::plot_ly(height = fw_chart_height("method", nrow(totals)))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(method_label, levels = order_lv), x = ~value, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      # The value inside the segment. Colour alone never carries it.
      #
      # IT FOLLOWS THE MODE. It used to print the count in both modes, so a 100%
      # stacked bar carried raw counts that summed to the method's total rather
      # than to the 100% the axis promised - the client caught it on a call, and
      # a reader who trusted the numbers over the axis would have read the chart
      # backwards. The floor that blanks a label is still a share either way:
      # what makes a label unreadable is how narrow the segment is, not which
      # number is in it.
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      # Indigo, not white. See FW_OUTCOME_LABEL_INK in config.R: none of the
      # four Wong fills is dark enough to carry white numerals.
      insidetextfont = list(color = unname(FW_OUTCOME_LABEL_INK[[o]]),
                            family = font$family, size = font$size),
      # THE HOVER HAS ITS OWN COPY OF THE COUNT. It used to read %{text}, which
      # is the in-bar label above - and that label is blanked under the share
      # floor, so exactly the segments a reader hovers to find out about were
      # the ones that showed "Unknown:  of 567". customdata is never blanked.
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
      # Stops plotly shrinking the in-bar counts to illegibility on a narrow
      # segment; below the floor it hides them instead, which is honest.
      # The same 1rem floor. mode = "hide" drops a label rather than
      # shrinking it, so a segment too narrow for the floor shows no number
      # instead of an unreadable one - the hover still has it.
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

fw_chart_duration <- function(data, sel) {
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

  p <- plotly::plot_ly(height = fw_chart_height("duration", nrow(totals)))
  p <- plotly::add_trace(
    p, data = d, type = "box", orientation = "h",
    x = ~duration_days, y = ~factor(method_label, levels = order_lv),
    name = "", showlegend = FALSE, hoverinfo = "x",
    # Interface colours, deliberately. The box is chrome rather than data - the
    # outcome markers on top of it carry the encoding - so it is the only chart
    # element drawn from the brand palette rather than the data palette.
    fillcolor = fw_rgba(FW_COLOURS$teal_tint, 0.45),
    line = list(color = FW_COLOURS$teal_text, width = FW_CHART$box_line),
    boxpoints = FALSE
  )
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "scatter", mode = "markers",
      x = ~duration_days, y = ~factor(method_label, levels = order_lv),
      name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    size = FW_CHART$point$size,
                    opacity = FW_CHART$point$opacity,
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$point$stroke)),
      hovertemplate = paste0("%{y}<br>", o, ": %{x:,.0f}",
                             fw_t("charts", "hover_days"), "<extra></extra>")
    )
  }

  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = "fwise-duration") |>
    plotly::layout(
      boxmode = "group",
      xaxis = list(
        title = fw_t("charts", "x_duration"),
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
        tickvals = FW_CHART$duration_ticks,
        ticktext = fw_t("charts", "duration_ticks")
      ),
      # NO HORIZONTAL RULES. "Plain background" means the vertical unit breaks
      # and nothing else; a y gridline here would be a line through the middle
      # of every box rather than a reference of any kind, since this axis is
      # method names.
      yaxis = list(title = "", automargin = TRUE, showgrid = FALSE)
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
#' @param filename what a PNG export is called. Each caller passes its own,
#'   because this one builder draws waterbodies, drivers and species.
fw_chart_category <- function(d, title, limit = NA_integer_,
                              filename = "fwise-categories") {
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
    mutate(label = paste0(category, "  (", total, ")"))
  order_lv <- paste0(totals$category, "  (", totals$total, ")")

  p <- plotly::plot_ly(height = fw_chart_height("category", nrow(totals)))
  for (o in FW_OUTCOME_LEVELS) {
    dd <- d[d$outcome == o, ]
    if (!nrow(dd)) next
    p <- plotly::add_trace(
      p, data = dd, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~n, name = o,
      marker = list(color = unname(FW_OUTCOME_COLOURS[[o]]),
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_outcome)),
      hovertemplate = paste0("%{y}<br>", o, ": %{x}<extra></extra>")
    )
  }
  fw_plotly_style(p, legend_labels = FW_OUTCOME_LEVELS,
                  filename = filename) |>
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = title, zeroline = FALSE, gridcolor = FW_COLOURS$border),
      yaxis = list(title = "", automargin = TRUE)
    )
}

#' Attempts by kind of waterbody
#'
#' The specific type rather than the still/flowing split: "Lake" and "Pond"
#' behave differently enough that collapsing them loses the useful part, and the
#' regime is one filter away in the sidebar.
fw_chart_waterbody <- function(sel) {
  d <- sel |>
    filter(!is.na(waterbody_type)) |>
    transmute(category = waterbody_type, outcome)
  fw_chart_category(d, fw_t("charts", "x_attempts"), limit = FW_TOP_N,
                    filename = "fwise-waterbody-types")
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

# ---- Methods against waterbody -----------------------------------------------

#' Which methods get used in which kind of water
#'
#' THE ONE CHART SEGMENTED BY METHOD RATHER THAN OUTCOME. It answers a question
#' the outcome charts cannot: standing at a lake, what have people actually
#' reached for? "How the methods compare" says how each method fared overall,
#' which is not the same thing - draining a pond and draining a river are one
#' method and two different propositions.
#'
#' Colour comes from FW_METHOD_COLOURS: seven distinct hues, because method is a
#' nominal category and the ramp that used to be here implied an order the data
#' does not have. See the long note in config.R for what was measured and why
#' the order of that vector must not be changed casually.
#'
#' THE COUNTS INSIDE THE SEGMENTS ARE NOT DECORATION. Seven categories is past
#' the point where colour alone can separate every possible pair, so the number
#' in the segment and the white rule between segments are the second and third
#' encodings. Do not remove either to tidy the chart up.
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
  other <- fw_t("charts", "other")
  wb <- d |> count(waterbody_type, name = "total") |> arrange(desc(total))
  if (nrow(wb) > FW_TOP_N) {
    keep <- wb$waterbody_type[seq_len(FW_TOP_N)]
    d$waterbody_type <- ifelse(d$waterbody_type %in% keep, d$waterbody_type,
                               other)
  }

  totals <- d |>
    count(waterbody_type, name = "total") |>
    mutate(is_other = waterbody_type == other) |>
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

  font <- fw_plot_font()
  p <- plotly::plot_ly(height = fw_chart_height("method_waterbody", nrow(totals)))
  for (m in method_ids) {
    seg <- dd[dd$method_id == m, ]
    if (!nrow(seg)) next
    p <- plotly::add_trace(
      p, data = seg, type = "bar", orientation = "h",
      y = ~factor(label, levels = order_lv), x = ~value,
      name = seg$method_name[1],
      marker = list(color = unname(FW_METHOD_COLOURS[[m]]),
                    # A hairline, kept on purpose: it is the separator that
                    # keeps two segments readable as two when their fills are
                    # the closest pair in the palette. See FW_CHART in config.R.
                    line = list(color = FW_COLOURS$surface,
                                width = FW_CHART$separator_method)),
      # The threshold is on SHARE, so the label only appears where the segment
      # is actually wide enough to hold it, whichever mode the chart is in - but
      # the NUMBER follows the mode, the same fix as fw_chart_method(). Printing
      # a count inside a 100% stacked bar contradicts the axis above it.
      text = ~ifelse(share < FW_CHART$label_min_share, "",
                     if (mode == "share") paste0(round(share), "%") else as.character(n)),
      textposition = "inside",
      # PER METHOD, not white throughout. Three of the seven fills are light
      # enough that white numerals on them fall under 4.5:1. See
      # FW_METHOD_LABEL_INK in config.R.
      insidetextfont = list(color = unname(FW_METHOD_LABEL_INK[[m]]),
                            family = font$family, size = font$size),
      # Own copy of the count, not %{text}: see fw_chart_method().
      hovertemplate = paste0("%{y}<br>", seg$method_name[1],
                             ": %{customdata}<extra></extra>"),
      customdata = ~paste0(n, fw_t("charts", "hover_of"), total)
    )
  }

  x_axis <- if (mode == "share") {
    list(title = fw_t("charts", "x_share_uses"), range = c(0, 100),
         ticksuffix = "%", zeroline = FALSE, gridcolor = FW_COLOURS$border)
  } else {
    list(title = fw_t("charts", "x_times_used"), zeroline = FALSE,
         gridcolor = FW_COLOURS$border)
  }

  # The key carries seven method names, which is the chart that made the old
  # fixed top margin overlap the bars. Names in trace order, so the reserved
  # space matches the key that is actually drawn.
  legend_labels <- vapply(method_ids,
                          function(m) dd$method_name[dd$method_id == m][1],
                          character(1))
  fw_plotly_style(p, legend_labels = unname(legend_labels),
                  filename = paste0("fwise-method-waterbody-", mode)) |>
    plotly::layout(
      barmode = "stack",
      # The same 1rem floor. mode = "hide" drops a label rather than
      # shrinking it, so a segment too narrow for the floor shows no number
      # instead of an unreadable one - the hover still has it.
      uniformtext = list(minsize = FW_TYPE$floor_px, mode = "hide"),
      xaxis = x_axis,
      yaxis = list(title = "", automargin = TRUE)
    )
}
