# charts_static.R
# The PDF report's figures: static ggplot twins of the page's plotly charts, and
# a static map. Written to PNG at print resolution by fw_gg_png() and placed by
# R/report_pdf.R.
#
# TWINS, NOT A SECOND IMPLEMENTATION OF THE NUMBERS. Each chart here draws from
# the same counting function its plotly original does - fw_method_data(),
# fw_category_data(), fw_method_waterbody_data(), fw_duration_data(), all in
# R/charts.R - so the PDF and the page cannot disagree about a single bar.
# Only the drawing is written twice, because plotly cannot be rendered to a
# static image on this deployment (that needs kaleido, which needs Python).
# dev/value_test.R checks each twin's drawn totals against the plotly traces.
#
# DRAWN AT PRINTED SIZE. Every figure is sized in millimetres to the column it
# is placed in (FW_PDF$text_width_mm), so a label set at FW_PRINT$floor prints
# at that size - the client's text floor holds on paper as it does on screen.
#
# NO WHITE. The plot background is transparent, so a figure takes the page's
# own tint, and the only fills are the data palettes and the brand tokens.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# ---- Type and theme ----------------------------------------------------------

#' Make Ubuntu available to ggplot, once per process
#'
#' Registered from the TTFs the PDF template uses (resources/report/fonts/), so
#' the charts and the text around them are one typeface whether or not the
#' machine has Ubuntu installed. Connect Cloud does not.
fw_gg_fonts <- local({
  done <- FALSE
  function(dir = file.path("resources", "report", "fonts")) {
    if (done) return(invisible(TRUE))
    systemfonts::register_font(
      name = "FWISE Ubuntu",
      plain = file.path(dir, "Ubuntu-Regular.ttf"),
      bold = file.path(dir, "Ubuntu-Bold.ttf"),
      italic = file.path(dir, "Ubuntu-Regular.ttf"),
      bolditalic = file.path(dir, "Ubuntu-Bold.ttf")
    )
    done <<- TRUE
    invisible(TRUE)
  }
})

FW_GG_FAMILY <- "FWISE Ubuntu"

#' The one theme every static figure shares
#'
#' @param axis_key the chart's name in FW_CHART$axis$styled, so the A/B axis
#'   styling reaches the PDF exactly as it reaches the page.
fw_gg_theme <- function(axis_key = "") {
  fw_gg_fonts()
  pt <- FW_PRINT$floor
  ink <- FW_COLOURS$ink
  styled <- axis_key %in% FW_CHART$axis$styled

  t <- theme_minimal(base_family = FW_GG_FAMILY, base_size = pt) +
    theme(
      text = element_text(colour = ink, size = pt),
      axis.text = element_text(colour = ink, size = pt),
      axis.title = element_text(colour = ink, size = pt),
      legend.text = element_text(colour = ink, size = pt),
      legend.title = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.location = "plot",
      legend.margin = margin(0, 0, 2, 0),
      legend.key.size = unit(pt * 0.9, "pt"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = FW_COLOURS$border, linewidth = 0.3),
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.margin = margin(2, 6, 2, 2)
    )
  if (styled) {
    t <- t + theme(
      # ggplot's linewidth is in units of about 0.75pt, so the page's 1.5px
      # line (FW_CHART$axis$line) prints at roughly the same weight.
      axis.line = element_line(colour = ink, linewidth = FW_CHART$axis$line / 3),
      axis.ticks = element_line(colour = ink, linewidth = 0.35),
      axis.ticks.length = unit(FW_CHART$axis$tick_len * 0.75, "pt")
    )
  }
  t
}

# ---- Writing -----------------------------------------------------------------

#' Write a figure to PNG at print resolution
#'
#' PNG rather than SVG or PDF: ragg draws the text with the registered font on
#' any machine, where a vector figure would embed or substitute fonts
#' differently depending on who opens it. At FW_CHART$export_dpi (300) a
#' column-wide figure is sharp on paper and a few hundred kilobytes.
#'
#' A transparent background, so the figure takes the page's tint.
fw_gg_png <- function(plot, path, width_mm = FW_PDF$text_width_mm, height_mm) {
  fw_gg_fonts()
  ragg::agg_png(path, width = width_mm, height = height_mm, units = "mm",
                res = FW_CHART$export_dpi, background = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(path)
}

#' A bar chart's printed height from its row count, in mm
#'
#' The same rule as the page (fw_chart_height()): the figure grows with its
#' rows, so seven methods are not crushed into the space two would use. Scaled
#' from the page's pixels to millimetres at the ratio the printed text is to
#' the screen's.
fw_gg_height <- function(chart, rows) {
  h <- FW_CHART$height[[chart]]
  px <- max(h[["min"]], h[["per_row"]] * rows + h[["pad"]])
  # A screen px is 0.2646 mm; the printed type is FW_PRINT$floor / 12 times the
  # size of the screen's 16px floor, so the rows are scaled with it.
  round(px * 0.2646 * FW_PRINT$floor / 12 * 0.95)
}

# ---- Stacked bars ------------------------------------------------------------

#' The shared horizontal stacked bar
#'
#' Every stacked chart on the page is the same shape - a bar per category,
#' split into segments, with the category's total in its label - so the static
#' twins share one drawing function in the way the plotly originals share
#' fw_chart_category().
#'
#' @param d        rows with `label`, `value`, `n`, `share` and the fill column
#' @param order_lv the bar labels, bottom to top (the page's order)
#' @param fill     the column that picks a segment's colour
#' @param levels   the fill values in key order
#' @param colours,inks named fills and the numeral colour on each
#' @param mode     "count" or "share", which sets the axis and the numerals
#' @param x_title  the axis title the page uses in that mode
#' @param numerals whether to print the value inside the segment, as the page
#'   does on the method charts and not on the kind-of-water chart
#' @param key_labels what the key calls each level, if not the level itself
fw_gg_stack <- function(d, order_lv, fill, levels, colours, inks, mode, x_title,
                        numerals = TRUE, axis_key = "", key_labels = levels) {
  d$label <- factor(d$label, levels = order_lv)
  d$fill <- factor(d[[fill]], levels = levels)
  d$text <- if (mode == "share") paste0(round(d$share), "%") else as.character(d$n)
  d$text[d$share < FW_CHART$label_min_share] <- ""
  # THE PRINTED EQUIVALENT OF plotly's uniformtext "hide". A share floor alone
  # is not enough on paper: a 10% segment of a short bar is a few millimetres
  # wide, and a three-digit count set at the text floor overruns it into its
  # neighbour. So a numeral is also dropped when the segment it sits in is
  # physically narrower than the numeral. The panel is taken as 60% of the
  # figure's width, which is the narrow end once the bar labels are drawn.
  axis_max <- if (mode == "share") 100 else
    max(tapply(d$value, d$label, sum), na.rm = TRUE) * 1.03
  seg_mm <- d$value / axis_max * FW_PDF$text_width_mm * 0.6
  text_mm <- nchar(d$text) * FW_PRINT$floor * 0.3528 * 0.58 + 1.5
  d$text[seg_mm < text_mm] <- ""
  d$ink <- unname(inks[as.character(d$fill)])

  p <- ggplot(d, aes(x = value, y = label, fill = fill, group = fill)) +
    geom_col(width = 0.72, position = position_stack(reverse = TRUE),
             colour = FW_COLOURS$surface, linewidth = 0.15) +
    scale_fill_manual(values = colours, breaks = levels, labels = key_labels,
                      drop = TRUE) +
    labs(x = x_title, y = NULL) +
    fw_gg_theme(axis_key)

  p <- p + if (mode == "share") {
    scale_x_continuous(limits = c(0, 100.001), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.01)))
  } else {
    scale_x_continuous(expand = expansion(mult = c(0, 0.03)))
  }

  if (numerals) {
    p <- p + geom_text(
      # group = fill, stated: the per-segment ink would otherwise join the
      # grouping, and the numerals would stack in a different order from the
      # bars they label.
      aes(label = text, colour = I(ink), group = fill),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      family = FW_GG_FAMILY, size = FW_PRINT$floor / ggplot2::.pt,
      show.legend = FALSE
    )
  }
  p
}

#' Outcomes within each method (the page's "Outcomes of methods" chart)
#' @return a ggplot, or NULL when nothing in the selection has a method
fw_gg_method <- function(data, sel, mode = "count") {
  md <- fw_method_data(data, sel, mode)
  if (is.null(md)) return(NULL)
  d <- mutate(md$d, label = method_label)
  fw_gg_stack(d, md$order_lv, "outcome", FW_OUTCOME_LEVELS, FW_OUTCOME_COLOURS,
              FW_OUTCOME_LABEL_INK, md$mode,
              fw_t("charts", if (md$mode == "share") "x_share" else "x_attempts"),
              axis_key = "methods")
}

#' Attempts by kind of water, stacked by outcome
fw_gg_waterbody <- function(sel, mode = "count") {
  cd <- fw_category_data(fw_waterbody_rows(sel), FW_TOP_N, mode)
  if (is.null(cd)) return(NULL)
  # The page's kind-of-water chart prints no numerals in its segments; the
  # count is in the bar's label either way. The twin does the same.
  fw_gg_stack(cd$d, cd$order_lv, "outcome", FW_OUTCOME_LEVELS,
              FW_OUTCOME_COLOURS, FW_OUTCOME_LABEL_INK, mode,
              fw_t("charts", if (mode == "share") "x_share" else "x_attempts"),
              numerals = FALSE, axis_key = "waterbody")
}

#' Methods used in each kind of water, stacked by method
fw_gg_method_waterbody <- function(data, sel, mode = "count") {
  md <- fw_method_waterbody_data(data, sel, mode)
  if (is.null(md)) return(NULL)
  names_by_id <- vapply(md$method_ids,
                        function(m) md$d$method_name[md$d$method_id == m][1],
                        character(1))
  fw_gg_stack(md$d, md$order_lv, "method_id", md$method_ids,
              FW_METHOD_COLOURS[md$method_ids], FW_METHOD_LABEL_INK, mode,
              fw_t("charts", if (mode == "share") "x_share_uses" else "x_times_used"),
              key_labels = unname(names_by_id)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

#' How long attempts took, by method, on a log axis
#'
#' The page's chart point for point: the same beeswarm offsets
#' (fw_duration_swarm()), the same box per method in the interface teal, and a
#' dotted ink line at each named unit break rather than decades.
fw_gg_duration <- function(data, sel) {
  dd <- fw_duration_data(data, sel)
  if (is.null(dd)) return(NULL)
  d <- dd$d
  d$outcome <- factor(d$outcome, levels = FW_OUTCOME_LEVELS)
  lim <- 10^fw_duration_range(d$duration_days)
  spread <- FW_CHART$duration_swarm$spread

  # Only the unit breaks inside this selection's range get a line; the axis
  # still names them all, and ggplot drops the labels that fall outside.
  breaks <- FW_CHART$duration_ticks[FW_CHART$duration_ticks >= lim[1] &
                                      FW_CHART$duration_ticks <= lim[2]]

  ggplot(d) +
    geom_vline(xintercept = breaks, colour = FW_COLOURS$ink,
               linetype = "dotted", linewidth = 0.45) +
    geom_boxplot(aes(x = duration_days, y = row, group = row),
                 orientation = "y", width = 2 * spread, outlier.shape = NA,
                 fill = FW_COLOURS$teal_tint, alpha = 0.45,
                 colour = FW_COLOURS$teal_text, linewidth = 0.4) +
    geom_point(aes(x = duration_days, y = y_dot, fill = outcome),
               shape = 21, size = 2.1, stroke = 0.25,
               colour = FW_COLOURS$surface, alpha = FW_CHART$point$opacity) +
    scale_fill_manual(values = FW_OUTCOME_COLOURS, breaks = FW_OUTCOME_LEVELS,
                      drop = TRUE) +
    scale_x_log10(limits = lim, breaks = FW_CHART$duration_ticks,
                  labels = fw_gg_stagger(fw_t("charts", "duration_ticks")),
                  expand = expansion(0)) +
    scale_y_continuous(breaks = seq_along(dd$order_lv), labels = dd$order_lv,
                       limits = c(0.5, length(dd$order_lv) + 0.5),
                       expand = expansion(0)) +
    labs(x = fw_t("charts", "x_duration"), y = NULL) +
    fw_gg_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    guides(fill = guide_legend(override.aes = list(size = 3.2, alpha = 1)))
}

#' Tick labels on two alternating lines
#'
#' The duration axis names a day, a week, a month, a year, five years and ten.
#' On screen plotly has the width to set them in one row; on a printed column
#' at the text floor, "5 years" and "10 years" are closer together than either
#' label is wide. Every second label drops to a second line, so each keeps its
#' full words and its own place under its gridline.
fw_gg_stagger <- function(labels) {
  ifelse(seq_along(labels) %% 2 == 0, paste0("\n", labels), labels)
}

# ---- The map -----------------------------------------------------------------

#' The world the static map is drawn on
#'
#' Natural Earth 1:110m, bundled by dev/build_report_basemap.R. Read once per
#' process.
fw_gg_world <- local({
  world <- NULL
  function(path = file.path("resources", "report", "ne_110m_countries.rds")) {
    # sf's namespace FIRST. readRDS() does not load the package a class
    # belongs to, and subsetting an sf object before sf is loaded falls through
    # to the data.frame method, which loses the geometry column's attribute.
    if (is.null(world)) {
      loadNamespace("sf")
      world <<- readRDS(path)
    }
    world
  }
})

#' Where the attempts in a selection happened, as a static map
#'
#' The page's map without the tiles: light land on a slightly darker sea, as
#' the Carto basemap draws it, with one dot per located attempt in its outcome
#' colour. A selection that spans most of the world is drawn whole; a regional
#' one is drawn close in, framed to its points. Both on plain longitude and
#' latitude, which is the projection the source outlines are cut for - an
#' Equal Earth reprojection drew stray lines where Natural Earth's polygons
#' meet the antimeridian.
#'
#' @return list(plot, height_mm), or NULL when no attempt has coordinates
fw_gg_map <- function(data, sel) {
  pts <- sel[!is.na(sel$latitude) & !is.na(sel$longitude),
             c("longitude", "latitude", "outcome")]
  if (!nrow(pts)) return(NULL)
  pts$outcome <- fw_outcome_factor(pts$outcome)
  # The rarer outcomes last, so a Failed dot is not buried under forty
  # Successful ones at the same site.
  pts <- pts[order(-table(pts$outcome)[as.character(pts$outcome)]), ]
  world <- fw_gg_world()
  world <- world[!world$iso3 %in% "ATA", ]
  width <- FW_PDF$text_width_mm

  lon <- range(pts$longitude); lat <- range(pts$latitude)
  global <- diff(lon) > 150 || diff(lat) > 70

  pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
  base <- ggplot() +
    geom_sf(data = world, fill = FW_COLOURS$surface, colour = FW_COLOURS$border,
            linewidth = 0.2) +
    geom_sf(data = pts_sf, aes(fill = outcome), shape = 21, size = 2.2,
            stroke = 0.3, colour = FW_COLOURS$surface, alpha = 0.9) +
    scale_fill_manual(values = FW_OUTCOME_COLOURS, breaks = FW_OUTCOME_LEVELS,
                      drop = TRUE) +
    guides(fill = guide_legend(override.aes = list(size = 3.2, alpha = 1))) +
    fw_gg_theme() +
    theme(
      panel.background = element_rect(fill = FW_COLOURS$page, colour = NA),
      panel.grid.major = element_blank(),
      axis.text = element_blank(), axis.title = element_blank(),
      axis.ticks = element_blank(), axis.line = element_blank()
    )

  if (global) {
    # The inhabited world, not the whole globe: Antarctica has no freshwater
    # eradications to show and would take a fifth of the frame to say so.
    plot <- base + coord_sf(xlim = c(-180, 180), ylim = c(-56, 84),
                            expand = FALSE)
    return(list(plot = plot, height_mm = round(width * 0.47)))
  }

  # A margin of a fifth of the span, at least two degrees, so a single site
  # sits in some of its surroundings rather than filling the frame. The frame
  # is then widened to a printable shape: never taller than it is wide.
  pad_lon <- max(2, diff(lon) * 0.2); pad_lat <- max(2, diff(lat) * 0.2)
  xl <- c(lon[1] - pad_lon, lon[2] + pad_lon)
  yl <- c(max(-85, lat[1] - pad_lat), min(85, lat[2] + pad_lat))
  aspect <- diff(yl) / (diff(xl) * cos(mean(yl) * pi / 180))
  if (aspect > 0.75) {
    grow <- (diff(yl) / 0.75 / cos(mean(yl) * pi / 180) - diff(xl)) / 2
    xl <- xl + c(-grow, grow); aspect <- 0.75
  } else if (aspect < 0.4) {
    grow <- (0.4 * diff(xl) * cos(mean(yl) * pi / 180) - diff(yl)) / 2
    yl <- c(max(-85, yl[1] - grow), min(85, yl[2] + grow)); aspect <- 0.4
  }
  plot <- base + coord_sf(xlim = xl, ylim = yl, expand = FALSE)
  list(plot = plot, height_mm = round(width * aspect) + 14)
}
