# charts_static.R
# The PDF report's figures: ggplot versions of the page's plotly charts, plus a
# static map, written to PNG by fw_gg_png() and placed by R/report_pdf.R.
#
# Each chart uses the same counting function as its plotly original
# (fw_method_data(), fw_category_data(), fw_duration_data()), so only the
# drawing is duplicated. plotly can't render static images here (kaleido needs
# Python). dev/value_test.R checks both against a base-R recount.
#
# Figures are drawn at printed size, so FW_PRINT$floor holds on paper.
# Transparent backgrounds: no white.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# ---- Type and theme ----------------------------------------------------------

#' Register Ubuntu for ggplot from resources/report/fonts/, once per process
#'
#' Connect Cloud doesn't have Ubuntu installed.
fw_gg_fonts = local({
  done = FALSE
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

FW_GG_FAMILY = "FWISE Ubuntu"

#' The shared theme for static figures
#'
#' Bare axes and the page's label gaps (px at 96 dpi converted to pt).
fw_gg_theme = function() {
  fw_gg_fonts()
  pt = FW_PRINT$floor
  ink = FW_COLOURS$ink
  gap = FW_CHART$tick_gap * 0.75

  theme_minimal(base_family = FW_GG_FAMILY, base_size = pt) +
    theme(
      text = element_text(colour = ink, size = pt),
      axis.text = element_text(colour = ink, size = pt),
      axis.text.x = element_text(margin = margin(t = gap)),
      axis.text.y = element_text(margin = margin(r = gap)),
      axis.title = element_text(colour = ink, size = pt),
      # Gap between tick labels and axis title, as fw_axis_title().
      axis.title.x = element_text(margin = margin(t = FW_CHART$title_gap * 0.75)),
      axis.title.y = element_text(margin = margin(r = FW_CHART$title_gap * 0.75)),
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
}

# ---- Writing -----------------------------------------------------------------

#' Write a figure to a transparent PNG at FW_CHART$export_dpi
#'
#' PNG so ragg draws the registered font the same everywhere.
fw_gg_png = function(plot, path, width_mm = FW_PDF$text_width_mm, height_mm) {
  fw_gg_fonts()
  ragg::agg_png(path, width = width_mm, height = height_mm, units = "mm",
                res = FW_CHART$export_dpi, background = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(path)
}

#' A bar chart's printed height in mm, by the page's fw_chart_height() rule
fw_gg_height = function(chart, rows) {
  h = FW_CHART$height[[chart]]
  px = max(h[["min"]], h[["per_row"]] * rows + h[["pad"]])
  # px -> mm (0.2646), scaled by printed vs screen type size.
  round(px * 0.2646 * FW_PRINT$floor / 12 * 0.95)
}

# ---- Stacked bars ------------------------------------------------------------

#' The shared horizontal stacked bar
#'
#' @param d        rows with `label`, `value`, `n`, `share` and the fill column
#' @param order_lv the bar labels, bottom to top (the page's order)
#' @param fill     the column that picks a segment's colour
#' @param levels   the fill values in key order
#' @param colours,inks named fills and the numeral colour on each
#' @param mode     "count" or "share", which sets the axis and the numerals
#' @param x_title  the axis title the page uses in that mode
#' @param numerals whether to print the value inside each segment
#' @param key_labels what the key calls each level, if not the level itself
fw_gg_stack = function(d, order_lv, fill, levels, colours, inks, mode, x_title,
                        numerals = TRUE, key_labels = levels) {
  d$label = factor(d$label, levels = order_lv)
  d$fill = factor(d[[fill]], levels = levels)
  d$text = if (mode == "share") paste0(round(d$share), "%") else as.character(d$n)
  d$text[d$share < FW_CHART$label_min_share] = ""
  # Print version of plotly's uniformtext "hide": also drop a numeral wider
  # than its segment. The panel is taken as 60% of the figure width.
  axis_max = if (mode == "share") 100 else
    max(tapply(d$value, d$label, sum), na.rm = TRUE) * 1.03
  seg_mm = d$value / axis_max * FW_PDF$text_width_mm * 0.6
  text_mm = nchar(d$text) * FW_PRINT$floor * 0.3528 * 0.58 + 1.5
  d$text[seg_mm < text_mm] = ""
  d$ink = unname(inks[as.character(d$fill)])

  # Rules behind the bars at the page's breaks, as fw_bar_rules().
  grid = if (mode == "share") {
    geom_vline(xintercept = seq(0, 100, FW_CHART$share_dtick),
               colour = FW_COLOURS$border, linewidth = FW_CHART$bar_grid * 0.3)
  } else {
    geom_vline(xintercept = 0, colour = FW_COLOURS$border,
               linewidth = FW_CHART$bar_grid * 0.3)
  }

  p = ggplot(d, aes(x = value, y = label, fill = fill, group = fill)) +
    grid +
    geom_col(width = 0.72, position = position_stack(reverse = TRUE),
             colour = FW_COLOURS$surface, linewidth = 0.15) +
    scale_fill_manual(values = colours, breaks = levels, labels = key_labels,
                      drop = TRUE) +
    labs(x = x_title, y = NULL) +
    fw_gg_theme()

  # A list, not two objects joined with `+`: ggplot2 won't add one ggproto to
  # another outside a plot.
  p = p + if (mode == "share") {
    # Same breaks as the page.
    list(scale_x_continuous(limits = c(0, 100.001),
                            breaks = seq(0, 100, FW_CHART$share_dtick),
                            labels = function(x) paste0(x, "%"),
                            expand = expansion(mult = c(0, 0.01))),
         # Right margin so "100%" isn't clipped.
         theme(panel.grid.major.x = element_blank(),
               plot.margin = margin(2, FW_PRINT$floor * 1.4, 2, 2)))
  } else {
    scale_x_continuous(expand = expansion(mult = c(0, 0.03)))
  }

  if (numerals) {
    p = p + geom_text(
      # group = fill, so numerals stack in the same order as the bars.
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
fw_gg_method = function(data, sel, mode = "count") {
  md = fw_method_data(data, sel, mode)
  if (is.null(md)) return(NULL)
  d = mutate(md$d, label = method_label)
  fw_gg_stack(d, md$order_lv, "outcome", fw_outcome_levels(md$mode), FW_OUTCOME_COLOURS,
              FW_OUTCOME_LABEL_INK, md$mode,
              fw_t("charts", if (md$mode == "share") "x_share" else "x_attempts"))
}

#' Attempts in still and flowing water, stacked by outcome
fw_gg_waterbody = function(sel, mode = "count") {
  cd = fw_category_data(fw_waterbody_rows(sel), mode = mode)
  if (is.null(cd)) return(NULL)
  fw_gg_stack(cd$d, cd$order_lv, "outcome", fw_outcome_levels(mode),
              FW_OUTCOME_COLOURS, FW_OUTCOME_LABEL_INK, mode,
              fw_t("charts", if (mode == "share") "x_share" else "x_attempts"))
}

#' How long attempts took, by method, on a log axis
#'
#' Matches the page: same beeswarm, teal box, and dotted ink lines at the named
#' unit breaks.
fw_gg_duration = function(data, sel) {
  dd = fw_duration_data(data, sel)
  if (is.null(dd)) return(NULL)
  d = dd$d
  lim = 10^fw_duration_range(d$duration_days)
  spread = FW_CHART$duration_swarm$spread

  # Same ticks as the page (fw_duration_ticks()).
  ticks = fw_duration_ticks(d$duration_days)
  # Lines only for breaks inside the range.
  breaks = ticks$vals[ticks$vals >= lim[1] & ticks$vals <= lim[2]]

  ggplot(d) +
    geom_vline(xintercept = breaks, colour = FW_COLOURS$ink,
               linetype = "dotted", linewidth = 0.45) +
    geom_boxplot(aes(x = duration_days, y = row, group = row),
                 orientation = "y", width = 2 * spread, outlier.shape = NA,
                 fill = FW_COLOURS$teal_tint, alpha = 0.45,
                 colour = FW_COLOURS$teal_text, linewidth = 0.4) +
    # One colour, no key: every dot is a successful attempt.
    geom_point(aes(x = duration_days, y = y_dot),
               fill = FW_OUTCOME_COLOURS[["Successful"]],
               shape = 21, size = 2.1, stroke = 0.25,
               colour = FW_COLOURS$surface, alpha = FW_CHART$point$opacity) +
    scale_x_log10(limits = lim, breaks = ticks$vals,
                  labels = fw_gg_stagger(ticks$text),
                  expand = expansion(0)) +
    scale_y_continuous(breaks = seq_along(dd$order_lv), labels = dd$order_lv,
                       limits = c(0.5, length(dd$order_lv) + 0.5),
                       expand = expansion(0)) +
    # No axis title, as on the page.
    labs(x = NULL, y = NULL) +
    fw_gg_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          # Right margin for the last tick's label.
          plot.margin = margin(2, FW_PRINT$floor * 2.2, 2, 2))
}

#' Tick labels on two alternating lines, so close labels don't overlap on paper
fw_gg_stagger = function(labels) {
  ifelse(seq_along(labels) %% 2 == 0, paste0("\n", labels), labels)
}

# ---- The map -----------------------------------------------------------------

#' The static map's layers: list(world, ocean, lakes)
#'
#' Natural Earth 1:110m. Read once per process.
fw_gg_basemap = local({
  layers = NULL
  function(path = file.path("resources", "report", "ne_110m_basemap.rds")) {
    # Load sf first: readRDS() doesn't, and subsetting an sf object without it
    # loses the geometry column.
    if (is.null(layers)) {
      loadNamespace("sf")
      layers <<- readRDS(path)
    }
    layers
  }
})

#' Axis labels as degrees with a hemisphere letter ("40°N"); "0" at zero
#'
#' NA breaks must stay NA or the axis shifts.
#'
#' @param pos,neg the letters for the positive and negative sides
fw_gg_degrees = function(pos, neg) {
  function(x) {
    ifelse(is.na(x), NA_character_,
           ifelse(x == 0, "0",
                  paste0(abs(round(x)), "°", ifelse(x > 0, pos, neg))))
  }
}

#' A scale bar, correct at the frame's middle latitude and labelled with it
#'
#' On an unprojected lon/lat plot the scale varies with latitude, so the label
#' says where it holds. Length is a round number (1, 2, 5 x 10^n) under a fifth
#' of the frame width.
#'
#' @param xl,yl the longitude and latitude limits the map is drawn to
#' @return a list of ggplot layers, to be added to the plot
fw_gg_scale_bar = function(xl, yl) {
  mid_lat = mean(yl)
  km_per_deg = 111.32 * cos(mid_lat * pi / 180)
  if (!is.finite(km_per_deg) || km_per_deg <= 0) return(NULL)

  target_km = diff(xl) * km_per_deg / 5
  if (!is.finite(target_km) || target_km <= 0) return(NULL)
  pow = 10^floor(log10(target_km))
  km = c(1, 2, 5, 10) * pow
  km = max(km[km <= target_km], pow)

  deg = km / km_per_deg
  # Bottom left, inset from the frame.
  x0 = xl[1] + diff(xl) * 0.05
  y0 = yl[1] + diff(yl) * 0.07
  label = fw_fill(fw_t("charts", "scale_bar"),
                   km = format(km, big.mark = ",", trim = TRUE),
                   lat = paste0(abs(round(mid_lat)), "°",
                                if (mid_lat >= 0) "N" else "S"))

  list(
    # A light keyline under the bar so it reads over sea and land.
    annotate("segment", x = x0, xend = x0 + deg, y = y0, yend = y0,
             colour = FW_COLOURS$surface, linewidth = 1.6, lineend = "butt"),
    annotate("segment", x = x0, xend = x0 + deg, y = y0, yend = y0,
             colour = FW_MAP_PRINT$frame, linewidth = 0.7, lineend = "butt"),
    annotate("text", x = x0, y = y0 + diff(yl) * 0.025, label = label,
             # annotate() sizes text in mm; .pt converts from pt.
             hjust = 0, vjust = 0, size = FW_PRINT$floor / ggplot2::.pt,
             colour = FW_MAP_PRINT$frame)
  )
}

#' The selection's attempts on a static map
#'
#' Land, water and lakes in FW_MAP_PRINT colours (client), a dot per located
#' attempt, degree labels, a frame and a scale bar. Wide selections show the
#' inhabited world; regional ones are framed to their points. Plain lon/lat:
#' Equal Earth drew stray lines at the antimeridian.
#'
#' @return list(plot, height_mm), or NULL when no attempt has coordinates
fw_gg_map = function(data, sel) {
  pts = sel[!is.na(sel$latitude) & !is.na(sel$longitude),
             c("longitude", "latitude", "outcome")]
  if (!nrow(pts)) return(NULL)
  pts$outcome = fw_outcome_factor(pts$outcome)
  # Rarer outcomes drawn last, so they're not buried.
  pts = pts[order(-table(pts$outcome)[as.character(pts$outcome)]), ]
  base_layers = fw_gg_basemap()
  world = base_layers$world
  world = world[!world$iso3 %in% "ATA", ]
  width = FW_PDF$text_width_mm

  lon = range(pts$longitude); lat = range(pts$latitude)
  global = diff(lon) > 150 || diff(lat) > 70

  pts_sf = sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
  base = ggplot() +
    # Ocean, then land, then lakes over the land. Order matters.
    geom_sf(data = base_layers$ocean, fill = FW_MAP_PRINT$water, colour = NA) +
    geom_sf(data = world, fill = FW_MAP_PRINT$land,
            colour = FW_MAP_PRINT$outline, linewidth = 0.2) +
    geom_sf(data = base_layers$lakes, fill = FW_MAP_PRINT$water,
            colour = FW_MAP_PRINT$outline, linewidth = 0.1) +
    geom_sf(data = pts_sf, aes(fill = outcome), shape = 21, size = 2.2,
            stroke = 0.3, colour = FW_COLOURS$surface, alpha = 0.9) +
    scale_fill_manual(values = FW_OUTCOME_COLOURS, breaks = FW_OUTCOME_LEVELS,
                      drop = TRUE) +
    guides(fill = guide_legend(override.aes = list(size = 3.2, alpha = 1))) +
    fw_gg_theme() +
    theme(
      # Panel matches the sea, so no seam where the frame runs past the data.
      panel.background = element_rect(fill = FW_MAP_PRINT$water, colour = NA),
      # Dark grey frame (client).
      panel.border = element_rect(fill = NA, colour = FW_MAP_PRINT$frame,
                                  linewidth = 0.6),
      # Ticks and labels, but no graticule lines over the sea.
      panel.grid.major = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = FW_PRINT$floor, colour = FW_COLOURS$ink_muted),
      axis.ticks = element_line(colour = FW_MAP_PRINT$frame, linewidth = 0.4),
      axis.line = element_blank()
    ) +
    scale_x_continuous(labels = fw_gg_degrees("E", "W")) +
    scale_y_continuous(labels = fw_gg_degrees("N", "S"))

  if (global) {
    # The inhabited world, without Antarctica.
    xl = c(-180, 180); yl = c(-56, 84)
    plot = base + fw_gg_scale_bar(xl, yl) +
      coord_sf(xlim = xl, ylim = yl, expand = FALSE)
    return(list(plot = plot, height_mm = round(width * 0.47)))
  }

  # Pad by a fifth of the span (at least 2 degrees), then widen to a printable
  # shape between 0.4 and 0.75 tall:wide.
  pad_lon = max(2, diff(lon) * 0.2); pad_lat = max(2, diff(lat) * 0.2)
  xl = c(lon[1] - pad_lon, lon[2] + pad_lon)
  yl = c(max(-85, lat[1] - pad_lat), min(85, lat[2] + pad_lat))
  aspect = diff(yl) / (diff(xl) * cos(mean(yl) * pi / 180))
  if (aspect > 0.75) {
    grow = (diff(yl) / 0.75 / cos(mean(yl) * pi / 180) - diff(xl)) / 2
    xl = xl + c(-grow, grow); aspect = 0.75
  } else if (aspect < 0.4) {
    grow = (0.4 * diff(xl) * cos(mean(yl) * pi / 180) - diff(yl)) / 2
    yl = c(max(-85, yl[1] - grow), min(85, yl[2] + grow)); aspect = 0.4
  }
  base = base + fw_gg_scale_bar(xl, yl)
  plot = base + coord_sf(xlim = xl, ylim = yl, expand = FALSE)
  list(plot = plot, height_mm = round(width * aspect) + 14)
}
