# charts_static.R
# The PDF report's figures: static ggplot twins of the page's plotly charts, and
# a static map. Written to PNG at print resolution by fw_gg_png() and placed by
# R/report_pdf.R.
#
# TWINS, NOT A SECOND IMPLEMENTATION OF THE NUMBERS. Each chart here draws from
# the same counting function its plotly original does - fw_method_data(),
# fw_category_data(), fw_duration_data(), all in
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
#' Bare axes, as on the page, with the same breathing room between an axis and
#' its labels (FW_CHART$tick_gap, taken as px at 96 dpi and printed in pt).
fw_gg_theme <- function() {
  fw_gg_fonts()
  pt <- FW_PRINT$floor
  ink <- FW_COLOURS$ink
  gap <- FW_CHART$tick_gap * 0.75

  theme_minimal(base_family = FW_GG_FAMILY, base_size = pt) +
    theme(
      text = element_text(colour = ink, size = pt),
      axis.text = element_text(colour = ink, size = pt),
      axis.text.x = element_text(margin = margin(t = gap)),
      axis.text.y = element_text(margin = margin(r = gap)),
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
                        numerals = TRUE, key_labels = levels) {
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
    fw_gg_theme()

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
              fw_t("charts", if (md$mode == "share") "x_share" else "x_attempts"))
}

#' Attempts by kind of water, stacked by outcome
fw_gg_waterbody <- function(sel, mode = "count") {
  cd <- fw_category_data(fw_waterbody_rows(sel), FW_TOP_N, mode)
  if (is.null(cd)) return(NULL)
  # Numerals in the segments, count or %, as the page's chart now prints them.
  fw_gg_stack(cd$d, cd$order_lv, "outcome", FW_OUTCOME_LEVELS,
              FW_OUTCOME_COLOURS, FW_OUTCOME_LABEL_INK, mode,
              fw_t("charts", if (mode == "share") "x_share" else "x_attempts"))
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

  # THE SAME TICKS THE PAGE'S CHART DRAWS, terminal one included, so the
  # printed picture and the screen agree on where the axis stops - see
  # fw_duration_ticks() in charts.R.
  ticks <- fw_duration_ticks(d$duration_days)
  # Only the breaks inside this selection's range get a line; the axis still
  # names them all, and ggplot drops the labels that fall outside.
  breaks <- ticks$vals[ticks$vals >= lim[1] & ticks$vals <= lim[2]]

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
    scale_x_log10(limits = lim, breaks = ticks$vals,
                  labels = fw_gg_stagger(ticks$text),
                  expand = expansion(0)) +
    scale_y_continuous(breaks = seq_along(dd$order_lv), labels = dd$order_lv,
                       limits = c(0.5, length(dd$order_lv) + 0.5),
                       expand = expansion(0)) +
    # NO AXIS TITLE, as on the page (client, 23 Sept 2026).
    labs(x = NULL, y = NULL) +
    fw_gg_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          # ROOM FOR THE TERMINAL TICK'S LABEL. It sits at the longest attempt
          # in the selection, which is at the right-hand end of the data, and
          # "20 years" centred on it runs off the panel and is clipped by the
          # default 6pt margin. Half a label's width, taken from the theme's
          # own text size.
          plot.margin = margin(2, FW_PRINT$floor * 2.2, 2, 2)) +
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
#'
#' THREE LAYERS since 23 Sept 2026: list(world, ocean, lakes). The lakes are
#' the reason - see the note in the builder about why a blue panel background
#' is not the same thing as blue water on a freshwater map.
fw_gg_basemap <- local({
  layers <- NULL
  function(path = file.path("resources", "report", "ne_110m_basemap.rds")) {
    # sf's namespace FIRST. readRDS() does not load the package a class
    # belongs to, and subsetting an sf object before sf is loaded falls through
    # to the data.frame method, which loses the geometry column's attribute.
    if (is.null(layers)) {
      loadNamespace("sf")
      layers <<- readRDS(path)
    }
    layers
  }
})

#' Axis labels as degrees with a hemisphere letter
#'
#' "40°N", "20°W", and a bare "0" at the equator and the prime meridian, where
#' a hemisphere letter would be wrong rather than merely redundant. ggplot
#' hands the breaks in as a numeric vector and may include NA for a break it
#' has dropped, which has to come back as NA or the axis silently shifts.
#'
#' @param pos,neg the letters for the positive and negative sides
fw_gg_degrees <- function(pos, neg) {
  function(x) {
    ifelse(is.na(x), NA_character_,
           ifelse(x == 0, "0",
                  paste0(abs(round(x)), "\u00b0", ifelse(x > 0, pos, neg))))
  }
}

#' A scale bar sized to the frame, correct at its own stated latitude
#'
#' ONE LATITUDE ONLY, AND IT SAYS WHICH. On an unprojected longitude/latitude
#' plot the scale changes with latitude - a degree of longitude is about 111km
#' at the equator and 78km at 45 degrees - so a single bar cannot be right
#' everywhere on the sheet. It is computed at the middle of what is framed and
#' labelled with that latitude, which makes it a claim a reader can check
#' instead of a decoration that is quietly wrong at the edges.
#'
#' The length is the nearest round number (1, 2 or 5 x a power of ten) under a
#' fifth of the frame's width, so the bar is a figure worth reading rather than
#' whatever a fifth of the frame happens to come to.
#'
#' @param xl,yl the longitude and latitude limits the map is drawn to
#' @return a list of ggplot layers, to be added to the plot
fw_gg_scale_bar <- function(xl, yl) {
  mid_lat <- mean(yl)
  km_per_deg <- 111.32 * cos(mid_lat * pi / 180)
  if (!is.finite(km_per_deg) || km_per_deg <= 0) return(NULL)

  target_km <- diff(xl) * km_per_deg / 5
  if (!is.finite(target_km) || target_km <= 0) return(NULL)
  pow <- 10^floor(log10(target_km))
  km <- c(1, 2, 5, 10) * pow
  km <- max(km[km <= target_km], pow)

  deg <- km / km_per_deg
  # Bottom left, inset by a twentieth of each span so the bar is inside the
  # frame rather than on it.
  x0 <- xl[1] + diff(xl) * 0.05
  y0 <- yl[1] + diff(yl) * 0.07
  label <- fw_fill(fw_t("charts", "scale_bar"),
                   km = format(km, big.mark = ",", trim = TRUE),
                   lat = paste0(abs(round(mid_lat)), "\u00b0",
                                if (mid_lat >= 0) "N" else "S"))

  list(
    # A white keyline under the bar so it reads over the sea as well as land.
    annotate("segment", x = x0, xend = x0 + deg, y = y0, yend = y0,
             colour = FW_COLOURS$surface, linewidth = 1.6, lineend = "butt"),
    annotate("segment", x = x0, xend = x0 + deg, y = y0, yend = y0,
             colour = FW_MAP_PRINT$frame, linewidth = 0.7, lineend = "butt"),
    annotate("text", x = x0, y = y0 + diff(yl) * 0.025, label = label,
             # annotate() sizes text in MM, not the pt that element_text()
             # takes; .pt is ggplot's own conversion between the two.
             hjust = 0, vjust = 0, size = FW_PRINT$floor / ggplot2::.pt,
             colour = FW_MAP_PRINT$frame)
  )
}

#' Where the attempts in a selection happened, as a static map
#'
#' The page's map without the tiles: green land, blue water, one dot per
#' located attempt in its outcome colour. A selection that spans most of the
#' world is drawn whole; a regional one is drawn close in, framed to its
#' points. Both on plain longitude and latitude, which is the projection the
#' source outlines are cut for - an Equal Earth reprojection drew stray lines
#' where Natural Earth's polygons meet the antimeridian.
#'
#' COLOUR IS THE CLIENT'S (23 Sept 2026), and it is the one picture in the app
#' that departs from the brand palette: everywhere else water and land are
#' surface tones, because the map is a ground for data rather than a subject.
#' On paper the client wanted it read as a map, so FW_MAP_PRINT in R/brand.R
#' carries its own green, blue and frame grey.
#'
#' GRATICULE LABELS AND A SCALE BAR, also the client's. The scale bar is
#' correct at ONE latitude only, because this is an unprojected lon/lat plot -
#' a degree of longitude is 111km at the equator and 78km at 45 degrees. It is
#' computed at the middle of whatever is framed and labelled with that
#' latitude, so it is a statement a reader can check rather than a claim about
#' the whole sheet.
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
  base_layers <- fw_gg_basemap()
  world <- base_layers$world
  world <- world[!world$iso3 %in% "ATA", ]
  width <- FW_PDF$text_width_mm

  lon <- range(pts$longitude); lat <- range(pts$latitude)
  global <- diff(lon) > 150 || diff(lat) > 70

  pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
  base <- ggplot() +
    # Ocean first and full-bleed, then land over it, then the lakes punched
    # back through the land. Order is the whole trick: a lake drawn before the
    # country it sits in disappears under it.
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
      # The ocean layer is the sea now, so the panel only shows where the
      # frame runs past the data - it takes the same blue so no seam shows.
      panel.background = element_rect(fill = FW_MAP_PRINT$water, colour = NA),
      # A DARK GREY FRAME (client): the map is a figure with an edge, not a
      # shape floating on the page.
      panel.border = element_rect(fill = NA, colour = FW_MAP_PRINT$frame,
                                  linewidth = 0.6),
      # Ticks and their labels come back. The graticule stays off - lines
      # across the sea competed with the dots.
      panel.grid.major = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = FW_PRINT$floor, colour = FW_COLOURS$ink_muted),
      axis.ticks = element_line(colour = FW_MAP_PRINT$frame, linewidth = 0.4),
      axis.line = element_blank()
    ) +
    scale_x_continuous(labels = fw_gg_degrees("E", "W")) +
    scale_y_continuous(labels = fw_gg_degrees("N", "S"))

  if (global) {
    # The inhabited world, not the whole globe: Antarctica has no freshwater
    # eradications to show and would take a fifth of the frame to say so.
    xl <- c(-180, 180); yl <- c(-56, 84)
    plot <- base + fw_gg_scale_bar(xl, yl) +
      coord_sf(xlim = xl, ylim = yl, expand = FALSE)
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
  base <- base + fw_gg_scale_bar(xl, yl)
  plot <- base + coord_sf(xlim = xl, ylim = yl, expand = FALSE)
  list(plot = plot, height_mm = round(width * aspect) + 14)
}
