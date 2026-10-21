# maps.R
# Every map is built here, so basemaps, markers and cards match across pages.
#
# Leaflet tiles are Web Mercator only. Fine for point maps; a country
# choropleth would need Equal Earth instead.
#
# Default basemap is Carto Voyager, with a Basemaps key (FW_CARTO_KEY). If the
# "API KEY REQUIRED" watermark returns, check the pixels (tiles still return
# 200), the key on the client's Carto account, that the parameter is `key`
# (`api_key` is ignored), and cached tiles.
#
# Attribution is a licence condition, so the control stays on.

library(shiny)

# The Carto Basemaps key. Public by design (it's in every tile request) and
# scoped by domain at Carto. FWISE_CARTO_KEY overrides it.
FW_CARTO_KEY = fw_env("FWISE_CARTO_KEY",
                       default = "cb1_3fo4_1_c9fdd55287ad4682f074548a")

#' A Carto raster tile URL for one basemap style
#'
#' The parameter must be `key`: Carto ignores `api_key` and watermarks the tiles.
fw_carto_url = function(style) {
  base = paste0("https://{s}.basemaps.cartocdn.com/rastertiles/", style,
                 "/{z}/{x}/{y}{r}.png")
  if (is.null(FW_CARTO_KEY) || !nzchar(FW_CARTO_KEY)) {
    return(base)
  }
  paste0(base, "?key=", FW_CARTO_KEY)
}

FW_CARTO_ATTRIB = paste(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
)

#' An empty map with the app's zoom and pan limits
#'
#' One world, no wrapping: tiles don't repeat (fw_add_basemaps()), panning is
#' held to the band, and fw_map_fit.js sets the zoom-out limit so one world
#' fills the width. Limits are map options, so the card script stays the only
#' render hook.
fw_leaflet = function() {
  map = leaflet::leaflet(options = leaflet::leafletOptions(
    minZoom = FW_MAP$min_zoom,
    maxBoundsViscosity = 1
  )) |>
    leaflet::setMaxBounds(-FW_MAP$max_lng, FW_MAP$lat_south,
                          FW_MAP$max_lng, FW_MAP$lat_north)
  # A widget dependency, so it loads after leaflet.js and is inlined into
  # saved reports.
  map$dependencies = c(map$dependencies, list(fw_map_fit_dependency()))
  map
}

#' The script that fits one world to a map's width. See fw_map_fit.js.
#'
#' The version ends in the file's hash, so an edit changes the URL and busts
#' the browser cache.
fw_map_fit_dependency = function() {
  src = normalizePath("resources/js", mustWork = TRUE)
  hash = strtoi(substr(tools::md5sum(file.path(src, "fw_map_fit.js")), 1, 7), 16L)
  htmltools::htmlDependency(
    name = "fw-map-fit", version = paste0("1.1.4.", hash),
    src = c(file = src),
    script = "fw_map_fit.js"
  )
}

#' The basemap stack and its layer control
#'
#' Four grounds. Terrain lets a reader judge whether a waterbody is
#' hydrologically isolated (from the client's metrics framework).
#'
#' @param overlays names of overlay groups to list in the control, if any
fw_add_basemaps = function(map, overlays = NULL) {
  plain     = fw_t("maps", "basemap_plain")
  water     = fw_t("maps", "basemap_water")
  terrain   = fw_t("maps", "basemap_terrain")
  satellite = fw_t("maps", "basemap_satellite")
  map |>
    leaflet::addTiles(
      urlTemplate = fw_carto_url("voyager_nolabels"),
      attribution = FW_CARTO_ATTRIB,
      group = plain,
      options = leaflet::tileOptions(noWrap = TRUE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.OceanBasemap", group = water,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap", group = terrain,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldImagery", group = satellite,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) |>
    # No label layer (client).
    leaflet::addLayersControl(
      baseGroups = c(plain, water, terrain, satellite),
      # Gotcha: never NULL, or leaflet.js shows a checkbox named "null".
      overlayGroups = if (length(overlays)) overlays else character(0),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}

#' Width/height ratio of the band every map shows, in Mercator
#'
#' About 1.67 for 80N to 60S.
fw_map_aspect = function() {
  merc = function(lat) log(tan(pi / 4 + lat * pi / 360))
  2 * pi / (merc(FW_MAP$lat_north) - merc(FW_MAP$lat_south))
}

#' Inline custom properties giving a map wrapper its shape. See .fw-map.
fw_map_shape_style = function() {
  sprintf("--fw-map-aspect: %.4f; --fw-map-max-h: %dvh;",
          fw_map_aspect(), FW_MAP$max_height_vh)
}

#' A map output at the band's shape, so the whole band shows on load
#'
#' The shape is CSS on a wrapper: leafletOutput() rejects aspect-ratio, min()
#' and calc() heights.
fw_map_output = function(output_id, class = NULL) {
  div(
    class = paste(c("fw-map", class), collapse = " "),
    style = fw_map_shape_style(),
    leaflet::leafletOutput(output_id, width = "100%", height = "100%")
  )
}

#' The Mercator note under the Explore and Plan maps (client wording)
fw_map_note = function() {
  p(class = "fw-caption fw-map-note", fw_t("maps", "mercator_note"))
}

# ---- Markers and popups ------------------------------------------------------

# Separator for multi-value popup fields, which travel as one string per row.
# Appears in no species name, method name or note.
FW_POPUP_SEP = "|"

#' Everything a record needs, joined once for the whole selection
#'
#' One row per attempt in `sel`: species per role, methods and contacts. Built
#' as one table, not per marker, so large maps stay fast. The map's cards and
#' records all read this. Attempts without coordinates are included;
#' fw_map_points() is the located subset.
fw_attempt_records = function(data, sel) {
  pts = sel
  # Empty input returns early without the popup columns: callers check nrow().
  if (!nrow(pts)) return(pts)

  # Protected species carry their Red List code (fw_species_shown()).
  species = fw_species_shown(fw_species_label(data$species))

  # Every species id per role, as one delimited string; split again in
  # fw_record_detail_html().
  role_cols = function(role_name, prefix) {
    out = data$attempt_species |>
      dplyr::filter(role == role_name, attempt_id %in% pts$attempt_id) |>
      dplyr::distinct(attempt_id, species_id) |>
      dplyr::left_join(dplyr::select(species, species_id, label, shown),
                       by = "species_id") |>
      dplyr::filter(!is.na(label)) |>
      dplyr::mutate(label = if (role_name == "beneficiary") shown else label) |>
      dplyr::group_by(attempt_id) |>
      dplyr::summarise(
        ids   = paste(species_id, collapse = FW_POPUP_SEP),
        names = paste(label, collapse = FW_POPUP_SEP),
        list  = paste(label, collapse = ", "),
        .groups = "drop"
      )
    # Renamed afterwards to avoid tidy-eval.
    names(out)[-1] = paste0(prefix, "_", names(out)[-1])
    out
  }

  # Method names only, in applied order (client: notes stay in the download).
  methods = data$attempt_method |>
    dplyr::filter(attempt_id %in% pts$attempt_id) |>
    dplyr::left_join(dplyr::select(data$method, method_id, method_name),
                     by = "method_id") |>
    dplyr::filter(!is.na(method_name)) |>
    dplyr::arrange(attempt_id, method_order) |>
    dplyr::group_by(attempt_id) |>
    dplyr::summarise(
      method_names = paste(unique(method_name), collapse = FW_POPUP_SEP),
      .groups = "drop"
    )

  # Blocker to ensure no privacy leaks: a private contact's name, address and
  # organisation are all blanked, as in fw_export_frame().
  contacts = data$contact |>
    dplyr::transmute(
      contact_id,
      contact_name  = dplyr::if_else(contact_public, contact_name, NA_character_),
      contact_email = dplyr::if_else(contact_public, contact_email,
                                     NA_character_),
      organisation  = dplyr::if_else(contact_public, organisation, NA_character_)
    )

  out = pts |>
    dplyr::left_join(role_cols("invasive", "inv"), by = "attempt_id") |>
    dplyr::left_join(role_cols("beneficiary", "ben"), by = "attempt_id") |>
    dplyr::left_join(methods, by = "attempt_id") |>
    dplyr::left_join(
      dplyr::rename(contacts, primary_contact_id = contact_id,
                    primary_contact_name = contact_name,
                    primary_contact_email = contact_email,
                    primary_contact_org = organisation),
      by = "primary_contact_id"
    ) |>
    dplyr::left_join(
      dplyr::rename(contacts, secondary_contact_id = contact_id,
                    secondary_contact_name = contact_name,
                    secondary_contact_email = contact_email,
                    secondary_contact_org = organisation),
      by = "secondary_contact_id"
    )

  out
}

#' The located attempts of a selection: fw_attempt_records() with coordinates
fw_map_points = function(data, sel) {
  fw_attempt_records(data, sel[!is.na(sel$latitude) & !is.na(sel$longitude), ])
}

# ---- Popup building ----------------------------------------------------------
#
# A hover card (enough to decide whether to open the record) and the full
# record (on click). Both are built in R; in "embed" mode the record rides in
# the same popup string inside a <template>.

# A pipe-delimited popup field back into its parts. Empty and NA both mean none.
fw_popup_parts = function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(as.character(x))) return(character(0))
  strsplit(as.character(x), FW_POPUP_SEP, fixed = TRUE)[[1]]
}

# One label/value row. Always drawn; an empty value says "Not noted" (client).
fw_popup_row = function(label, value, html = FALSE,
                         fallback = fw_t("species", "p_not_noted")) {
  esc = htmltools::htmlEscape
  if (length(value) != 1 || is.na(value) || !nzchar(as.character(value))) {
    return(paste0('<div class="fw-popup__row"><span class="fw-popup__key">',
                  esc(label),
                  '</span><span class="fw-popup__val fw-popup__val--none">',
                  esc(fallback), "</span></div>"))
  }
  paste0('<div class="fw-popup__row"><span class="fw-popup__key">',
         esc(label), '</span><span class="fw-popup__val">',
         if (html) as.character(value) else esc(as.character(value)),
         "</span></div>")
}

# A pipe-delimited field as a bulleted list, at most `max` items. NA for none,
# so fw_popup_row() says "Not noted". Items are escaped unless `html`.
fw_popup_list = function(x, max = 3L, html = FALSE) {
  items = if (length(x) > 1) x else fw_popup_parts(x)
  items = utils::head(items[!is.na(items) & nzchar(items)], max)
  if (!length(items)) return(NA_character_)
  if (!html) items = htmltools::htmlEscape(items)
  paste0('<ul class="fw-popup__list"><li>',
         paste(items, collapse = "</li><li>"), "</li></ul>")
}

# The unlabelled country line under the title, "Not noted" when absent.
fw_popup_place = function(country) {
  none = is.na(country) || !nzchar(country)
  paste0('<p class="fw-popup__place', if (none) " fw-popup__val--none", '">',
         htmltools::htmlEscape(if (none) fw_t("species", "p_not_noted") else country),
         "</p>")
}

#' The years an attempt ran: "1998-2004", or just the start year
fw_popup_years = function(start, end) {
  if (is.na(start)) return(NA_character_)
  # Same-year or earlier end isn't a range.
  if (is.na(end) || end <= start) return(as.character(start))
  paste0(start, "-", end)
}

#' How long an attempt lasted, from duration_days
#'
#' Days under a year, then years to one decimal, without a ".0" (client). Used
#' for every duration in the app: cards, records, the duration chart's hover
#' and its last tick. NA when not recorded.
fw_popup_duration = function(days) {
  if (length(days) != 1 || is.na(days)) return(NA_character_)
  if (days < 365) {
    d = round(days)
    return(paste(d, fw_t("maps", if (d == 1) "day_one" else "day_many")))
  }
  y = round(days / 365, 1)
  whole = y == round(y)
  paste(formatC(y, format = "f", digits = if (whole) 0 else 1),
        fw_t("maps", if (y == 1) "year_one" else "year_many"))
}

#' The treated size with its unit (ha or km, from area_unit). NA if none.
fw_popup_area = function(row) {
  if (length(row$area_treated) != 1 || is.na(row$area_treated)) {
    return(NA_character_)
  }
  paste(format(row$area_treated, big.mark = ",", trim = TRUE),
        row$area_unit %|na|% "")
}

#' The kind of water with the treated size after it: "Lake (120 ha)"
fw_popup_waterbody = function(row) {
  kind = row$waterbody_type
  area = fw_popup_area(row)
  if (is.na(area)) return(kind)
  if (length(kind) != 1 || is.na(kind) || !nzchar(kind)) return(area)
  paste0(kind, " (", area, ")")
}

#' The hover card: enough to decide whether to open the record
#'
#' Two photos (first invasive and first protected species), cached only, never
#' a live lookup, rendered once per species, lazy-loaded. Hidden on small
#' screens. Photo credits are a licence condition: keep them.
#'
#' @param thumbs figure HTML keyed by species_id, from fw_map_thumb_cache(), or
#'   NULL for no photograph at all
#' @param thumb_ref carry the photographs by species id rather than inline; see
#'   fw_popup_thumb()
fw_map_hover_html = function(row, thumbs = NULL, thumb_ref = FALSE) {
  esc = htmltools::htmlEscape

  paste0(
    # The id lets a click ask the server for the record (lazy mode).
    '<div class="fw-popup" data-fw-id="', esc(row$attempt_id), '">',
    # Fields in the client's order, all always drawn (see fw_popup_row()).
    '<h3 class="fw-popup__title">',
    esc(row$site_name %|na|% fw_t("species", "p_not_noted")),
    "</h3>",
    fw_popup_place(row$country),
    fw_popup_thumb(row, thumbs, ref = thumb_ref),
    fw_popup_row(fw_t("species", "p_targeted"), row$inv_list),
    fw_popup_row(fw_t("species", "p_protected"), row$ben_list),
    # Outcome in words too, not only the dot colour.
    fw_popup_row(fw_t("species", "p_outcome"), row$outcome),
    fw_popup_row(fw_t("species", "p_began"),
                 fw_popup_years(row$start_year, row$end_year)),
    fw_popup_row(fw_t("species", "p_duration"), fw_popup_duration(row$duration_days)),
    fw_popup_row(fw_t("species", "p_waterbody"), fw_popup_waterbody(row)),
    fw_popup_row(fw_t("species", "p_methods"), fw_popup_list(row$method_names),
                 html = TRUE),
    # The only click target in the card (data-fw-open), besides the marker.
    '<p class="fw-popup__more"><button type="button" class="fw-popup__more-btn" data-fw-open>',
    esc(fw_t("species", "more_hint")), "</button></p>",
    "</div>"
  )
}

#' The hover card's two labelled photos: targeted and protected
#'
#' Both slots always show, with a placeholder (no request) when there's no
#' species or photo, so cards line up.
#'
#' With `ref = TRUE` the block carries only the two species ids and the card
#' script fills it from a dictionary sent once with the map
#' (fw_map_card_render()), which saves about 1 MB per redraw. Saved reports
#' keep them inline.
fw_popup_thumb = function(row, thumbs, ref = FALSE) {
  if (ref) {
    first = function(x) {
      ids = fw_popup_parts(x)
      if (length(ids)) ids[1] else ""
    }
    return(paste0('<div class="fw-popup__figure" data-fw-figures="2" data-fw-thumbs="',
                  htmltools::htmlEscape(first(row$inv_ids), attribute = TRUE),
                  FW_POPUP_SEP,
                  htmltools::htmlEscape(first(row$ben_ids), attribute = TRUE),
                  '"></div>'))
  }
  one = function(ids_col, role_key) {
    ids = fw_popup_parts(ids_col)
    # No species: "None noted". Species without a photo: "No photograph".
    fig = if (length(ids)) thumbs[[ids[1]]] else fw_popup_thumb_unrecorded()
    if (is.null(fig) || !nzchar(fig)) fig = fw_popup_thumb_none()
    paste0('<div class="fw-popup__figure-item">',
           '<p class="fw-popup-fig__role">',
           htmltools::htmlEscape(fw_t("species", role_key)), "</p>",
           fig, "</div>")
  }

  paste0('<div class="fw-popup__figure" data-fw-figures="2">',
         one(row$inv_ids, "fig_invasive"),
         one(row$ben_ids, "fig_beneficiary"),
         "</div>")
}

#' The blank tile for a species with no photograph ("No photograph available")
#'
#' Same markup as fw_species_figure()'s placeholder.
fw_popup_thumb_none = function(text = fw_t("species", "no_image")) {
  as.character(htmltools::tags$div(
    class = "fw-species-figure fw-species-figure--none",
    htmltools::tags$span(class = "fw-species-figure__placeholder", text)
  ))
}

#' The blank tile for a role with no species recorded ("None noted")
fw_popup_thumb_unrecorded = function() {
  fw_popup_thumb_none(fw_t("species", "fig_none"))
}

#' The full record wrapped in a <template>, for embedding in a popup
#'
#' Public addresses (client request) go through fw_email_reveal(); private
#' contacts are already blanked in fw_attempt_records().
#'
#' @param figure_cache optional figure HTML keyed by species_id, from
#'   fw_map_figure_cache()
fw_map_detail_html = function(row, species_tbl, live = FALSE, figure_cache = NULL) {
  # A <template>, not a hidden div: display:none still fetches <img>s, a
  # template fetches nothing until cloned on click.
  paste0(
    '<template class="fw-popup__detail">',
    fw_record_detail_html(row, species_tbl, live, figure_cache = figure_cache),
    "</template>"
  )
}

#' The full record, as the panel shows it
#'
#' Embedded by the map, or sent by fw_map_detail_server() on click.
fw_record_detail_html = function(row, species_tbl, live = FALSE,
                                  figure_cache = NULL) {
  esc = htmltools::htmlEscape
  labels = fw_species_label(species_tbl)

  # A labelled column of photos for one role, with arrows when there are
  # several. Each photo has its species name under it.
  figures = function(ids, names, role_label) {
    ids = fw_popup_parts(ids)
    names = fw_popup_parts(names)
    slides = if (!length(ids)) {
      paste0('<div class="fw-popup-fig__slide" data-fw-slide="0">',
             fw_popup_thumb_unrecorded(), "</div>")
    } else {
      paste0(vapply(seq_along(ids), function(i) {
        fig = figure_cache[ids[i]]
        if (is.null(figure_cache) || is.na(fig)) {
          # Alt text is the plain label; the caption may carry the IUCN code.
          plain = labels$label[match(ids[i], labels$species_id)]
          fig = fw_species_figure_for(species_tbl, ids[i], plain, live = live)
        }
        paste0('<div class="fw-popup-fig__slide" data-fw-slide="', i - 1L, '"',
               if (i > 1L) " hidden" else "", ">",
               unname(fig),
               '<p class="fw-popup-fig__name">', esc(names[i]), "</p>",
               "</div>")
      }, character(1)), collapse = "")
    }
    paste0(
      '<div class="fw-popup-fig" data-fw-figure>',
      '<p class="fw-popup-fig__role">', esc(role_label), "</p>",
      slides,
      if (length(ids) > 1L) {
        paste0(
          '<div class="fw-popup-fig__nav">',
          '<button type="button" class="fw-popup-fig__btn" data-fw-step="-1"',
          ' aria-label="', esc(fw_t("species", "fig_prev")), '">&lsaquo;</button>',
          '<span class="fw-popup-fig__count" data-fw-count>1 / ',
          length(ids), "</span>",
          '<button type="button" class="fw-popup-fig__btn" data-fw-step="1"',
          ' aria-label="', esc(fw_t("species", "fig_next")), '">&rsaquo;</button>',
          "</div>"
        )
      } else "",
      "</div>"
    )
  }

  # Name and organisation, then a reveal button for a public address
  # (fw_email_reveal(), delegated, so it works in injected markup).
  person = function(name, email, org, id) {
    # An organisation with no named person is the contact itself.
    if (is.na(name) || !nzchar(name)) { name = org; org = NA_character_ }
    if (is.na(name) || !nzchar(name)) return(NA_character_)
    who = esc(name)
    if (!is.na(org) && nzchar(org)) who = paste0(who, ", ", esc(org))
    if (is.na(email) || !nzchar(email)) return(who)
    paste0(who, " ", as.character(fw_email_reveal(
      email,
      label = fw_t("networking", "email_action"),
      aria = fw_fill(fw_t("a11y", "email_name"), name = name),
      contact_id = if (!is.na(id)) id
    )))
  }

  area = fw_popup_area(row)

  # Primary then secondary contact.
  contacts = c(
    person(row$primary_contact_name, row$primary_contact_email,
           row$primary_contact_org, row$primary_contact_id),
    person(row$secondary_contact_name, row$secondary_contact_email,
           row$secondary_contact_org, row$secondary_contact_id)
  )
  has_link = !is.na(row$reference_link) && nzchar(row$reference_link)

  # Fields in the client's order, all always drawn.
  paste0(
    '<div class="fw-popup-detail">',
    '<h2 class="fw-popup-detail__title">',
    esc(row$site_name %|na|% fw_t("species", "p_not_noted")),
    "</h2>",
    fw_popup_place(row$country),

    # Invasive left, beneficiary right, both labelled.
    '<div class="fw-popup-detail__figures">',
    figures(row$inv_ids, row$inv_names, fw_t("species", "p_species")),
    figures(row$ben_ids, row$ben_names, fw_t("species", "p_beneficiary")),
    "</div>",

    '<div class="fw-popup-detail__rows">',
    fw_popup_row(fw_t("species", "p_species"), row$inv_list),
    fw_popup_row(fw_t("species", "p_beneficiary"), row$ben_list),
    fw_popup_row(fw_t("species", "p_outcome"), row$outcome),
    fw_popup_row(fw_t("species", "p_began"),
                 fw_popup_years(row$start_year, row$end_year)),
    fw_popup_row(fw_t("species", "p_duration"), fw_popup_duration(row$duration_days)),
    fw_popup_row(fw_t("species", "p_driver"), row$driver),
    fw_popup_row(fw_t("species", "p_waterbody"), row$waterbody_type),
    fw_popup_row(fw_t("species", "p_area"), area),
    fw_popup_row(fw_t("species", "p_methods"), fw_popup_list(row$method_names),
                 html = TRUE),
    fw_popup_row(fw_t("species", "p_method_desc"), row$method_description),
    fw_popup_row(fw_t("species", "p_verified"), row$verification_method),
    fw_popup_row(fw_t("species", "p_contacts"),
                 fw_popup_list(contacts, max = 2L, html = TRUE), html = TRUE),
    fw_popup_row(fw_t("species", "p_reference"), row$reference),
    # A "Read the source" button when there's a link; nothing otherwise.
    if (has_link) {
      paste0('<p class="fw-popup-detail__link"><a class="fw-popup-detail__link-btn" href="',
             esc(row$reference_link),
             '" target="_blank" rel="noopener noreferrer">',
             esc(fw_t("species", "p_read_source")), "</a></p>")
    } else "",
    '<p class="fw-popup-detail__hint">',
    esc(fw_t("species", "p_download_hint")), "</p>",
    "</div>",
    "</div>"
  )
}

#' The popup for one attempt: the hover card, plus the record when embedded
#'
#' @param detail "embed" includes the record; "lazy" leaves it to
#'   fw_map_detail_server().
fw_map_popup = function(row, species_tbl, live = FALSE,
                         detail = c("embed", "lazy"), figure_cache = NULL,
                         thumbs = NULL, thumb_ref = FALSE) {
  detail = match.arg(detail)
  hover = fw_map_hover_html(row, thumbs, thumb_ref = thumb_ref)
  if (detail == "lazy") return(hover)
  paste0(hover, fw_map_detail_html(row, species_tbl, live, figure_cache = figure_cache))
}

#' Hover-card thumbnails, rendered once per species
#'
#' Only the first species of each role per attempt. "" for a species with no
#' cached image; fw_popup_thumb() draws the placeholder. Never live.
fw_map_thumb_cache = function(data, pts) {
  first_of = function(col) {
    vapply(col, function(x) {
      ids = fw_popup_parts(x)
      if (length(ids)) ids[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
  }
  first = c(first_of(pts$inv_ids), first_of(pts$ben_ids))
  ids = unique(first[!is.na(first)])
  if (!length(ids)) return(character(0))
  labels = fw_species_label(data$species)
  names_for = labels$label[match(ids, labels$species_id)]
  stats::setNames(
    vapply(seq_along(ids), function(i) {
      img = fw_species_image_cached(data$species, ids[i])
      if (is.null(img)) return("")
      fw_species_figure(img, names_for[i])
    }, character(1)),
    ids
  )
}

#' Every species figure a set of records needs, rendered once each
#'
#' Rendering per marker was most of a large map's build time.
fw_map_figure_cache = function(data, pts) {
  ids = unique(unlist(lapply(c(pts$inv_ids, pts$ben_ids), fw_popup_parts)))
  if (!length(ids)) return(character(0))
  labels = fw_species_label(data$species)
  names_for = labels$label[match(ids, labels$species_id)]
  stats::setNames(
    vapply(seq_along(ids), function(i) {
      fw_species_figure_for(data$species, ids[i], names_for[i], live = FALSE)
    }, character(1)),
    ids
  )
}

# ---- The marker card ---------------------------------------------------------

# The hover card is one fixed-position element on <body>, not a Leaflet popup:
# popups are clipped by the map and open on click. The script lifts each
# marker's popup HTML and unbinds it, so without JS the plain popup still
# works. Hover is cheap (one innerHTML); a 120ms delay avoids opening cards as
# the pointer crosses the map. Taps open the full record.
#
# Gotchas: this is an R double-quoted string, so no double quotes in comments.
# The cluster group adds markers on a timer, so markers are wired on
# `layeradd` too (see wire()).
FW_MAP_CARD_JS = "
function (el, x, data) {
  var map = this;
  // Thumbnail dictionary sent once with the map (fw_map_card_render()).
  // Empty in a saved report, whose cards carry their photos inline.
  var THUMBS = (data && data.thumbs) || {};
  var THUMB_NONE = (data && data.none) || '';
  var THUMB_UNRECORDED = (data && data.unrecorded) || THUMB_NONE;
  var THUMB_ROLES = (data && data.roles) || ['', ''];
  var CLOSE_LABEL = '{{CLOSE}}';
  var CARD_LABEL = '{{LABEL}}';
  var DETAIL_LABEL = '{{DETAIL}}';
  // Input a click writes an attempt id to (lazy mode). Empty when every record
  // is embedded; '#' in the Detailed report (scroll to the card).
  var DETAIL_INPUT = '{{INPUT}}';
  var OPEN_DELAY = 120;   // long enough that crossing a marker is not opening it
  var SHUT_DELAY = 260;   // long enough to cross the gap into the card
  // Below this width the card is a bottom sheet. Same value as .fw-map-card.
  var STACK_BP = {{STACKBP}};
  // FW_MAP$record_zoom. Never zooms back out.
  var RECORD_ZOOM = {{RECORDZOOM}};

  // One card and one detail dialog per document, so listeners don't multiply
  // on re-render. Built with DOM calls (no double quotes in an R string).
  var panel = document.getElementById('fw-map-detail');
  if (!panel) {
    panel = document.createElement('div');
    panel.id = 'fw-map-detail';
    panel.className = 'fw-map-detail';
    panel.hidden = true;

    var backdrop = document.createElement('div');
    backdrop.className = 'fw-map-detail__backdrop';
    backdrop.setAttribute('data-fw-dismiss', '');

    var dialog = document.createElement('div');
    dialog.className = 'fw-map-detail__dialog';
    dialog.setAttribute('role', 'dialog');
    dialog.setAttribute('aria-modal', 'true');
    dialog.setAttribute('aria-label', DETAIL_LABEL);
    dialog.setAttribute('tabindex', '-1');

    var panelClose = document.createElement('button');
    panelClose.type = 'button';
    panelClose.className = 'fw-map-detail__close';
    panelClose.setAttribute('aria-label', CLOSE_LABEL);
    panelClose.setAttribute('data-fw-dismiss', '');
    panelClose.innerHTML = '&times;';

    var panelInner = document.createElement('div');
    panelInner.className = 'fw-map-detail__body';

    dialog.appendChild(panelClose);
    dialog.appendChild(panelInner);
    panel.appendChild(backdrop);
    panel.appendChild(dialog);
    document.body.appendChild(panel);
  }
  var panelDialog = panel.querySelector('.fw-map-detail__dialog');
  var panelBody = panel.querySelector('.fw-map-detail__body');

  var card = document.getElementById('fw-map-card');
  if (!card) {
    card = document.createElement('div');
    card.id = 'fw-map-card';
    card.className = 'fw-map-card';
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-label', CARD_LABEL);
    card.hidden = true;
    var shutBtn = document.createElement('button');
    shutBtn.type = 'button';
    shutBtn.className = 'fw-map-card__close';
    shutBtn.setAttribute('aria-label', CLOSE_LABEL);
    shutBtn.innerHTML = '&times;';
    var cardBody = document.createElement('div');
    cardBody.className = 'fw-map-card__body';
    card.appendChild(shutBtn);
    card.appendChild(cardBody);
    document.body.appendChild(card);
  }
  var body = card.querySelector('.fw-map-card__body');

  function cancel() {
    if (card.fwTimer) { clearTimeout(card.fwTimer); card.fwTimer = null; }
  }
  function shut() {
    cancel();
    card.hidden = true;
    // Emptied, so no image request stays open.
    body.innerHTML = '';
    // Cleared, so a click can't open the previous marker's record.
    card.fwLayer = null;
    card.fwOpen = null;
  }
  function place(latlng) {
    // Narrow screens: pinned to the bottom as a sheet.
    if (window.matchMedia('(max-width: ' + STACK_BP + 'px)').matches) {
      card.style.left = '';
      card.style.top = '';
      card.classList.add('fw-map-card--sheet');
      return;
    }
    card.classList.remove('fw-map-card--sheet');
    var pt = map.latLngToContainerPoint(latlng);
    var box = map.getContainer().getBoundingClientRect();
    var mx = box.left + pt.x;
    var my = box.top + pt.y;
    var w = card.offsetWidth;
    var h = card.offsetHeight;
    var left = Math.min(mx - w / 2, window.innerWidth - w - 8);
    // A small gap, so the pointer can reach the card before it closes.
    var top = my - h - 12;                 // above the marker, clear of it
    if (top < 8) top = my + 14;            // no room above: below it instead
    if (top + h > window.innerHeight - 8) {
      top = Math.max(8, window.innerHeight - h - 8);
    }
    card.style.left = Math.max(8, left) + 'px';
    card.style.top = top + 'px';
  }
  // Fill by-reference photo slots from the dictionary. Always two tiles.
  function fillThumbs(root) {
    var slots = root.querySelectorAll('[data-fw-thumbs]');
    slots.forEach(function (slot) {
      var ids = slot.getAttribute('data-fw-thumbs').split('|');
      var html = '';
      for (var i = 0; i < 2; i++) {
        // An empty id is a role with no species: say not recorded.
        var fig = ids[i] ? (THUMBS[ids[i]] || THUMB_NONE) : THUMB_UNRECORDED;
        html += '<div class=\"fw-popup__figure-item\">' +
                '<p class=\"fw-popup-fig__role\">' + THUMB_ROLES[i] + '</p>' +
                fig + '</div>';
      }
      slot.innerHTML = html;
      slot.removeAttribute('data-fw-thumbs');
    });
  }
  function open(layer) {
    if (!layer.fwCard) return;
    cancel();
    // The card lives on <body>, so remember its marker, and this map's opener
    // (the card's click listener is shared by every map on the page).
    card.fwLayer = layer;
    card.fwOpen = panelOpen;
    body.innerHTML = layer.fwCard;
    fillThumbs(body);
    // Measured while invisible, so it doesn't flash at the old position.
    card.style.visibility = 'hidden';
    card.hidden = false;
    place(layer.getLatLng());
    card.style.visibility = '';
  }

  // ---- The detail panel ----------------------------------------------------

  function panelShut() {
    if (panel.hidden) return;
    panel.hidden = true;
    panelBody.innerHTML = '';
    // Return focus to where the reader was.
    if (panel.fwReturn && panel.fwReturn.focus) {
      panel.fwReturn.focus({ preventScroll: true });
    }
    panel.fwReturn = null;
  }

  // Show a record in the panel. The return focus is taken once.
  function panelMount(node) {
    shut();
    if (!panel.fwReturn) panel.fwReturn = document.activeElement;
    panelBody.innerHTML = '';
    panelBody.appendChild(node);
    panel.hidden = false;
    panelDialog.scrollTop = 0;
    panelDialog.focus({ preventScroll: true });
  }

  // Open from an embedded <template>. False if there is none (lazy mode).
  function panelOpenHtml(html) {
    var holder = document.createElement('div');
    holder.innerHTML = html;
    var tpl = holder.querySelector('template.fw-popup__detail');
    if (!tpl) return false;
    // Photos are requested only now, when the template is cloned.
    panelMount(tpl.content.cloneNode(true));
    return true;
  }

  // An open, empty panel with the busy badge while the server fetches the
  // record. Replaced by panelOpenDetail().
  function panelPending() {
    var node = document.createElement('div');
    node.className = 'fw-map-detail__pending';
    node.setAttribute('role', 'status');
    panelMount(node);
  }

  // Open a record the server sent (no template needed).
  function panelOpenDetail(html) {
    var holder = document.createElement('div');
    holder.innerHTML = html;
    if (!holder.firstElementChild) return false;
    panelMount(holder.firstElementChild);
    return true;
  }

  // Called by the fw-map-detail message handler in fw_client_script().
  panel.fwOpenHtml = panelOpenHtml;
  panel.fwOpenDetail = panelOpenDetail;

  // Zoom to the attempt being opened. Not in the saved report.
  function zoomTo(layer) {
    if (DETAIL_INPUT === '#' || !layer.getLatLng) return;
    map.setView(layer.getLatLng(), Math.max(map.getZoom(), RECORD_ZOOM),
                { animate: true });
  }

  function panelOpen(layer) {
    if (!layer.fwCard) return;
    zoomTo(layer);
    // Parsed from the stored string, since shut() empties the card.
    if (panelOpenHtml(layer.fwCard)) return;
    // Not embedded: ask the server by id.
    if (!DETAIL_INPUT) return;
    var holder = document.createElement('div');
    holder.innerHTML = layer.fwCard;
    var root = holder.querySelector('[data-fw-id]');
    if (!root) return;
    // Detailed report: scroll to the record card with this id.
    if (DETAIL_INPUT === '#') {
      var target = document.getElementById(root.getAttribute('data-fw-id'));
      shut();
      if (target) {
        target.scrollIntoView();
        if (target.focus) { target.setAttribute('tabindex', '-1'); target.focus({ preventScroll: true }); }
      }
      return;
    }
    if (!window.Shiny) return;
    shut();
    // Take the return focus before panelPending() moves it into the dialog.
    panel.fwReturn = document.activeElement;
    panelPending();
    Shiny.setInputValue(DETAIL_INPUT, root.getAttribute('data-fw-id'),
                        { priority: 'event' });
  }

  // Delegated: the panel's contents are replaced on every open.
  if (!panel.fwWired) {
    panel.fwWired = true;

    panel.addEventListener('click', function (e) {
      if (e.target.closest('[data-fw-dismiss]')) { panelShut(); return; }

      var btn = e.target.closest('[data-fw-step]');
      if (!btn) return;
      var fig = btn.closest('[data-fw-figure]');
      if (!fig) return;
      var slides = fig.querySelectorAll('[data-fw-slide]');
      if (slides.length < 2) return;
      var at = 0;
      slides.forEach(function (sl, i) { if (!sl.hidden) at = i; });
      var next = (at + parseInt(btn.getAttribute('data-fw-step'), 10) +
                  slides.length) % slides.length;
      slides.forEach(function (sl, i) { sl.hidden = (i !== next); });
      var count = fig.querySelector('[data-fw-count]');
      if (count) count.textContent = (next + 1) + ' / ' + slides.length;
    });

    document.addEventListener('keydown', function (e) {
      if (panel.hidden) return;
      if (e.key === 'Escape' || e.key === 'Esc') {
        // Captured first, so one Escape closes only the panel.
        e.stopPropagation();
        panelShut();
      }
    }, true);
  }

  // Wired once per page.
  if (!card.fwWired) {
    card.fwWired = true;
    card.addEventListener('mouseenter', cancel);
    card.addEventListener('mouseleave', shut);
    card.querySelector('.fw-map-card__close').addEventListener('click', shut);
    // Only the green button opens the record (client). Uses card.fwOpen, the
    // opener of the map that showed the card. See open().
    card.addEventListener('click', function (e) {
      if (!e.target.closest('[data-fw-open]')) return;
      if (card.fwLayer && card.fwOpen) card.fwOpen(card.fwLayer);
    });
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' || e.key === 'Esc') shut();
    });
    // A click anywhere else closes the card. Clicks inside a map are left to
    // Leaflet, or this capturing listener would close a card as it opens.
    document.addEventListener('click', function (e) {
      var el = e.target;
      if (card.contains(el)) return;
      if (el.closest && el.closest('.leaflet-container')) return;
      shut();
    }, true);
    // A fixed card can't follow the page, so it closes on scroll, except a
    // scroll inside the card itself.
    window.addEventListener('scroll', function (e) {
      if (e.target && e.target.nodeType === 1 && card.contains(e.target)) return;
      shut();
    }, true);
    window.addEventListener('resize', shut);
  }

  // A re-render is a new selection.
  shut();
  panelShut();

  // Move a marker's popup onto the card and panel. Idempotent.
  function wireOne(layer) {
    if (layer.fwWired || !layer.getPopup || !layer.getLatLng) return;
    var popup = layer.getPopup();
    if (!popup) return;
    layer.fwWired = true;
    layer.fwCard = popup.getContent();
    layer.unbindPopup();
    layer.on('mouseover', function (e) {
      var hovered = e.target;
      cancel();
      card.fwTimer = setTimeout(function () { open(hovered); }, OPEN_DELAY);
    });
    layer.on('mouseout', function () {
      cancel();
      card.fwTimer = setTimeout(shut, SHUT_DELAY);
    });
    // A click opens the full record. The flag stops the map click handler
    // (vector layer clicks bubble to the map) closing it straight away.
    layer.on('click', function (e) {
      if (e.originalEvent) { e.originalEvent.fwHandled = true; }
      panelOpen(e.target);
    });
  }

  // Every marker, now and later: the cluster group adds markers on a timer,
  // so also wire whatever it announces via layeradd.
  function wire(container) {
    container.eachLayer(function (layer) {
      if (layer.eachLayer) { wire(layer); return; }
      wireOne(layer);
    });
    if (container.fwListening) return;
    container.fwListening = true;
    container.on('layeradd', function (e) {
      if (!e.layer) return;
      if (e.layer.eachLayer) { wire(e.layer); return; }
      wireOne(e.layer);
    });
  }
  wire(map);

  // The card closes on pan or zoom; the modal panel doesn't.
  map.on('movestart', shut);
  map.on('zoomstart', shut);
  map.on('click', function (e) {
    if (e.originalEvent && e.originalEvent.fwHandled) return;   // it was a marker
    shut();
  });
}
"

#' The card script with this build's copy filled in
#'
#' A function so the copy is read at draw time, not source time.
#' @param detail_input the namespaced Shiny input a click writes an attempt id
#'   into, or NULL when every record is embedded in its marker
fw_map_card_js = function(detail_input = NULL) {
  # Escape for a single-quoted JS string.
  lit = function(x) {
    x = gsub("\\", "\\\\", x, fixed = TRUE)
    gsub("'", "\\'", x, fixed = TRUE)
  }
  js = gsub("{{CLOSE}}", lit(fw_t("species", "card_close")), FW_MAP_CARD_JS,
             fixed = TRUE)
  js = gsub("{{LABEL}}", lit(fw_t("species", "card_label")), js, fixed = TRUE)
  js = gsub("{{DETAIL}}", lit(fw_t("species", "detail_label")), js, fixed = TRUE)
  # Same breakpoint as the stylesheet.
  js = gsub("{{STACKBP}}", sub("px$", "", FW_BREAKPOINTS$sm), js, fixed = TRUE)
  js = gsub("{{RECORDZOOM}}", as.integer(FW_MAP$record_zoom), js, fixed = TRUE)
  gsub("{{INPUT}}", lit(detail_input %||% ""), js, fixed = TRUE)
}

#' Send the record behind a clicked marker or card (lazy mode)
#'
#' @param sel a reactive returning the current selection. Ids outside it are
#'   ignored, so a stale click after a rebuild shows nothing.
fw_map_detail_server = function(input, session, input_name, data, sel) {
  send = function(id) {
    if (!is.character(id) || length(id) != 1 || is.na(id)) return()
    s = sel()
    row = s[!is.na(s$attempt_id) & s$attempt_id == id, ]
    if (!nrow(row)) return()
    rec = fw_attempt_records(data, row)
    if (!nrow(rec)) return()
    session$sendCustomMessage(
      "fw-map-detail",
      list(html = fw_record_detail_html(rec[1, ], data$species))
    )
  }

  observeEvent(input[[input_name]], send(input[[input_name]]))
}

#' How overlapping markers are grouped
#'
#' Numbers in FW_MAP$cluster. Every group is a counted ring (.fw-cluster) so it
#' never looks like a single attempt; the title explains the number to screen
#' readers and on hover.
fw_cluster_options = function() {
  cl = FW_MAP$cluster
  # The copy lands inside a single-quoted JavaScript string.
  title = gsub("'", "\\'", fw_fill(fw_t("maps", "stack_title"), n = "{n}"), fixed = TRUE)
  leaflet::markerClusterOptions(
    showCoverageOnHover = FALSE,
    zoomToBoundsOnClick = TRUE,
    spiderfyOnMaxZoom = TRUE,
    spiderfyDistanceMultiplier = 1.5,
    spiderLegPolylineOptions = list(weight = 1.5, color = FW_COLOURS$ink_muted,
                                    opacity = 0.6),
    maxClusterRadius = htmlwidgets::JS(sprintf(
      "function (zoom) { return zoom >= %d ? %d : 0; }",
      cl$fine_zoom, cl$fine_radius)),
    iconCreateFunction = htmlwidgets::JS(sprintf(paste0(
      "function (c) {",
      "  var n = c.getChildCount();",
      "  var title = '%s'.replace('{n}', n);",
      "  return L.divIcon({ html: '<span title=\"' + title + '\" aria-label=\"' + title + '\">' + n + '</span>',",
      "                     className: 'fw-cluster', iconSize: [%d, %d] });",
      "}"),
      title, cl$icon_size, cl$icon_size))
  )
}

#' Attempt markers, coloured and labelled by outcome
#'
#' "embed": every record inside its marker (for saved files, no server).
#' "lazy": hover cards only, records fetched on click via
#'   fw_map_detail_server(); photos sent once as a dictionary. Much smaller.
#' "anchor": lazy, with clicks scrolling to the record cards in the Detailed
#'   report.
#'
#' @param live whether popups may reach Wikimedia for an uncached species
#' @param detail "embed", "lazy" or "anchor", as above
#' @param detail_input the namespaced input lazy mode reports clicks to,
#'   paired with fw_map_detail_server()
fw_add_attempt_markers = function(map, data, sel, live = FALSE,
                                   detail = c("embed", "lazy", "anchor"),
                                   detail_input = NULL) {
  detail = match.arg(detail)
  if (detail == "lazy" && is.null(detail_input)) {
    stop("detail = \"lazy\" needs detail_input, the input the map reports clicks to.",
         call. = FALSE)
  }
  # "#" is the card script's sentinel for anchor mode.
  if (detail == "anchor") {
    detail = "lazy"
    detail_input = "#"
  }
  pts = fw_map_points(data, sel)
  lazy = detail == "lazy"
  # Built in both modes: every hover card has thumbnails.
  thumbs = if (nrow(pts)) fw_map_thumb_cache(data, pts) else character(0)
  if (!nrow(pts)) {
    # Still wire the card script, which creates the record panel.
    v = FW_MAP$empty_view
    return(
      map |>
        leaflet::setView(v$lng, v$lat, zoom = v$zoom) |>
        fw_map_card_render(if (lazy) detail_input)
    )
  }

  map |>
    fw_add_marker_layer(data, pts, live = live, detail = detail,
                        thumbs = thumbs) |>
    fw_add_outcome_legend() |>
    fw_map_card_render(if (lazy) detail_input, if (lazy) thumbs)
}

#' The markers for a set of located attempts, and the view that fits them
#'
#' Split out so Explore can send it through leafletProxy().
#'
#' @param pts fw_map_points() output, at least one row
#' @param thumbs fw_map_thumb_cache() output covering `pts`. In lazy mode the
#'   map's dictionary must cover them (see fw_map_card_render()).
fw_add_marker_layer = function(map, data, pts, live = FALSE,
                                detail = c("embed", "lazy"), thumbs = NULL) {
  detail = match.arg(detail)
  outcome = ifelse(is.na(pts$outcome), "Unknown", pts$outcome)
  figure_cache = if (detail == "embed") fw_map_figure_cache(data, pts) else NULL
  popups = vapply(seq_len(nrow(pts)), function(i) {
    fw_map_popup(pts[i, ], data$species, live = live, detail = detail,
                 figure_cache = figure_cache, thumbs = thumbs,
                 thumb_ref = detail == "lazy")
  }, character(1))

  map |>
    leaflet::addCircleMarkers(
      lng = pts$longitude, lat = pts$latitude,
      radius = FW_MAP$marker$radius, weight = FW_MAP$marker$weight,
      opacity = FW_MAP$marker$opacity, fillOpacity = FW_MAP$marker$fill_opacity,
      color = FW_COLOURS$surface,
      fillColor = unname(FW_OUTCOME_COLOURS[outcome]),
      clusterOptions = fw_cluster_options(),
      # No tooltip: the hover card covers it.
      popup = popups,
      # Only used without JavaScript.
      popupOptions = leaflet::popupOptions(maxWidth = FW_MAP$popup$max_width,
                                           minWidth = FW_MAP$popup$min_width,
                                           className = "fw-popup-wrap")
    ) |>
    fw_fit_points(pts)
}

#' Fit the view to a set of points, capped at FW_MAP$fit_max_zoom
#'
#' The cap stops a single-coordinate selection fitting at infinite zoom.
fw_fit_points = function(map, pts) {
  leaflet::fitBounds(map, min(pts$longitude), min(pts$latitude),
                     max(pts$longitude), max(pts$latitude),
                     options = list(maxZoom = FW_MAP$fit_max_zoom))
}

#' The outcome legend
fw_add_outcome_legend = function(map) {
  leaflet::addLegend(
    map, position = "bottomright", colors = unname(FW_OUTCOME_COLOURS),
    labels = names(FW_OUTCOME_COLOURS), opacity = FW_MAP$legend_opacity,
    title = fw_t("species", "p_outcome")
  )
}

#' Install the card script, with the photo dictionary if there is one
#'
#' The dictionary goes in onRender()'s data, sent once with the widget; proxy
#' marker swaps reuse it, so it must cover them (Explore passes
#' fw_map_thumbs_all()).
#'
#' @param detail_input as for fw_map_card_js()
#' @param thumbs fw_map_thumb_cache() output, or NULL for cards with their
#'   figures inline
fw_map_card_render = function(map, detail_input = NULL, thumbs = NULL) {
  js = fw_map_card_js(detail_input)
  if (is.null(thumbs)) return(htmlwidgets::onRender(map, js))
  esc = htmltools::htmlEscape
  htmlwidgets::onRender(map, js, data = list(
    # A list, so it arrives as an object even when empty or length one.
    thumbs = as.list(thumbs),
    none = fw_popup_thumb_none(),
    unrecorded = fw_popup_thumb_unrecorded(),
    roles = c(esc(fw_t("species", "fig_invasive")),
              esc(fw_t("species", "fig_beneficiary")))
  ))
}

#' Hover-card photos for the whole database, built once per process
#'
#' Explore's map uses these so any later proxy selection is covered.
fw_map_thumbs_all = local({
  cache = NULL
  function(data) {
    if (is.null(cache) || !identical(cache$data, data)) {
      cache <<- list(data = data,
                     thumbs = fw_map_thumb_cache(data, fw_map_points(data, data$attempt)))
    }
    cache$thumbs
  }
})
