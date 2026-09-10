# maps.R
# Every map in the app is built through here, so the basemap, the marker style
# and the popup cannot drift between the report builder and the dashboard.
#
# PROJECTION. Leaflet renders raster tiles in Web Mercator and nothing else -
# every keyless provider serves that projection only. That is acceptable for the
# POINT maps, where the user is zooming in to look at one site and Mercator's
# area distortion says nothing false about a dot. It is NOT acceptable for a
# country choropleth: in Mercator, Greenland and Canada shout while Africa and
# Indonesia whisper, which inverts the gap story the landing page is for. Those
# views get fw_equal_earth_map() instead, which drops tiles entirely and draws a
# countries GeoJSON in Equal Earth.
#
# BASEMAP. The grey Esri canvas was replaced on client feedback that it read as
# drab. Carto's Voyager is the default: colourful, minimal, and it draws
# waterbodies clearly, which matters more here than roads.
#
# ON THE "CARTO WATERMARKS KEYLESS TILES" NOTE - RESOLVED, KEY IN HAND. Carto
# began drawing "API KEY REQUIRED / carto.com/basemaps/apikey" diagonally across
# every keyless raster tile, and because the response was still a 200 PNG, a
# status-code check did not catch it. The client has since supplied a Basemaps
# key, set as the default below, and the watermark is gone.
#
# IF THE WATERMARK COMES BACK, look at the pixels rather than the status code,
# then check three things in order: the key is still valid on the client's Carto
# account; the query parameter is still `key` (Carto's raster service names it
# that - `api_key` is silently ignored and you get the watermark back); and your
# browser and Carto's CDN are not both serving a cached tile from before the fix,
# which a force-refresh clears.
#
# Attribution is a licence condition with or without a key, so the attribution
# control stays on. See HANDOVER.md section 5.5.

library(shiny)

# THE ONE PLACE THE CARTO KEY IS WRITTEN DOWN. Every Carto tile URL in the app
# is built by fw_carto_url(), so a key change is this line and nothing else.
#
# In the source rather than in a secret store on purpose: a Basemaps key travels
# to the browser inside every tile request, so it is public the moment the app
# serves a map and treating it as a secret would buy nothing. Carto scopes it by
# domain instead. FWISE_CARTO_KEY still overrides it, so a redeploy can move to a
# new key without a code change.
FW_CARTO_KEY <- fw_env("FWISE_CARTO_KEY",
                       default = "cb1_3fo4_1_c9fdd55287ad4682f074548a")

#' A Carto raster tile URL for one basemap style
#'
#' The parameter is `key`. Carto's raster service ignores `api_key` without
#' erroring and serves watermarked tiles instead, which is the failure this
#' function exists to make un-repeatable.
fw_carto_url <- function(style) {
  base <- paste0("https://{s}.basemaps.cartocdn.com/rastertiles/", style,
                 "/{z}/{x}/{y}{r}.png")
  if (is.null(FW_CARTO_KEY) || !nzchar(FW_CARTO_KEY)) {
    return(base)
  }
  paste0(base, "?key=", FW_CARTO_KEY)
}

FW_CARTO_ATTRIB <- paste(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# Named so the layer control reads as a question the user might actually ask,
# rather than as a list of vendors.
FW_BASEMAP_PLAIN  <- "Plain"
FW_BASEMAP_WATER  <- "Water"
FW_BASEMAP_TERRAIN <- "Terrain"
FW_BASEMAP_SATELLITE <- "Satellite"

#' The basemap stack and its layer control
#'
#' Four grounds, one visible at a time. Terrain is not decoration: the client's
#' metrics framework asks for a topographic option specifically so a reader can
#' eyeball whether a waterbody is hydrologically isolated, which is the single
#' biggest predictor of whether an eradication holds.
#'
#' @param overlays names of overlay groups to list in the control, if any
fw_add_basemaps <- function(map, overlays = NULL) {
  map |>
    leaflet::addTiles(
      urlTemplate = fw_carto_url("voyager_nolabels"),
      attribution = FW_CARTO_ATTRIB,
      group = FW_BASEMAP_PLAIN,
      options = leaflet::tileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.OceanBasemap", group = FW_BASEMAP_WATER,
      options = leaflet::providerTileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap", group = FW_BASEMAP_TERRAIN,
      options = leaflet::providerTileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldImagery", group = FW_BASEMAP_SATELLITE,
      options = leaflet::providerTileOptions(noWrap = FALSE)
    ) |>
    # Place labels ride ABOVE the data, so a marker never hides the name of the
    # lake it sits on. Carto serves them as their own transparent layer.
    leaflet::addTiles(
      urlTemplate = fw_carto_url("voyager_only_labels"),
      attribution = "",
      options = leaflet::tileOptions(noWrap = FALSE, zIndex = 650)
    ) |>
    leaflet::addLayersControl(
      baseGroups = c(FW_BASEMAP_PLAIN, FW_BASEMAP_WATER,
                     FW_BASEMAP_TERRAIN, FW_BASEMAP_SATELLITE),
      overlayGroups = overlays,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}

#' A map that sizes itself to the viewport
#'
#' The height is a clamp() in CSS rather than an argument, because leaflet's
#' leafletOutput() runs its height through htmltools::validateCssUnit(), which
#' accepts only a bare number, a percentage or a single unit - it rejects
#' clamp(), min(), max() and calc() outright. So the wrapper carries the
#' responsive height and the widget fills it.
fw_map_output <- function(output_id, class = NULL) {
  div(
    class = paste(c("fw-map", class), collapse = " "),
    leaflet::leafletOutput(output_id, width = "100%", height = "100%")
  )
}

# ---- Markers and popups ------------------------------------------------------

#' Everything a popup needs, joined once for the whole selection
#'
#' Built as a table rather than looked up per marker: 914 markers each running
#' their own filter over the bridge tables is the difference between a map that
#' opens and one that hangs.
fw_map_points <- function(data, sel) {
  pts <- sel[!is.na(sel$latitude) & !is.na(sel$longitude), ]
  if (!nrow(pts)) return(pts)

  species <- fw_species_label(data$species)
  inv <- data$attempt_species |>
    dplyr::filter(role == "invasive", attempt_id %in% pts$attempt_id) |>
    dplyr::left_join(dplyr::select(species, species_id, label), by = "species_id")

  # The FIRST invasive species carries the photograph. An attempt against four
  # species cannot show four pictures in a popup, and the full list is named in
  # text directly underneath, so nothing is hidden by the choice.
  lead <- inv |>
    dplyr::group_by(attempt_id) |>
    dplyr::summarise(
      lead_species_id = dplyr::first(species_id),
      lead_species = dplyr::first(label),
      all_species = paste(unique(label), collapse = ", "),
      .groups = "drop"
    )

  contacts <- dplyr::select(data$contact, attempt_contact_id = contact_id,
                            contact_name)

  pts |>
    dplyr::left_join(lead, by = "attempt_id") |>
    dplyr::left_join(
      dplyr::rename(contacts, primary_contact_id = attempt_contact_id,
                    primary_contact_name = contact_name),
      by = "primary_contact_id"
    )
}

#' The popup for one attempt
#'
#' WHAT THE CLIENT ASKED FOR: the person behind the record, the species it
#' targeted, and a picture of that species. The first two are the point of
#' FWISE - a marker that names someone you can write to is a next step, where a
#' coloured dot is only a statistic.
#'
#' Attribution for the credit: the contact where there is one, otherwise the
#' published reference, because 888 of 914 attempts have a reference and only
#' 721 have a named contact. Email NEVER appears here - redaction happens
#' upstream in fw_contacts_summary() and this must not reach around it.
fw_map_popup <- function(row, species_tbl, live = FALSE) {
  esc <- htmltools::htmlEscape
  line <- function(label, value) {
    if (is.na(value) || !nzchar(as.character(value))) return("")
    paste0('<div class="fw-popup__row"><span class="fw-popup__key">',
           esc(label), '</span><span class="fw-popup__val">',
           esc(as.character(value)), "</span></div>")
  }

  figure <- if (!is.na(row$lead_species_id)) {
    fw_species_figure_for(species_tbl, row$lead_species_id,
                          row$lead_species, live = live)
  } else {
    fw_species_figure(NULL, NA)
  }

  outcome <- if (is.na(row$outcome)) "Unknown" else row$outcome
  attribution <- row$primary_contact_name
  if (is.na(attribution) || !nzchar(attribution)) attribution <- row$reference

  paste0(
    '<div class="fw-popup">',
    figure,
    '<h3 class="fw-popup__title">',
    esc(row$site_name %|na|% fw_t("species", "unnamed_site")),
    "</h3>",
    line(fw_t("species", "p_country"), row$country),
    line(fw_t("species", "p_species"), row$all_species),
    # The outcome is words as well as colour, so it never depends on the dot.
    line(fw_t("species", "p_outcome"), outcome),
    line(fw_t("species", "p_began"), row$start_year),
    line(fw_t("species", "p_recorded_by"), attribution),
    "</div>"
  )
}

# ---- The marker card ---------------------------------------------------------

# WHY THE RECORD IS NOT SHOWN IN A LEAFLET POPUP ANY MORE.
#
# Two complaints, one cause. A popup opened on CLICK, which is a poor fit for a
# map somebody is scanning; and a popup is a child of .leaflet-container, which
# clips its children, so a record taller than the map - a photograph and five
# rows, against a map that is 320px tall on a laptop - had its bottom cut off.
# autoPan cannot help: there is no position inside the map that it fits.
#
# So the record is drawn in ONE fixed-position card on <body>, opened on hover
# and placed against the marker by this script. position:fixed puts it outside
# the reach of the map's clipping and of every other overflow on the page, which
# is the whole point of moving it out.
#
# THE CONTENT STILL TRAVELS AS A POPUP. addCircleMarkers() below still passes
# `popup =`, and this script lifts the HTML off each marker and unbinds it. That
# keeps all the popup-building in R where it belongs, keeps Leaflet from also
# opening its own clipped copy on click, and means a browser that never runs
# this script still gets working popups rather than markers that do nothing.
#
# HOVER IS CHEAP HERE, which is why it is safe to open on it. Every card's HTML
# was built server-side with the map, so opening one is a single innerHTML - no
# request, no layout of 914 anything. The only network cost is the photograph,
# and a 120ms hover intent keeps a pointer crossing the map from asking for a
# dozen of them. Tapping still works, for touch, where there is no hover at all.
FW_MAP_CARD_JS <- "
function (el, x) {
  var map = this;
  var CLOSE_LABEL = '{{CLOSE}}';
  var CARD_LABEL = '{{LABEL}}';
  var OPEN_DELAY = 120;   // long enough that crossing a marker is not opening it
  var SHUT_DELAY = 260;   // long enough to cross the gap into the card

  // ONE card for the document. Only one map is ever on screen - they are on
  // different tabs - and a single element stops the listeners below from
  // multiplying every time Shiny re-renders a map.
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
    // Emptied rather than just hidden, so a card nobody is looking at is not
    // still holding a Wikimedia image request open.
    body.innerHTML = '';
  }
  function place(latlng) {
    var pt = map.latLngToContainerPoint(latlng);
    var box = map.getContainer().getBoundingClientRect();
    var mx = box.left + pt.x;
    var my = box.top + pt.y;
    var w = card.offsetWidth;
    var h = card.offsetHeight;
    var left = Math.min(mx - w / 2, window.innerWidth - w - 8);
    // The gap has to be small: the reader crosses it with the pointer to reach
    // the card, and every pixel of it is time the card is counting down to
    // close. Just clear of a 6px marker and its halo.
    var top = my - h - 12;                 // above the marker, clear of it
    if (top < 8) top = my + 14;            // no room above: below it instead
    if (top + h > window.innerHeight - 8) {
      top = Math.max(8, window.innerHeight - h - 8);
    }
    card.style.left = Math.max(8, left) + 'px';
    card.style.top = top + 'px';
  }
  function open(layer) {
    if (!layer.fwCard) return;
    cancel();
    body.innerHTML = layer.fwCard;
    // Measured while still invisible, so it never flashes at the last marker's
    // position on its way to this one's.
    card.style.visibility = 'hidden';
    card.hidden = false;
    place(layer.getLatLng());
    card.style.visibility = '';
  }

  // Wired once for the life of the page: these listeners are about the card and
  // the window, not about any particular map.
  if (!card.fwWired) {
    card.fwWired = true;
    card.addEventListener('mouseenter', cancel);
    card.addEventListener('mouseleave', shut);
    card.querySelector('.fw-map-card__close').addEventListener('click', shut);
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' || e.key === 'Esc') shut();
    });
    // Anywhere else on the page - a navbar tab, a filter, a chart - closes it.
    // Without this a card opened from the map is still floating over whatever
    // the reader moved on to. Clicks INSIDE a map are left alone: Leaflet's own
    // handlers below decide those, and this listener captures, so it would
    // otherwise close a card in the same click that opened it.
    document.addEventListener('click', function (e) {
      var el = e.target;
      if (card.contains(el)) return;
      if (el.closest && el.closest('.leaflet-container')) return;
      shut();
    }, true);
    // A fixed card cannot follow the page under it, so it leaves rather than
    // drifting away from the marker it belongs to. Capture, because the scroll
    // that matters may be inside a pane rather than on the window.
    window.addEventListener('scroll', function (e) {
      // A scroll INSIDE the card is the reader working their way down a long
      // record, not the page moving out from under it. Without this test the
      // capturing listener closed the card the moment it was scrolled.
      if (e.target && e.target.nodeType === 1 && card.contains(e.target)) return;
      shut();
    }, true);
    window.addEventListener('resize', shut);
  }

  // A re-render is a new selection; nothing of the last one should still be up.
  shut();

  function wire(container) {
    container.eachLayer(function (layer) {
      if (layer.eachLayer) { wire(layer); return; }
      if (!layer.getPopup || !layer.getLatLng) return;
      var popup = layer.getPopup();
      if (!popup) return;
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
      // Touch has no hover; a tap arrives here. The flag is not decoration: a
      // click on a VECTOR layer fires on the layer AND then on the map, unlike
      // a click on a marker, so without it the map handler below shut the card
      // in the same gesture that opened it and a tap did nothing at all.
      layer.on('click', function (e) {
        if (e.originalEvent) { e.originalEvent.fwHandled = true; }
        open(e.target);
      });
    });
  }
  wire(map);

  map.on('movestart', shut);
  map.on('zoomstart', shut);
  map.on('click', function (e) {
    if (e.originalEvent && e.originalEvent.fwHandled) return;   // it was a marker
    shut();
  });
}
"

#' The card script with this build's copy in it
#'
#' A function rather than a constant so the labels are read when a map is drawn
#' rather than when this file is sourced, which keeps R/ free of load-order
#' rules between copy.R and here.
fw_map_card_js <- function() {
  # The labels land inside a single-quoted JavaScript string, so both a
  # backslash and an apostrophe in the copy would break the script.
  lit <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    gsub("'", "\\'", x, fixed = TRUE)
  }
  js <- gsub("{{CLOSE}}", lit(fw_t("species", "card_close")), FW_MAP_CARD_JS,
             fixed = TRUE)
  gsub("{{LABEL}}", lit(fw_t("species", "card_label")), js, fixed = TRUE)
}

#' Attempt markers, coloured and labelled by outcome
#'
#' @param live whether popups may reach Wikimedia for an uncached species
fw_add_attempt_markers <- function(map, data, sel, live = FALSE) {
  pts <- fw_map_points(data, sel)
  if (!nrow(pts)) return(leaflet::setView(map, 0, 20, zoom = 2))

  outcome <- ifelse(is.na(pts$outcome), "Unknown", pts$outcome)
  popups <- vapply(seq_len(nrow(pts)), function(i) {
    fw_map_popup(pts[i, ], data$species, live = live)
  }, character(1))

  map |>
    leaflet::addCircleMarkers(
      lng = pts$longitude, lat = pts$latitude,
      radius = 6, weight = 1.5, opacity = 1, fillOpacity = 0.75,
      color = "#ffffff",
      fillColor = unname(FW_OUTCOME_COLOURS[outcome]),
      # NO TOOLTIP. There used to be one naming the site and its outcome,
      # because a record you had to click for was no use to somebody scanning
      # the map. The card now opens on hover and says all of that and more, so a
      # tooltip would only be a smaller duplicate fighting it for the same
      # patch of screen directly above the marker.
      popup = popups,
      # These options only apply if fw_map_card_js() never runs, which is the
      # no-JavaScript case; with it, the popup is unbound and the card is what
      # opens. Kept in step with the card's width all the same.
      popupOptions = leaflet::popupOptions(maxWidth = 320, minWidth = 260,
                                           className = "fw-popup-wrap")
    ) |>
    leaflet::addLegend(
      position = "bottomright", colors = unname(FW_OUTCOME_COLOURS),
      labels = names(FW_OUTCOME_COLOURS), opacity = 0.85,
      title = fw_t("species", "p_outcome")
    ) |>
    leaflet::fitBounds(min(pts$longitude), min(pts$latitude),
                       max(pts$longitude), max(pts$latitude)) |>
    htmlwidgets::onRender(fw_map_card_js())
}
