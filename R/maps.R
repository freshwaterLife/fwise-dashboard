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

# The layer control's names come from fw_t("maps", "basemap_*"): named so it
# reads as a question the user might ask rather than as a list of vendors.

#' The basemap stack and its layer control
#'
#' Four grounds, one visible at a time. Terrain is not decoration: the client's
#' metrics framework asks for a topographic option specifically so a reader can
#' eyeball whether a waterbody is hydrologically isolated, which is the single
#' biggest predictor of whether an eradication holds.
#'
#' @param overlays names of overlay groups to list in the control, if any
fw_add_basemaps <- function(map, overlays = NULL) {
  plain     <- fw_t("maps", "basemap_plain")
  water     <- fw_t("maps", "basemap_water")
  terrain   <- fw_t("maps", "basemap_terrain")
  satellite <- fw_t("maps", "basemap_satellite")
  map |>
    leaflet::addTiles(
      urlTemplate = fw_carto_url("voyager_nolabels"),
      attribution = FW_CARTO_ATTRIB,
      group = plain,
      options = leaflet::tileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.OceanBasemap", group = water,
      options = leaflet::providerTileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldTopoMap", group = terrain,
      options = leaflet::providerTileOptions(noWrap = FALSE)
    ) |>
    leaflet::addProviderTiles(
      "Esri.WorldImagery", group = satellite,
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
      baseGroups = c(plain, water, terrain, satellite),
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

# Multi-value popup fields travel as one delimited string per attempt, because
# these are data frame columns and a list column cannot survive the row-at-a-time
# indexing in fw_add_attempt_markers(). A pipe, because it appears in no species
# name, method name or note in the data.
FW_POPUP_SEP <- "|"

#' Everything a popup needs, joined once for the whole selection
#'
#' Built as a table rather than looked up per marker: 914 markers each running
#' their own filter over the bridge tables is the difference between a map that
#' opens and one that hangs.
fw_map_points <- function(data, sel) {
  pts <- sel[!is.na(sel$latitude) & !is.na(sel$longitude), ]
  # RETURNS A NARROWER FRAME WHEN EMPTY: none of the joins below have run, so
  # the popup columns are absent rather than present-and-empty. Every caller has
  # to test nrow() before touching a popup column, which fw_add_attempt_markers()
  # does - it is the only caller, and it returns a bare map at this point.
  if (!nrow(pts)) return(pts)

  species <- fw_species_label(data$species)

  # Species for one role, gathered per attempt. EVERY id is kept, not just the
  # first: the detail panel shows the photographs of all of them behind a pair
  # of arrows, so the popup can no longer be built from a single lead species.
  # The ids travel as one delimited string because this is a data frame column,
  # and are split again in fw_map_detail_html().
  role_cols <- function(role_name, prefix) {
    out <- data$attempt_species |>
      dplyr::filter(role == role_name, attempt_id %in% pts$attempt_id) |>
      dplyr::distinct(attempt_id, species_id) |>
      dplyr::left_join(dplyr::select(species, species_id, label),
                       by = "species_id") |>
      dplyr::filter(!is.na(label)) |>
      dplyr::group_by(attempt_id) |>
      dplyr::summarise(
        ids   = paste(species_id, collapse = FW_POPUP_SEP),
        names = paste(label, collapse = FW_POPUP_SEP),
        list  = paste(label, collapse = ", "),
        .groups = "drop"
      )
    # Renamed afterwards rather than with tidy-eval in summarise(), so this file
    # needs nothing attached beyond dplyr:: itself.
    names(out)[-1] <- paste0(prefix, "_", names(out)[-1])
    out
  }

  # Methods WITH THEIR OWN NOTES, in the order they were applied. Deliberately
  # not fw_export_frame()'s method_notes, which collapses the notes across
  # methods with unique() and loses which note belongs to which method - the
  # pairing is the useful part ("Rotenone - Betamax vet.").
  methods <- data$attempt_method |>
    dplyr::filter(attempt_id %in% pts$attempt_id) |>
    dplyr::left_join(dplyr::select(data$method, method_id, method_name),
                     by = "method_id") |>
    dplyr::arrange(attempt_id, method_order) |>
    dplyr::group_by(attempt_id) |>
    dplyr::summarise(
      method_list = paste(unique(method_name[!is.na(method_name)]),
                          collapse = ", "),
      method_pairs = paste(
        ifelse(is.na(method_notes) | !nzchar(method_notes),
               method_name,
               paste0(method_name, " - ", method_notes)),
        collapse = FW_POPUP_SEP
      ),
      .groups = "drop"
    )

  # EMAIL PASSES THROUGH THE email_public GATE AND NOTHING ELSE. Same expression
  # as fw_export_frame(); see the note above fw_map_detail_html() for why an
  # address is shown here at all.
  contacts <- data$contact |>
    dplyr::transmute(
      contact_id,
      contact_name,
      contact_email = dplyr::if_else(email_public, contact_email,
                                     NA_character_),
      organisation
    )

  pts |>
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
}

# ---- Popup building ----------------------------------------------------------
#
# TWO VIEWS OF ONE RECORD, BUILT TOGETHER AND SHIPPED AS ONE STRING.
#
# The hover card answers "is this one worth stopping for" - species, what was
# done, what happened, who recorded it. The detail panel answers everything
# else, and only opens when the reader asks for it by clicking.
#
# Both halves travel in the same `popup =` string, with the detail inside a
# hidden div. That is what keeps every field of popup-building in R: Leaflet
# gives one content slot per marker, and a second channel would mean a parallel
# vector threaded through addCircleMarkers() and onRender() by index. The hidden
# div costs nothing in the card - it is never displayed there - and the script
# parses it back out of the stored string when a marker is clicked.

# A pipe-delimited popup field back into its parts. Empty and NA both mean none.
fw_popup_parts <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(as.character(x))) return(character(0))
  strsplit(as.character(x), FW_POPUP_SEP, fixed = TRUE)[[1]]
}

# One label/value row. Returns "" for an absent value, so a row a record does
# not have simply is not drawn.
fw_popup_row <- function(label, value, html = FALSE) {
  esc <- htmltools::htmlEscape
  if (length(value) != 1 || is.na(value) || !nzchar(as.character(value))) {
    return("")
  }
  paste0('<div class="fw-popup__row"><span class="fw-popup__key">',
         esc(label), '</span><span class="fw-popup__val">',
         if (html) as.character(value) else esc(as.character(value)),
         "</span></div>")
}

#' The years an attempt ran, as one phrase
#'
#' "1998-2004 (6 years)" rather than a bare start year. 144 of 914 attempts have
#' no end year, and a good part of those are still running, so a missing end is
#' shown as the start year alone - never as a dash to nowhere, which reads as
#' data that should be there and is not.
fw_popup_years <- function(start, end) {
  if (is.na(start)) return(NA_character_)
  # An end year that is the same as the start, or earlier than it, is not a
  # range. "2000-2000 (0 years)" reads as a fault in the data rather than as a
  # campaign that began and finished inside one year.
  if (is.na(end) || end <= start) return(as.character(start))
  n <- round(end - start)
  unit <- if (n == 1) fw_t("maps", "year_one") else fw_t("maps", "year_many")
  paste0(start, "-", end, " (", n, " ", unit, ")")
}

#' The hover card: enough to decide whether to open the record
#'
#' NO PHOTOGRAPH. Hovering is cheap precisely because opening a card is one
#' innerHTML and no request; a picture in every card would put a Wikimedia fetch
#' behind every pointer that crosses the map.
fw_map_hover_html <- function(row) {
  esc <- htmltools::htmlEscape
  outcome <- if (is.na(row$outcome)) "Unknown" else row$outcome
  recorded <- row$primary_contact_name
  if (is.na(recorded) || !nzchar(recorded)) recorded <- row$reference

  paste0(
    '<div class="fw-popup">',
    '<h3 class="fw-popup__title">',
    esc(row$site_name %|na|% fw_t("species", "unnamed_site")),
    "</h3>",
    fw_popup_row(fw_t("species", "p_country"), row$country),
    fw_popup_row(fw_t("species", "p_species"), row$inv_list),
    fw_popup_row(fw_t("species", "p_beneficiary"), row$ben_list),
    fw_popup_row(fw_t("species", "p_method"), row$method_list),
    # The outcome is words as well as colour, so it never depends on the dot.
    fw_popup_row(fw_t("species", "p_outcome"), outcome),
    fw_popup_row(fw_t("species", "p_recorded_by"), recorded),
    '<p class="fw-popup__more">', esc(fw_t("species", "more_hint")), "</p>",
    "</div>"
  )
}

#' The detail panel: the whole record, opened on click
#'
#' WHY AN EMAIL ADDRESS APPEARS HERE. It did not used to, and the comment that
#' stood here said it never would. The client asked for a contact you can write
#' to from the map, on the reasoning that a marker naming somebody reachable is
#' a next step where a coloured dot is only a statistic - which is the point of
#' the networking side of FWISE.
#'
#' The email_public gate is NOT reached around: fw_map_points() applies exactly
#' the same expression fw_export_frame() does, and a contact who has not made
#' their address public arrives here as NA and is rendered as a plain name.
#'
#' The trade-off this accepts is that a published address is scrapeable. The app
#' has a handler that avoids it - fw-mailto in ui_helpers.R assembles the
#' address client-side so it never enters the DOM - but that needs a Shiny
#' binding, and this card's content is injected with innerHTML and never bound.
#' If the client would rather have obfuscation than a working link, that is the
#' thing to change, not the gate.
fw_map_detail_html <- function(row, species_tbl, live = FALSE) {
  esc <- htmltools::htmlEscape

  # A labelled column of photographs for one role. Several species become
  # several slides behind a pair of arrows; the label under each says which
  # animal is which, so the pictures are never carrying the identification on
  # their own.
  figures <- function(ids, names, role_label) {
    ids <- fw_popup_parts(ids)
    names <- fw_popup_parts(names)
    slides <- if (!length(ids)) {
      paste0('<div class="fw-popup-fig__slide" data-fw-slide="0">',
             fw_species_figure(NULL, NA), "</div>")
    } else {
      paste0(vapply(seq_along(ids), function(i) {
        paste0('<div class="fw-popup-fig__slide" data-fw-slide="', i - 1L, '"',
               if (i > 1L) " hidden" else "", ">",
               fw_species_figure_for(species_tbl, ids[i], names[i],
                                     live = live),
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

  # A contact, as a mailto where the address is public and plain text where it
  # is not. Never an empty link.
  person <- function(name, email, org) {
    if (is.na(name) || !nzchar(name)) return(NA_character_)
    who <- if (!is.na(email) && nzchar(email)) {
      paste0('<a href="mailto:', esc(email), '">', esc(name), "</a>")
    } else {
      esc(name)
    }
    if (!is.na(org) && nzchar(org)) paste0(who, ", ", esc(org)) else who
  }

  methods <- fw_popup_parts(row$method_pairs)
  outcome <- if (is.na(row$outcome)) "Unknown" else row$outcome
  area <- if (!is.na(row$area_treated)) {
    paste(format(row$area_treated, big.mark = ",", trim = TRUE),
          row$area_unit %|na|% "")
  } else NA_character_

  # A <template>, NOT a hidden div, and this is not a style preference.
  # display:none does not stop a browser fetching an <img src>: a hidden div
  # here meant every hover card quietly pulled up to eight Wikimedia
  # photographs that nobody was going to look at, which is the exact cost the
  # hover card exists to avoid. Template content is inert - parsed, never
  # rendered, nothing fetched - until the script clones it on click.
  paste0(
    '<template class="fw-popup__detail">',
    '<div class="fw-popup-detail">',
    '<h2 class="fw-popup-detail__title">',
    esc(row$site_name %|na|% fw_t("species", "unnamed_site")),
    "</h2>",
    '<p class="fw-popup-detail__place">',
    esc(paste(stats::na.omit(c(row$region, row$country)), collapse = ", ")),
    "</p>",

    # Invasive left, beneficiary right, both labelled.
    '<div class="fw-popup-detail__figures">',
    figures(row$inv_ids, row$inv_names, fw_t("species", "p_species")),
    figures(row$ben_ids, row$ben_names, fw_t("species", "p_beneficiary")),
    "</div>",

    '<div class="fw-popup-detail__rows">',
    fw_popup_row(fw_t("species", "p_outcome"), outcome),
    fw_popup_row(fw_t("species", "p_verified"), row$verification_method),
    fw_popup_row(fw_t("species", "p_verified_notes"), row$verification_notes),
    fw_popup_row(fw_t("species", "p_began"),
                 fw_popup_years(row$start_year, row$end_year)),
    fw_popup_row(
      fw_t("species", "p_method"),
      if (length(methods)) {
        paste0("<ul class=\"fw-popup__list\"><li>",
               paste(esc(methods), collapse = "</li><li>"), "</li></ul>")
      } else NA_character_,
      html = TRUE
    ),
    fw_popup_row(fw_t("species", "p_method_desc"), row$method_description),
    fw_popup_row(fw_t("species", "p_waterbody"), row$waterbody_type),
    fw_popup_row(fw_t("species", "p_area"), area),
    fw_popup_row(fw_t("species", "p_driver"), row$driver),
    fw_popup_row(fw_t("species", "p_recorded_by"),
                 person(row$primary_contact_name, row$primary_contact_email,
                        row$primary_contact_org),
                 html = TRUE),
    fw_popup_row(fw_t("species", "p_also"),
                 person(row$secondary_contact_name,
                        row$secondary_contact_email,
                        row$secondary_contact_org),
                 html = TRUE),
    fw_popup_row(fw_t("species", "p_reference"), row$reference),
    # 432 of 914 attempts have a link. Where there is none the reference above
    # stands on its own rather than a button going nowhere.
    if (!is.na(row$reference_link) && nzchar(row$reference_link)) {
      paste0('<p class="fw-popup-detail__link"><a href="',
             esc(row$reference_link),
             '" target="_blank" rel="noopener noreferrer">',
             esc(fw_t("species", "p_read_source")), "</a></p>")
    } else "",
    "</div>",
    "</div>",
    "</template>"
  )
}

#' The popup for one attempt: the hover card with the detail panel inside it
fw_map_popup <- function(row, species_tbl, live = FALSE) {
  paste0(fw_map_hover_html(row), fw_map_detail_html(row, species_tbl, live))
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
  var DETAIL_LABEL = '{{DETAIL}}';
  var OPEN_DELAY = 120;   // long enough that crossing a marker is not opening it
  var SHUT_DELAY = 260;   // long enough to cross the gap into the card

  // ONE card for the document. Only one map is ever on screen - they are on
  // different tabs - and a single element stops the listeners below from
  // multiplying every time Shiny re-renders a map.
  // The detail dialog. One per document, for the same reason as the card.
  // Built with DOM calls rather than an innerHTML string: this whole script is
  // an R double-quoted string, so every double quote in it would need escaping
  // and the markup would stop being readable.
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

  // ---- The detail panel ----------------------------------------------------

  function panelShut() {
    if (panel.hidden) return;
    panel.hidden = true;
    panelBody.innerHTML = '';
    // Focus goes back where the reader left it, or the gesture that opened the
    // panel has quietly moved them to the top of the document.
    if (panel.fwReturn && panel.fwReturn.focus) {
      panel.fwReturn.focus({ preventScroll: true });
    }
    panel.fwReturn = null;
  }

  function panelOpen(layer) {
    if (!layer.fwCard) return;
    // Parsed out of the STORED STRING, not out of the card's DOM: shut() below
    // empties the card, so by the time a click is handled the detail markup may
    // no longer be anywhere on the page.
    var holder = document.createElement('div');
    holder.innerHTML = layer.fwCard;
    var tpl = holder.querySelector('template.fw-popup__detail');
    if (!tpl) return;
    // Cloning the template's content is the moment the photographs are asked
    // for. Until here they are inert markup and no request has been made.
    var detail = tpl.content.cloneNode(true);
    shut();
    panel.fwReturn = document.activeElement;
    panelBody.innerHTML = '';
    panelBody.appendChild(detail);
    panel.hidden = false;
    panelDialog.scrollTop = 0;
    panelDialog.focus({ preventScroll: true });
  }

  // DELEGATED, all of it. The panel's contents are replaced on every open, so a
  // listener bound to a button inside it would be gone the next time round.
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
        // Taken before the card's own Escape handler can act on it, so one
        // press closes the panel rather than both at once.
        e.stopPropagation();
        panelShut();
      }
    }, true);
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
  panelShut();

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
      // A CLICK IS A REQUEST FOR THE WHOLE RECORD, not a second way of getting
      // the hover card. The flag is not decoration: a click on a VECTOR layer
      // fires on the layer AND then on the map, unlike a click on a marker, so
      // without it the map handler below shut the card in the same gesture that
      // opened it and a tap did nothing at all.
      //
      // Touch has no hover, so a tap arrives here too and opens the full
      // record. That is the right result on a phone, where the hover card can
      // never be shown at all.
      layer.on('click', function (e) {
        if (e.originalEvent) { e.originalEvent.fwHandled = true; }
        panelOpen(e.target);
      });
    });
  }
  wire(map);

  // The panel is deliberately NOT closed by panning or zooming. It is a modal
  // over the page rather than a label pinned to a marker, so it has no position
  // to drift away from - and a reader who scrolls the record is not asking to
  // lose it.
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
  js <- gsub("{{LABEL}}", lit(fw_t("species", "card_label")), js, fixed = TRUE)
  gsub("{{DETAIL}}", lit(fw_t("species", "detail_label")), js, fixed = TRUE)
}

#' Attempt markers, coloured and labelled by outcome
#'
#' @param live whether popups may reach Wikimedia for an uncached species
fw_add_attempt_markers <- function(map, data, sel, live = FALSE) {
  pts <- fw_map_points(data, sel)
  if (!nrow(pts)) {
    v <- FW_MAP$empty_view
    return(leaflet::setView(map, v$lng, v$lat, zoom = v$zoom))
  }

  outcome <- ifelse(is.na(pts$outcome), "Unknown", pts$outcome)
  popups <- vapply(seq_len(nrow(pts)), function(i) {
    fw_map_popup(pts[i, ], data$species, live = live)
  }, character(1))

  map |>
    leaflet::addCircleMarkers(
      lng = pts$longitude, lat = pts$latitude,
      radius = FW_MAP$marker$radius, weight = FW_MAP$marker$weight,
      opacity = FW_MAP$marker$opacity, fillOpacity = FW_MAP$marker$fill_opacity,
      color = FW_COLOURS$surface,
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
      popupOptions = leaflet::popupOptions(maxWidth = FW_MAP$popup$max_width,
                                           minWidth = FW_MAP$popup$min_width,
                                           className = "fw-popup-wrap")
    ) |>
    leaflet::addLegend(
      position = "bottomright", colors = unname(FW_OUTCOME_COLOURS),
      labels = names(FW_OUTCOME_COLOURS), opacity = FW_MAP$legend_opacity,
      title = fw_t("species", "p_outcome")
    ) |>
    leaflet::fitBounds(min(pts$longitude), min(pts$latitude),
                       max(pts$longitude), max(pts$latitude)) |>
    htmlwidgets::onRender(fw_map_card_js())
}
