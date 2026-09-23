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

#' An empty map with the app's zoom and pan limits
#'
#' Every map starts here. The limits stop a reader zooming or panning out past
#' the edge of the world, which left grey bars above and below it. See
#' FW_MAP$min_zoom. Set as map options rather than in an onRender hook, so the
#' card script stays the widget's only render hook.
fw_leaflet <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(
    worldCopyJump = TRUE,
    minZoom = FW_MAP$min_zoom,
    maxBoundsViscosity = 1
  )) |>
    leaflet::setMaxBounds(-FW_MAP$max_lng, -FW_MAP$max_lat,
                          FW_MAP$max_lng, FW_MAP$max_lat)
}

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
    # NO PLACE LABELS. A Carto label layer used to ride above the data, in
    # local languages. The client asked for no text on the ground; Water and
    # Terrain keep the English labels drawn into their own tiles.
    leaflet::addLayersControl(
      baseGroups = c(plain, water, terrain, satellite),
      # NEVER NULL. leaflet.js wraps a null in an array, and the switcher then
      # shows a checkbox named null. An empty vector goes over as [].
      overlayGroups = if (length(overlays)) overlays else character(0),
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

#' The line under every interactive map saying why it is in Mercator
#'
#' See PROJECTION at the top of this file. Worded by the client; drawn under
#' the Explore and Plan maps, which are the two Leaflet maps a reader sees.
fw_map_note <- function() {
  p(class = "fw-caption fw-map-note", fw_t("maps", "mercator_note"))
}

# ---- Markers and popups ------------------------------------------------------

# Multi-value popup fields travel as one delimited string per attempt, because
# these are data frame columns and a list column cannot survive the row-at-a-time
# indexing in fw_add_attempt_markers(). A pipe, because it appears in no species
# name, method name or note in the data.
FW_POPUP_SEP <- "|"

#' Everything a record needs, joined once for the whole selection
#'
#' One row per attempt in `sel`, in the order given, with the species of each
#' role, the methods with their notes and the contacts joined on. Built as a
#' table rather than looked up per record: 914 markers each running their own
#' filter over the bridge tables is the difference between a map that opens and
#' one that hangs.
#'
#' THIS IS THE RECORD, WHEREVER IT IS SHOWN. The map's markers, the map's detail
#' panel all read from this frame, so a record cannot say
#' one thing in one place and another elsewhere. The three attempts with no
#' coordinates are here too; fw_map_points() is the located subset.
fw_attempt_records <- function(data, sel) {
  pts <- sel
  # RETURNS A NARROWER FRAME WHEN EMPTY: none of the joins below have run, so
  # the popup columns are absent rather than present-and-empty. Every caller has
  # to test nrow() before touching a popup column.
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

  # Methods BY NAME ONLY, in the order they were applied, at most three (the
  # data never has more). The notes used to ride along ("Rotenone - Betamax
  # vet.") and the client removed them from the map (21 Sept 2026): the card
  # and the record list what was used, and the detail is in the download.
  methods <- data$attempt_method |>
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

  # THE WHOLE CONTACT PASSES THROUGH THE email_public GATE, name included
  # (client, 23 Sept 2026). Same rule as fw_export_frame() and
  # fw_contacts_summary(); see the note above fw_map_detail_html() for why an
  # address is shown here at all. A private contact leaves the record's
  # Contacts row empty, which draws as "Not noted" - the attempt is still
  # there in full, there is simply nobody named to write to about it.
  contacts <- data$contact |>
    dplyr::transmute(
      contact_id,
      contact_name  = dplyr::if_else(email_public, contact_name, NA_character_),
      contact_email = dplyr::if_else(email_public, contact_email,
                                     NA_character_),
      organisation  = dplyr::if_else(email_public, organisation, NA_character_)
    )

  out <- pts |>
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

#' The located attempts of a selection, as the map draws them
#'
#' fw_attempt_records() for the rows that have coordinates. Same narrower-frame
#' rule when empty; fw_add_attempt_markers() tests nrow() before touching a
#' popup column and returns a bare map at that point.
fw_map_points <- function(data, sel) {
  fw_attempt_records(data, sel[!is.na(sel$latitude) & !is.na(sel$longitude), ])
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

# One label/value row.
#
# EVERY ROW IS ALWAYS DRAWN (client, 21 Sept 2026). A field with nothing in it
# - missing, blank or NA - says "Not noted" rather than vanishing, on the hover
# card and in the record alike, so every record has the same rows in the same
# places and a gap reads as a gap in the data rather than a layout fault.
fw_popup_row <- function(label, value, html = FALSE,
                         fallback = fw_t("species", "p_not_noted")) {
  esc <- htmltools::htmlEscape
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
fw_popup_list <- function(x, max = 3L, html = FALSE) {
  items <- if (length(x) > 1) x else fw_popup_parts(x)
  items <- utils::head(items[!is.na(items) & nzchar(items)], max)
  if (!length(items)) return(NA_character_)
  if (!html) items <- htmltools::htmlEscape(items)
  paste0('<ul class="fw-popup__list"><li>',
         paste(items, collapse = "</li><li>"), "</li></ul>")
}

# The unlabelled country line under the title, "Not noted" when absent.
fw_popup_place <- function(country) {
  none <- is.na(country) || !nzchar(country)
  paste0('<p class="fw-popup__place', if (none) " fw-popup__val--none", '">',
         htmltools::htmlEscape(if (none) fw_t("species", "p_not_noted") else country),
         "</p>")
}

#' The years an attempt ran
#'
#' "1998-2004", or the start year alone where there is no end - 144 of 914
#' attempts have none, and a good part of those are still running, so a dash to
#' nowhere would read as missing data. How long it lasted is its own field now
#' (fw_popup_duration()), from the recorded duration rather than year
#' arithmetic, so this no longer carries a bracket.
fw_popup_years <- function(start, end) {
  if (is.na(start)) return(NA_character_)
  # An end year that is the same as the start, or earlier than it, is not a
  # range. "2000-2000" reads as a fault in the data rather than as a campaign
  # that began and finished inside one year.
  if (is.na(end) || end <= start) return(as.character(start))
  paste0(start, "-", end)
}

#' How long an attempt lasted, from duration_days
#'
#' Days under a year ("1 day", "20 days"), years to one decimal place from 365
#' on ("1.0 years", "1.1 years"), at the client's request (21 Sept 2026). NA
#' when no duration is recorded, which the row shows as "Not noted".
fw_popup_duration <- function(days) {
  if (length(days) != 1 || is.na(days)) return(NA_character_)
  if (days < 365) {
    d <- round(days)
    return(paste(d, fw_t("maps", if (d == 1) "day_one" else "day_many")))
  }
  paste(formatC(round(days / 365, 1), format = "f", digits = 1),
        fw_t("maps", "year_many"))
}

#' The treated size, with its unit
#'
#' ONE FORMATTER FOR BOTH CARDS. The hover card gained this field on 23 Sept
#' 2026, beside the kind of water; the detail panel has always had it, and the
#' two printed the same number differently for a week. The unit comes off the
#' row (`area_unit`: hectares for still water, kilometres for flowing), so a
#' figure never appears without one.
#'
#' NA when nothing is recorded, which fw_popup_row() shows as "Not noted" and
#' which fw_popup_waterbody() treats as "no size to add".
fw_popup_area <- function(row) {
  if (length(row$area_treated) != 1 || is.na(row$area_treated)) {
    return(NA_character_)
  }
  paste(format(row$area_treated, big.mark = ",", trim = TRUE),
        row$area_unit %|na|% "")
}

#' The kind of water, with the treated size after it
#'
#' "Lake (120 ha)", or just "Lake" where no size was recorded (client, 23 Sept
#' 2026). One row rather than two: the size is a property of the water body
#' being described, and on a hover card meant to be read at a glance it earns
#' its place beside it rather than on a line of its own.
fw_popup_waterbody <- function(row) {
  kind <- row$waterbody_type
  area <- fw_popup_area(row)
  if (is.na(area)) return(kind)
  if (length(kind) != 1 || is.na(kind) || !nzchar(kind)) return(area)
  paste0(kind, " (", area, ")")
}

#' The hover card: enough to decide whether to open the record
#'
#' PHOTOGRAPHS, AND THIS IS A REVERSAL. The card carried none, on the
#' reasoning that hovering is cheap precisely because opening a card is one
#' innerHTML and no request. The client asked for the picture back: a species
#' photograph is what makes somebody stop on a marker, and they would rather
#' test that with readers than reason about it.
#'
#' WHAT KEEPS THE COST BOUNDED, because the original reasoning was not wrong:
#'
#'   TWO IMAGES, NOT SIXTEEN. The first invasive species and the first
#'   beneficiary - what the attempt was against, and what it was for. The
#'   detail panel still shows every species in both roles.
#'
#'   CACHE ONLY, NEVER A LIVE LOOKUP. The URL comes from species.csv through
#'   fw_map_thumb_cache(). A species with no cached image gets no thumbnail and
#'   no request - see fw_species_figure(), which returns a placeholder rather
#'   than a gap.
#'
#'   RENDERED ONCE PER SPECIES, not once per marker, for the same reason the
#'   detail figures are.
#'
#'   loading="lazy" ON THE IMG, which fw_species_figure() already sets. The card
#'   markup sits in the marker's popup string from the start, so without it a
#'   900-marker map would fetch 900 thumbnails before anyone hovered anything.
#'
#' IT IS HIDDEN ON A SMALL SCREEN. See .fw-popup__figure - on a phone the card
#' is most of the viewport and the picture pushed the outcome off the bottom.
#'
#' THE CREDIT TRAVELS WITH IT. fw_species_figure() renders the caption and the
#' licence link, and that is a condition of using these images rather than
#' decoration. Do not strip it to save the space.
#'
#' @param thumbs figure HTML keyed by species_id, from fw_map_thumb_cache(), or
#'   NULL for no photograph at all
#' @param thumb_ref carry the photographs by species id rather than inline; see
#'   fw_popup_thumb()
fw_map_hover_html <- function(row, thumbs = NULL, thumb_ref = FALSE) {
  esc <- htmltools::htmlEscape

  paste0(
    # The id is how a click asks the server for the rest of the record when
    # the detail is not embedded. See fw_add_attempt_markers().
    '<div class="fw-popup" data-fw-id="', esc(row$attempt_id), '">',
    # THE FIELDS, IN THE CLIENT'S ORDER (23 Sept 2026), every one always
    # drawn: Location and Country unlabelled, then Targeted, Protected,
    # Outcome, Years, Duration, Kind of water and Method(s). A field with
    # nothing in it says "Not noted" - see fw_popup_row(). The photographs sit
    # between the place and the rows, as they did.
    #
    # KIND OF WATER MOVED ABOVE METHOD(S) and picked up the treated size in
    # the same round - see fw_popup_waterbody(). The setting is what a reader
    # is matching against their own site, so it comes before what was done
    # about it.
    '<h3 class="fw-popup__title">',
    esc(row$site_name %|na|% fw_t("species", "p_not_noted")),
    "</h3>",
    fw_popup_place(row$country),
    fw_popup_thumb(row, thumbs, ref = thumb_ref),
    fw_popup_row(fw_t("species", "p_targeted"), row$inv_list),
    fw_popup_row(fw_t("species", "p_protected"), row$ben_list),
    # The outcome is words as well as colour, so it never depends on the dot.
    fw_popup_row(fw_t("species", "p_outcome"), row$outcome),
    fw_popup_row(fw_t("species", "p_began"),
                 fw_popup_years(row$start_year, row$end_year)),
    fw_popup_row(fw_t("species", "p_duration"), fw_popup_duration(row$duration_days)),
    fw_popup_row(fw_t("species", "p_waterbody"), fw_popup_waterbody(row)),
    fw_popup_row(fw_t("species", "p_methods"), fw_popup_list(row$method_names),
                 html = TRUE),
    # A BUTTON, NOT A LINE OF QUIET TEXT. It read as a caption and the client
    # reported readers not realising the card opened into anything.
    #
    # IT IS THE ONLY CLICK TARGET IN THE CARD. The whole card used to open the
    # record, and the client asked for that to belong to the marker and this
    # button alone. data-fw-open is what the delegated handler in
    # FW_MAP_CARD_JS looks for; a click anywhere else in the card does nothing.
    '<p class="fw-popup__more"><button type="button" class="fw-popup__more-btn" data-fw-open>',
    esc(fw_t("species", "more_hint")), "</button></p>",
    "</div>"
  )
}

#' The hover card's thumbnails: what was targeted, and what benefited
#'
#' TWO PICTURES NOW, and it was one. The client asked for the beneficiary
#' alongside the target, on the reasoning that "carp removed" and "what the carp
#' were removed FOR" are the same story and the card was only telling half of
#' it. Still the FIRST species of each role and no more - the detail panel is
#' where every species in an attempt is shown, behind a pair of arrows.
#'
#' EACH PICTURE IS LABELLED, IN ONE WORD. With one photograph the reader could
#' assume it was the target; with two side by side and nothing to tell them
#' apart, they cannot. The labels are fig_invasive and fig_beneficiary rather
#' than the p_* pair the text rows use, because a column this narrow wraps
#' "Species that benefited" onto a second line and then the two photographs
#' under the two labels no longer start at the same height.
#'
#' BOTH SLOTS, ALWAYS, and this used to be the opposite. A role with no cached
#' photograph drew nothing, a role with no species drew nothing, and an attempt
#' with neither got no figure block - so the card was two pictures wide on one
#' marker, one picture wide on the next and text-only on the third, and the rows
#' underneath started at a different height on each. The client asked for the
#' beneficiary to be there whether or not there is a photograph of it, on the
#' same reasoning the text rows were fixed on: a blank card saying "None noted"
#' tells the reader the beneficiary was not recorded, where an absent tile tells
#' them nothing and looks like a layout fault.
#'
#' A PLACEHOLDER COSTS NO REQUEST. It is a div with a line of text in it - see
#' fw_species_figure() - so forcing both slots does not undo what the thumbnail
#' cache exists to save, which is the Wikimedia fetches.
#'
#' BY REFERENCE ON A PAGE. With `ref = TRUE` the block carries only the two
#' species ids and the card script fills it in from a dictionary the map was
#' given once (see fw_map_card_render()). Inline, the same 215 figures were
#' repeated across 911 cards: 1 MB of a 2.5 MB widget, sent on every
#' redraw. The saved report keeps them inline, because the file has nothing
#' to be handed a dictionary by except its own markers - and it is the same
#' markup either way, so one stylesheet rule still covers both.
fw_popup_thumb <- function(row, thumbs, ref = FALSE) {
  if (ref) {
    first <- function(x) {
      ids <- fw_popup_parts(x)
      if (length(ids)) ids[1] else ""
    }
    return(paste0('<div class="fw-popup__figure" data-fw-figures="2" data-fw-thumbs="',
                  htmltools::htmlEscape(first(row$inv_ids), attribute = TRUE),
                  FW_POPUP_SEP,
                  htmltools::htmlEscape(first(row$ben_ids), attribute = TRUE),
                  '"></div>'))
  }
  one <- function(ids_col, role_key) {
    ids <- fw_popup_parts(ids_col)
    # NO SPECIES IN THE ROLE says so in the text rows' words; a species with no
    # cached photograph keeps the blank tile's own.
    fig <- if (length(ids)) thumbs[[ids[1]]] else fw_popup_thumb_unrecorded()
    if (is.null(fig) || !nzchar(fig)) fig <- fw_popup_thumb_none()
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

#' The blank tile a hover card shows where there is no photograph
#'
#' The same markup fw_species_figure() returns for a species with no image, so
#' one rule in _components.scss styles both, with the hover card's own wording:
#' "None noted" answers the card's question - was anything recorded here - where
#' "No photograph available" answers a question about the picture library.
fw_popup_thumb_none <- function(text = fw_t("species", "fig_none")) {
  as.character(htmltools::tags$div(
    class = "fw-species-figure fw-species-figure--none",
    htmltools::tags$span(class = "fw-species-figure__placeholder", text)
  ))
}

#' The blank tile for a role with no species recorded at all
#'
#' Worded like the card's text rows ("Not noted"), at the client's request,
#' so the tile and the row under it give the same answer. Used by the hover
#' card and the detail panel alike.
fw_popup_thumb_unrecorded <- function() {
  fw_popup_thumb_none(fw_t("species", "p_none"))
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
#'
#' @param figure_cache an optional named character vector of ready-made figure
#'   HTML keyed by species_id, from fw_map_figure_cache(). A species found
#'   there is not rendered again; anything else is built as before.
fw_map_detail_html <- function(row, species_tbl, live = FALSE, figure_cache = NULL) {
  # A <template>, NOT a hidden div, and this is not a style preference.
  # display:none does not stop a browser fetching an <img src>: a hidden div
  # here meant every hover card quietly pulled up to eight Wikimedia
  # photographs that nobody was going to look at, which is the exact cost the
  # hover card exists to avoid. Template content is inert - parsed, never
  # rendered, nothing fetched - until the script clones it on click.
  paste0(
    '<template class="fw-popup__detail">',
    fw_record_detail_html(row, species_tbl, live, figure_cache = figure_cache),
    "</template>"
  )
}

#' The whole record, as the panel shows it
#'
#' The markup inside fw_map_detail_html()'s template, on its own. The map
#' embeds it in a marker; fw_map_detail_server() sends it straight to the panel
#' when a marker is clicked.
#'
#' PREVIOUS AND NEXT USED TO SIT AT THE FOOT, drawn here when a server was
#' there to answer them and omitted in a saved report. The client removed them:
#' a reader opens a record from the marker they chose on the map, and stepping
#' sideways into whatever happened to be next in the selection's row order is
#' not a journey any of them were making. The panel is closed and the next
#' marker is clicked instead, which is the same two actions with the map still
#' in front of the reader. fw_record_neighbour() went with the buttons.
fw_record_detail_html <- function(row, species_tbl, live = FALSE,
                                  figure_cache = NULL) {
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
             fw_popup_thumb_unrecorded(), "</div>")
    } else {
      paste0(vapply(seq_along(ids), function(i) {
        fig <- figure_cache[ids[i]]
        if (is.null(figure_cache) || is.na(fig)) {
          fig <- fw_species_figure_for(species_tbl, ids[i], names[i], live = live)
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

  area <- fw_popup_area(row)

  # CONTACTS AS A LIST, primary then secondary, replacing the separate
  # "Recorded by" and "Also recorded by" rows.
  contacts <- c(
    person(row$primary_contact_name, row$primary_contact_email,
           row$primary_contact_org),
    person(row$secondary_contact_name, row$secondary_contact_email,
           row$secondary_contact_org)
  )
  has_link <- !is.na(row$reference_link) && nzchar(row$reference_link)

  # THE FIELDS, IN THE CLIENT'S ORDER (21 Sept 2026), and EVERY ONE ALWAYS
  # DRAWN: a field with nothing in it says "Not noted" (fw_popup_row()). The
  # verification notes are gone - more often unhelpful than detailed, in the
  # client's reading of the records.
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
    # 432 of 914 attempts have a link. Where there is one it is a green button,
    # matching the hover card's; where there is none, nothing is drawn (21 Sept
    # 2026 - the "Not noted" line was dropped at the client's request).
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

#' The popup for one attempt: the hover card, with the detail panel inside it
#' when it is being embedded
#'
#' @param detail "embed" puts the whole record in the string, "lazy" sends the
#'   hover card alone and leaves the record to fw_map_detail_server().
fw_map_popup <- function(row, species_tbl, live = FALSE,
                         detail = c("embed", "lazy"), figure_cache = NULL,
                         thumbs = NULL, thumb_ref = FALSE) {
  detail <- match.arg(detail)
  hover <- fw_map_hover_html(row, thumbs, thumb_ref = thumb_ref)
  if (detail == "lazy") return(hover)
  paste0(hover, fw_map_detail_html(row, species_tbl, live, figure_cache = figure_cache))
}

#' The thumbnail each hover card needs, rendered once per species
#'
#' A MUCH SMALLER SET THAN fw_map_figure_cache(). That one renders every species
#' in either role across the whole selection - 306 of them on a full build. This
#' one renders only the FIRST invasive species of each attempt, which is far
#' fewer distinct species and is all the hover card shows.
#'
#' live = FALSE, always. A build renders these all at once and not one of them
#' may reach Wikimedia while it does.
fw_map_thumb_cache <- function(data, pts) {
  first_of <- function(col) {
    vapply(col, function(x) {
      ids <- fw_popup_parts(x)
      if (length(ids)) ids[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
  }
  # BOTH ROLES NOW, at the client's request: the card shows the animal that was
  # targeted and the one that stood to gain. Still the FIRST of each and no
  # more, which is what keeps this set far smaller than fw_map_figure_cache()'s.
  # A species that appears in both roles is rendered once - the cache is keyed
  # by species_id, not by role.
  first <- c(first_of(pts$inv_ids), first_of(pts$ben_ids))
  ids <- unique(first[!is.na(first)])
  if (!length(ids)) return(character(0))
  labels <- fw_species_label(data$species)
  names_for <- labels$label[match(ids, labels$species_id)]
  stats::setNames(
    vapply(seq_along(ids), function(i) {
      # AN EMPTY STRING FOR A SPECIES WITH NO CACHED IMAGE, and the decision
      # about what to draw in its place is fw_popup_thumb()'s rather than this
      # function's - it renders the blank "None noted" tile, because the client
      # wants the slot held open whether or not there is a photograph in it.
      # This one only answers whether an image exists.
      img <- fw_species_image_cached(data$species, ids[i])
      if (is.null(img)) return("")
      fw_species_figure(img, names_for[i])
    }, character(1)),
    ids
  )
}

#' Every species figure a set of markers will need, rendered once each
#'
#' THIS IS WHERE THE BUILD TIME WENT. A full build renders 2,646 figures for
#' 306 distinct species, and each one was an htmltools tag tree rendered from
#' scratch - four fifths of the 2.6 seconds a 900-marker map took. Rendered
#' once per species here and handed to fw_map_detail_html() as a lookup.
#'
#' The caption is the species label, which is the same string the popup
#' carries for that id, so a cached figure is identical to a fresh one.
fw_map_figure_cache <- function(data, pts) {
  ids <- unique(unlist(lapply(c(pts$inv_ids, pts$ben_ids), fw_popup_parts)))
  if (!length(ids)) return(character(0))
  labels <- fw_species_label(data$species)
  names_for <- labels$label[match(ids, labels$species_id)]
  stats::setNames(
    vapply(seq_along(ids), function(i) {
      fw_species_figure_for(data$species, ids[i], names_for[i], live = FALSE)
    }, character(1)),
    ids
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
function (el, x, data) {
  var map = this;
  // The hover cards' photographs, sent ONCE with the map rather than inside
  // every card: species id to figure markup, plus the blank tile and the two
  // role captions. See fw_popup_thumb() and fw_map_card_render(). Absent in a
  // saved report, whose cards carry their figures inline.
  var THUMBS = (data && data.thumbs) || {};
  var THUMB_NONE = (data && data.none) || '';
  var THUMB_UNRECORDED = (data && data.unrecorded) || THUMB_NONE;
  var THUMB_ROLES = (data && data.roles) || ['', ''];
  var CLOSE_LABEL = '{{CLOSE}}';
  var CARD_LABEL = '{{LABEL}}';
  var DETAIL_LABEL = '{{DETAIL}}';
  // The Shiny input a click writes an attempt id into when the record is not
  // in the marker. Empty in a saved report, where every record is embedded.
  var DETAIL_INPUT = '{{INPUT}}';
  var OPEN_DELAY = 120;   // long enough that crossing a marker is not opening it
  var SHUT_DELAY = 260;   // long enough to cross the gap into the card
  // The width below which the card stops being a tooltip beside a marker and
  // becomes a sheet at the bottom of the screen. Injected from R so it is the
  // SAME NUMBER as the media query in .fw-map-card - two copies of a breakpoint
  // that disagree is a card styled one way and positioned the other.
  var STACK_BP = {{STACKBP}};

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
    // The layer the card was showing, dropped with the markup it belonged to.
    // A stale one here would send a click on the NEXT card to the last
    // marker's record.
    card.fwLayer = null;
    card.fwOpen = null;
  }
  function place(latlng) {
    // NARROW SCREENS DO NOT ANCHOR TO THE MARKER. The card is as wide as the
    // viewport there (see .fw-map-card's media query), so there is no room
    // beside a marker left to aim at - the arithmetic below would clamp it to
    // the same place every time while still jittering by a pixel or two as the
    // reader taps around. Pinned to the bottom instead, the way a phone puts a
    // sheet, which also keeps it clear of the finger that opened it.
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
  // A card that carries its photographs by reference gets them here, from
  // this map's dictionary. The slot is always two tiles wide: a role with no
  // cached image gets the blank tile, as it does inline.
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
    // WHICH RECORD THE CARD IS SHOWING, so a click on the card can open it.
    // The card is a child of document.body rather than of the map, so it has
    // no route back to the marker it came from except this.
    card.fwLayer = layer;
    // AND WHICH MAP'S OPENER TO USE. The card's click listener is wired once
    // for the page, by whichever map rendered first, so a panelOpen captured
    // there sent Explore's clicks to the Plan page's detail input - where the
    // server dropped them as not in its selection. The opener travels with
    // the layer instead, so it is always this map's.
    card.fwOpen = panelOpen;
    body.innerHTML = layer.fwCard;
    fillThumbs(body);
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

  // Put a record in the panel and show it. The focus to return to is taken
  // once and kept: a reader who steps through several records with the
  // previous/next buttons still lands back on the marker or card they opened
  // the first one from.
  function panelMount(node) {
    shut();
    if (!panel.fwReturn) panel.fwReturn = document.activeElement;
    panelBody.innerHTML = '';
    panelBody.appendChild(node);
    panel.hidden = false;
    panelDialog.scrollTop = 0;
    panelDialog.focus({ preventScroll: true });
  }

  // Open the panel on the detail template found in a popup string. Returns
  // false when the string holds no template, which is the lazy case below -
  // panelOpen() depends on that false to know it must ask the server.
  function panelOpenHtml(html) {
    var holder = document.createElement('div');
    holder.innerHTML = html;
    var tpl = holder.querySelector('template.fw-popup__detail');
    if (!tpl) return false;
    // Cloning the template's content is the moment the photographs are asked
    // for. Until here they are inert markup and no request has been made.
    panelMount(tpl.content.cloneNode(true));
    return true;
  }

  // Open the panel on a record the SERVER sent. No template to look for and
  // none wanted: a template keeps photographs inert while a record rides
  // inside a hover card nobody may open, and this record was asked for by
  // name and is going straight onto the screen.
  function panelOpenDetail(html) {
    var holder = document.createElement('div');
    holder.innerHTML = html;
    if (!holder.firstElementChild) return false;
    panelMount(holder.firstElementChild);
    return true;
  }

  // The server's answer to a request arrives through the 'fw-map-detail'
  // message handler in fw_client_script(), which calls fwOpenDetail.
  panel.fwOpenHtml = panelOpenHtml;
  panel.fwOpenDetail = panelOpenDetail;

  function panelOpen(layer) {
    if (!layer.fwCard) return;
    // Parsed out of the STORED STRING, not out of the card's DOM: shut() below
    // empties the card, so by the time a click is handled the detail markup may
    // no longer be anywhere on the page.
    if (panelOpenHtml(layer.fwCard)) return;
    // Nothing embedded: ask the server for the record by its id. The focus
    // to return to is taken now, before the round trip moves it.
    if (!DETAIL_INPUT || !window.Shiny) return;
    var holder = document.createElement('div');
    holder.innerHTML = layer.fwCard;
    var root = holder.querySelector('[data-fw-id]');
    if (!root) return;
    shut();
    panel.fwReturn = document.activeElement;
    Shiny.setInputValue(DETAIL_INPUT, root.getAttribute('data-fw-id'),
                        { priority: 'event' });
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
    // THE GREEN BUTTON IS THE CLICK TARGET, not the card. The client asked
    // for the record to open from the marker or this button only. Delegated,
    // because the card's contents are replaced on every open. The card sits on
    // document.body, outside the Leaflet container, so the marker's own click
    // handler never sees this and the capturing document handler below
    // deliberately ignores it.
    //
    // card.fwOpen, NOT panelOpen: this listener is wired once for the page,
    // and the panelOpen in scope here belongs to whichever map ran first. See
    // open().
    card.addEventListener('click', function (e) {
      if (!e.target.closest('[data-fw-open]')) return;
      if (card.fwLayer && card.fwOpen) card.fwOpen(card.fwLayer);
    });
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

  // One marker: lift its record off the popup, unbind the popup, and put the
  // card and the panel on its events instead. Idempotent, because the same
  // marker announces itself more than once (see wire()).
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
  }

  // Every marker in a container, now and later. THE LATER IS NOT OPTIONAL:
  // the cluster group the markers sit in takes single additions in batches on
  // a timer (Leaflet.markercluster's layer-support build), so when this script
  // runs the group can still be empty, and a one-off walk wired nothing. Each
  // group therefore also wires whatever it announces from here on.
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
#' @param detail_input the namespaced Shiny input a click writes an attempt id
#'   into, or NULL when every record is embedded in its marker
fw_map_card_js <- function(detail_input = NULL) {
  # The labels land inside a single-quoted JavaScript string, so both a
  # backslash and an apostrophe in the copy would break the script.
  lit <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    gsub("'", "\\'", x, fixed = TRUE)
  }
  js <- gsub("{{CLOSE}}", lit(fw_t("species", "card_close")), FW_MAP_CARD_JS,
             fixed = TRUE)
  js <- gsub("{{LABEL}}", lit(fw_t("species", "card_label")), js, fixed = TRUE)
  js <- gsub("{{DETAIL}}", lit(fw_t("species", "detail_label")), js, fixed = TRUE)
  # The breakpoint goes in as a bare number, from the same FW_BREAKPOINTS entry
  # the stylesheet uses, so the script and .fw-map-card cannot drift apart.
  js <- gsub("{{STACKBP}}", sub("px$", "", FW_BREAKPOINTS$sm), js, fixed = TRUE)
  gsub("{{INPUT}}", lit(detail_input %||% ""), js, fixed = TRUE)
}

#' Serve the record behind a marker, or a card, when it is clicked
#'
#' The server half of detail = "lazy" in fw_add_marker_layer(). The click
#' arrives in input[[input_name]] as an attempt id, the record is built for
#' that one attempt - a few milliseconds - and sent back as the same HTML the
#' embedded mode would have carried, which the card script then opens.
#'
#' ONE CLICK, ONE RECORD. This used to answer a second input as well -
#' input[[<input_name>_step]], the panel's previous/next buttons, resolved
#' against `sel()`'s row order - and the client removed those buttons. What
#' `sel()` is still for is the guard below.
#'
#' @param sel a reactive returning the current selection, in display order. An
#'   id outside it is ignored rather than answered, so a stale click after a
#'   rebuild cannot show a record the reader did not select.
fw_map_detail_server <- function(input, session, input_name, data, sel) {
  send <- function(id) {
    if (!is.character(id) || length(id) != 1 || is.na(id)) return()
    s <- sel()
    row <- s[!is.na(s$attempt_id) & s$attempt_id == id, ]
    if (!nrow(row)) return()
    rec <- fw_attempt_records(data, row)
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
#' The numbers live in FW_MAP$cluster (config.R); the reasoning is there too.
#' The group icon is drawn by .fw-cluster in _components.scss, not the plugin's
#' default green blob: one counted ring, at every zoom.
#'
#' IT USED TO BE TWO LOOKS - a plain indigo dot the size of a marker below
#' FW_MAP$cluster$fine_zoom, the counted ring above it - chosen by reading
#' `c._zoom`, since a cluster object belongs to one zoom level. The client asked
#' for the number everywhere, because an unlabelled dot is indistinguishable
#' from a single attempt.
#'
#' The title is still set even though the count is now drawn: it is what says
#' what the number means, to a screen reader and on hover. The legs of a
#' fanned-out group are the muted ink so they read as chrome rather than data.
fw_cluster_options <- function() {
  cl <- FW_MAP$cluster
  # The copy lands inside a single-quoted JavaScript string.
  title <- gsub("'", "\\'", fw_fill(fw_t("maps", "stack_title"), n = "{n}"), fixed = TRUE)
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
#' TWO WAYS TO CARRY THE RECORD, and the page and the report need different
#' ones. "embed" puts every record inside its marker, which is what a map
#' saved to a file needs because there is no server to ask once it is on
#' someone's desk. "lazy" sends only the hover card - about a sixth of the
#' bytes - and fetches the record on click through fw_map_detail_server(),
#' which is what the page needs because a 900-marker build was shipping six
#' megabytes of records that almost nobody would open. Measured: the embedded
#' payload was 6.2 MB and the detail templates were 4.7 MB of it.
#'
#' A LAZY MAP ALSO SENDS EACH PHOTOGRAPH ONCE. Its cards name their two species
#' and the card script fills them in from a dictionary shipped with the widget
#' (fw_map_card_render()). With the figures inline the full map was still
#' 2.5 MB, and is 1.5 MB this way; see fw_popup_thumb().
#'
#' STACKED MARKERS ARE GROUPED. See FW_MAP$cluster for the rule; in short,
#' only markers that sit on top of one another group until the reader is
#' zoomed well in, so the coarse view is still coloured dots.
#'
#' @param live whether popups may reach Wikimedia for an uncached species
#' @param detail "embed" or "lazy", as above
#' @param detail_input the namespaced input id the lazy mode reports clicks
#'   to. Required for "lazy"; the calling module pairs it with
#'   fw_map_detail_server().
fw_add_attempt_markers <- function(map, data, sel, live = FALSE,
                                   detail = c("embed", "lazy"),
                                   detail_input = NULL) {
  detail <- match.arg(detail)
  if (detail == "lazy" && is.null(detail_input)) {
    stop("detail = \"lazy\" needs detail_input, the input the map reports clicks to.",
         call. = FALSE)
  }
  pts <- fw_map_points(data, sel)
  lazy <- detail == "lazy"
  # Built in BOTH modes, unlike the figure cache: the hover card carries its
  # thumbnail whether or not the record behind it is embedded. A lazy map hands
  # the set to the card script once; an embedded one writes it into each card.
  thumbs <- if (nrow(pts)) fw_map_thumb_cache(data, pts) else character(0)
  if (!nrow(pts)) {
    # STILL WIRE THE CARD SCRIPT, so an empty map behaves like any other: it
    # is what creates the record panel on <body>.
    v <- FW_MAP$empty_view
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
#' The half of fw_add_attempt_markers() that changes with the selection, split
#' out so the Explore page can send it through leaflet::leafletProxy() and
#' leave the tiles, the legend and the card script where they are. Everything
#' here is a call a proxy accepts.
#'
#' @param pts fw_map_points() output, at least one row
#' @param thumbs fw_map_thumb_cache() output covering `pts`. For "lazy" the
#'   cards only name their species, and whatever dictionary the map was
#'   rendered with has to cover them - see fw_map_card_render().
fw_add_marker_layer <- function(map, data, pts, live = FALSE,
                                detail = c("embed", "lazy"), thumbs = NULL) {
  detail <- match.arg(detail)
  outcome <- ifelse(is.na(pts$outcome), "Unknown", pts$outcome)
  figure_cache <- if (detail == "embed") fw_map_figure_cache(data, pts) else NULL
  popups <- vapply(seq_len(nrow(pts)), function(i) {
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
    fw_fit_points(pts)
}

#' Fit the view to a set of points
#'
#' CAPPED AT FW_MAP$fit_max_zoom. A selection whose attempts all share one
#' coordinate - Belgium's and Austria's do - has a bounding box of zero size,
#' and Leaflet fits that at an infinite zoom: no tiles load and the map is
#' grey. The cap is also what stops two neighbouring sites opening at street
#' level.
fw_fit_points <- function(map, pts) {
  leaflet::fitBounds(map, min(pts$longitude), min(pts$latitude),
                     max(pts$longitude), max(pts$latitude),
                     options = list(maxZoom = FW_MAP$fit_max_zoom))
}

#' The outcome legend
fw_add_outcome_legend <- function(map) {
  leaflet::addLegend(
    map, position = "bottomright", colors = unname(FW_OUTCOME_COLOURS),
    labels = names(FW_OUTCOME_COLOURS), opacity = FW_MAP$legend_opacity,
    title = fw_t("species", "p_outcome")
  )
}

#' Install the card script, with the photograph dictionary if there is one
#'
#' The dictionary is what cards built with thumb_ref = TRUE are filled from.
#' It rides in onRender()'s data argument, so it is sent once with the widget
#' rather than once per marker, and a proxy that swaps the markers later does
#' not have to send it again - provided the set given here covers them, which
#' is why the Explore page passes the whole database's (fw_map_thumbs_all()).
#'
#' @param detail_input as for fw_map_card_js()
#' @param thumbs fw_map_thumb_cache() output, or NULL for cards with their
#'   figures inline
fw_map_card_render <- function(map, detail_input = NULL, thumbs = NULL) {
  js <- fw_map_card_js(detail_input)
  if (is.null(thumbs)) return(htmlwidgets::onRender(map, js))
  esc <- htmltools::htmlEscape
  htmlwidgets::onRender(map, js, data = list(
    # A list, not a named vector, so it arrives as an object even when empty
    # or of length one.
    thumbs = as.list(thumbs),
    none = fw_popup_thumb_none(),
    unrecorded = fw_popup_thumb_unrecorded(),
    roles = c(esc(fw_t("species", "fig_invasive")),
              esc(fw_t("species", "fig_beneficiary")))
  ))
}

#' The hover-card photographs for the whole database, built once per process
#'
#' What the Explore page renders its map with, so any selection its proxy
#' draws later is already covered. Cached against the data it was built from;
#' `identical()` on the same object is a pointer comparison, so the check
#' costs nothing on the app's one shared FW_DATA.
fw_map_thumbs_all <- local({
  cache <- NULL
  function(data) {
    if (is.null(cache) || !identical(cache$data, data)) {
      cache <<- list(data = data,
                     thumbs = fw_map_thumb_cache(data, fw_map_points(data, data$attempt)))
    }
    cache$thumbs
  }
})
