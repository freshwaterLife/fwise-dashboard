# species_images.R
# Species photographs from Wikimedia, and the markup that shows one.
#
# Sources, in order:
#   1. The cache in species.csv (dev/fetch_species_images.R).
#   2. A live lookup, memoised per process, only when a record is opened.
#
# Every photo shows its credit and licence (a licence condition): the author
# links to the Commons page, the licence to its deed when there is one. No
# licence means no photo; a missing author falls back to the source.

library(shiny)

FW_SPECIES_UA = paste(
  "FWISE-dashboard/1.0 (https://www.freshwaterlife.org;",
  "freshwater invasive species eradication database)"
)

#' Is this a value we can put in an attribute?
#'
#' NULL, NA and "" all count as absent.
fw_has_str = function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' The licence deed URL, or NA
#'
#' Only http(s) URLs (Commons metadata is community-edited); protocol-relative
#' URLs are normalised. Keep in step with dev/fetch_species_images.R.
fw_licence_url = function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x = trimws(as.character(x)[1])
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  if (grepl("^//", x)) x = paste0("https:", x)
  if (!grepl("^https?://", x)) return(NA_character_)
  x
}

# ---- The cache ---------------------------------------------------------------

#' Look a species' imagery up in the published cache
#'
#' @return a one-row list with url, credit, licence, licence_url and page_url,
#'   or NULL
fw_species_image_cached = function(species, species_id) {
  if (!"image_url" %in% names(species)) return(NULL)
  row = species[species$species_id == species_id, ]
  if (!nrow(row)) return(NULL)
  get1 = function(col) {
    if (!col %in% names(row)) return(NA_character_)
    as.character(row[[col]][1])
  }
  url = get1("image_url")
  credit = get1("image_credit")
  licence = get1("image_licence")
  if (is.na(url) || !nzchar(url)) return(NULL)
  # No licence, no picture.
  if (is.na(licence)) return(NULL)
  if (is.na(credit) || !nzchar(credit)) credit = fw_t("species", "credit_fallback")
  list(url = url, credit = credit, licence = licence,
       licence_url = get1("image_licence_url"),
       page_url = get1("image_page_url"))
}

# ---- The live fallback -------------------------------------------------------

#' Resolve one species against Wikipedia and Commons, right now
#'
#' Memoised per process, misses included. Every failure returns NULL.
fw_species_image_fetch = function(scientific_name, common_name = NA) {
  titles = unique(stats::na.omit(c(scientific_name, common_name)))
  titles = titles[nzchar(titles)]
  if (!length(titles)) return(NULL)

  api = function(base, params) {
    out = try(
      httr2::request(base) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_user_agent(FW_SPECIES_UA) |>
        httr2::req_timeout(8) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      silent = TRUE
    )
    if (inherits(out, "try-error")) NULL else out
  }
  strip = function(x) {
    if (is.null(x)) return(NA_character_)
    x = trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", "", x)))
    if (!nzchar(x)) NA_character_ else x
  }

  for (title in titles) {
    j = api("https://en.wikipedia.org/w/api.php", list(
      action = "query", format = "json", redirects = 1,
      prop = "pageimages", piprop = "name|thumbnail",
      pithumbsize = 500, titles = title
    ))
    page = j$query$pages
    if (is.null(page) || !length(page)) next
    page = page[[1]]
    if (is.null(page$pageimage) || is.null(page$thumbnail)) next

    k = api("https://commons.wikimedia.org/w/api.php", list(
      action = "query", format = "json",
      prop = "imageinfo", iiprop = "extmetadata|url",
      titles = paste0("File:", page$pageimage)
    ))
    cpage = k$query$pages
    if (is.null(cpage) || !length(cpage)) next
    info = cpage[[1]]$imageinfo
    if (is.null(info) || !length(info)) next
    meta = info[[1]]$extmetadata

    # No Artist: credit the source, as dev/fetch_species_images.R does.
    credit = strip(meta$Artist$value)
    if (is.na(credit)) credit = strip(meta$Credit$value)
    if (is.na(credit)) credit = fw_t("species", "credit_fallback")
    licence = strip(meta$LicenseShortName$value)
    if (is.na(licence)) next

    return(list(
      url = sub("[?].*$", "", page$thumbnail$source),
      credit = credit, licence = licence,
      licence_url = fw_licence_url(meta$LicenseUrl$value),
      page_url = info[[1]]$descriptionurl %||% NA_character_
    ))
  }
  NULL
}

# memoise caches NULLs too, so a miss is asked once.
if (requireNamespace("memoise", quietly = TRUE)) {
  fw_species_image_fetch = memoise::memoise(fw_species_image_fetch)
}

# ---- Markup ------------------------------------------------------------------

#' A species photograph with its credit, or a labelled placeholder
#'
#' The loading state is a CSS skeleton under the <img>. The caption is
#' "<author> / <licence>", each linked when there's a URL.
#'
#' @param img  the resolved list, or NULL for the placeholder
#' @param name what to caption it with, and what the placeholder says
fw_species_figure = function(img, name) {
  if (is.null(img)) {
    return(as.character(tags$div(
      class = "fw-species-figure fw-species-figure--none",
      tags$span(class = "fw-species-figure__placeholder",
                fw_t("species", "no_image"))
    )))
  }
  # New tab, noopener noreferrer.
  out = function(url, text) {
    if (!fw_has_str(url)) return(text)
    tags$a(href = url, target = "_blank", rel = "noopener noreferrer", text)
  }
  as.character(tags$figure(
    class = "fw-species-figure",
    tags$img(
      class = "fw-species-figure__img",
      src = img$url, alt = paste(fw_t("species", "alt_prefix"), name),
      loading = "lazy", decoding = "async"
    ),
    tags$figcaption(
      class = "fw-species-figure__credit",
      out(img$page_url, img$credit),
      fw_t("species", "credit_sep"),
      out(fw_licence_url(img$licence_url), img$licence)
    )
  ))
}

#' The figure for a species id, cache first and live only if asked
#'
#' @param live whether to fall back to the network. FALSE for bulk builds.
fw_species_figure_for = function(species, species_id, name, live = FALSE) {
  img = fw_species_image_cached(species, species_id)
  if (is.null(img) && isTRUE(live)) {
    row = species[species$species_id == species_id, ]
    if (nrow(row)) {
      img = fw_species_image_fetch(row$scientific_name[1], row$common_name[1])
    }
  }
  fw_species_figure(img, name)
}
