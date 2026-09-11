# species_images.R
# Species photographs from Wikimedia, and the markup that shows one.
#
# TWO SOURCES, IN THIS ORDER.
#   1. The cache in species.csv, resolved offline by dev/fetch_species_images.R
#      and committed with the data. This covers everything and costs nothing.
#   2. A live lookup, memoised for the life of the process, for a species added
#      since the last cache run. It fires ONLY when a popup for that species is
#      actually opened - never at map-render time, which would put hundreds of
#      third-party requests in front of the first paint.
#
# ATTRIBUTION IS NOT OPTIONAL. Most of these are CC BY-SA, which requires the
# licence and a credit to travel with the image, so both are stored beside the
# URL and rendered under every photograph.
#
# AND THE LICENCE IS A LINK. CC BY and CC BY-SA both ask for a link to the
# licence itself, which the licence's NAME in plain text does not give them, so
# the caption reads "<author, linked to the file page> / <licence, linked to its
# deed>". The deed URL comes from Commons alongside everything else and is
# cached in species.csv as image_licence_url. Where there is no deed to link to
# - most public-domain tags, which is 111 of the 308 photographs - the licence
# stays plain text, which is correct rather than a gap: there is no licence for
# a link to point at.
#
# The LICENCE is what is mandatory: a photograph whose terms we cannot state is
# one we have no business republishing, so a row without one falls back to the
# placeholder. The CREDIT falls back instead of failing - a good many Commons
# files carry a licence but no Artist field, and where no author is recorded,
# crediting the source and linking the file page is what CC asks for.

library(shiny)

FW_SPECIES_UA <- paste(
  "FWISE-dashboard/1.0 (https://www.freshwaterlife.org;",
  "freshwater invasive species eradication database)"
)

#' Is this a value we can put in an attribute?
#'
#' NULL, NA and "" all mean "not present" here, and a caller holding a list from
#' either source may hand over any of the three. `if (!is.na(x))` on a NULL is
#' an error rather than FALSE, which is what this exists to stop.
fw_has_str <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' The licence deed URL, or NA
#'
#' Deliberately strict about the scheme. This value ends up as an href in a
#' popup, and Commons' extmetadata is community-edited: anything that is not
#' plainly an http(s) URL is dropped rather than rendered. Protocol-relative and
#' http URLs both appear in the wild and are normalised rather than discarded.
#'
#' The twin of this lives in dev/fetch_species_images.R, which is where the
#' cached values come from; they are kept in step deliberately, as strip() and
#' the credit fallback already are.
fw_licence_url <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x <- trimws(as.character(x)[1])
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  if (grepl("^//", x)) x <- paste0("https:", x)
  if (!grepl("^https?://", x)) return(NA_character_)
  x
}

# ---- The cache ---------------------------------------------------------------

#' Look a species' imagery up in the published cache
#'
#' @return a one-row list with url, credit, licence, licence_url and page_url,
#'   or NULL
fw_species_image_cached <- function(species, species_id) {
  if (!"image_url" %in% names(species)) return(NULL)
  row <- species[species$species_id == species_id, ]
  if (!nrow(row)) return(NULL)
  get1 <- function(col) {
    if (!col %in% names(row)) return(NA_character_)
    as.character(row[[col]][1])
  }
  url <- get1("image_url")
  credit <- get1("image_credit")
  licence <- get1("image_licence")
  if (is.na(url) || !nzchar(url)) return(NULL)
  # No licence, no picture. The credit always resolves to something - see the
  # fallback in fw_species_image_fetch() - but a photograph whose licence we
  # cannot state is one we have no business republishing.
  if (is.na(licence)) return(NULL)
  if (is.na(credit) || !nzchar(credit)) credit <- fw_t("species", "credit_fallback")
  list(url = url, credit = credit, licence = licence,
       licence_url = get1("image_licence_url"),
       page_url = get1("image_page_url"))
}

# ---- The live fallback -------------------------------------------------------

#' Resolve one species against Wikipedia and Commons, right now
#'
#' Memoised per process, INCLUDING the misses: a species Wikipedia does not have
#' should be asked about once, not once per popup. Every failure path returns
#' NULL rather than raising, because a missing photograph must never take a map
#' down with it.
fw_species_image_fetch <- function(scientific_name, common_name = NA) {
  titles <- unique(stats::na.omit(c(scientific_name, common_name)))
  titles <- titles[nzchar(titles)]
  if (!length(titles)) return(NULL)

  api <- function(base, params) {
    out <- try(
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
  strip <- function(x) {
    if (is.null(x)) return(NA_character_)
    x <- trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", "", x)))
    if (!nzchar(x)) NA_character_ else x
  }

  for (title in titles) {
    j <- api("https://en.wikipedia.org/w/api.php", list(
      action = "query", format = "json", redirects = 1,
      prop = "pageimages", piprop = "name|thumbnail",
      pithumbsize = 500, titles = title
    ))
    page <- j$query$pages
    if (is.null(page) || !length(page)) next
    page <- page[[1]]
    if (is.null(page$pageimage) || is.null(page$thumbnail)) next

    k <- api("https://commons.wikimedia.org/w/api.php", list(
      action = "query", format = "json",
      prop = "imageinfo", iiprop = "extmetadata|url",
      titles = paste0("File:", page$pageimage)
    ))
    cpage <- k$query$pages
    if (is.null(cpage) || !length(cpage)) next
    info <- cpage[[1]]$imageinfo
    if (is.null(info) || !length(info)) next
    meta <- info[[1]]$extmetadata

    # Same fallback as dev/fetch_species_images.R: a Commons file often carries
    # a licence but no Artist, and crediting the source satisfies CC where no
    # author is recorded. Only the licence is mandatory.
    credit <- strip(meta$Artist$value)
    if (is.na(credit)) credit <- strip(meta$Credit$value)
    if (is.na(credit)) credit <- fw_t("species", "credit_fallback")
    licence <- strip(meta$LicenseShortName$value)
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

# memoise caches the NULLs too, which is the point: a species with no article
# must not be asked about again on every popup for the rest of the session.
if (requireNamespace("memoise", quietly = TRUE)) {
  fw_species_image_fetch <- memoise::memoise(fw_species_image_fetch)
}

# ---- Markup ------------------------------------------------------------------

#' A species photograph with its credit, or a labelled placeholder
#'
#' THE LOADING STATE IS CSS, NOT JAVASCRIPT. The <img> sits on a pulsing
#' skeleton and covers it when it paints, so a slow Wikimedia CDN shows movement
#' rather than a blank box, with no script and nothing to go wrong. Popups are
#' built server-side in bulk, so anything per-image and scripted would run
#' hundreds of times for markers nobody opens.
#'
#' THE CAPTION IS TWO LINKS, NOT ONE. It reads "<author> / <licence>": the
#' author points at the Commons file page, which is where the work and its full
#' terms are, and the licence points at its own deed, which is what CC BY and
#' CC BY-SA actually require and what naming the licence in plain text does not
#' give them. Either link degrades to plain text on its own when the URL is
#' missing - a public-domain tag has no deed to point at, and that is not a gap.
#'
#' @param img  the resolved list, or NULL for the placeholder
#' @param name what to caption it with, and what the placeholder says
fw_species_figure <- function(img, name) {
  if (is.null(img)) {
    return(as.character(tags$div(
      class = "fw-species-figure fw-species-figure--none",
      tags$span(class = "fw-species-figure__placeholder",
                fw_t("species", "no_image"))
    )))
  }
  # A link out of a popup, wherever it goes: new tab, and never handing the
  # referrer or a window handle to Wikimedia.
  out <- function(url, text) {
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
#' @param live whether to fall back to the network. FALSE everywhere a lot of
#'   these are built at once.
fw_species_figure_for <- function(species, species_id, name, live = FALSE) {
  img <- fw_species_image_cached(species, species_id)
  if (is.null(img) && isTRUE(live)) {
    row <- species[species$species_id == species_id, ]
    if (nrow(row)) {
      img <- fw_species_image_fetch(row$scientific_name[1], row$common_name[1])
    }
  }
  fw_species_figure(img, name)
}
