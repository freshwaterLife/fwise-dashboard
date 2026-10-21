# species_enrich.R
# Lookups for new species. Sourced by dev/qa.R (section 3) and
# dev/fetch_species_images.R.
#
#   family      GBIF species match. No key.
#   IUCN        Red List API v4. Needs IUCN_API_TOKEN (free, non-commercial,
#               api.iucnredlist.org); without it IUCN columns stay blank.
#   photograph  Wikipedia's lead image, with Commons credit and licence.
#
# Fills blanks only; misses are listed, never guessed. Network calls only, no
# file reads or writes.

FW_UA = paste(
  "FWISE-dashboard/1.0 (https://www.freshwaterlife.org;",
  "freshwater invasive species eradication database)"
)
FW_WIKI_PAUSE = 1.0
FW_THUMB_PX = 500

# ---- The API calls -----------------------------------------------------------

fw_api = function(base, params) {
  url = httr2::request(base) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(FW_UA) |>
    httr2::req_timeout(20) |>
    # One flaky response shouldn't abandon a long run.
    httr2::req_retry(max_tries = 3, backoff = ~ 2)
  out = try(httr2::req_perform(url), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  try(httr2::resp_body_json(out), silent = TRUE)
}

#' The lead image filename for a Wikipedia article
#'
#' redirects=1: most scientific names redirect to the common-name article.
fw_wiki_page_image = function(title) {
  j = fw_api("https://en.wikipedia.org/w/api.php", list(
    action = "query", format = "json", redirects = 1,
    prop = "pageimages", piprop = "name|thumbnail",
    pithumbsize = FW_THUMB_PX, titles = title
  ))
  pages = j$query$pages
  if (is.null(pages) || !length(pages)) return(NULL)
  page = pages[[1]]
  if (!is.null(page$missing) || is.null(page$pageimage)) return(NULL)
  list(
    file = page$pageimage,
    # Strip the analytics query string; it is not part of the image.
    thumb = sub("[?].*$", "", page$thumbnail$source %||% NA_character_),
    title = page$title
  )
}

#' Author, licence and file page for a Commons file
fw_commons_credit = function(file) {
  j = fw_api("https://commons.wikimedia.org/w/api.php", list(
    action = "query", format = "json",
    prop = "imageinfo", iiprop = "extmetadata|url",
    titles = paste0("File:", file)
  ))
  pages = j$query$pages
  if (is.null(pages) || !length(pages)) return(NULL)
  info = pages[[1]]$imageinfo
  if (is.null(info) || !length(info)) return(NULL)
  meta = info[[1]]$extmetadata

  # Artist arrives as a fragment of HTML often wrapping a link.
  strip = function(x) {
    if (is.null(x)) return(NA_character_)
    x = gsub("<[^>]*>", "", x)
    x = gsub("&amp;", "&", x, fixed = TRUE)
    x = trimws(gsub("[[:space:]]+", " ", x))
    if (!nzchar(x)) NA_character_ else x
  }
  # No Artist: credit the source and link the file page (satisfies CC).
  list(
    credit      = fw_first_present(strip(meta$Artist$value),
                                   strip(meta$Credit$value),
                                   "Wikimedia Commons"),
    licence     = strip(meta$LicenseShortName$value),
    licence_url = fw_licence_url(meta$LicenseUrl$value),
    page_url    = info[[1]]$descriptionurl %||% NA_character_
  )
}

#' The licence deed URL, or NA
#'
#' Public-domain files usually have none. Protocol-relative and http URLs are
#' normalised here.
fw_licence_url = function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x = trimws(as.character(x)[1])
  if (!nzchar(x)) return(NA_character_)
  if (grepl("^//", x)) x = paste0("https:", x)
  if (!grepl("^https?://", x)) return(NA_character_)
  x
}

`%||%` = function(x, y) if (is.null(x) || length(x) == 0) y else x

#' The first argument that is neither NULL nor NA nor empty
fw_first_present = function(...) {
  for (x in list(...)) {
    if (!is.null(x) && length(x) && !is.na(x) && nzchar(x)) return(x)
  }
  NA_character_
}

#' One species, both calls, or NULL
#'
#' Scientific name first, then common name. "Barbus sp." misses, correctly.
fw_resolve_species = function(scientific, common) {
  for (title in unique(na.omit(c(scientific, common)))) {
    if (!nzchar(title)) next
    img = fw_wiki_page_image(title)
    Sys.sleep(FW_WIKI_PAUSE)
    if (is.null(img) || is.na(img$thumb)) next
    cr = fw_commons_credit(img$file)
    Sys.sleep(FW_WIKI_PAUSE)
    # Only the licence is mandatory; the credit always resolves to something.
    if (is.null(cr) || is.na(cr$licence)) next
    return(list(url = img$thumb, credit = cr$credit,
                licence = cr$licence, licence_url = cr$licence_url,
                page_url = cr$page_url,
                matched = title, article = img$title))
  }
  NULL
}

# ---- Backfill: licence URLs -------------------------------------------------

#' The "File:..." title inside a Commons file-page URL
#'
#' Decoded; httr2 re-encodes it.
fw_commons_title = function(page_url) {
  if (is.na(page_url) || !grepl("/wiki/", page_url, fixed = TRUE)) {
    return(NA_character_)
  }
  utils::URLdecode(sub("^.*/wiki/", "", page_url))
}

fw_backfill_licence_urls = function(species) {
  need = which(
    !is.na(species$image_url) & nzchar(species$image_url) &
      !is.na(species$image_page_url) &
      (is.na(species$image_licence_url) | !nzchar(species$image_licence_url))
  )
  if (!length(need)) return(species)

  titles = vapply(species$image_page_url[need], fw_commons_title, character(1),
                   USE.NAMES = FALSE)
  need = need[!is.na(titles)]
  titles = titles[!is.na(titles)]
  if (!length(need)) return(species)

  cat(sprintf("backfilling licence URLs for %d rows\n", length(need)))
  # Match by title: the API keys by page id and normalises spaces.
  norm = function(x) gsub(" ", "_", x, fixed = TRUE)
  found = 0L
  for (start in seq(1, length(need), by = 50)) {
    idx = start:min(start + 49L, length(need))
    j = fw_api("https://commons.wikimedia.org/w/api.php", list(
      action = "query", format = "json",
      prop = "imageinfo", iiprop = "extmetadata",
      titles = paste(titles[idx], collapse = "|")
    ))
    Sys.sleep(FW_WIKI_PAUSE)
    pages = j$query$pages
    if (is.null(pages) || !length(pages)) next
    urls = stats::setNames(
      vapply(pages, function(p) {
        info = p$imageinfo
        if (is.null(info) || !length(info)) return(NA_character_)
        fw_licence_url(info[[1]]$extmetadata$LicenseUrl$value)
      }, character(1)),
      norm(vapply(pages, function(p) p$title %||% NA_character_, character(1)))
    )
    hit = urls[norm(titles[idx])]
    got = !is.na(hit)
    species$image_licence_url[need[idx][got]] = unname(hit[got])
    found = found + sum(got)
    cat(sprintf("  [%3d/%3d] %d with a deed URL\n",
                min(idx[length(idx)], length(need)), length(need), found))
  }
  cat(sprintf("backfill: %d of %d rows now link their licence\n",
              found, length(need)))
  species
}

# ---- Family: GBIF -------------------------------------------------------------

#' GBIF's backbone match for a scientific name, or NULL
#'
#' EXACT matches only (FUZZY and HIGHERRANK would be guesses).
fw_gbif_match = function(scientific) {
  if (is.na(scientific) || !nzchar(scientific)) return(NULL)
  j = fw_api("https://api.gbif.org/v1/species/match",
              list(name = scientific, strict = "true"))
  if (is.null(j) || inherits(j, "try-error") || !identical(j$matchType, "EXACT")) return(NULL)
  list(family = j$family %||% NA_character_, matched = j$scientificName %||% NA_character_)
}

# ---- IUCN Red List: API v4 ------------------------------------------------------

# The category names species.csv stores beside the code (iucn_category).
FW_IUCN_CATEGORIES = c(
  EX = "Extinct", EW = "Extinct in the Wild", CR = "Critically Endangered",
  EN = "Endangered", VU = "Vulnerable", NT = "Near Threatened",
  LC = "Least Concern", DD = "Data Deficient", NE = "Not Evaluated",
  "LR/cd" = "Lower Risk/conservation dependent", "LR/nt" = "Lower Risk/near threatened",
  "LR/lc" = "Lower Risk/least concern"
)

#' The latest GLOBAL Red List assessment for a scientific name, or NULL
#'
#' Token in Authorization with no "Bearer". Trinomials use infra_name.
#' Regional-only assessments are ignored.
fw_iucn_lookup = function(scientific, token = Sys.getenv("IUCN_API_TOKEN")) {
  if (!nzchar(token) || is.na(scientific)) return(NULL)
  parts = strsplit(trimws(scientific), "\\s+")[[1]]
  if (length(parts) < 2 || grepl("^sp+\\.?$", parts[2])) return(NULL)
  params = list(genus_name = parts[1], species_name = parts[2])
  if (length(parts) >= 3) params$infra_name = parts[3]
  resp = try(httr2::request("https://api.iucnredlist.org/api/v4/taxa/scientific_name") |>
                httr2::req_url_query(!!!params) |>
                httr2::req_headers(Authorization = token) |>
                httr2::req_user_agent(FW_UA) |>
                httr2::req_timeout(20) |>
                httr2::req_error(is_error = function(resp) FALSE) |>
                httr2::req_perform(), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (httr2::resp_status(resp) == 401) stop("IUCN refused IUCN_API_TOKEN (HTTP 401).", call. = FALSE)
  if (httr2::resp_status(resp) != 200) return(NULL)
  j = httr2::resp_body_json(resp)
  global = Filter(function(a) {
    isTRUE(a$latest) && any(vapply(a$scopes %||% list(), function(s)
      identical(as.character(s$code), "1") || identical(s$description$en, "Global"), logical(1)))
  }, j$assessments %||% list())
  if (!length(global)) return(NULL)
  a = global[[1]]
  code = a$red_list_category_code %||% NA_character_
  list(iucn_status = code,
       iucn_category = unname(FW_IUCN_CATEGORIES[code]) %||% NA_character_,
       iucn_url = a$url %||% NA_character_,
       iucn_year = as.character(a$year_published %||% NA_character_),
       iucn_matched_name = j$taxon$scientific_name %||% scientific)
}

# ---- All three, for the species an import adds -----------------------------------

#' Fill the blanks of new species rows from GBIF, IUCN and Wikipedia/Commons
#'
#' The three lookups are arguments so they can be stubbed.
#'
#' @param species rows with FW_SPECIES_COLUMNS
#' @return list(species = the rows, log = one row per species and source)
fw_enrich_species = function(species, gbif = fw_gbif_match, iucn = fw_iucn_lookup,
                              photo = fw_resolve_species) {
  log = list()
  note = function(i, source, result) log[[length(log) + 1]] <<- tibble::tibble(
    species_id = species$species_id[i],
    label = fw_species_label(species[i, ])$label, source = source, result = result)
  blank = function(x) is.na(x) | !nzchar(x)
  for (i in seq_len(nrow(species))) {
    sci = species$scientific_name[i]
    if (blank(species$family[i]) && species$taxa[i] %in% "Fish") {
      g = gbif(sci)
      if (!is.null(g) && !blank(g$family)) {
        species$family[i] = g$family
        note(i, "family (GBIF)", paste0(g$family, " (GBIF name: ", g$matched, ")"))
      } else note(i, "family (GBIF)", "NOT FOUND - fill it in species.csv or the new_species tab")
    }
    if (blank(species$iucn_status[i])) {
      if (!nzchar(Sys.getenv("IUCN_API_TOKEN")) && identical(iucn, fw_iucn_lookup)) {
        note(i, "IUCN", "skipped - IUCN_API_TOKEN is not set")
      } else {
        u = iucn(sci)
        if (!is.null(u)) {
          for (col in names(u)) species[[col]][i] = u[[col]]
          note(i, "IUCN", paste(u$iucn_status, u$iucn_year))
        } else note(i, "IUCN", "NOT FOUND - no global assessment under this name")
      }
    }
    if (blank(species$image_url[i])) {
      p = photo(sci, species$common_name[i])
      if (!is.null(p)) {
        species$image_url[i] = p$url; species$image_credit[i] = p$credit
        species$image_licence[i] = p$licence; species$image_licence_url[i] = p$licence_url
        species$image_page_url[i] = p$page_url
        note(i, "photo", paste(p$licence, "-", p$credit))
      } else note(i, "photo", "NOT FOUND - the app shows no photograph")
    }
  }
  list(species = species,
       log = if (length(log)) dplyr::bind_rows(log)
             else tibble::tibble(species_id = character(0), label = character(0),
                                 source = character(0), result = character(0)))
}
