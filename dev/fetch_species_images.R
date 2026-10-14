# fetch_species_images.R
# Resolve a photograph for each species from Wikipedia and Wikimedia Commons,
# and write the result into fwise-data/species.csv.
#
#     Rscript dev/fetch_species_images.R           # fill in what is missing
#     Rscript dev/fetch_species_images.R --refresh # re-resolve everything
#     Rscript dev/fetch_species_images.R --limit 20
#
# WHY THIS IS A BUILD SCRIPT AND NOT SOMETHING THE APP DOES. config.R promises
# that local development makes no network calls, and a popup that waits on a
# third-party API is a popup that sometimes does not open. Resolving once and
# committing the result keeps the running app offline and instant. The app still
# has a live fallback for a species added since the last run - see
# fw_species_image() in R/species_images.R - but on a full cache it never fires.
#
# ATTRIBUTION IS PART OF THE DATA, NOT A NICETY. Most of these images are CC
# BY-SA, which requires the author and the licence to be shown wherever the
# image is. So image_credit, image_licence and image_page_url are resolved and
# stored alongside the URL, and a row missing them is treated as a miss rather
# than shipped without them.
#
# AND THE LICENCE MUST BE A LINK, NOT A NAME. Both CC BY and CC BY-SA require a
# link to the licence itself, so "CC BY-SA 4.0" as plain text does not satisfy
# them however prominently it is shown. Commons returns LicenseUrl in the same
# extmetadata call as the author and the short name, so image_licence_url costs
# nothing extra to collect; fw_species_figure() wraps the name in it. Rows
# resolved before the column existed are filled in by the backfill pass at the
# bottom of this script, which needs the Commons call only.
#
# BE POLITE. One request a second, a real User-Agent naming the project, and the
# script is idempotent so a re-run does not re-ask for what it already has.

suppressMessages({
  library(dplyr)
  library(readr)
})

source("R/brand.R")   # config.R reads brand tokens, so brand.R comes first
source("R/config.R")

# The API calls, the polite pause and the licence backfill live in
# dev/species_enrich.R, which dev/qa.R uses for the species an import adds.
source("dev/species_enrich.R")

args <- commandArgs(TRUE)
refresh <- "--refresh" %in% args
limit <- if ("--limit" %in% args) {
  as.integer(args[which(args == "--limit") + 1L])
} else NA_integer_

# ---- The run -----------------------------------------------------------------

path <- FW_SPECIES_CSV
species <- read_csv(path, show_col_types = FALSE, progress = FALSE)

# The image columns, added once and thereafter carried by the file.
for (col in c("image_url", "image_credit", "image_licence",
              "image_licence_url", "image_page_url")) {
  if (!col %in% names(species)) species[[col]] <- NA_character_
  species[[col]] <- as.character(species[[col]])
}

todo <- if (refresh) seq_len(nrow(species)) else which(is.na(species$image_url))
if (!is.na(limit)) todo <- head(todo, limit)

cat(sprintf("%d species, %d to resolve%s\n", nrow(species), length(todo),
            if (refresh) " (refresh)" else ""))

hits <- 0L
misses <- character(0)
for (i in seq_along(todo)) {
  r <- todo[i]
  got <- fw_resolve_species(species$scientific_name[r], species$common_name[r])
  # %||% only catches NULL; a great many rows have no scientific name at all.
  label <- species$scientific_name[r]
  if (is.na(label) || !nzchar(label)) label <- species$common_name[r]
  if (is.na(label)) label <- species$species_id[r]
  if (is.null(got)) {
    misses <- c(misses, label)
    cat(sprintf("  [%3d/%3d] %-42s -\n", i, length(todo), substr(label, 1, 42)))
    next
  }
  species$image_url[r]         <- got$url
  species$image_credit[r]      <- got$credit
  species$image_licence[r]     <- got$licence
  species$image_licence_url[r] <- got$licence_url
  species$image_page_url[r]    <- got$page_url
  hits <- hits + 1L
  cat(sprintf("  [%3d/%3d] %-42s %s\n", i, length(todo),
              substr(label, 1, 42), substr(got$licence, 1, 22)))

  # Written as it goes, so an interrupted run keeps what it already found.
  if (i %% 20 == 0) write_csv(species, path, na = "")
}

# ---- Backfill: licence URLs -------------------------------------------------
#
# image_licence_url arrived after the first 308 photographs were already
# resolved, and re-running the whole script to collect one field would mean 600
# needless Wikipedia calls. These rows already name their Commons file, so the
# Commons half of the lookup is all that is missing - and Commons takes up to 50
# titles per query, which turns 300 polite one-a-second requests into seven.
#
# Idempotent like the rest of the script: a row that already has a licence URL,
# and a licence with no deed to link to (most public-domain tags), are both left
# alone, so a second run asks for nothing.

species <- fw_backfill_licence_urls(species)

write_csv(species, path, na = "")

cat(sprintf("\nresolved %d, missed %d, cache now %d of %d\n",
            hits, length(misses), sum(!is.na(species$image_url)), nrow(species)))
if (length(misses)) {
  cat("misses:\n")
  cat(paste0("  ", misses, collapse = "\n"), "\n")
}
