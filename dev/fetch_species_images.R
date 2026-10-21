# fetch_species_images.R
# Resolve a photograph for each species from Wikipedia and Wikimedia Commons,
# and write the result into fwise-data/species.csv.
#
#     Rscript dev/fetch_species_images.R           # fill in what is missing
#     Rscript dev/fetch_species_images.R --refresh # re-resolve everything
#     Rscript dev/fetch_species_images.R --limit 20
#
# Resolved once and committed, so the app stays offline (it has a live
# fallback for new species). Credit, licence, licence URL and file page are
# stored with each URL (CC BY-SA needs them); a row missing a licence is a
# miss.

suppressMessages({
  library(dplyr)
  library(readr)
})

source("R/brand.R")   # config.R reads brand tokens, so brand.R comes first
source("R/config.R")
source("dev/species_enrich.R")

args = commandArgs(TRUE)
refresh = "--refresh" %in% args
limit = if ("--limit" %in% args) {
  as.integer(args[which(args == "--limit") + 1L])
} else NA_integer_

# ---- The run -----------------------------------------------------------------

path = FW_SPECIES_CSV
species = read_csv(path, show_col_types = FALSE, progress = FALSE)

# The image columns, added once and thereafter carried by the file.
for (col in c("image_url", "image_credit", "image_licence",
              "image_licence_url", "image_page_url")) {
  if (!col %in% names(species)) species[[col]] = NA_character_
  species[[col]] = as.character(species[[col]])
}

todo = if (refresh) seq_len(nrow(species)) else which(is.na(species$image_url))
if (!is.na(limit)) todo = head(todo, limit)

cat(sprintf("%d species, %d to resolve%s\n", nrow(species), length(todo),
            if (refresh) " (refresh)" else ""))

hits = 0L
misses = character(0)
for (i in seq_along(todo)) {
  r = todo[i]
  got = fw_resolve_species(species$scientific_name[r], species$common_name[r])
  # %||% only catches NULL; many rows have no scientific name.
  label = species$scientific_name[r]
  if (is.na(label) || !nzchar(label)) label = species$common_name[r]
  if (is.na(label)) label = species$species_id[r]
  if (is.null(got)) {
    misses = c(misses, label)
    cat(sprintf("  [%3d/%3d] %-42s -\n", i, length(todo), substr(label, 1, 42)))
    next
  }
  species$image_url[r]         = got$url
  species$image_credit[r]      = got$credit
  species$image_licence[r]     = got$licence
  species$image_licence_url[r] = got$licence_url
  species$image_page_url[r]    = got$page_url
  hits = hits + 1L
  cat(sprintf("  [%3d/%3d] %-42s %s\n", i, length(todo),
              substr(label, 1, 42), substr(got$licence, 1, 22)))

  # Written as it goes, so an interrupted run keeps its progress.
  if (i %% 20 == 0) write_csv(species, path, na = "")
}

# ---- Backfill: licence URLs -------------------------------------------------
#
# Fills missing licence URLs from Commons only, 50 titles per query.
# Idempotent: rows with a URL, or a licence with no deed, are skipped.

species = fw_backfill_licence_urls(species)

write_csv(species, path, na = "")

cat(sprintf("\nresolved %d, missed %d, cache now %d of %d\n",
            hits, length(misses), sum(!is.na(species$image_url)), nrow(species)))
if (length(misses)) {
  cat("misses:\n")
  cat(paste0("  ", misses, collapse = "\n"), "\n")
}
