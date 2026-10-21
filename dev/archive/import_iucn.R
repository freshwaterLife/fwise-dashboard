# import_iucn.R
# ARCHIVED. Wrote the IUCN columns into species.csv from a one-off download
# keyed on "Common (Scientific)" labels, and filled blank taxa and family. New
# species now get IUCN data from dev/qa.R (section 3).
#
#     Rscript dev/archive/import_iucn.R <download.csv>
#
# Stops before writing if a species has two statuses or isn't held.

suppressPackageStartupMessages({ library(readr) })
source(file.path("R", "brand.R")); source(file.path("R", "config.R"))
source(file.path("R", "data_load.R"))

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript dev/archive/import_iucn.R <download.csv>", call. = FALSE)

rd = function(p) utils::read.csv(p, colClasses = "character", na.strings = c("", "NA"),
                                  check.names = FALSE, encoding = "UTF-8")
iu = rd(args[1])
species = rd(FW_SPECIES_CSV)
for (col in setdiff(FW_SPECIES_COLUMNS, names(species))) species[[col]] = NA_character_

# ---- One row per species -------------------------------------------------------
IUCN_COLS = c("iucn_status", "iucn_category", "iucn_url", "iucn_year", "iucn_note",
               "iucn_matched_name")
missing = setdiff(c("species", "taxa", "fish_family", IUCN_COLS), names(iu))
if (length(missing)) stop("The download has no column: ", paste(missing, collapse = ", "), call. = FALSE)

key = function(d) do.call(paste, c(lapply(d[IUCN_COLS], function(x) ifelse(is.na(x), "", x)), sep = "\r"))
conflict = tapply(key(iu), iu$species, function(k) length(unique(k)) > 1)
if (any(conflict)) stop("Listed twice with different assessments:\n  ",
                        paste(names(conflict)[conflict], collapse = "\n  "), call. = FALSE)
iu = iu[!duplicated(iu$species), ]

# ---- Match on the label --------------------------------------------------------
label = fw_species_label(species)$label
row = match(iu$species, label)
if (anyNA(row)) stop("Not a species in species.csv:\n  ",
                     paste(iu$species[is.na(row)], collapse = "\n  "), call. = FALSE)
unlisted = setdiff(label, iu$species)

for (col in IUCN_COLS) species[[col]][row] = iu[[col]]

blank_taxa = is.na(species$taxa[row]) & !is.na(iu$taxa)
species$taxa[row[blank_taxa]] = iu$taxa[blank_taxa]
fish = species$taxa[row] %in% "Fish"
blank_fam = fish & is.na(species$family[row]) & !is.na(iu$fish_family)
species$family[row[blank_fam]] = iu$fish_family[blank_fam]

write_csv(species[, FW_SPECIES_COLUMNS], FW_SPECIES_CSV, na = "")

# ---- Report --------------------------------------------------------------------
message("species.csv: ", length(row), " of ", nrow(species), " species matched; ",
        sum(!is.na(iu$iucn_status)), " with a status")
print(table(status = iu$iucn_status, useNA = "ifany"))
if (length(unlisted)) message("\nNot in the download (IUCN left as it was):\n  ",
                              paste(unlisted, collapse = "\n  "))
filled = c(paste0(iu$species[blank_taxa], ": taxa ", iu$taxa[blank_taxa]),
            paste0(iu$species[blank_fam], ": family ", iu$fish_family[blank_fam]))
if (length(filled)) message("\nFilled where blank (", length(filled), "):\n  ",
                            paste(filled, collapse = "\n  "))
left = species[is.na(species$taxa) | (species$taxa %in% "Fish" & is.na(species$family)), ]
if (nrow(left)) message("\nStill without taxa/family: ",
                        paste(fw_species_label(left)$label, collapse = "; "))
message("\nNext: Rscript dev/fetch_species_images.R")
