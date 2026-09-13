# reconcile_source.R
# PROVES THE DATA HOLDS EVERY SOURCE COLUMN AND EVERY SOURCE VALUE, identically
# coded. Reads the client's export under fwise-data/source/, the row-to-id map
# beside it, and attempts.csv, species.csv and contacts.csv, and walks all 74
# source columns. Each has exactly one rule; a column with no rule is itself a
# failure, so a new column in a future export cannot slip through unnoticed.
#
#     Rscript dev/reconcile_source.R
#
# Plain base R for the checks, deliberately: the app's load uses dplyr, and a
# test that repeats the app's own joins would only prove they agree with
# themselves.
#
# WHAT COUNTS AS A DIFFERENCE. Cells are compared as trimmed text with blank
# and NA treated as the same absence and Windows line endings inside a cell
# read as plain newlines, which is the only normalisation the data applies. The only transformations the data layer
# makes are the country/region split and the water-regime corrections, and
# both are asserted rather than skipped. Two things are REPORTED but do not
# fail the run, because both are deliberate: the regime corrections named in
# FW_REGIME_BY_TYPE, and a contact whose email differed between rows in the
# export and now carries one address.

source(file.path("R", "brand.R")); source(file.path("R", "config.R"))
source(file.path("R", "data_load.R"))

read_text <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
}
# The same normalisation the migration applied, and nothing else: trim, and
# "\r\n" inside a cell becomes "\n".
norm <- function(x) { x <- trimws(gsub("\r\n?", "\n", x)); x[!is.na(x) & x == ""] <- NA; x }

src_dir <- file.path(FW_DATA_DIR, "source")
raw_path <- list.files(src_dir, pattern = "^fwise_.*[0-9]\\.csv$", full.names = TRUE)
if (!length(raw_path)) raw_path <- list.files(FW_DATA_DIR, pattern = "^fwise_.*[0-9]\\.csv$", full.names = TRUE)
if (length(raw_path) != 1) stop("Expected exactly one raw export, found: ", paste(raw_path, collapse = ", "))
ids_path <- file.path(src_dir, sub("\\.csv$", "_ids.csv", basename(raw_path)))

raw <- read_text(raw_path); raw[] <- lapply(raw, norm)
map <- read_text(ids_path)
att <- read_text(FW_ATTEMPTS_CSV); att[] <- lapply(att, norm)
spp <- read_text(FW_SPECIES_CSV);  spp[] <- lapply(spp, norm)
con <- read_text(FW_CONTACTS_CSV); con[] <- lapply(con, norm)

stopifnot(nrow(map) == nrow(raw))
a <- att[match(map$attempt_id, att$attempt_id), ]
if (any(is.na(a$attempt_id))) stop("Source rows whose attempt_id is not in attempts.csv: ",
                                   sum(is.na(a$attempt_id)))
n <- nrow(raw)

same <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
split_multi <- function(x, sep) if (is.na(x)) character(0) else trimws(strsplit(x, sep, fixed = TRUE)[[1]])
split_us    <- function(x) if (is.na(x)) character(0) else trimws(strsplit(x, "_", fixed = TRUE)[[1]])

# ---- Lookups ------------------------------------------------------------------
sp_label <- ifelse(!is.na(spp$common_name) & !is.na(spp$scientific_name),
                   paste0(spp$common_name, " (", spp$scientific_name, ")"),
                   ifelse(!is.na(spp$scientific_name), spp$scientific_name, spp$common_name))
names(sp_label) <- spp$species_id
sp_taxa   <- setNames(spp$taxa, spp$species_id)
sp_family <- setNames(spp$family, spp$species_id)
con_by_id <- con[match(con$contact_id, con$contact_id), ]; rownames(con_by_id) <- con$contact_id

# ---- Results collector ---------------------------------------------------------
results <- list(); details <- list(); reported <- list()
record <- function(source_col, destination, checked, bad_rows, note = "", fail = TRUE) {
  results[[length(results) + 1]] <<- data.frame(
    source_column = source_col, destination = destination, rows_checked = checked,
    rows_differing = length(bad_rows), stringsAsFactors = FALSE)
  if (length(bad_rows)) {
    d <- data.frame(
      source_row = bad_rows, attempt_id = a$attempt_id[bad_rows],
      source = as.character(note$src[bad_rows]), data = as.character(note$dst[bad_rows]),
      stringsAsFactors = FALSE)
    if (fail) details[[source_col]] <<- d else reported[[source_col]] <<- d
  }
}
checked_cols <- character(0)
rule <- function(source_col, ...) { checked_cols <<- c(checked_cols, source_col); record(source_col, ...) }

# ---- 1. Identity columns ------------------------------------------------------
identity <- c(
  "Location Name" = "site_name",
  "System" = "waterbody_type",
  "Latitude (decimal degrees)" = "latitude", "Longitude (decimal degrees)" = "longitude",
  "Area Treated (size)" = "area_treated", "Area Treated (unit)" = "area_unit",
  "Area Treated Notes" = "area_notes",
  "Depth (m)" = "depth_m", "Depth Notes" = "depth_notes",
  "Volume (m3)" = "volume_m3", "Volume Notes" = "volume_notes",
  "Max Flow (m3/s)" = "max_flow_m3s",
  "Water Temperature (C)" = "water_temp_c", "Water Temperature (C) Notes" = "water_temp_notes",
  "Year of Invasion" = "invasion_year", "Eradication Start Year" = "start_year",
  "Eradication End Year" = "end_year", "Duration (days)" = "duration_days",
  "Eradication or Control" = "eradication_or_control", "Eradication Reason" = "driver",
  "Outcome" = "outcome",
  "Method Description" = "method_description", "Labor Effort (person days)" = "labour_person_days",
  "Target Ingredient Basis" = "target_ingredient_basis",
  "Target Toxin Concentration (mg/L)" = "toxin_conc_target_mg_l",
  "Target Toxin Concentration Notes" = "conc_target_notes",
  "Measured Toxin Concentration (mg/L)" = "toxin_conc_measured_mg_l",
  "Measured Toxin Concentration Notes" = "conc_measured_notes",
  "Neutralizing Agent" = "neutralising_agent", "Neutralizing Agent Notes" = "neutralising_notes",
  "Eradication Verification Method" = "verification_method",
  "Eradication Verification Notes" = "verification_notes",
  "Source" = "source", "Sent" = "sent",
  "Eradication Reference" = "reference", "Eradication Link" = "reference_link",
  "Notes for FWISE" = "notes_for_fwise", "Submission Date" = "submitted_at"
)
for (sc in names(identity)) {
  dc <- identity[[sc]]
  bad <- which(!same(raw[[sc]], a[[dc]]))
  rule(sc, dc, n, bad, list(src = raw[[sc]], dst = a[[dc]]))
}
# Key: authoritative where filled, so it must equal attempt_id there.
bad <- which(!is.na(raw$Key) & raw$Key != a$attempt_id)
rule("Key", "attempt_id (where filled)", sum(!is.na(raw$Key)), bad,
     list(src = raw$Key, dst = a$attempt_id))

# ---- 2. Species slots, taxa and family ----------------------------------------
check_role <- function(prefix, taxa_col, family_col, cell_col) {
  slots <- paste0(prefix, " ", 1:8)
  bad_sp <- integer(0); bad_tx <- integer(0); bad_fm <- integer(0)
  src_sp <- character(n); dst_sp <- character(n); src_tx <- character(n); dst_tx <- character(n)
  src_fm <- character(n); dst_fm <- character(n)
  for (r in seq_len(n)) {
    want <- unlist(raw[r, slots]); want <- want[!is.na(want)]
    ids  <- split_multi(a[[cell_col]][r], FW_MULTI_SEP)
    got  <- unname(sp_label[ids])
    src_sp[r] <- paste(want, collapse = " ; "); dst_sp[r] <- paste(got, collapse = " ; ")
    if (!identical(unname(want), got)) bad_sp <- c(bad_sp, r)
    # Taxa align to the slots by position.
    tw <- split_us(raw[[taxa_col]][r])
    if (length(tw)) {
      tg <- unname(sp_taxa[ids])[seq_along(tw)]
      src_tx[r] <- paste(tw, collapse = "_"); dst_tx[r] <- paste(tg, collapse = "_")
      if (!identical(tw, tg)) bad_tx <- c(bad_tx, r)
    }
    # Family lists only the fish slots, in order.
    fw <- split_us(raw[[family_col]][r])
    if (length(fw)) {
      fish <- ids[!is.na(sp_taxa[ids]) & sp_taxa[ids] == "Fish"]
      fg <- unname(sp_family[fish])[seq_along(fw)]
      src_fm[r] <- paste(fw, collapse = "_"); dst_fm[r] <- paste(fg, collapse = "_")
      if (!identical(fw, fg)) bad_fm <- c(bad_fm, r)
    }
  }
  for (s in slots) rule(s, paste0(cell_col, " -> species.csv"), n, if (s == slots[1]) bad_sp else integer(0),
                        list(src = src_sp, dst = dst_sp))
  rule(taxa_col, "species.csv$taxa", sum(!is.na(raw[[taxa_col]])), bad_tx, list(src = src_tx, dst = dst_tx))
  rule(family_col, "species.csv$family (fish slots)", sum(!is.na(raw[[family_col]])), bad_fm,
       list(src = src_fm, dst = dst_fm))
}
check_role("Invasive Species Eradicated", "Invasive Taxa", "Invasive Fish Family", "invasive_species")
check_role("Eradication Beneficiary", "Beneficiary Taxa", "Beneficiary Fish Family", "beneficiary_species")

# ---- 3. Methods --------------------------------------------------------------
m_cols <- c("Primary Method", "Secondary Method", "Tertiary Method")
n_cols <- c("Primary Methods Notes", "Secondary Methods Notes", "Tertiary Methods Notes")
bad_m <- integer(0); bad_n <- integer(0)
src_m <- character(n); dst_m <- character(n); src_n <- character(n); dst_n <- character(n)
for (r in seq_len(n)) {
  want <- unlist(raw[r, m_cols]); keep <- !is.na(want)
  all_notes <- unlist(raw[r, n_cols])
  wn <- all_notes[keep]; want <- want[keep]
  got <- split_multi(a$methods[r], FW_MULTI_SEP)
  src_m[r] <- paste(want, collapse = " ; "); dst_m[r] <- paste(got, collapse = " ; ")
  if (!identical(unname(want), got)) bad_m <- c(bad_m, r)
  entries <- split_multi(a$method_notes[r], FW_NOTES_SEP)
  # No method at all: the notes cell holds any slot note verbatim.
  if (!length(want)) {
    wn <- unname(all_notes[!is.na(all_notes)])
    src_n[r] <- paste(wn, collapse = " | "); dst_n[r] <- paste(entries, collapse = " | ")
    if (!identical(wn, entries)) bad_n <- c(bad_n, r)
    next
  }
  # A note sitting in a blank method slot beside a filled one would have no
  # home; assert there is none rather than skipping it.
  if (any(!is.na(all_notes[!keep]))) { bad_n <- c(bad_n, r); src_n[r] <- "note in blank slot"; next }
  gn <- if (length(entries)) {
    vapply(seq_along(entries), function(i) {
      e <- entries[i]; pre <- paste0(got[i], ":")
      if (startsWith(e, pre)) e <- substring(e, nchar(pre) + 1)
      e <- trimws(e); if (nzchar(e)) e else NA_character_
    }, "")
  } else rep(NA_character_, length(want))
  src_n[r] <- paste(ifelse(is.na(wn), "", wn), collapse = " | ")
  dst_n[r] <- paste(ifelse(is.na(gn), "", gn), collapse = " | ")
  if (length(gn) != length(wn) || !all(same(unname(wn), gn))) bad_n <- c(bad_n, r)
}
for (i in 1:3) {
  rule(m_cols[i], "methods", n, if (i == 1) bad_m else integer(0), list(src = src_m, dst = dst_m))
  rule(n_cols[i], "method_notes", n, if (i == 1) bad_n else integer(0), list(src = src_n, dst = dst_n))
}

# ---- 4. Contacts -------------------------------------------------------------
check_contact <- function(prefix, id_col) {
  nm <- raw[[paste0(prefix, " Contact Name")]]; em <- raw[[paste0(prefix, " Contact Email")]]
  og <- raw[[paste0(prefix, " Contact Organisation")]]
  c_ <- con_by_id[a[[id_col]], ]
  # A source row with no name must have no id, and vice versa.
  bad_nm <- which(!same(nm, c_$contact_name) & !(is.na(nm) & is.na(a[[id_col]])))
  bad_og <- which(!is.na(nm) & !same(og, c_$organisation))
  # Email: one address per contact. A row whose address differed from the one
  # kept is reported, not failed - it is the de-duplication doing its job.
  bad_em <- which(!is.na(nm) & !same(em, c_$contact_email))
  rule(paste0(prefix, " Contact Name"), paste0(id_col, " -> contacts.csv$contact_name"), n, bad_nm,
       list(src = nm, dst = c_$contact_name))
  rule(paste0(prefix, " Contact Organisation"), "contacts.csv$organisation", sum(!is.na(nm)), bad_og,
       list(src = og, dst = c_$organisation))
  rule(paste0(prefix, " Contact Email"), "contacts.csv$contact_email (one per contact)",
       sum(!is.na(nm)), bad_em, list(src = em, dst = c_$contact_email), fail = FALSE)
}
check_contact("Primary", "primary_contact_id")
check_contact("Secondary", "secondary_contact_id")
redact <- tolower(raw[["Redact Email"]])
bad <- integer(0)
for (r in seq_len(n)) {
  want_public <- !(!is.na(redact[r]) && redact[r] %in% c("yes", "true", "y", "1"))
  for (idc in c("primary_contact_id", "secondary_contact_id")) {
    id <- a[[idc]][r]
    if (!is.na(id) && (toupper(con_by_id[id, "email_public"]) == "TRUE") != want_public) bad <- c(bad, r)
  }
}
rule("Redact Email", "contacts.csv$email_public", n, unique(bad),
     list(src = raw[["Redact Email"]], dst = rep("", n)))

# ---- 5. The two transformations ----------------------------------------------
# The export files a territory under the state that administers it; the data
# gives a territory with its own ISO 3166-1 entry its own country, and iso3 and
# continent follow the country. See fw_validate_geography() in R/data_load.R.
FW_TERRITORY_COUNTRY <- c("United States (Guam)" = "Guam")
src_country <- ifelse(raw$Country %in% names(FW_TERRITORY_COUNTRY),
                      unname(FW_TERRITORY_COUNTRY[raw$Country]), raw$Country)
rebuilt <- ifelse(is.na(a$region), a$country, paste0(a$country, " (", a$region, ")"))
bad <- which(!same(src_country, rebuilt))
rule("Country", "country + region (a territory is its own country)", n, bad,
     list(src = raw$Country, dst = rebuilt))

expected <- ifelse(raw$System %in% names(FW_REGIME_BY_TYPE),
                   unname(FW_REGIME_BY_TYPE[raw$System]), raw[["System Simple"]])
corrected <- which(!same(raw[["System Simple"]], expected))
bad <- which(!same(a$water_regime, expected))
rule("System Simple", "water_regime (FW_REGIME_BY_TYPE corrections applied)", n, bad,
     list(src = raw[["System Simple"]], dst = a$water_regime))
record("System Simple (corrections)", "water_regime", n, corrected,
       list(src = raw[["System Simple"]], dst = a$water_regime), fail = FALSE)

# ---- 6. Every source column has a rule; nothing in the data is orphaned -------
unruled <- setdiff(names(raw), checked_cols)
orphan_sp <- setdiff(spp$species_id, unlist(lapply(c(a$invasive_species, a$beneficiary_species),
                                                    split_multi, sep = FW_MULTI_SEP)))
orphan_co <- setdiff(con$contact_id, c(a$primary_contact_id, a$secondary_contact_id))
extra_att <- setdiff(att$attempt_id, map$attempt_id)

# ---- Report ------------------------------------------------------------------
tab <- do.call(rbind, results)
cat(sprintf("%-38s %-46s %6s %6s\n", "source column", "destination", "rows", "diff"))
for (i in seq_len(nrow(tab))) cat(sprintf("%-38s %-46s %6d %6d\n", tab$source_column[i],
                                          substr(tab$destination[i], 1, 46), tab$rows_checked[i], tab$rows_differing[i]))
show <- function(lst, label) {
  for (nm in names(lst)) {
    d <- lst[[nm]]
    cat(sprintf("\n%s: %s (%d rows)\n", label, nm, nrow(d)))
    for (i in seq_len(min(nrow(d), 12))) cat(sprintf("  row %4d %s\n    source: %s\n    data:   %s\n",
                                                     d$source_row[i], d$attempt_id[i], d$source[i], d$data[i]))
    if (nrow(d) > 12) cat("  ...\n")
  }
}
show(reported, "REPORTED (deliberate)")
show(details,  "DIFFERENCE")

fail <- length(details) > 0
if (length(unruled)) { cat("\nSource columns with NO rule:", paste(unruled, collapse = ", "), "\n"); fail <- TRUE }
if (length(orphan_sp)) { cat("\nspecies.csv rows no attempt references:", length(orphan_sp), "\n"); fail <- TRUE }
if (length(orphan_co)) { cat("\ncontacts.csv rows no attempt references:", length(orphan_co), "\n"); fail <- TRUE }
if (length(extra_att)) cat("\nattempts.csv rows not from this export (submissions since):", length(extra_att), "\n")
cat(sprintf("\n%d source columns, %d with a rule, %d differing.\n", ncol(raw), length(unique(checked_cols)),
            sum(tab$rows_differing[tab$source_column %in% names(details)])))
if (fail) stop("Reconciliation failed.", call. = FALSE)
cat("Reconciliation clean: every source column and value is in the data.\n")
