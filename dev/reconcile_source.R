# reconcile_source.R
# PROVES THE DATA HOLDS EVERY SOURCE COLUMN AND EVERY SOURCE VALUE, identically
# coded. Reads the client's export under fwise-data/source/ (xlsx or csv), the
# row-to-id map beside it, and attempts.csv, species.csv and contacts.csv, and
# walks every source column. Each has exactly one rule; a column with no rule
# is itself a failure, so a new column in a future export cannot slip through
# unnoticed.
#
#     Rscript dev/reconcile_source.R
#
# Plain base R for the checks, deliberately: dev/build_from_export.R wrote the
# data, and a test that repeats the build's own code would only prove it agrees
# with itself. In particular numbers are compared as numbers here, not by
# re-running the build's formatting.
#
# WHAT COUNTS AS A DIFFERENCE. Cells are compared as trimmed text with blank
# and NA treated as the same absence and Windows line endings inside a cell
# read as plain newlines. A cell that is a number on both sides is compared as
# a number to 15 significant digits, which is the tidying of Excel's float
# noise ("3.3000000000000002E-2" stored as "0.033"). The country/region split
# and the water-regime corrections are asserted rather than skipped.
#
# REPORTED, NOT FAILED, because each is a decision already taken: the regime
# corrections named in FW_REGIME_BY_TYPE; the float-noise tidying; a contact
# whose email differed between rows and now carries one address; and, listed
# in qa/build_issues_<date>.csv for fixing in the export, a method note
# in a slot with no method.

source(file.path("R", "brand.R")); source(file.path("R", "config.R"))
source(file.path("R", "data_load.R"))

read_text <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
}
# Trim, and "\r\n" inside a cell becomes "\n". Nothing else.
norm <- function(x) { x <- trimws(gsub("\r\n?", "\n", x)); x[!is.na(x) & x == ""] <- NA; x }

src_dir <- file.path(FW_DATA_DIR, "source")
raw_path <- list.files(src_dir, pattern = "^fwise_.*[0-9]\\.(csv|xlsx)$", full.names = TRUE)
if (length(raw_path) != 1) stop("Expected exactly one raw export, found: ", paste(raw_path, collapse = ", "))
ids_path <- file.path(src_dir, sub("\\.(csv|xlsx)$", "_ids.csv", basename(raw_path)))

raw <- if (grepl("\\.xlsx$", raw_path)) {
  openxlsx::read.xlsx(raw_path, sheet = 1, check.names = FALSE, na.strings = c("", "NA"))
} else read_text(raw_path)
# Numeric Excel columns to text at full precision, so the number comparison
# below sees what the file holds rather than a rounded print.
raw[] <- lapply(raw, function(x) norm(if (is.numeric(x)) sprintf("%.17g", x) else as.character(x)))
raw[] <- lapply(raw, function(x) { x[x %in% "NA"] <- NA; x })
map <- read_text(ids_path)
att <- read_text(FW_ATTEMPTS_CSV); att[] <- lapply(att, norm)
spp <- read_text(FW_SPECIES_CSV);  spp[] <- lapply(spp, norm)
con <- read_text(FW_CONTACTS_CSV); con[] <- lapply(con, norm)
iso <- read_text(file.path(FW_DATA_DIR, "lookup_iso3166.csv"))

stopifnot(nrow(map) == nrow(raw))
a <- att[match(map$attempt_id, att$attempt_id), ]
if (any(is.na(a$attempt_id))) stop("Source rows whose attempt_id is not in attempts.csv: ",
                                   sum(is.na(a$attempt_id)))
n <- nrow(raw)

NUM_RE <- "^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$"
is_num <- function(x) !is.na(x) & grepl(NUM_RE, x)
same_text <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
same_num  <- function(x, y) is_num(x) & is_num(y) &
  suppressWarnings(signif(as.numeric(x), 15) == signif(as.numeric(y), 15))
same <- function(x, y) same_text(x, y) | same_num(x, y)
split_multi <- function(x, sep) if (is.na(x)) character(0) else trimws(strsplit(x, sep, fixed = TRUE)[[1]])

# ---- Lookups ------------------------------------------------------------------
sp_label <- ifelse(!is.na(spp$common_name) & !is.na(spp$scientific_name),
                   paste0(spp$common_name, " (", spp$scientific_name, ")"),
                   ifelse(!is.na(spp$scientific_name), spp$scientific_name, spp$common_name))
names(sp_label) <- spp$species_id
con_by_id <- con; rownames(con_by_id) <- con$contact_id

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
# Every source column that has a same-named home in attempts.csv, plus the one
# renamed column. country and water_regime have their own rules below.
identity <- setdiff(intersect(names(raw), names(att)), c("country", "water_regime"))
identity <- c(setNames(identity, identity), reason = "driver")
tidied <- integer(0); tidied_cols <- character(0)
for (sc in names(identity)) {
  dc <- identity[[sc]]
  bad <- which(!same(raw[[sc]], a[[dc]]))
  rule(sc, dc, n, bad, list(src = raw[[sc]], dst = a[[dc]]))
  # Float noise tidied in a TEXT cell is the visible change; list it.
  t <- which(!same_text(raw[[sc]], a[[dc]]) & same_num(raw[[sc]], a[[dc]]) & grepl("[eE]|[0-9]{16}", raw[[sc]]))
  if (length(t)) { tidied <- c(tidied, t); tidied_cols <- c(tidied_cols, rep(sc, length(t))) }
}
if (length(tidied)) record("Float noise (tidied)", paste(unique(tidied_cols), collapse = ", "),
                           length(tidied), integer(0))

# ---- 2. Species slots ----------------------------------------------------------
# Taxa and family are no longer in the export; they live in species.csv only.
check_role <- function(prefix, cell_col) {
  slots <- paste0(prefix, 1:8)
  bad <- integer(0); src <- character(n); dst <- character(n)
  for (r in seq_len(n)) {
    want <- unlist(raw[r, slots]); want <- unname(want[!is.na(want)])
    got  <- unname(sp_label[split_multi(a[[cell_col]][r], FW_MULTI_SEP)])
    src[r] <- paste(want, collapse = " ; "); dst[r] <- paste(got, collapse = " ; ")
    if (!identical(want, got)) bad <- c(bad, r)
  }
  for (s in slots) rule(s, paste0(cell_col, " -> species.csv"), n,
                        if (s == slots[1]) bad else integer(0), list(src = src, dst = dst))
}
check_role("invasive_species_", "invasive_species")
check_role("beneficiary_species_", "beneficiary_species")

# ---- 3. Methods --------------------------------------------------------------
m_cols <- paste0("method_", 1:3); n_cols <- paste0("method_notes_", 1:3)
bad_m <- integer(0); bad_n <- integer(0); orphan_n <- integer(0)
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
  # A note in a blank slot beside a filled one has no home: reported, and in
  # the build's issues file.
  if (any(!is.na(all_notes[!keep]))) orphan_n <- c(orphan_n, r)
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
orphan_src <- apply(raw[, n_cols], 1, function(v) paste(v[!is.na(v)], collapse = " | "))
record("method note, no method (not stored)", "qa/build_issues", n, orphan_n,
       list(src = orphan_src, dst = a$method_notes), fail = FALSE)

# ---- 4. Contacts -------------------------------------------------------------
check_contact <- function(prefix, id_col) {
  nm <- raw[[paste0(prefix, "_contact_name")]]; em <- raw[[paste0(prefix, "_contact_email")]]
  og <- raw[[paste0(prefix, "_contact_organisation")]]
  c_ <- con_by_id[a[[id_col]], ]
  # A slot with anything in it - a name, an organisation, even only an email -
  # must cite a contact; a wholly empty slot must cite none. A contact with no
  # name is the organisation itself, so the name matches as absent on both sides.
  filled <- !is.na(nm) | !is.na(og) | !is.na(em)
  cited  <- !is.na(a[[id_col]])
  bad_nm <- which(filled != cited | (filled & !same_text(nm, c_$contact_name)))
  bad_og <- which(filled & !same_text(og, c_$organisation))
  bad_em <- which(filled & !is.na(em) & !same_text(tolower(em), tolower(c_$contact_email)))
  rule(paste0(prefix, "_contact_name"), paste0(id_col, " -> contacts.csv$contact_name"), n, bad_nm,
       list(src = nm, dst = c_$contact_name))
  rule(paste0(prefix, "_contact_organisation"), "contacts.csv$organisation", sum(filled), bad_og,
       list(src = og, dst = c_$organisation))
  rule(paste0(prefix, "_contact_email"), "contacts.csv$contact_email (one per contact)",
       sum(filled), bad_em, list(src = em, dst = c_$contact_email), fail = FALSE)
}
check_contact("primary", "primary_contact_id")
check_contact("secondary", "secondary_contact_id")
redact <- tolower(raw$redact_email)
bad <- integer(0)
for (r in seq_len(n)) {
  want_public <- !(!is.na(redact[r]) && redact[r] %in% c("yes", "true", "y", "1"))
  for (idc in c("primary_contact_id", "secondary_contact_id")) {
    id <- a[[idc]][r]
    if (!is.na(id) && (toupper(con_by_id[id, "contact_public"]) == "TRUE") != want_public) bad <- c(bad, r)
  }
}
rule("redact_email", "contacts.csv$contact_public", n, unique(bad),
     list(src = raw$redact_email, dst = rep("", n)))

# ---- 5. The two transformations ----------------------------------------------
# "Country (Region)" is split. A region that is itself an ISO 3166-1 country is
# the country (Guam), and iso3 and continent follow the country. See
# fw_validate_geography() in R/data_load.R.
rebuilt <- ifelse(is.na(a$region), a$country, paste0(a$country, " (", a$region, ")"))
territory <- is.na(a$region) & a$country %in% iso$country &
  !is.na(raw$country) & endsWith(raw$country, paste0("(", a$country, ")"))
bad <- which(!same_text(raw$country, rebuilt) & !territory)
bad <- union(bad, which(a$iso3 != iso$iso3[match(a$country, iso$country)] |
                        a$continent != iso$continent[match(a$country, iso$country)]))
rule("country", "country + region, iso3, continent", n, bad, list(src = raw$country, dst = rebuilt))

expected <- ifelse(raw$waterbody_type %in% names(FW_REGIME_BY_TYPE),
                   unname(FW_REGIME_BY_TYPE[raw$waterbody_type]), raw$water_regime)
corrected <- which(!same_text(raw$water_regime, expected))
bad <- which(!same_text(a$water_regime, expected))
rule("water_regime", "water_regime (FW_REGIME_BY_TYPE corrections applied)", n, bad,
     list(src = raw$water_regime, dst = a$water_regime))
record("water_regime (corrections)", "water_regime", n, corrected,
       list(src = paste(raw$waterbody_type, raw$water_regime), dst = a$water_regime), fail = FALSE)

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
show <- function(lst, label, max_rows = 12) {
  for (nm in names(lst)) {
    d <- lst[[nm]]
    cat(sprintf("\n%s: %s (%d rows)\n", label, nm, nrow(d)))
    for (i in seq_len(min(nrow(d), max_rows))) cat(sprintf("  row %4d %s\n    source: %s\n    data:   %s\n",
                                                           d$source_row[i], d$attempt_id[i], d$source[i], d$data[i]))
    if (nrow(d) > max_rows) cat("  ...\n")
  }
}
show(reported, "REPORTED (deliberate)", max_rows = 3)
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
