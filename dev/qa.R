# qa.R
# THE REVIEW LOOP. Submissions arrive in fwise-data/inbox/ as one CSV each, in
# the exact shape of attempts.csv. This script turns them into something a
# person can review, and then folds what they approved into the data.
#
#     Rscript dev/qa.R stage
#         Gathers every unprocessed submission into qa/review_<date>.csv, with
#         readable name columns beside every id column, and lists the species
#         and contacts the database does not hold yet in qa/species_new_<date>.csv
#         and qa/contacts_new_<date>.csv. Prints what looks off-list or
#         duplicated. Changes nothing else.
#
#     Rscript dev/qa.R fold qa/review_<date>.csv [--keep-pending]
#         Applies the reviewer's decisions: adds the species and contacts marked
#         `add`, rewrites every `new:` reference to an id, appends the rows
#         marked `approved` to attempts.csv, moves their inbox files to
#         inbox/merged/, rewrites metadata.json. Rows still `pending` are left
#         in the inbox unless --keep-pending, which folds them in as pending
#         (invisible publicly). Rows marked `rejected` go to qa/rejected.csv.
#
# Then commit and push fwise-data, and restart the app.
#
# WHAT THE REVIEWER DOES between the two commands, in a spreadsheet:
#   - in review_<date>.csv: correct any value, set `status` to approved or
#     rejected. The *_names columns are for reading and are dropped on fold.
#   - in species_new_<date>.csv and contacts_new_<date>.csv: set `action` on
#     every row to `add` (a genuinely new row; fix names, taxa, family first)
#     or `use:<existing id>` (it is one we already hold). The fold refuses to
#     run while any action is blank.
#
# IDEMPOTENT. A submission whose attempt_id is already in attempts.csv is
# skipped by both commands, so running either twice does nothing new.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(purrr); library(tibble)
})
source(file.path("R", "brand.R")); source(file.path("R", "config.R"))
source(file.path("R", "github.R")); source(file.path("R", "data_load.R"))
source(file.path("R", "submit.R"))

args <- commandArgs(trailingOnly = TRUE)
cmd  <- args[1]
if (is.na(cmd) || !cmd %in% c("stage", "fold")) {
  stop("Usage:\n  Rscript dev/qa.R stage\n  Rscript dev/qa.R fold qa/review_<date>.csv [--keep-pending]", call. = FALSE)
}

QA_DIR    <- file.path(FW_DATA_DIR, "qa")
INBOX     <- fw_inbox_dir()
MERGED    <- file.path(INBOX, "merged")
today     <- format(Sys.Date())

read_text <- function(path) read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
blank_na  <- function(df) mutate(df, across(everything(), ~ { x <- str_trim(.x); if_else(is.na(x) | x == "", NA_character_, x) }))

attempts <- blank_na(read_text(FW_ATTEMPTS_CSV))
species  <- blank_na(read_text(FW_SPECIES_CSV))
contacts <- blank_na(read_text(FW_CONTACTS_CSV))
fw_validate_files(attempts, species, contacts)

sp_label <- fw_species_label(species)
label_of <- setNames(sp_label$label, sp_label$species_id)
contact_label <- function(id) {
  m <- match(id, contacts$contact_id)
  ifelse(is.na(m), NA_character_,
         paste0(contacts$contact_name[m],
                ifelse(is.na(contacts$organisation[m]), "", paste0(", ", contacts$organisation[m]))))
}

# Readable form of an id cell: names for ids, [NEW] for unresolved references.
describe_species <- function(cell) {
  vapply(cell, function(x) {
    items <- fw_split_multi(x)
    if (!length(items)) return(NA_character_)
    paste(vapply(items, function(it) {
      if (fw_is_new_ref(it)) paste0("[NEW] ", fw_parse_new_ref(it, 2)[1])
      else if (it %in% names(label_of)) unname(label_of[it])
      else paste0("[UNKNOWN ID] ", it)
    }, ""), collapse = FW_MULTI_SEP)
  }, "", USE.NAMES = FALSE)
}
describe_contact <- function(cell) {
  vapply(cell, function(x) {
    if (is.na(x)) return(NA_character_)
    if (fw_is_new_ref(x)) { p <- fw_parse_new_ref(x, 4); return(paste0("[NEW] ", p[1], if (nzchar(p[2])) paste0(", ", p[2]) else "")) }
    lbl <- contact_label(x); if (is.na(lbl)) paste0("[UNKNOWN ID] ", x) else lbl
  }, "", USE.NAMES = FALSE)
}

iso <- fw_read_lookup("lookup_iso3166.csv")

# ==============================================================================
if (cmd == "stage") {
# ==============================================================================

files <- if (dir.exists(INBOX)) list.files(INBOX, pattern = "\\.csv$", full.names = TRUE) else character(0)
if (!length(files)) { message("Inbox is empty: ", INBOX); quit(save = "no") }

rows <- lapply(files, function(f) {
  d <- read_text(f)
  bad <- !identical(names(d), FW_ATTEMPT_COLUMNS)
  if (bad) stop(basename(f), " does not have the columns of attempts.csv.\n  missing: ",
                paste(setdiff(FW_ATTEMPT_COLUMNS, names(d)), collapse = ", "),
                "\n  unexpected: ", paste(setdiff(names(d), FW_ATTEMPT_COLUMNS), collapse = ", "), call. = FALSE)
  d
})
inbox <- blank_na(bind_rows(rows))
already <- inbox$attempt_id %in% attempts$attempt_id
if (any(already)) message(sum(already), " submission(s) already in attempts.csv, skipped: ",
                          paste(inbox$attempt_id[already], collapse = ", "))
inbox <- inbox[!already, ]
if (!nrow(inbox)) { message("Nothing new to review."); quit(save = "no") }

# ---- The review file ---------------------------------------------------------
review <- inbox
add_after <- function(df, after, name, value) {
  i <- match(after, names(df)); df[[name]] <- value
  df[, append(setdiff(names(df), name), name, after = i)]
}
review <- add_after(review, "invasive_species",     "invasive_species_names",     describe_species(review$invasive_species))
review <- add_after(review, "beneficiary_species",  "beneficiary_species_names",  describe_species(review$beneficiary_species))
review <- add_after(review, "primary_contact_id",   "primary_contact_names",      describe_contact(review$primary_contact_id))
review <- add_after(review, "secondary_contact_id", "secondary_contact_names",    describe_contact(review$secondary_contact_id))

# ---- Species the database does not hold ---------------------------------------
new_sp <- list()
for (i in seq_len(nrow(inbox))) for (cell in c(inbox$invasive_species[i], inbox$beneficiary_species[i])) {
  for (it in fw_split_multi(cell)) if (fw_is_new_ref(it)) {
    p <- fw_parse_new_ref(it, 2)
    new_sp[[length(new_sp) + 1]] <- tibble(submitted_as = p[1], taxa = p[2], from_attempt = inbox$attempt_id[i])
  }
}
species_new <- NULL
if (length(new_sp)) {
  parse_name <- function(x) {
    sci <- str_match(x, "\\(([^()]*)\\)\\s*$")[, 2]
    com <- str_trim(str_remove(x, "\\s*\\(([^()]*)\\)\\s*$"))
    list(common = if_else(is.na(com) | com == "", NA_character_, com), sci = str_trim(sci))
  }
  species_new <- bind_rows(new_sp) |>
    group_by(submitted_as) |>
    summarise(taxa = first(na.omit(taxa)) %||% NA_character_,
              from_attempt = paste(unique(from_attempt), collapse = FW_MULTI_SEP), .groups = "drop")
  nm <- parse_name(species_new$submitted_as)
  species_new$common_name <- nm$common; species_new$scientific_name <- nm$sci
  species_new$family <- "Unknown"
  # Exact label already held (added since the submission was made): pre-fill.
  exact <- unname(setNames(sp_label$species_id, sp_label$label)[species_new$submitted_as])
  # Same scientific name under another common name: warn, do not decide.
  same_binomial <- vapply(seq_len(nrow(species_new)), function(k) {
    sci <- species_new$scientific_name[k]
    if (is.na(sci)) return(NA_character_)
    hit <- sp_label$species_id[!is.na(sp_label$scientific_name) & sp_label$scientific_name == sci]
    if (!length(hit)) NA_character_ else paste(paste(hit, label_of[hit]), collapse = FW_MULTI_SEP)
  }, "")
  species_new <- species_new |>
    mutate(same_binomial_as = same_binomial,
           action = if_else(!is.na(exact), paste0("use:", exact), NA_character_)) |>
    select(submitted_as, common_name, scientific_name, taxa, family, same_binomial_as, action, from_attempt)
}

# ---- Contacts the database does not hold --------------------------------------
new_co <- list()
for (i in seq_len(nrow(inbox))) for (cell in c(inbox$primary_contact_id[i], inbox$secondary_contact_id[i])) {
  if (!is.na(cell) && fw_is_new_ref(cell)) {
    p <- fw_parse_new_ref(cell, 4)
    new_co[[length(new_co) + 1]] <- tibble(submitted_as = sub("^new:", "", cell), contact_name = p[1],
                                           organisation = if_else(nzchar(p[2]), p[2], NA_character_),
                                           contact_email = if_else(nzchar(p[3]), p[3], NA_character_),
                                           email_public = !identical(p[4], "private"),
                                           from_attempt = inbox$attempt_id[i])
  }
}
contacts_new <- NULL
if (length(new_co)) {
  contacts_new <- bind_rows(new_co) |>
    group_by(submitted_as) |>
    summarise(across(c(contact_name, organisation, contact_email), first),
              email_public = all(email_public),
              from_attempt = paste(unique(from_attempt), collapse = FW_MULTI_SEP), .groups = "drop")
  key <- function(n, o) paste(n, coalesce(o, ""), sep = "|")
  exact <- unname(setNames(contacts$contact_id, key(contacts$contact_name, contacts$organisation))[
    key(contacts_new$contact_name, contacts_new$organisation)])
  name_only <- vapply(contacts_new$contact_name, function(n) {
    hit <- contacts$contact_id[contacts$contact_name == n]
    if (!length(hit)) NA_character_ else paste(paste(hit, contact_label(hit)), collapse = FW_MULTI_SEP)
  }, "", USE.NAMES = FALSE)
  contacts_new <- contacts_new |>
    mutate(matches_existing = if_else(!is.na(exact), paste(exact, contact_label(exact)),
                                      if_else(!is.na(name_only), paste("NAME ONLY:", name_only), NA_character_)),
           action = if_else(!is.na(exact), paste0("use:", exact), NA_character_)) |>
    select(submitted_as, contact_name, organisation, contact_email, email_public,
           matches_existing, action, from_attempt)
}

# ---- Things worth a look -----------------------------------------------------
vocab <- function(col) sort(unique(na.omit(attempts[[col]])))
flag <- function(label, ids) if (length(ids)) message("  ! ", label, ": ", paste(ids, collapse = ", "))
message("\nChecks:")
if (!is.null(iso)) flag("country not in the ISO list", inbox$attempt_id[!inbox$country %in% iso$country])
methods_seen <- unlist(lapply(inbox$methods, fw_split_multi))
flag("method not in FW_METHODS (will load as class 'other')",
     inbox$attempt_id[vapply(inbox$methods, function(m) any(!fw_split_multi(m) %in% FW_METHODS$method_name), TRUE)])
for (col in c("driver", "waterbody_type", "neutralising_agent", "area_unit", "water_regime", "outcome")) {
  flag(paste0(col, " not in the current vocabulary"),
       inbox$attempt_id[!is.na(inbox[[col]]) & !inbox[[col]] %in% vocab(col)])
}
dup_key <- paste(tolower(attempts$site_name), attempts$start_year)
flag("possible duplicate of an existing attempt (same site name and start year)",
     inbox$attempt_id[paste(tolower(inbox$site_name), inbox$start_year) %in% dup_key])
if (!is.null(species_new) && any(!is.na(species_new$same_binomial_as)))
  message("  ! ", sum(!is.na(species_new$same_binomial_as)),
          " proposed species share a scientific name with one already held - check before adding")

# ---- Write ---------------------------------------------------------------------
dir.create(QA_DIR, showWarnings = FALSE)
review_path <- file.path(QA_DIR, paste0("review_", today, ".csv"))
write_csv(review, review_path, na = "")
message("\n", nrow(review), " submission(s) staged in ", review_path)
if (!is.null(species_new)) {
  p <- file.path(QA_DIR, paste0("species_new_", today, ".csv")); write_csv(species_new, p, na = "")
  message(nrow(species_new), " new species proposed in ", p)
}
if (!is.null(contacts_new)) {
  p <- file.path(QA_DIR, paste0("contacts_new_", today, ".csv")); write_csv(contacts_new, p, na = "")
  message(nrow(contacts_new), " contact(s) to resolve in ", p,
          " (", sum(!is.na(contacts_new$action)), " pre-matched)")
}
message("\nReview, set status and every action, then:\n  Rscript dev/qa.R fold ", review_path)

# ==============================================================================
} else {
# ==============================================================================

review_path <- args[2]
keep_pending <- "--keep-pending" %in% args
if (is.na(review_path) || !file.exists(review_path)) stop("Give the review file to fold.", call. = FALSE)
stamp <- str_match(basename(review_path), "review_(.*)\\.csv$")[, 2]
sp_path <- file.path(dirname(review_path), paste0("species_new_", stamp, ".csv"))
co_path <- file.path(dirname(review_path), paste0("contacts_new_", stamp, ".csv"))

review <- blank_na(read_text(review_path))
review <- review[, setdiff(names(review), grep("_names$", names(review), value = TRUE))]
if (!identical(names(review), FW_ATTEMPT_COLUMNS))
  stop("The review file's columns are not attempts.csv's. Was a column deleted or renamed?", call. = FALSE)

bad_status <- !review$status %in% FW_STATUS
if (any(bad_status)) stop("status must be one of ", paste(FW_STATUS, collapse = ", "), "; found: ",
                          paste(unique(review$status[bad_status]), collapse = ", "), call. = FALSE)
already <- review$attempt_id %in% attempts$attempt_id
if (any(already)) message(sum(already), " already folded, skipped: ", paste(review$attempt_id[already], collapse = ", "))
review <- review[!already, ]

# ---- Decisions on new species and contacts ------------------------------------
read_decisions <- function(path, what, known_ids) {
  if (!file.exists(path)) return(NULL)
  d <- blank_na(read_text(path))
  if (any(is.na(d$action))) stop("Every row of ", basename(path), " needs an action (add or use:<id>). ",
                                 sum(is.na(d$action)), " blank.", call. = FALSE)
  use <- str_match(d$action, "^use:(.+)$")[, 2]
  ok <- d$action == "add" | (!is.na(use) & use %in% known_ids)
  if (!all(ok)) stop("Unrecognised action or unknown id in ", basename(path), ": ",
                     paste(d$action[!ok], collapse = ", "), call. = FALSE)
  d$use_id <- use
  d
}
sp_dec <- read_decisions(sp_path, "species", species$species_id)
co_dec <- read_decisions(co_path, "contacts", contacts$contact_id)

# Mint for every `add`, then every submitted_as maps to an id.
sp_map <- character(0); co_map <- character(0)
if (!is.null(sp_dec)) {
  adds <- sp_dec$action == "add"
  ids <- fw_mint_ids("SP", sum(adds), exclude = species$species_id)
  if (any(adds)) species <- bind_rows(species, tibble(
    species_id = ids, common_name = sp_dec$common_name[adds], scientific_name = sp_dec$scientific_name[adds],
    taxa = sp_dec$taxa[adds], family = sp_dec$family[adds], iucn_status = NA_character_,
    image_url = NA_character_, image_credit = NA_character_, image_licence = NA_character_,
    image_licence_url = NA_character_, image_page_url = NA_character_))
  sp_dec$id <- sp_dec$use_id; sp_dec$id[adds] <- ids
  sp_map <- setNames(sp_dec$id, sp_dec$submitted_as)
}
if (!is.null(co_dec)) {
  adds <- co_dec$action == "add"
  ids <- fw_mint_ids("CO", sum(adds), exclude = contacts$contact_id)
  if (any(adds)) contacts <- bind_rows(contacts, tibble(
    contact_id = ids, contact_name = co_dec$contact_name[adds], contact_email = co_dec$contact_email[adds],
    organisation = co_dec$organisation[adds],
    email_public = toupper(as.character(toupper(co_dec$email_public[adds]) %in% c("TRUE", "YES")))))
  co_dec$id <- co_dec$use_id; co_dec$id[adds] <- ids
  co_map <- setNames(co_dec$id, co_dec$submitted_as)
}

resolve_species_cell <- function(x) {
  items <- fw_split_multi(x); if (!length(items)) return(NA_character_)
  out <- vapply(items, function(it) {
    if (!fw_is_new_ref(it)) return(it)
    id <- unname(sp_map[fw_parse_new_ref(it, 2)[1]])
    if (is.na(id)) stop("No decision for species '", fw_parse_new_ref(it, 2)[1], "' - re-run stage.", call. = FALSE)
    id
  }, "")
  paste(out, collapse = FW_MULTI_SEP)
}
resolve_contact_cell <- function(x) {
  if (is.na(x) || !fw_is_new_ref(x)) return(x)
  id <- unname(co_map[sub("^new:", "", x)])
  if (is.na(id)) stop("No decision for contact '", fw_parse_new_ref(x, 4)[1], "' - re-run stage.", call. = FALSE)
  id
}
review$invasive_species     <- vapply(review$invasive_species,     resolve_species_cell, "", USE.NAMES = FALSE)
review$beneficiary_species  <- vapply(review$beneficiary_species,  resolve_species_cell, "", USE.NAMES = FALSE)
review$primary_contact_id   <- vapply(review$primary_contact_id,   resolve_contact_cell, "", USE.NAMES = FALSE)
review$secondary_contact_id <- vapply(review$secondary_contact_id, resolve_contact_cell, "", USE.NAMES = FALSE)

# ---- Which rows go in ---------------------------------------------------------
take <- review$status == "approved" | (keep_pending & review$status == "pending")
rejected <- review[review$status == "rejected", ]
folded <- review[take, ]
left <- review[!take & review$status == "pending", ]

if (nrow(folded)) {
  if (!is.null(iso)) {
    m <- match(folded$country, iso$country)
    folded$iso3 <- coalesce(folded$iso3, iso$iso3[m])
    folded$continent <- coalesce(folded$continent, iso$continent[m])
    if (any(is.na(folded$iso3))) message("  ! no ISO match for country: ",
                                         paste(unique(folded$country[is.na(folded$iso3)]), collapse = ", "))
  }
  folded$last_updated <- today
  # Nothing goes into attempts.csv that would stop the app at its next load.
  fw_unpack(bind_rows(attempts, folded), species, contacts)
  attempts <- bind_rows(attempts, folded)
}

# ---- Write everything ---------------------------------------------------------
write_csv(attempts[, FW_ATTEMPT_COLUMNS], FW_ATTEMPTS_CSV, na = "")
write_csv(species[, FW_SPECIES_COLUMNS],  FW_SPECIES_CSV,  na = "")
write_csv(contacts[, FW_CONTACT_COLUMNS], FW_CONTACTS_CSV, na = "")
if (nrow(rejected)) {
  p <- file.path(QA_DIR, "rejected.csv")
  write_csv(rejected, p, na = "", append = file.exists(p))
}
done <- c(folded$attempt_id, rejected$attempt_id)
moved <- 0L
for (id in done) {
  src <- file.path(INBOX, paste0(id, ".csv"))
  if (!file.exists(src)) next
  dir.create(MERGED, showWarnings = FALSE, recursive = TRUE)
  if (file.rename(src, file.path(MERGED, basename(src)))) moved <- moved + 1L
}
meta <- list(release = today, generated_by = "dev/qa.R fold",
             row_counts = list(attempts = nrow(attempts), species = nrow(species), contacts = nrow(contacts)))
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), file.path(FW_DATA_DIR, "metadata.json"))

message("\nFolded ", nrow(folded), " attempt(s) (", sum(folded$status == "approved"), " approved, ",
        sum(folded$status == "pending"), " pending); ", nrow(rejected), " rejected; ",
        nrow(left), " left pending in the inbox.")
if (!is.null(sp_dec)) message("species.csv: +", sum(sp_dec$action == "add"),
                              if (any(sp_dec$action == "add")) ". Run dev/fetch_species_images.R to fetch the new photo(s)." else "")
if (!is.null(co_dec)) message("contacts.csv: +", sum(co_dec$action == "add"))
message("Moved ", moved, " inbox file(s) to ", MERGED)
message("attempts now: ", nrow(attempts), " (", sum(attempts$status == "approved"), " approved)")
message("\nCommit and push fwise-data, then restart the app for this to reach it.")
}
