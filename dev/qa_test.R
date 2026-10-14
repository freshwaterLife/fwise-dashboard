# qa_test.R
# THE REVIEW AND IMPORT, END TO END, ON A COPY. Seven submissions go into a
# local inbox; the checks must find exactly the problems planted in them; the
# reviewer's fixes are made; the import is resolved, enriched (with the three
# network lookups stubbed) and written to a temporary copy of fwise-data. Every
# value that lands is recomputed here from the inputs and the lookups, not read
# back from the code that wrote it.
#
#     Rscript dev/qa_test.R
#
# No network, no sheet, and the real fwise-data is only ever read. Exits
# non-zero on any failure.

suppressPackageStartupMessages(library(shiny))
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)
source("dev/species_enrich.R")

failures <- 0L
ok <- function(lbl, got, want = TRUE) {
  pass <- identical(got, want)
  if (!pass) failures <<- failures + 1L
  cat(sprintf("  %-62s %s\n", lbl,
              if (pass) "PASS" else paste0("*** FAIL *** got ", paste(format(got), collapse = "|"),
                                          " want ", paste(format(want), collapse = "|"))))
}

# ---- A temporary data checkout and a local inbox ------------------------------
real <- FW_DATA_DIR
tmp <- file.path(tempdir(), "fw-qa-test"); unlink(tmp, recursive = TRUE)
dir.create(file.path(tmp, "data"), recursive = TRUE)
for (f in c("attempts.csv", "species.csv", "contacts.csv", "lookup_iso3166.csv", "metadata.json"))
  file.copy(file.path(real, f), file.path(tmp, "data", f))
FW_DATA_DIR <- file.path(tmp, "data")
FWISE_DATA_TOKEN <- NULL; FWISE_DATA_SOURCE <- NULL
FWISE_INBOX_SHEET_ID <- NULL
FW_DEV_DIR <- file.path(tmp, "dev")
ok("the inbox is local", fw_inbox_mode(), "local")

before_text <- readChar(file.path(FW_DATA_DIR, "attempts.csv"), file.size(file.path(FW_DATA_DIR, "attempts.csv")), useBytes = TRUE)
db0 <- fw_qa_read_db()
iso <- db0$iso
lab <- fw_species_label(db0$species)
carp <- lab$species_id[lab$label == "Common carp (Cyprinus carpio)"]
parker <- "CO-20260908-V9ZYU3"
ok("fixture: carp and Parker Bradley are held",
   length(carp) == 1 && db0$contacts$contact_name[db0$contacts$contact_id == parker] == "Parker Bradley")
ok("fixture: Parker Bradley starts public", db0$contacts$contact_public[db0$contacts$contact_id == parker], "TRUE")

row <- function(id, ...) {
  base <- list(qa_status = "done", imported = "no", attempt_id = id, status = "approved",
               submitted_at = "2026-10-01T10:00:00Z", consent_data_use = "yes",
               site_name = paste("Site", id), country = "Norway", latitude = "59.9", longitude = "10.5",
               waterbody_type = "Lake", water_regime = "Lentic", start_year = "2010", end_year = "2011",
               driver = "Fisheries", outcome = "Successful", methods = "Rotenone",
               invasive_species = "Common carp (Cyprinus carpio)", contact_public = "yes")
  fw_flatten_record(utils::modifyList(base, list(...)))
}
charr <- "Arctic charr (Salvelinus alpinus)"
fw_inbox_append(FW_INBOX_TABS$submissions, list(
  row("FW-T-HELD", contact_name = "Parker Bradley",
      contact_organisation = "Alaska Department of Fish and Game (ADF&G)", contact_public = "no"),
  row("FW-T-NEWSP", country = "Australia", region = "Western Australia", latitude = "-31.24",
      longitude = "121.77", invasive_species = charr, contact_name = "Chris Bird",
      contact_organisation = "DPIRD", contact_email = "chris@dpird.wa.gov.au", contact_public = "no"),
  row("FW-T-TYPO", country = "Australia", contact_name = "Chris Brid", contact_organisation = "DPIRD",
      contact_email = "chris@dpird.wa.gov.au"),
  row("FW-T-GUAM", region = "Guam"),
  row("FW-T-REJ", status = "rejected"),
  row("FW-T-DRIVER", driver = "Alien invasion", contact_name = "Parker Bradly",
      contact_organisation = "ADF&G", contact_email = "parker.bradley@alaska.gov"),
  row("FW-T-NOTDONE", qa_status = "not done", status = "pending",
      invasive_species = "Comon carp (Cyprinus carpio)")
), FW_INBOX_COLUMNS)
fw_inbox_append(FW_INBOX_TABS$new_species,
                fw_new_species_rows(data.frame(label = charr, taxa = NA_character_), "FW-T-NEWSP"),
                FW_NEW_SPECIES_COLUMNS)

# ---- Section 2: the checks find exactly what was planted ---------------------
cat("\n-- checks --\n")
inbox <- fw_inbox_read(FW_INBOX_TABS$submissions)
new_species <- fw_inbox_read(FW_INBOX_TABS$new_species)
checks <- fw_inbox_check(inbox, new_species, db0)
errs <- checks[checks$level == "error", ]
ok("errors are on exactly the three planted rows",
   sort(unique(errs$attempt_id)), sort(c("FW-T-NEWSP", "FW-T-GUAM", "FW-T-NOTDONE")))
ok("a new species with no taxa is an error",
   any(errs$attempt_id == "FW-T-NEWSP" & grepl("no taxa", errs$problem)))
ok("a region that is a country is an error",
   any(errs$attempt_id == "FW-T-GUAM" & errs$column == "region"))
ok("a typed species we do not have is an error",
   any(errs$attempt_id == "FW-T-NOTDONE" & errs$value %in% "Comon carp (Cyprinus carpio)"))
ok("an off-list driver is only a warning",
   any(checks$attempt_id == "FW-T-DRIVER" & checks$column == "driver" & checks$level == "warning"))
ok("negative coordinates raise nothing",
   any(checks$attempt_id == "FW-T-NEWSP" & checks$column %in% c("latitude", "longitude")), FALSE)
ok("a rejected row is not checked for content", any(checks$attempt_id %in% "FW-T-REJ"), FALSE)

cm <- fw_contact_matches(inbox, db0)
ok("a misspelt held contact is matched to them by email",
   any(cm$attempt_id == "FW-T-DRIVER" & cm$candidate == parker & cm$reason == "same email"))
ok("the same contributor misspelt in another row is matched",
   any(cm$attempt_id == "FW-T-TYPO" & grepl("FW-T-NEWSP", cm$candidate) & grepl("another open row", cm$reason)))
ok("an exact name and organisation is not offered as a 'match'", any(cm$attempt_id == "FW-T-HELD"), FALSE)
sm <- fw_species_matches(inbox, db0)
ok("a misspelt species is matched to the held one",
   any(sm$attempt_id == "FW-T-NOTDONE" & sm$candidate == carp & sm$reason == "same scientific name"))

sel <- fw_inbox_ready(inbox, db0)
ok("ready: QA done and not imported", sort(inbox$attempt_id[sel$ready]),
   sort(c("FW-T-HELD", "FW-T-NEWSP", "FW-T-TYPO", "FW-T-GUAM", "FW-T-REJ", "FW-T-DRIVER")))
ok("rejected rows are marked, not taken", inbox$attempt_id[sel$reject], "FW-T-REJ")

# A match never merges: with the typo left in, the two spellings are two people.
r_typo <- fw_inbox_resolve(inbox[inbox$attempt_id %in% c("FW-T-NEWSP", "FW-T-TYPO", "FW-T-DRIVER"), ],
                           data.frame(label = charr, common_name = "Arctic charr",
                                      scientific_name = "Salvelinus alpinus", taxa = "Fish", family = NA),
                           db0)
ok("unresolved spellings stay separate contacts", nrow(r_typo$contacts_add), 3L)

# ---- The reviewer's fixes, made in the "sheet" -------------------------------
ns_path <- fw_inbox_local_path(FW_INBOX_TABS$new_species)
ns <- utils::read.csv(ns_path, colClasses = "character", check.names = FALSE)
ns$taxa <- "Fish"
readr::write_csv(ns, ns_path, na = "")
fw_inbox_set(FW_INBOX_TABS$submissions, "FW-T-GUAM", "region", NA)
fw_inbox_set(FW_INBOX_TABS$submissions, "FW-T-TYPO", "contact_name", "Chris Bird")
fw_inbox_set(FW_INBOX_TABS$submissions, "FW-T-DRIVER", "contact_id", parker)

inbox <- fw_inbox_read(FW_INBOX_TABS$submissions)
new_species <- fw_inbox_read(FW_INBOX_TABS$new_species)
checks <- fw_inbox_check(inbox, new_species, db0)
sel <- fw_inbox_ready(inbox, db0)
ok("after the fixes no ready row has an error",
   any(checks$level == "error" & checks$attempt_id %in% inbox$attempt_id[sel$ready]), FALSE)
ok("the not-done row still has its error",
   any(checks$level == "error" & checks$attempt_id == "FW-T-NOTDONE"))
ok("a new fish with no family is a warning", any(checks$attempt_id == "FW-T-NEWSP" & grepl("no family", checks$problem)))

# ---- Sections 3 and 4 --------------------------------------------------------
cat("\n-- resolve and enrich --\n")
today <- Sys.Date()
resolved <- fw_inbox_resolve(inbox[sel$take, ], new_species, db0, today = today)
ok("five attempts to add", sort(resolved$to_add$attempt_id),
   sort(c("FW-T-HELD", "FW-T-NEWSP", "FW-T-TYPO", "FW-T-GUAM", "FW-T-DRIVER")))
ok("one new species", nrow(resolved$species_add), 1L)
ok("one new contact for the two identical spellings", nrow(resolved$contacts_add), 1L)
ok("the held contact who asked for private is to be hidden", resolved$contacts_private, parker)

stub_gbif  <- function(sci) list(family = "Salmonidae", matched = sci)
stub_iucn  <- function(sci) list(iucn_status = "LC", iucn_category = "Least Concern", iucn_url = "https://iucn/x",
                                 iucn_year = "2008", iucn_matched_name = sci)
stub_photo <- function(sci, common) list(url = "https://img/x.jpg", credit = "A Photographer",
                                         licence = "CC BY 4.0", licence_url = "https://cc/by4",
                                         page_url = "https://commons/x")
en <- fw_enrich_species(resolved$species_add, stub_gbif, stub_iucn, stub_photo)
ok("enrich: family from GBIF", en$species$family, "Salmonidae")
ok("enrich: IUCN status and category", c(en$species$iucn_status, en$species$iucn_category), c("LC", "Least Concern"))
ok("enrich: photo with credit and licence link",
   c(en$species$image_url, en$species$image_credit, en$species$image_licence_url),
   c("https://img/x.jpg", "A Photographer", "https://cc/by4"))
ok("enrich: one log line per source", sort(en$log$source), sort(c("family (GBIF)", "IUCN", "photo")))
typed <- resolved$species_add; typed$family <- "Typed family"; typed$iucn_status <- "VU"
en2 <- fw_enrich_species(typed, stub_gbif, stub_iucn, stub_photo)
ok("enrich: the reviewer's values are never replaced", c(en2$species$family, en2$species$iucn_status),
   c("Typed family", "VU"))
none <- fw_enrich_species(resolved$species_add, function(s) NULL, function(s) NULL, function(s, c) NULL)
ok("enrich: a miss stays blank and is listed",
   is.na(none$species$family) && all(grepl("NOT FOUND", none$log$result)))

# ---- Section 5 ---------------------------------------------------------------
cat("\n-- write --\n")
snapshot <- inbox[sel$take | sel$reject, FW_INBOX_COLUMNS, drop = FALSE]
w <- fw_qa_write_import(resolved, en$species, snapshot, today = today)
db1 <- fw_qa_read_db()
added <- db1$attempts[match(resolved$to_add$attempt_id, db1$attempts$attempt_id), ]
src <- inbox[match(added$attempt_id, inbox$attempt_id), ]

ok("attempts grew by five", nrow(db1$attempts), nrow(db0$attempts) + 5L)
ok("the rows it did not touch are byte for byte the same",
   startsWith(readChar(file.path(FW_DATA_DIR, "attempts.csv"), file.size(file.path(FW_DATA_DIR, "attempts.csv")),
                       useBytes = TRUE), before_text))
ok("iso3 is the lookup's for each country", added$iso3, iso$iso3[match(src$country, iso$country)])
ok("continent is the lookup's for each country", added$continent, iso$continent[match(src$country, iso$country)])
ok("last_updated is the import date", unique(added$last_updated), format(today))
ok("every row arrives approved", unique(added$status), "approved")
ok("a held species is its id", added$invasive_species[added$attempt_id == "FW-T-HELD"], carp)
new_sp <- db1$species[!db1$species$species_id %in% db0$species$species_id, ]
ok("the new species is in species.csv once, with its label",
   fw_species_label(new_sp)$label, charr)
ok("and the attempt points at it", added$invasive_species[added$attempt_id == "FW-T-NEWSP"], new_sp$species_id)
ok("and it carries what enrichment found", c(new_sp$taxa, new_sp$family, new_sp$iucn_status, new_sp$image_licence),
   c("Fish", "Salmonidae", "LC", "CC BY 4.0"))
ok("negative coordinates survive as typed",
   c(added$latitude[added$attempt_id == "FW-T-NEWSP"], added$longitude[added$attempt_id == "FW-T-NEWSP"]),
   c("-31.24", "121.77"))
chris <- added$primary_contact_id[added$attempt_id %in% c("FW-T-NEWSP", "FW-T-TYPO")]
ok("the two rows of one contributor share one new contact",
   length(unique(chris)) == 1 && !chris[1] %in% db0$contacts$contact_id)
ok("that contact is private, because one row asked",
   db1$contacts$contact_public[db1$contacts$contact_id == chris[1]], "FALSE")
ok("an exact name and organisation reuses the held contact",
   added$primary_contact_id[added$attempt_id == "FW-T-HELD"], parker)
ok("the reviewer's contact_id is used", added$primary_contact_id[added$attempt_id == "FW-T-DRIVER"], parker)
ok("private wins over the held contact's public", db1$contacts$contact_public[db1$contacts$contact_id == parker], "FALSE")
ok("no other contact changed",
   identical(db1$contacts[db1$contacts$contact_id %in% setdiff(db0$contacts$contact_id, parker), ],
             db0$contacts[db0$contacts$contact_id %in% setdiff(db0$contacts$contact_id, parker), ]))
ok("the rejected row is not in attempts.csv", "FW-T-REJ" %in% db1$attempts$attempt_id, FALSE)
ok("the not-done row is not in attempts.csv", "FW-T-NOTDONE" %in% db1$attempts$attempt_id, FALSE)

inbox <- fw_inbox_read(FW_INBOX_TABS$submissions)
imp <- stats::setNames(inbox$imported, inbox$attempt_id)
ok("the sheet marks the five imported", unname(imp[resolved$to_add$attempt_id]), rep("yes", 5))
ok("the sheet marks the rejected row", unname(imp["FW-T-REJ"]), "rejected")
ok("the not-done row is still waiting", unname(imp["FW-T-NOTDONE"]), "no")
meta <- jsonlite::fromJSON(file.path(FW_DATA_DIR, "metadata.json"))
ok("metadata: release is the import date", meta$release, format(today))
ok("metadata: counts are the files'", unlist(meta$row_counts),
   c(attempts = nrow(db1$attempts), species = nrow(db1$species), contacts = nrow(db1$contacts)))
loaded <- fw_load_data()
ok("the app loads the result, with the five published",
   nrow(loaded$attempt), sum(db0$attempts$status == "approved") + 5L)
ok("the in-review count is now the one waiting row", fw_review_count(loaded),
   as.integer(attr(loaded, "status_counts")[["pending"]] + 1L))

# ---- Running it again, and repairing a failed sheet write ---------------------
cat("\n-- idempotence --\n")
again <- fw_inbox_ready(inbox, db1)
ok("a second run takes nothing", sum(again$take | again$reject), 0L)
r2 <- fw_inbox_resolve(inbox[again$take, ], new_species, db1)
fw_qa_write_import(r2, r2$species_add, inbox[again$take | again$reject, FW_INBOX_COLUMNS, drop = FALSE])
ok("and writes nothing new", nrow(fw_qa_read_db()$attempts), nrow(db1$attempts))
w_again <- fw_qa_write_import(resolved, en$species, inbox[0, FW_INBOX_COLUMNS, drop = FALSE])
ok("replaying the same import adds nothing",
   c(length(w_again$attempts_added), length(w_again$species_added), length(w_again$contacts_added)), c(0L, 0L, 0L))

# The sheet write "failed" for one row: it still says no.
fw_inbox_set(FW_INBOX_TABS$submissions, "FW-T-HELD", "imported", "no")
inbox <- fw_inbox_read(FW_INBOX_TABS$submissions)
chk <- fw_inbox_check(inbox, new_species, fw_qa_read_db())
ok("the check says it is already in the data, as a warning",
   any(chk$attempt_id == "FW-T-HELD" & chk$level == "warning" & grepl("already in attempts", chk$problem)))
heal <- fw_inbox_ready(inbox, fw_qa_read_db())
ok("and the import does not take it again", "FW-T-HELD" %in% inbox$attempt_id[heal$take], FALSE)
wh <- fw_qa_write_import(fw_inbox_resolve(inbox[heal$take, ], new_species, fw_qa_read_db()),
                         r2$species_add, inbox[heal$take | heal$reject, FW_INBOX_COLUMNS, drop = FALSE])
ok("re-running the write marks it imported", wh$marked_imported, "FW-T-HELD")
ok("with no duplicate row", sum(fw_qa_read_db()$attempts$attempt_id == "FW-T-HELD"), 1L)

# The sheet changed between resolve and write.
snap <- fw_inbox_read(FW_INBOX_TABS$submissions)
snap <- snap[snap$attempt_id == "FW-T-NOTDONE", FW_INBOX_COLUMNS]
fw_inbox_set(FW_INBOX_TABS$submissions, "FW-T-NOTDONE", "site_name", "Edited after resolve")
ok("a sheet edited after resolve stops the write",
   inherits(tryCatch(fw_qa_write_import(r2, r2$species_add, snap), error = identity), "error"))

# ---- Section 7: the old inbox, once ------------------------------------------
cat("\n-- cutover --\n")
legacy <- as.data.frame(as.list(stats::setNames(rep(NA_character_, length(FW_ATTEMPT_COLUMNS)), FW_ATTEMPT_COLUMNS)))
legacy$attempt_id <- "FW-OLD-1"; legacy$status <- "pending"; legacy$country <- "Norway"
legacy$invasive_species <- paste0(carp, "; new:Pike perch (Sander lucioperca)|Fish")
legacy$beneficiary_species <- NA
legacy$primary_contact_id <- "new:Jo Doe|Some Org|jo@x.org|private"
legacy2 <- legacy; legacy2$attempt_id <- "FW-OLD-2"; legacy2$invasive_species <- carp
legacy2$primary_contact_id <- parker
conv <- fw_inbox_from_legacy(rbind(legacy, legacy2), db0)
s1 <- conv$submissions[[1]]; s2 <- conv$submissions[[2]]
ok("legacy: ids become labels", unname(s1["invasive_species"]),
   "Common carp (Cyprinus carpio); Pike perch (Sander lucioperca)")
ok("legacy: a new: contact is split", unname(s1[c("contact_name", "contact_organisation", "contact_email", "contact_public")]),
   c("Jo Doe", "Some Org", "jo@x.org", "no"))
ok("legacy: a held contact keeps its id and shows its name",
   unname(s2[c("contact_id", "contact_name")]), c(parker, "Parker Bradley"))
ok("legacy: waits for QA", unname(s1[c("qa_status", "imported")]), c("not done", "no"))
ok("legacy: the row has the inbox's columns", names(s1), FW_INBOX_COLUMNS)
ok("legacy: the typed species gets a new_species row",
   unname(conv$new_species[[1]][c("label", "taxa", "from_attempt")]),
   c("Pike perch (Sander lucioperca)", "Fish", "FW-OLD-1"))

unlink(tmp, recursive = TRUE)
cat("\n")
if (failures > 0L) stop(failures, " qa assertion(s) failed", call. = FALSE)
cat("All qa tests passed.\n")
