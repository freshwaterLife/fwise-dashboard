# qa.R
# THE REVIEW AND IMPORT, ONE SECTION AT A TIME. Submissions from the contribute
# form land as rows of the `submissions` tab of the inbox Google Sheet (and,
# for a species the contributor typed in, the `new_species` tab). The reviewer
# checks and corrects them THERE, and this script imports what they approve.
#
# HOW TO RUN IT. Open fwise-dashboard in RStudio (the working directory must be
# fwise-dashboard/) and run one section at a time: put the cursor in a section
# and press Ctrl+Alt+T (Cmd+Option+T on a Mac), or select lines and press
# Ctrl+Enter. Read what each section prints before running the next.
#
#   0  Setup        load the app's code; say which sheet and data checkout
#   1  Read         the sheet, as `inbox` and `new_species`
#   2  Check        problems into `checks`; possible duplicates into
#                   `contact_matches` and `species_matches`
#                   -> FIX THINGS IN THE SHEET, then run 1 and 2 again.
#                   -> When a row is right, set its qa_status to "done" and its
#                      status to "approved" (or "rejected").
#   3  Resolve      what would be imported: `to_add`, `species_add`, `contacts_add`
#   4  Enrich       family, IUCN status and photo for the new species
#   5  WRITE        the only section that changes anything: the data files,
#                   then the sheet's `imported` column
#   6  Publish      the git commands that put the import live
#   7  Cutover      ONE-OFF, see the section
#
# NOTHING IS IMPORTED TWICE. A row is taken only while `imported` is "no" and
# its attempt_id is not already in attempts.csv, and section 5 checks again
# straight before it writes. Running any section twice is safe.
#
# What the reviewer needs on their machine: this repository and fwise-data
# side by side, renv restored, and in .Renviron GS4_SA_KEY_B64 and
# FWISE_INBOX_SHEET_ID (or FWISE_FORMS_SHEET_ID), plus IUCN_API_TOKEN for
# section 4. With no sheet set, the script works on dev/forms/*.csv instead,
# which is what a submission on a machine with no keys writes.


# ==== 0. Setup ================================================================

suppressPackageStartupMessages(library(shiny))
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)
source("dev/species_enrich.R")

#' git in the fwise-data checkout, never prompting for a password
fw_qa_git <- function(...) {
  suppressWarnings(system2("git", c("-C", shQuote(FW_DATA_DIR), ...),
                           stdout = TRUE, stderr = TRUE, env = "GIT_TERMINAL_PROMPT=0"))
}

#' The checkout's branch, whether it has uncommitted changes, and how far
#' behind its remote it is (NA when that cannot be told)
fw_qa_git_state <- function() {
  fw_qa_git("fetch", "--quiet")
  behind <- fw_qa_git("rev-list", "--count", "HEAD..@{u}")
  list(branch = fw_qa_git("rev-parse", "--abbrev-ref", "HEAD")[1],
       changes = fw_qa_git("status", "--porcelain"),
       behind = suppressWarnings(as.integer(behind[1])))
}

fw_qa_show <- function(x, title) if (interactive()) utils::View(x, title)

git <- fw_qa_git_state()
message("Data checkout: ", FW_DATA_DIR, "  (branch ", git$branch, ")")
if (length(git$changes)) message("  ! it has uncommitted changes - section 5 will not write until they are committed or undone")
if (isTRUE(git$behind > 0)) message("  ! it is ", git$behind, " commit(s) behind - pull fwise-data before going on")
message("Inbox: ", if (fw_inbox_mode() == "sheets")
  paste0("Google Sheet ", FWISE_INBOX_SHEET_ID) else
  paste0("LOCAL files in ", fw_forms_dir(), " (no sheet set)"))
message("IUCN lookups in section 4: ", if (nzchar(Sys.getenv("IUCN_API_TOKEN"))) "on" else "OFF (IUCN_API_TOKEN not set)")


# ==== 1. Read the sheet =======================================================

inbox       <- fw_inbox_read(FW_INBOX_TABS$submissions)
new_species <- fw_inbox_read(FW_INBOX_TABS$new_species)
db          <- fw_qa_read_db()

open_rows <- inbox[fw_inbox_open(inbox, db), , drop = FALSE]
message(nrow(inbox), " row(s) in the sheet; ", nrow(open_rows), " not yet imported (",
        sum(open_rows$qa_status %in% "done"), " with QA done)")
fw_qa_show(open_rows, "open rows")


# ==== 2. Check ================================================================

inbox       <- fw_inbox_read(FW_INBOX_TABS$submissions)
new_species <- fw_inbox_read(FW_INBOX_TABS$new_species)
db          <- fw_qa_read_db()

checks          <- fw_inbox_check(inbox, new_species, db)
contact_matches <- fw_contact_matches(inbox, db)
species_matches <- fw_species_matches(inbox, db)

# One line per open row: how it stands.
open_rows <- inbox[fw_inbox_open(inbox, db), , drop = FALSE]
row_summary <- tibble::tibble(
  sheet_row = open_rows$sheet_row, attempt_id = open_rows$attempt_id,
  site_name = open_rows$site_name, qa_status = open_rows$qa_status, status = open_rows$status,
  errors   = vapply(open_rows$attempt_id, function(id) sum(checks$attempt_id %in% id & checks$level == "error"), 0L),
  warnings = vapply(open_rows$attempt_id, function(id) sum(checks$attempt_id %in% id & checks$level == "warning"), 0L)
)
print(row_summary, n = Inf)
message(sum(checks$level == "error"), " error(s), ", sum(checks$level == "warning"), " warning(s); ",
        nrow(contact_matches), " possible contact match(es); ", nrow(species_matches), " possible species match(es)")
message("Errors stop a row being imported. Warnings do not: QA done means you have looked at them.\n",
        "To use a contact we hold, type its CO- id in the row's contact_id. To use a species we hold,\n",
        "replace the typed label with the candidate_label. Then run sections 1 and 2 again.")
fw_qa_show(checks, "checks")
if (nrow(contact_matches)) fw_qa_show(contact_matches, "contact_matches")
if (nrow(species_matches)) fw_qa_show(species_matches, "species_matches")


# ==== 3. Resolve: what would be imported ======================================

# Leave NULL to import every row whose QA is done; or name some, e.g.
# import_ids <- c("FW-20261012-7K3QX9")
import_ids <- NULL

inbox       <- fw_inbox_read(FW_INBOX_TABS$submissions)
new_species <- fw_inbox_read(FW_INBOX_TABS$new_species)
db          <- fw_qa_read_db()

sel <- fw_inbox_ready(inbox, db, import_ids)

# Checked again here, on exactly these rows: an earlier run of section 2 may
# be out of date.
checks <- fw_inbox_check(inbox, new_species, db)
blocking <- checks[checks$level == "error" &
                     (is.na(checks$attempt_id) | checks$attempt_id %in% inbox$attempt_id[sel$ready]), ]
if (nrow(blocking)) {
  fw_qa_show(blocking, "blocking errors")
  print(blocking[, c("sheet_row", "attempt_id", "column", "value", "problem")], n = Inf)
  stop(nrow(blocking), " error(s) on rows marked done. Fix them in the sheet and run section 3 again.",
       call. = FALSE)
}

resolved <- fw_inbox_resolve(inbox[sel$take, , drop = FALSE], new_species, db)
species_add <- resolved$species_add
# What section 5 will compare against, to be sure the sheet has not changed.
qa_snapshot <- inbox[sel$take | sel$reject, FW_INBOX_COLUMNS, drop = FALSE]

message(nrow(resolved$to_add), " attempt(s) to add, ", sum(sel$reject), " to mark rejected; ",
        nrow(species_add), " new species, ", nrow(resolved$contacts_add), " new contact(s)",
        if (length(resolved$contacts_private)) paste0("; ", length(resolved$contacts_private),
          " held contact(s) to hide (asked for private): ", paste(resolved$contacts_private, collapse = ", ")))
fw_qa_show(resolved$to_add, "to_add")
if (nrow(species_add)) fw_qa_show(species_add, "species_add")
if (nrow(resolved$contacts_add)) fw_qa_show(resolved$contacts_add, "contacts_add")


# ==== 4. Enrich the new species ===============================================
#
# Family (fish only) from GBIF, the IUCN Red List status, and a photograph with
# its credit and licence. Fills blanks only; one request a second, so allow a
# few seconds per species. Skip this section and section 5 writes the species
# without them - dev/fetch_species_images.R can add photographs later.

if (!exists("resolved")) stop("Run section 3 first.", call. = FALSE)
enriched <- fw_enrich_species(resolved$species_add)
species_add <- enriched$species
enrich_log <- enriched$log
print(enrich_log, n = Inf)
fw_qa_show(species_add, "species_add (enriched)")


# ==== 5. WRITE ================================================================
#
# The data files first, then the sheet. If the sheet write fails, run this
# section again: rows already in attempts.csv are marked imported and nothing
# is added twice.

if (!exists("resolved")) stop("Run section 3 (and 4) first.", call. = FALSE)

local({
  git <- fw_qa_git_state()
  if (length(git$changes)) stop("fwise-data has uncommitted changes. Commit or undo them first:\n  ",
                                paste(git$changes, collapse = "\n  "), call. = FALSE)
  if (isTRUE(git$behind > 0)) stop("fwise-data is ", git$behind, " commit(s) behind. Pull it, then run sections 3-5 again.",
                                   call. = FALSE)
  if (interactive()) {
    answer <- readline(sprintf("Write %d attempt(s), %d species, %d contact(s) and mark %d rejected? Type yes: ",
                               nrow(resolved$to_add), nrow(species_add), nrow(resolved$contacts_add),
                               sum(qa_snapshot$status %in% "rejected")))
    if (!identical(trimws(answer), "yes")) stop("Nothing written.", call. = FALSE)
  }
  w <- fw_qa_write_import(resolved, species_add, qa_snapshot)
  message("Written: +", length(w$attempts_added), " attempts (now ", w$attempts_total, "), +",
          length(w$species_added), " species, +", length(w$contacts_added), " contacts",
          if (length(w$contacts_hidden)) paste0(", hidden: ", paste(w$contacts_hidden, collapse = ", ")))
  message("Sheet: ", length(w$marked_imported), " row(s) marked imported, ",
          length(w$marked_rejected), " marked rejected.")
})
rm(resolved, species_add, qa_snapshot)


# ==== 6. Publish ==============================================================
#
# The import is in your fwise-data checkout but not live yet. Commit and push
# it, then restart the app (Connect Cloud: Republish) - the app reads the data
# once, when it starts.

message(paste(fw_qa_git("status", "--short"), collapse = "\n"))
message("\nIn a terminal:\n",
        "  cd ", FW_DATA_DIR, "\n",
        "  git add attempts.csv species.csv contacts.csv metadata.json\n",
        "  git commit -m \"Import submissions ", format(Sys.Date()), "\"\n",
        "  git push\n",
        "Then restart the app.")


# ==== 7. Cutover (ONE-OFF) ====================================================
#
# Until October 2026 the app wrote each submission to fwise-data/inbox/ as a
# CSV. Run this ONCE, after the new app is deployed and with fwise-data's main
# branch checked out and pulled, to move whatever is still there into the
# sheet. Then delete inbox/ from fwise-data and delete this section and
# fw_inbox_from_legacy() in R/qa_checks.R.

local({
  legacy_dir <- file.path(FW_DATA_DIR, "inbox")
  files <- list.files(legacy_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) return(message("Nothing in ", legacy_dir))
  legacy <- fw_qa_blank_na(dplyr::bind_rows(lapply(files, function(f)
    readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE))))
  db <- fw_qa_read_db()
  inbox <- fw_inbox_read(FW_INBOX_TABS$submissions)
  legacy <- legacy[!legacy$attempt_id %in% c(inbox$attempt_id, db$attempts$attempt_id), ]
  if (!nrow(legacy)) return(message("Every file in ", legacy_dir, " is already in the sheet or the data."))
  conv <- fw_inbox_from_legacy(legacy, db)
  if (interactive() && !identical(trimws(readline(sprintf(
    "Append %d submission(s) and %d new species row(s) to the sheet? Type yes: ",
    length(conv$submissions), length(conv$new_species)))), "yes")) stop("Nothing written.", call. = FALSE)
  fw_inbox_append(FW_INBOX_TABS$submissions, conv$submissions, FW_INBOX_COLUMNS)
  fw_inbox_append(FW_INBOX_TABS$new_species, conv$new_species, FW_NEW_SPECIES_COLUMNS)
  message("Appended ", length(conv$submissions), " submission(s): ", paste(legacy$attempt_id, collapse = ", "))
})
