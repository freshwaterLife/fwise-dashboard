# qa.R
# THE REVIEW AND IMPORT, ONE SECTION AT A TIME. Submissions from the contribute
# form land as rows of the `submissions` tab of the inbox Google Sheet (and,
# for a species the contributor typed in, the `new_species` tab). The reviewer
# checks and corrects them THERE, and this script imports what they approve.
#
# HOW TO RUN IT. In RStudio or VS Code, with R started in fwise/ or
# fwise-dashboard/, run one section at a time: put the cursor in a section and
# press Ctrl+Alt+T (Cmd+Option+T on a Mac), or select lines and press
# Ctrl+Enter. Read what each section prints before running the next. Section 0
# loads every function the rest needs - there is no need to run the app first.
#
#   0  Setup        load the app's code; say which sheet and data checkout
#   1  Read and     the sheet, as `inbox` and `new_species`; problems into
#      check        `checks`; possible duplicates into `contact_matches` and
#                   `species_matches`
#                   -> FIX THINGS IN THE SHEET, then run 1 again.
#                   -> When a row is right, set its qa_status to "done" and its
#                      status to "approved" (or "rejected").
#   2  Resolve      what would be imported: `to_add`, `species_add`, `contacts_add`
#   3  Enrich       family, IUCN status and photo for the new species
#   4  WRITE        the only section that changes anything: the data files,
#                   then the sheet's `imported` column
#   5  Publish      the git commands that put the import live
#
# NOTHING SHOULD BE IMPORTED TWICE. 
# A row is taken only while `imported` is "no" and
# its attempt_id is not already in attempts.csv, and section 4 checks again
# straight before it writes. Running any section twice is safe.
#
# What the reviewer needs on their machine: this repository and fwise-data
# side by side in one folder (fwise/fwise-dashboard and fwise/fwise-data),
# renv restored, and in fwise-dashboard/.Renviron GS4_SA_KEY_B64 and
# FWISE_INBOX_SHEET_ID (or FWISE_FORMS_SHEET_ID), plus IUCN_API_TOKEN for
# section 3. 


# ==== 0. Setup ================================================================

# Find fwise-dashboard/ from wherever R was started (fwise/, fwise-dashboard/
# or fwise-dashboard/dev/) and work from there: R/config.R takes its paths from
# the working directory, and fwise-data is the sibling ../fwise-data.
local({
  root = Find(function(d) file.exists(file.path(d, "R", "config.R")) &&
                 file.exists(file.path(d, "dev", "qa.R")),
               c(".", "fwise-dashboard", ".."))
  if (is.null(root)) stop("Cannot find fwise-dashboard/. Start R in fwise/ or fwise-dashboard/.",
                          call. = FALSE)
  setwd(root)
})
message("Dashboard: ", getwd())
# R reads .Renviron only from the folder it started in, so read this one now,
# before R/config.R picks up the keys.
if (file.exists(".Renviron")) readRenviron(".Renviron")

suppressPackageStartupMessages(library(shiny))
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)
source("dev/species_enrich.R")

if (!file.exists(file.path(FW_DATA_DIR, FW_ATTEMPTS_FILE)))
  stop("No ", FW_ATTEMPTS_FILE, " in ", FW_DATA_DIR,
       ". Clone fwise-data next to fwise-dashboard, in the same folder.", call. = FALSE)

#' git in the fwise-data checkout, never prompting for a password
fw_qa_git = function(...) {
  suppressWarnings(system2("git", c("-C", shQuote(FW_DATA_DIR), ...),
                           stdout = TRUE, stderr = TRUE, env = "GIT_TERMINAL_PROMPT=0"))
}

#' The checkout's branch, whether it has uncommitted changes, and how far
#' behind its remote it is (NA when that cannot be told)
fw_qa_git_state = function() {
  fw_qa_git("fetch", "--quiet")
  behind = fw_qa_git("rev-list", "--count", "HEAD..@{u}")
  list(branch = fw_qa_git("rev-parse", "--abbrev-ref", "HEAD")[1],
       changes = fw_qa_git("status", "--porcelain"),
       behind = suppressWarnings(as.integer(behind[1])))
}

git = fw_qa_git_state()
message("Data checkout: ", FW_DATA_DIR, "  (branch ", git$branch, ")")
if (length(git$changes)) message("  ! it has uncommitted changes - section 4 will not write until they are committed or undone")
if (isTRUE(git$behind > 0)) message("  ! it is ", git$behind, " commit(s) behind - pull fwise-data before going on")
message("Inbox: ", if (fw_inbox_mode() == "sheets")
  paste0("Google Sheet ", FWISE_INBOX_SHEET_ID) else
  paste0("LOCAL files in ", fw_forms_dir(), " (no sheet set)"))
message("IUCN lookups in section 3: ", if (nzchar(Sys.getenv("IUCN_API_TOKEN"))) "on" else "OFF (IUCN_API_TOKEN not set)")


# ==== 1. Read and check =======================================================

if (!exists("fw_inbox_read")) stop("Run section 0 first.", call. = FALSE)
inbox       = fw_inbox_read(FW_INBOX_TABS$submissions)
new_species = fw_inbox_read(FW_INBOX_TABS$new_species)
db          = fw_qa_read_db()

open_rows = inbox[fw_inbox_open(inbox, db), , drop = FALSE]
message(nrow(inbox), " row(s) in the sheet; ", nrow(open_rows), " not yet imported (",
        sum(open_rows$qa_status %in% "done"), " with QA done)")

checks          = fw_inbox_check(inbox, new_species, db)
contact_matches = fw_contact_matches(inbox, db)
species_matches = fw_species_matches(inbox, db)

# One line per open row: how it stands.
row_summary = tibble::tibble(
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
        "replace the typed label with the candidate_label. Then run section 1 again.")
fw_qa_show(checks, "checks")
if (nrow(contact_matches)) fw_qa_show(contact_matches, "contact_matches")
if (nrow(species_matches)) fw_qa_show(species_matches, "species_matches")


# ==== 2. Resolve: what would be imported ======================================

# Leave NULL to import every row whose QA is done; or name some, e.g.
# import_ids = c("FW-20261012-7K3QX9")
import_ids = NULL

if (!exists("fw_inbox_read")) stop("Run section 0 first.", call. = FALSE)
inbox       = fw_inbox_read(FW_INBOX_TABS$submissions)
new_species = fw_inbox_read(FW_INBOX_TABS$new_species)
db          = fw_qa_read_db()

sel = fw_inbox_ready(inbox, db, import_ids)

# Checked again here, on exactly these rows: an earlier run of section 1 may
# be out of date.
checks = fw_inbox_check(inbox, new_species, db)
blocking = checks[checks$level == "error" &
                     (is.na(checks$attempt_id) | checks$attempt_id %in% inbox$attempt_id[sel$ready]), ]
if (nrow(blocking)) {
  fw_qa_show(blocking, "blocking errors")
  print(blocking[, c("sheet_row", "attempt_id", "column", "value", "problem")], n = Inf)
  stop(nrow(blocking), " error(s) on rows marked done. Fix them in the sheet and run section 2 again.",
       call. = FALSE)
}

resolved = fw_inbox_resolve(inbox[sel$take, , drop = FALSE], new_species, db)
species_add = resolved$species_add
# What section 4 will compare against, to be sure the sheet has not changed.
qa_snapshot = inbox[sel$take | sel$reject, FW_INBOX_COLUMNS, drop = FALSE]

message(nrow(resolved$to_add), " attempt(s) to add, ", sum(sel$reject), " to mark rejected; ",
        nrow(species_add), " new species, ", nrow(resolved$contacts_add), " new contact(s)",
        if (length(resolved$contacts_private)) paste0("; ", length(resolved$contacts_private),
          " held contact(s) to hide (asked for private): ", paste(resolved$contacts_private, collapse = ", ")))
fw_qa_show(resolved$to_add, "to_add")
if (nrow(species_add)) fw_qa_show(species_add, "species_add")
if (nrow(resolved$contacts_add)) fw_qa_show(resolved$contacts_add, "contacts_add")


# ==== 3. Enrich the new species ===============================================
#
# Family (fish only) from GBIF, the IUCN Red List status, and a photograph with
# its credit and licence. Fills blanks only; one request a second, so allow a
# few seconds per species. Skip this section and section 4 writes the species
# without them - dev/fetch_species_images.R can add photographs later.

if (!exists("resolved")) stop("Run section 2 first.", call. = FALSE)
enriched = fw_enrich_species(resolved$species_add)
species_add = enriched$species
enrich_log = enriched$log
print(enrich_log, n = Inf)
fw_qa_show(species_add, "species_add (enriched)")


# ==== 4. WRITE ================================================================
#
# The data files first, then the sheet. If the sheet write fails, run this
# section again: rows already in attempts.csv are marked imported and nothing
# is added twice.

if (!exists("resolved")) stop("Run section 2 (and 3) first.", call. = FALSE)

local({
  git = fw_qa_git_state()
  if (length(git$changes)) stop("fwise-data has uncommitted changes. Commit or undo them first:\n  ",
                                paste(git$changes, collapse = "\n  "), call. = FALSE)
  if (isTRUE(git$behind > 0)) stop("fwise-data is ", git$behind, " commit(s) behind. Pull it, then run sections 2-4 again.",
                                   call. = FALSE)
  if (interactive()) {
    answer = readline(sprintf("Write %d attempt(s), %d species, %d contact(s) and mark %d rejected? Type yes: ",
                               nrow(resolved$to_add), nrow(species_add), nrow(resolved$contacts_add),
                               sum(qa_snapshot$status %in% "rejected")))
    if (!identical(trimws(answer), "yes")) stop("Nothing written.", call. = FALSE)
  }
  w = fw_qa_write_import(resolved, species_add, qa_snapshot)
  message("Written: +", length(w$attempts_added), " attempts (now ", w$attempts_total, "), +",
          length(w$species_added), " species, +", length(w$contacts_added), " contacts",
          if (length(w$contacts_hidden)) paste0(", hidden: ", paste(w$contacts_hidden, collapse = ", ")))
  message("Sheet: ", length(w$marked_imported), " row(s) marked imported, ",
          length(w$marked_rejected), " marked rejected.")
})
rm(resolved, species_add, qa_snapshot)


# ==== 5. Publish ==============================================================
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
