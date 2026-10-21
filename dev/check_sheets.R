# check_sheets.R
# Is the Google Sheet set up the way the app writes to it?
#
#     Rscript dev/check_sheets.R
#
# Needs FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64 (in .Renviron, or the shell). It
# signs in as the service account, reads row 1 of each tab and compares it
# with the columns the app writes - FW_NEWSLETTER_COLUMNS and
# FW_FEEDBACK_COLUMNS in R/forms_store.R. It WRITES NOTHING: a test sign-up is
# a separate, deliberate step (see the README).
#
#     Rscript dev/check_sheets.R --write-headers
#
# also writes the headers into any tab whose row 1 is COMPLETELY empty - the
# one write this script will make, and never over anything already there.
#
# Extra columns to the right of the app's are fine (staff notes, a status).
# Anything else - a missing tab, a header out of order - is reported with what
# to change, and the script exits non-zero.
#
# THE SUBMISSIONS INBOX (Oct 2026) is checked too: the `submissions` and
# `new_species` tabs of FWISE_INBOX_SHEET_ID (the forms sheet unless set).
#
#     Rscript dev/check_sheets.R --setup-inbox
#
# makes those two tabs ready for review: creates them if missing, writes their
# headers if row 1 is empty, freezes row 1, formats every cell as PLAIN TEXT
# (so Sheets cannot turn "1-2" into a date or drop a leading zero when the
# reviewer types), and puts dropdowns on qa_status, imported, status and
# contact_public. It never changes a value already in the sheet.

if (!file.exists("app.R") || !dir.exists("R")) {
  stop("Run this from the fwise-dashboard folder:\n  cd fwise-dashboard\n  Rscript dev/check_sheets.R",
       call. = FALSE)
}

library(shiny)
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)

if (fw_forms_mode() != "sheets") {
  stop("Set FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64 first; see .Renviron.example.",
       call. = FALSE)
}

key = fw_google_key()
cat("Service account: ", key$client_email, "\n", sep = "")
cat("(the sheet must be shared with that address as an Editor)\n\n")
token = fw_google_token()

write_headers = "--write-headers" %in% commandArgs(trailingOnly = TRUE)
setup_inbox = "--setup-inbox" %in% commandArgs(trailingOnly = TRUE)

problems = 0L
check_tab = function(tab, want, sheet_id = FWISE_FORMS_SHEET_ID, write = write_headers) {
  range = utils::URLencode(paste0(tab, "!1:1"), reserved = TRUE)
  resp = httr2::request(sprintf("%s/%s/values/%s", FW_SHEETS_API, sheet_id, range)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status = httr2::resp_status(resp)
  if (status != 200) {
    cat("  ", tab, ": ", fw_sheets_message(tab, status), "\n", sep = "")
    problems <<- problems + 1L
    return(invisible())
  }
  got = unlist(httr2::resp_body_json(resp)$values[[1]] %||% list())
  if (!length(got) && write) {
    put = httr2::request(sprintf("%s/%s/values/%s", FW_SHEETS_API, sheet_id,
                                  utils::URLencode(paste0(tab, "!A1"), reserved = TRUE))) |>
      httr2::req_url_query(valueInputOption = "RAW") |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_method("PUT") |>
      httr2::req_body_json(list(values = list(as.list(want)))) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(put) == 200) {
      cat("  ", tab, ": headers written\n", sep = "")
      got = want
    } else {
      cat("  ", tab, ": could not write headers (HTTP ", httr2::resp_status(put), ")\n", sep = "")
    }
  }
  head = got[seq_len(min(length(got), length(want)))]
  if (identical(head, want)) {
    extra = setdiff(got, want)
    cat("  ", tab, ": OK", if (length(extra)) paste0(" (plus your columns: ",
        paste(extra, collapse = ", "), ")"), "\n", sep = "")
  } else {
    cat("  ", tab, ": row 1 should start with exactly\n      ",
        paste(want, collapse = ", "), "\n    but holds\n      ",
        if (length(got)) paste(got, collapse = ", ") else "(nothing)", "\n", sep = "")
    problems <<- problems + 1L
  }
}

check_tab(FW_SHEET_TABS$newsletter, FW_NEWSLETTER_COLUMNS)
check_tab(FW_SHEET_TABS$feedback, FW_FEEDBACK_COLUMNS)

# ---- The submissions inbox -----------------------------------------------------

#' One spreadsheets:batchUpdate call
sheet_batch = function(requests) {
  resp = httr2::request(sprintf("%s/%s:batchUpdate", FW_SHEETS_API, FWISE_INBOX_SHEET_ID)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(requests = requests)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200)
    stop("Sheets batchUpdate failed (HTTP ", httr2::resp_status(resp), "): ",
         httr2::resp_body_string(resp), call. = FALSE)
  httr2::resp_body_json(resp)
}

#' The numeric sheetId of each tab, by title, with each tab's column count as
#' the attribute "cols"
tab_ids = function() {
  resp = httr2::request(sprintf("%s/%s", FW_SHEETS_API, FWISE_INBOX_SHEET_ID)) |>
    httr2::req_url_query(fields = "sheets.properties") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform()
  props = lapply(httr2::resp_body_json(resp)$sheets, `[[`, "properties")
  titles = vapply(props, function(p) p$title, character(1))
  out = stats::setNames(vapply(props, function(p) p$sheetId, numeric(1)), titles)
  attr(out, "cols") = stats::setNames(vapply(props, function(p)
    as.numeric(p$gridProperties$columnCount %||% 0), numeric(1)), titles)
  out
}

#' Plain text everywhere, row 1 frozen, dropdowns on the reviewer's columns
setup_tab = function(tab, columns, dropdowns) {
  ids = tab_ids()
  if (!tab %in% names(ids)) {
    sheet_batch(list(list(addSheet = list(properties = list(title = tab)))))
    cat("  ", tab, ": tab created\n", sep = "")
    ids = tab_ids()
  }
  sid = ids[[tab]]
  # A new tab is 26 columns wide; the submissions tab needs more, and a
  # dropdown on a column past the edge is refused. Widened first, never narrowed.
  width = max(attr(ids, "cols")[[tab]], length(columns))
  reqs = list(
    list(updateSheetProperties = list(
      properties = list(sheetId = sid, gridProperties = list(columnCount = width)),
      fields = "gridProperties.columnCount")),
    list(repeatCell = list(
      range = list(sheetId = sid),
      cell = list(userEnteredFormat = list(numberFormat = list(type = "TEXT"))),
      fields = "userEnteredFormat.numberFormat")),
    list(updateSheetProperties = list(
      properties = list(sheetId = sid, gridProperties = list(frozenRowCount = 1)),
      fields = "gridProperties.frozenRowCount"))
  )
  for (col in names(dropdowns)) {
    j = match(col, columns) - 1
    reqs[[length(reqs) + 1]] = list(setDataValidation = list(
      range = list(sheetId = sid, startRowIndex = 1, startColumnIndex = j, endColumnIndex = j + 1),
      rule = list(condition = list(type = "ONE_OF_LIST",
                                   values = lapply(dropdowns[[col]], function(v) list(userEnteredValue = v))),
                  strict = TRUE, showCustomUi = TRUE)))
  }
  sheet_batch(reqs)
  cat("  ", tab, ": plain text, row 1 frozen",
      if (length(dropdowns)) paste0(", dropdowns on ", paste(names(dropdowns), collapse = ", ")), "\n", sep = "")
}

cat("\nSubmissions inbox (", FWISE_INBOX_SHEET_ID, "):\n", sep = "")
if (setup_inbox) {
  setup_tab(FW_INBOX_TABS$submissions, FW_INBOX_COLUMNS,
            list(qa_status = FW_QA_STATUS, imported = FW_IMPORTED, status = FW_STATUS,
                 contact_public = c("yes", "no")))
  setup_tab(FW_INBOX_TABS$new_species, FW_NEW_SPECIES_COLUMNS, list())
}
check_tab(FW_INBOX_TABS$submissions, FW_INBOX_COLUMNS, FWISE_INBOX_SHEET_ID,
          write = write_headers || setup_inbox)
check_tab(FW_INBOX_TABS$new_species, FW_NEW_SPECIES_COLUMNS, FWISE_INBOX_SHEET_ID,
          write = write_headers || setup_inbox)

cat("\n")
if (problems > 0L) stop(problems, " tab(s) need attention", call. = FALSE)
cat("The sheet is ready.\n")
