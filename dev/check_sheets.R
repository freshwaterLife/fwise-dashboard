# check_sheets.R
# Checks the Google Sheet tabs match what the app writes.
#
#     Rscript dev/check_sheets.R                  # read-only header check
#     Rscript dev/check_sheets.R --write-headers  # fill row 1 of empty tabs
#     Rscript dev/check_sheets.R --setup-inbox    # prepare the inbox tabs
#
# Needs FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64. Compares row 1 of each tab
# with FW_NEWSLETTER_COLUMNS, FW_FEEDBACK_COLUMNS and the inbox columns
# (FWISE_INBOX_SHEET_ID, defaulting to the forms sheet). Extra columns on the
# right are fine; anything else fails.
#
# --setup-inbox creates the `submissions` and `new_species` tabs if missing,
# writes headers into an empty row 1, freezes it, sets every cell to plain
# text (no date or number coercion) and adds dropdowns on qa_status, imported,
# status and contact_public. It never changes existing values.

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
  # Widen the tab first (new tabs are 26 columns), never narrow.
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
