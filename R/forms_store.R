# forms_store.R
# The only file that talks to Google Sheets: newsletter, feedback and the
# submissions inbox.
#
# Forms: store_newsletter_signup() and store_feedback() return
# list(success =). Sheets mode needs FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64;
# otherwise rows go to dev/forms/<tab>.csv.
#
# Inbox: fw_inbox_read/append/set() on the `submissions` and `new_species`
# tabs. The app appends; dev/qa.R reads and writes `imported` back. Switch is
# fw_inbox_mode().
#
# Appends only, so simultaneous sends both land. fw_inbox_set() finds rows by
# attempt_id at write time, never by a remembered row number (the sheet may be
# sorted). RAW input, so nothing typed becomes a formula or date.
#
# The Google JWT is signed with openssl here (fw_google_jwt()) to avoid the
# jose dependency.

# Tab columns, in order. Row 1 must match (dev/check_sheets.R checks). Staff
# may add columns to the right.
FW_NEWSLETTER_COLUMNS = c("submitted_at", "name", "email", "organisation",
                           "consent", "consent_wording", "terms_version",
                           "entry_point")
FW_FEEDBACK_COLUMNS = c("submitted_at", "page", "message", "email",
                         "terms_version")

# The tab names in the sheet. Also the file names in local mode.
FW_SHEET_TABS = list(newsletter = "newsletter", feedback = "feedback")

FW_SHEETS_API = "https://sheets.googleapis.com/v4/spreadsheets"
FW_SHEETS_SCOPE = "https://www.googleapis.com/auth/spreadsheets"

# Sheets call timeout. A hung call blocks everyone on the process.
FW_SHEETS_TIMEOUT_S = 10

# ---- Mode --------------------------------------------------------------------

#' "sheets" when both settings are present, "local" otherwise
fw_forms_mode = function() {
  if (!is.null(FWISE_FORMS_SHEET_ID) && !is.null(GS4_SA_KEY_B64)) "sheets" else "local"
}

# ---- Public ------------------------------------------------------------------

#' Store one newsletter sign-up
#'
#' @param record a named list carrying FW_NEWSLETTER_COLUMNS, from
#'   fw_newsletter_record()
#' @return list(success = TRUE/FALSE)
store_newsletter_signup = function(record) {
  fw_store_row(FW_SHEET_TABS$newsletter, record, FW_NEWSLETTER_COLUMNS)
}

#' Store one piece of feedback
#'
#' @param record a named list carrying FW_FEEDBACK_COLUMNS, from
#'   fw_feedback_record()
#' @return list(success = TRUE/FALSE)
store_feedback = function(record) {
  fw_store_row(FW_SHEET_TABS$feedback, record, FW_FEEDBACK_COLUMNS)
}

#' Write one row wherever this deployment writes
#'
#' No local fallback if the sheet write fails: Connect Cloud's disk is wiped
#' on restart. The form reports the failure and keeps the input. The log names
#' the tab only, never the contents.
fw_store_row = function(tab, record, columns) {
  cells = fw_sheet_cells(fw_form_row(record, columns))
  ok = tryCatch({
    if (fw_forms_mode() == "sheets") fw_sheet_append(tab, cells)
    else fw_forms_write_local(tab, cells, columns)
    message("Form row written to ", fw_forms_mode(), " tab ", tab)
    TRUE
  }, error = function(e) {
    warning("Form row write to ", tab, " failed: ", conditionMessage(e))
    FALSE
  })
  list(success = ok)
}

# ---- Rows --------------------------------------------------------------------

#' One record as a character vector in column order, blank where absent
fw_form_row = function(record, columns) {
  vapply(columns, function(col) {
    v = record[[col]]
    if (is.null(v) || length(v) == 0 || is.na(v[1])) "" else as.character(v[1])
  }, character(1), USE.NAMES = FALSE)
}

#' Neutralise anything a spreadsheet would run as a formula
#'
#' Prefixes an apostrophe to cells starting with = + - @, for anyone who
#' opens the tab as CSV in Excel.
fw_sheet_cells = function(x) {
  x = as.character(x)
  risky = !is.na(x) & grepl("^[=+@-]", x)
  x[risky] = paste0("'", x[risky])
  x
}

# ---- Local mode --------------------------------------------------------------

#' Where local-mode rows land: dev/forms/, gitignored with the rest of dev/
fw_forms_dir = function() file.path(FW_DEV_DIR, "forms")

#' Append one row to dev/forms/<tab>.csv, writing the header on first use
fw_forms_write_local = function(tab, cells, columns) {
  dir = fw_forms_dir()
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path = file.path(dir, paste0(tab, ".csv"))
  row = as.data.frame(as.list(stats::setNames(cells, columns)),
                       stringsAsFactors = FALSE, check.names = FALSE)
  readr::write_csv(row, path, na = "", append = file.exists(path))
  path
}

# ---- Google: the key and the token -------------------------------------------

#' The service account key, parsed
#'
#' Raw JSON (starts with a brace) or base64 of it, for one-line consoles.
fw_google_key = function(value = GS4_SA_KEY_B64) {
  value = trimws(value %||% "")
  if (!nzchar(value)) stop("GS4_SA_KEY_B64 is not set.", call. = FALSE)
  json = if (startsWith(value, "{")) value
          else rawToChar(jsonlite::base64_dec(gsub("[[:space:]]", "", value)))
  key = tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE),
                  error = function(e) NULL)
  need = c("client_email", "private_key")
  if (is.null(key) || !all(need %in% names(key))) {
    stop("GS4_SA_KEY_B64 is not a service account JSON key (it needs ",
         "client_email and private_key).", call. = FALSE)
  }
  key$token_uri = key$token_uri %||% "https://oauth2.googleapis.com/token"
  key
}

#' base64url without padding, the encoding a JWT uses
fw_b64url = function(x) {
  if (is.character(x)) x = charToRaw(enc2utf8(x))
  gsub("=+$", "", chartr("+/", "-_", openssl::base64_encode(x)))
}

#' A signed assertion asking Google for a Sheets token
#'
#' The scope goes in the claim, not the token request.
#'
#' @param key  from fw_google_key()
#' @param now  seconds since the epoch, for the tests
fw_google_jwt = function(key, now = as.integer(Sys.time())) {
  header = jsonlite::toJSON(list(alg = "RS256", typ = "JWT"), auto_unbox = TRUE)
  claim = jsonlite::toJSON(list(
    iss = key$client_email, scope = FW_SHEETS_SCOPE, aud = key$token_uri,
    iat = now, exp = now + 3600L
  ), auto_unbox = TRUE)
  signing_input = paste0(fw_b64url(header), ".", fw_b64url(claim))
  sig = openssl::signature_create(charToRaw(signing_input), hash = openssl::sha256,
                                   key = openssl::read_key(key$private_key))
  paste0(signing_input, ".", fw_b64url(sig))
}

# Cached token, renewed a minute before its hour is up.
FW_GOOGLE_TOKEN = new.env(parent = emptyenv())

fw_google_token = function() {
  now = as.integer(Sys.time())
  if (!is.null(FW_GOOGLE_TOKEN$value) && FW_GOOGLE_TOKEN$expires > now + 60L) {
    return(FW_GOOGLE_TOKEN$value)
  }
  key = fw_google_key()
  resp = httr2::request(key$token_uri) |>
    httr2::req_body_form(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion = fw_google_jwt(key, now)
    ) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200) {
    stop("Google refused the service account key (HTTP ", httr2::resp_status(resp),
         "). Check GS4_SA_KEY_B64 is the current key for an active service ",
         "account.", call. = FALSE)
  }
  body = httr2::resp_body_json(resp)
  FW_GOOGLE_TOKEN$value = body$access_token
  FW_GOOGLE_TOKEN$expires = now + as.integer(body$expires_in %||% 3600L)
  FW_GOOGLE_TOKEN$value
}

# ---- Google: the append ------------------------------------------------------

#' The append request, built but not sent
#'
#' Separate so dev/plan_test.R can check it offline.
#'
#' @param cells one row as a character vector, or several as a list of them
fw_sheet_request = function(tab, cells, token, sheet_id = FWISE_FORMS_SHEET_ID) {
  rows = if (is.list(cells)) cells else list(cells)
  range = utils::URLencode(paste0(tab, "!A1"), reserved = TRUE)
  httr2::request(sprintf("%s/%s/values/%s:append", FW_SHEETS_API, sheet_id, range)) |>
    httr2::req_url_query(valueInputOption = "RAW", insertDataOption = "INSERT_ROWS") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(values = lapply(rows, function(r) as.list(unname(r))))) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S) |>
    # Retry only 429/503 (nothing written). Not timeouts: the row may exist.
    httr2::req_retry(max_tries = 2)
}

#' Append one row (or a list of rows) to a tab
fw_sheet_append = function(tab, cells, sheet_id = FWISE_FORMS_SHEET_ID) {
  resp = httr2::req_perform(fw_sheet_request(tab, cells, fw_google_token(), sheet_id))
  status = httr2::resp_status(resp)
  if (status == 200) return(invisible(TRUE))
  stop(fw_sheets_message(tab, status), call. = FALSE)
}

#' What a failed append means, in words someone can act on in the server log
fw_sheets_message = function(tab, status) {
  hint = switch(
    as.character(status),
    "400" = paste0("Usually the tab is missing: the sheet needs a tab named '",
                   tab, "' exactly."),
    "401" = "The access token was rejected. Check GS4_SA_KEY_B64.",
    "403" = paste("The service account cannot edit the sheet. Share the sheet",
                  "with the key's client_email as an Editor, and check the",
                  "Google Sheets API is enabled in its Cloud project."),
    "404" = "No sheet with that id. Check FWISE_FORMS_SHEET_ID (or FWISE_INBOX_SHEET_ID).",
    paste0("Unexpected HTTP ", status, ".")
  )
  paste0("Google Sheets refused the call on tab '", tab, "' (HTTP ", status, "). ", hint)
}

# ---- Google: reading a tab and writing cells back ----------------------------

#' The read request for a whole tab, built but not sent
#'
#' FORMATTED_VALUE: the text the reviewer sees (tabs are plain-text format).
fw_sheet_read_request = function(tab, token, sheet_id) {
  range = utils::URLencode(tab, reserved = TRUE)
  httr2::request(sprintf("%s/%s/values/%s", FW_SHEETS_API, sheet_id, range)) |>
    httr2::req_url_query(valueRenderOption = "FORMATTED_VALUE", majorDimension = "ROWS") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S) |>
    httr2::req_retry(max_tries = 2)
}

#' The batchUpdate request that writes single cells, built but not sent
#'
#' @param updates a list of list(range = "tab!B7", value = "yes")
fw_sheet_update_request = function(updates, token, sheet_id) {
  data = lapply(updates, function(u) list(range = u$range, values = list(list(u$value))))
  httr2::request(sprintf("%s/%s/values:batchUpdate", FW_SHEETS_API, sheet_id)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(valueInputOption = "RAW", data = data)) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S)
}

#' A tab's rows as a data frame of text, keyed by header
#'
#' Pads short rows; blanks are NA. `sheet_row` is for the reviewer only;
#' nothing writes by it.
fw_sheet_values_frame = function(values) {
  if (!length(values)) return(data.frame(sheet_row = integer(0)))
  header = trimws(unlist(values[[1]]))
  body = values[-1]
  cols = lapply(seq_along(header), function(j) vapply(body, function(r) {
    v = if (length(r) >= j) as.character(r[[j]]) else ""
    v = trimws(v)
    if (nzchar(v)) v else NA_character_
  }, character(1)))
  names(cols) = header
  out = as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  out = out[, nzchar(names(out)), drop = FALSE]
  out$sheet_row = seq_len(nrow(out)) + 1L
  out
}

#' Read a whole tab of the sheet
fw_sheet_read = function(tab, sheet_id) {
  resp = httr2::req_perform(fw_sheet_read_request(tab, fw_google_token(), sheet_id))
  status = httr2::resp_status(resp)
  if (status != 200) stop(fw_sheets_message(tab, status), call. = FALSE)
  fw_sheet_values_frame(httr2::resp_body_json(resp)$values %||% list())
}

#' A column number as the sheet's letters: 1 -> A, 27 -> AA
fw_sheet_col_letter = function(n) {
  out = character(0)
  while (n > 0) {
    r = (n - 1) %% 26
    out = c(LETTERS[r + 1], out)
    n = (n - 1) %/% 26
  }
  paste(out, collapse = "")
}

#' The cell ranges for one column of the rows holding the given ids
#'
#' Read just before writing, in case the sheet was sorted.
fw_sheet_cell_ranges = function(tab, frame, attempt_ids, column) {
  j = match(column, setdiff(names(frame), "sheet_row"))
  if (is.na(j)) stop("Tab '", tab, "' has no column '", column, "'.", call. = FALSE)
  rows = frame$sheet_row[match(attempt_ids, frame$attempt_id)]
  if (anyNA(rows)) stop("Not in tab '", tab, "': ",
                        paste(attempt_ids[is.na(rows)], collapse = ", "), call. = FALSE)
  paste0(tab, "!", fw_sheet_col_letter(j), rows)
}

# ---- The submissions inbox ---------------------------------------------------

FW_INBOX_TABS = list(submissions = "submissions", new_species = "new_species")

#' "sheets" when the inbox sheet and the key are both set, "local" otherwise
fw_inbox_mode = function() {
  if (!is.null(FWISE_INBOX_SHEET_ID) && !is.null(GS4_SA_KEY_B64)) "sheets" else "local"
}

#' The local stand-in for an inbox tab: dev/forms/<tab>.csv
fw_inbox_local_path = function(tab) file.path(fw_forms_dir(), paste0(tab, ".csv"))

#' Read an inbox tab, every column text, blanks as NA, plus sheet_row
fw_inbox_read = function(tab) {
  if (fw_inbox_mode() == "sheets") return(fw_sheet_read(tab, FWISE_INBOX_SHEET_ID))
  path = fw_inbox_local_path(tab)
  if (!file.exists(path)) {
    # No local file yet: an empty tab with headers.
    cols = if (identical(tab, FW_INBOX_TABS$new_species)) FW_NEW_SPECIES_COLUMNS else FW_INBOX_COLUMNS
    d = as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols))
    d$sheet_row = integer(0)
    return(d)
  }
  d = utils::read.csv(path, colClasses = "character", check.names = FALSE,
                       na.strings = "", encoding = "UTF-8")
  d[] = lapply(d, function(x) { x = trimws(x); x[!is.na(x) & !nzchar(x)] = NA; x })
  d$sheet_row = seq_len(nrow(d)) + 1L
  d
}

#' Append rows to an inbox tab
#'
#' No apostrophe guard (fw_sheet_cells()): the inbox is read over the API, and
#' it would turn -31.24 into text.
#'
#' @param rows a list of character vectors, each in the tab's column order
#' @param columns the tab's columns, for the local file's header
fw_inbox_append = function(tab, rows, columns) {
  if (!length(rows)) return(invisible(TRUE))
  rows = lapply(rows, function(r) { r[is.na(r)] = ""; as.character(r) })
  if (fw_inbox_mode() == "sheets") return(fw_sheet_append(tab, rows, FWISE_INBOX_SHEET_ID))
  for (r in rows) fw_forms_write_local(tab, r, columns)
  invisible(TRUE)
}

#' Write one column of the rows holding the given attempt ids
#'
#' @param values one per id, or one for all
fw_inbox_set = function(tab, attempt_ids, column, values) {
  if (!length(attempt_ids)) return(invisible(TRUE))
  values = rep_len(as.character(values), length(attempt_ids))
  if (fw_inbox_mode() == "sheets") {
    frame = fw_sheet_read(tab, FWISE_INBOX_SHEET_ID)
    ranges = fw_sheet_cell_ranges(tab, frame, attempt_ids, column)
    updates = Map(function(r, v) list(range = r, value = v), ranges, values)
    resp = httr2::req_perform(fw_sheet_update_request(unname(updates), fw_google_token(),
                                                       FWISE_INBOX_SHEET_ID))
    status = httr2::resp_status(resp)
    if (status != 200) stop(fw_sheets_message(tab, status), call. = FALSE)
    return(invisible(TRUE))
  }
  path = fw_inbox_local_path(tab)
  d = utils::read.csv(path, colClasses = "character", check.names = FALSE,
                       na.strings = "", encoding = "UTF-8")
  i = match(attempt_ids, d$attempt_id)
  if (anyNA(i)) stop("Not in ", basename(path), ": ",
                     paste(attempt_ids[is.na(i)], collapse = ", "), call. = FALSE)
  d[[column]][i] = values
  readr::write_csv(d, path, na = "")
  invisible(TRUE)
}
