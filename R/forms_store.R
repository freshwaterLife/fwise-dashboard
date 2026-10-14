# forms_store.R
# Where newsletter sign-ups, feedback AND SUBMISSIONS are written. THE ONLY
# FILE THAT KNOWS ABOUT GOOGLE SHEETS.
#
# THE FORMS: store_newsletter_signup(record) and store_feedback(record), each
# returning list(success = TRUE/FALSE). Where they write is decided by
# fw_forms_mode(): with FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64 both set they
# append one row to the tab of that name in the Google Sheet; without them they
# append to dev/forms/<tab>.csv on this machine, so the forms work on a clean
# checkout with no credentials at all.
#
# THE SUBMISSIONS INBOX (Oct 2026): fw_inbox_read(), fw_inbox_append() and
# fw_inbox_set(), on the `submissions` and `new_species` tabs. The app appends
# to them; dev/qa.R reads them and writes the `imported` column back. Same
# switch, fw_inbox_mode(), with FWISE_INBOX_SHEET_ID (the forms sheet unless
# set) and the same local fallback in dev/forms/.
#
# AN APPEND, NEVER A READ-MODIFY-WRITE. Sheets' values:append adds a row after
# the last one on Google's side, so two people pressing the button in the same
# second both land, and nothing here has to read the sheet first. The one write
# that is not an append, fw_inbox_set(), finds its rows by attempt_id at the
# moment it writes, never by a row number remembered from an earlier read: the
# reviewer may have sorted the sheet in between.
#
# RAW, NEVER USER_ENTERED. With RAW the sheet stores exactly the text it was
# given; nothing a visitor types is ever parsed as a formula or a date.
#
# NO JOSE. httr2's own JWT flow needs the jose package, which this app does not
# otherwise use. A Google service-account token is one RS256 signature over two
# small JSON objects, and openssl (already deployed) makes that signature, so
# the token exchange is written out here instead: fw_google_jwt() and
# fw_google_token().

# The columns of each tab, in order. Row 1 of each tab in the sheet must hold
# exactly these headers; dev/check_sheets.R compares them. Staff may add their
# own columns to the RIGHT of these (status, notes) - the app only ever writes
# this many cells per row, starting in column A.
FW_NEWSLETTER_COLUMNS <- c("submitted_at", "name", "email", "organisation",
                           "consent", "consent_wording", "terms_version",
                           "entry_point")
FW_FEEDBACK_COLUMNS <- c("submitted_at", "page", "message", "email",
                         "terms_version")

# The tab names in the sheet. Also the file names in local mode.
FW_SHEET_TABS <- list(newsletter = "newsletter", feedback = "feedback")

FW_SHEETS_API <- "https://sheets.googleapis.com/v4/spreadsheets"
FW_SHEETS_SCOPE <- "https://www.googleapis.com/auth/spreadsheets"

# Seconds before a Sheets call is abandoned. The write runs on the session's
# own thread, so a hung call holds up everyone on the same process - see the
# same reasoning at FW_GH_TIMEOUT_S in R/github.R.
FW_SHEETS_TIMEOUT_S <- 10

# ---- Mode --------------------------------------------------------------------

#' "sheets" when both settings are present, "local" otherwise
fw_forms_mode <- function() {
  if (!is.null(FWISE_FORMS_SHEET_ID) && !is.null(GS4_SA_KEY_B64)) "sheets" else "local"
}

# ---- Public ------------------------------------------------------------------

#' Store one newsletter sign-up
#'
#' @param record a named list carrying FW_NEWSLETTER_COLUMNS, from
#'   fw_newsletter_record()
#' @return list(success = TRUE/FALSE)
store_newsletter_signup <- function(record) {
  fw_store_row(FW_SHEET_TABS$newsletter, record, FW_NEWSLETTER_COLUMNS)
}

#' Store one piece of feedback
#'
#' @param record a named list carrying FW_FEEDBACK_COLUMNS, from
#'   fw_feedback_record()
#' @return list(success = TRUE/FALSE)
store_feedback <- function(record) {
  fw_store_row(FW_SHEET_TABS$feedback, record, FW_FEEDBACK_COLUMNS)
}

#' Write one row wherever this deployment writes
#'
#' NO FALLBACK TO THE LOCAL FILE IF THE SHEET WRITE FAILS. On Connect Cloud the
#' container's disk is thrown away on restart, so a fallback would thank someone
#' for a sign-up that is already gone. The form says it failed and keeps what
#' they typed, so trying again costs them one click. Same rule as
#' fw_submit_attempt() in R/submit.R.
#'
#' The log line names the tab and never the contents: the server log is not a
#' place for anyone's email address.
fw_store_row <- function(tab, record, columns) {
  cells <- fw_sheet_cells(fw_form_row(record, columns))
  ok <- tryCatch({
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
fw_form_row <- function(record, columns) {
  vapply(columns, function(col) {
    v <- record[[col]]
    if (is.null(v) || length(v) == 0 || is.na(v[1])) "" else as.character(v[1])
  }, character(1), USE.NAMES = FALSE)
}

#' Neutralise anything a spreadsheet would run as a formula
#'
#' RAW already stops the sheet itself from evaluating a cell. This is for the
#' NEXT step: someone downloads the tab as CSV and opens it in Excel, which
#' treats a cell starting with = + - or @ as a formula whatever Sheets did. A
#' leading apostrophe makes it text in both. Only those four, so an ordinary
#' name or message is stored exactly as typed.
fw_sheet_cells <- function(x) {
  x <- as.character(x)
  risky <- !is.na(x) & grepl("^[=+@-]", x)
  x[risky] <- paste0("'", x[risky])
  x
}

# ---- Local mode --------------------------------------------------------------

#' Where local-mode rows land: dev/forms/, gitignored with the rest of dev/
fw_forms_dir <- function() file.path(FW_DEV_DIR, "forms")

#' Append one row to dev/forms/<tab>.csv, writing the header on first use
fw_forms_write_local <- function(tab, cells, columns) {
  dir <- fw_forms_dir()
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(tab, ".csv"))
  row <- as.data.frame(as.list(stats::setNames(cells, columns)),
                       stringsAsFactors = FALSE, check.names = FALSE)
  readr::write_csv(row, path, na = "", append = file.exists(path))
  path
}

# ---- Google: the key and the token -------------------------------------------

#' The service account key, parsed
#'
#' GS4_SA_KEY_B64 may hold the JSON itself or base64 of it: a console that
#' takes one line at a time mangles the key's embedded newlines, base64 has
#' none. Anything that starts with a brace is taken as JSON.
fw_google_key <- function(value = GS4_SA_KEY_B64) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) stop("GS4_SA_KEY_B64 is not set.", call. = FALSE)
  json <- if (startsWith(value, "{")) value
          else rawToChar(jsonlite::base64_dec(gsub("[[:space:]]", "", value)))
  key <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE),
                  error = function(e) NULL)
  need <- c("client_email", "private_key")
  if (is.null(key) || !all(need %in% names(key))) {
    stop("GS4_SA_KEY_B64 is not a service account JSON key (it needs ",
         "client_email and private_key).", call. = FALSE)
  }
  key$token_uri <- key$token_uri %||% "https://oauth2.googleapis.com/token"
  key
}

#' base64url without padding, the encoding a JWT uses
fw_b64url <- function(x) {
  if (is.character(x)) x <- charToRaw(enc2utf8(x))
  gsub("=+$", "", chartr("+/", "-_", openssl::base64_encode(x)))
}

#' A signed assertion asking Google for a Sheets token
#'
#' THE SCOPE GOES IN THE CLAIM. Google reads it from the assertion, not from
#' the token request - which is the one thing httr2's generic flow would have
#' put in the wrong place.
#'
#' @param key  from fw_google_key()
#' @param now  seconds since the epoch, for the tests
fw_google_jwt <- function(key, now = as.integer(Sys.time())) {
  header <- jsonlite::toJSON(list(alg = "RS256", typ = "JWT"), auto_unbox = TRUE)
  claim <- jsonlite::toJSON(list(
    iss = key$client_email, scope = FW_SHEETS_SCOPE, aud = key$token_uri,
    iat = now, exp = now + 3600L
  ), auto_unbox = TRUE)
  signing_input <- paste0(fw_b64url(header), ".", fw_b64url(claim))
  sig <- openssl::signature_create(charToRaw(signing_input), hash = openssl::sha256,
                                   key = openssl::read_key(key$private_key))
  paste0(signing_input, ".", fw_b64url(sig))
}

# The token, held for the life of the process. Google issues it for an hour;
# it is renewed a minute early so a request never goes out with one that
# expires on the way.
FW_GOOGLE_TOKEN <- new.env(parent = emptyenv())

fw_google_token <- function() {
  now <- as.integer(Sys.time())
  if (!is.null(FW_GOOGLE_TOKEN$value) && FW_GOOGLE_TOKEN$expires > now + 60L) {
    return(FW_GOOGLE_TOKEN$value)
  }
  key <- fw_google_key()
  resp <- httr2::request(key$token_uri) |>
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
  body <- httr2::resp_body_json(resp)
  FW_GOOGLE_TOKEN$value <- body$access_token
  FW_GOOGLE_TOKEN$expires <- now + as.integer(body$expires_in %||% 3600L)
  FW_GOOGLE_TOKEN$value
}

# ---- Google: the append ------------------------------------------------------

#' The append request, built but not sent
#'
#' Separate from fw_sheet_append() so dev/plan_test.R can check the URL, the
#' options and the body without a network or a key.
#'
#' @param cells one row as a character vector, or several as a list of them
fw_sheet_request <- function(tab, cells, token, sheet_id = FWISE_FORMS_SHEET_ID) {
  rows <- if (is.list(cells)) cells else list(cells)
  range <- utils::URLencode(paste0(tab, "!A1"), reserved = TRUE)
  httr2::request(sprintf("%s/%s/values/%s:append", FW_SHEETS_API, sheet_id, range)) |>
    httr2::req_url_query(valueInputOption = "RAW", insertDataOption = "INSERT_ROWS") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(values = lapply(rows, function(r) as.list(unname(r))))) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S) |>
    # Only 429 and 503 are retried - answers that mean nothing was written. A
    # timeout is NOT retried: the row may have landed, and a retry would write
    # it twice.
    httr2::req_retry(max_tries = 2)
}

#' Append one row (or a list of rows) to a tab
fw_sheet_append <- function(tab, cells, sheet_id = FWISE_FORMS_SHEET_ID) {
  resp <- httr2::req_perform(fw_sheet_request(tab, cells, fw_google_token(), sheet_id))
  status <- httr2::resp_status(resp)
  if (status == 200) return(invisible(TRUE))
  stop(fw_sheets_message(tab, status), call. = FALSE)
}

#' What a failed append means, in words someone can act on in the server log
fw_sheets_message <- function(tab, status) {
  hint <- switch(
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
#' FORMATTED_VALUE, so the script reads exactly the text the reviewer sees in
#' the cell. The inbox tabs are formatted as plain text (dev/check_sheets.R
#' --setup-inbox), so that text is what was typed or appended.
fw_sheet_read_request <- function(tab, token, sheet_id) {
  range <- utils::URLencode(tab, reserved = TRUE)
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
fw_sheet_update_request <- function(updates, token, sheet_id) {
  data <- lapply(updates, function(u) list(range = u$range, values = list(list(u$value))))
  httr2::request(sprintf("%s/%s/values:batchUpdate", FW_SHEETS_API, sheet_id)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(valueInputOption = "RAW", data = data)) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_SHEETS_TIMEOUT_S)
}

#' A tab's rows as a data frame of text, keyed by header
#'
#' Sheets drops trailing empty cells from each row, so short rows are padded.
#' Blank cells come back as NA. `sheet_row` is the row's number in the sheet
#' AT THE TIME OF READING, for the reviewer to find it; nothing writes by it.
fw_sheet_values_frame <- function(values) {
  if (!length(values)) return(data.frame(sheet_row = integer(0)))
  header <- trimws(unlist(values[[1]]))
  body <- values[-1]
  cols <- lapply(seq_along(header), function(j) vapply(body, function(r) {
    v <- if (length(r) >= j) as.character(r[[j]]) else ""
    v <- trimws(v)
    if (nzchar(v)) v else NA_character_
  }, character(1)))
  names(cols) <- header
  out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  out <- out[, nzchar(names(out)), drop = FALSE]
  out$sheet_row <- seq_len(nrow(out)) + 1L
  out
}

#' Read a whole tab of the sheet
fw_sheet_read <- function(tab, sheet_id) {
  resp <- httr2::req_perform(fw_sheet_read_request(tab, fw_google_token(), sheet_id))
  status <- httr2::resp_status(resp)
  if (status != 200) stop(fw_sheets_message(tab, status), call. = FALSE)
  fw_sheet_values_frame(httr2::resp_body_json(resp)$values %||% list())
}

#' A column number as the sheet's letters: 1 -> A, 27 -> AA
fw_sheet_col_letter <- function(n) {
  out <- character(0)
  while (n > 0) {
    r <- (n - 1) %% 26
    out <- c(LETTERS[r + 1], out)
    n <- (n - 1) %/% 26
  }
  paste(out, collapse = "")
}

#' The cell ranges for one column of the rows holding the given ids
#'
#' Read straight before the write, so the rows are where they are NOW.
fw_sheet_cell_ranges <- function(tab, frame, attempt_ids, column) {
  j <- match(column, setdiff(names(frame), "sheet_row"))
  if (is.na(j)) stop("Tab '", tab, "' has no column '", column, "'.", call. = FALSE)
  rows <- frame$sheet_row[match(attempt_ids, frame$attempt_id)]
  if (anyNA(rows)) stop("Not in tab '", tab, "': ",
                        paste(attempt_ids[is.na(rows)], collapse = ", "), call. = FALSE)
  paste0(tab, "!", fw_sheet_col_letter(j), rows)
}

# ---- The submissions inbox ---------------------------------------------------

FW_INBOX_TABS <- list(submissions = "submissions", new_species = "new_species")

#' "sheets" when the inbox sheet and the key are both set, "local" otherwise
fw_inbox_mode <- function() {
  if (!is.null(FWISE_INBOX_SHEET_ID) && !is.null(GS4_SA_KEY_B64)) "sheets" else "local"
}

#' The local stand-in for an inbox tab: dev/forms/<tab>.csv
fw_inbox_local_path <- function(tab) file.path(fw_forms_dir(), paste0(tab, ".csv"))

#' Read an inbox tab, every column text, blanks as NA, plus sheet_row
fw_inbox_read <- function(tab) {
  if (fw_inbox_mode() == "sheets") return(fw_sheet_read(tab, FWISE_INBOX_SHEET_ID))
  path <- fw_inbox_local_path(tab)
  if (!file.exists(path)) return(data.frame(sheet_row = integer(0)))
  d <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                       na.strings = "", encoding = "UTF-8")
  d[] <- lapply(d, function(x) { x <- trimws(x); x[!is.na(x) & !nzchar(x)] <- NA; x })
  d$sheet_row <- seq_len(nrow(d)) + 1L
  d
}

#' Append rows to an inbox tab
#'
#' NO fw_sheet_cells() HERE. Its apostrophe guards a CSV someone opens in
#' Excel; the inbox is read over the API, and the apostrophe would turn a
#' latitude of -31.24 into the text '-31.24.
#'
#' @param rows a list of character vectors, each in the tab's column order
#' @param columns the tab's columns, for the local file's header
fw_inbox_append <- function(tab, rows, columns) {
  if (!length(rows)) return(invisible(TRUE))
  rows <- lapply(rows, function(r) { r[is.na(r)] <- ""; as.character(r) })
  if (fw_inbox_mode() == "sheets") return(fw_sheet_append(tab, rows, FWISE_INBOX_SHEET_ID))
  for (r in rows) fw_forms_write_local(tab, r, columns)
  invisible(TRUE)
}

#' Write one column of the rows holding the given attempt ids
#'
#' @param values one per id, or one for all
fw_inbox_set <- function(tab, attempt_ids, column, values) {
  if (!length(attempt_ids)) return(invisible(TRUE))
  values <- rep_len(as.character(values), length(attempt_ids))
  if (fw_inbox_mode() == "sheets") {
    frame <- fw_sheet_read(tab, FWISE_INBOX_SHEET_ID)
    ranges <- fw_sheet_cell_ranges(tab, frame, attempt_ids, column)
    updates <- Map(function(r, v) list(range = r, value = v), ranges, values)
    resp <- httr2::req_perform(fw_sheet_update_request(unname(updates), fw_google_token(),
                                                       FWISE_INBOX_SHEET_ID))
    status <- httr2::resp_status(resp)
    if (status != 200) stop(fw_sheets_message(tab, status), call. = FALSE)
    return(invisible(TRUE))
  }
  path <- fw_inbox_local_path(tab)
  d <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                       na.strings = "", encoding = "UTF-8")
  i <- match(attempt_ids, d$attempt_id)
  if (anyNA(i)) stop("Not in ", basename(path), ": ",
                     paste(attempt_ids[is.na(i)], collapse = ", "), call. = FALSE)
  d[[column]][i] <- values
  readr::write_csv(d, path, na = "")
  invisible(TRUE)
}
