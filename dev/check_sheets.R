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

library(shiny)
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)

if (fw_forms_mode() != "sheets") {
  stop("Set FWISE_FORMS_SHEET_ID and GS4_SA_KEY_B64 first; see .Renviron.example.",
       call. = FALSE)
}

key <- fw_google_key()
cat("Service account: ", key$client_email, "\n", sep = "")
cat("(the sheet must be shared with that address as an Editor)\n\n")
token <- fw_google_token()

write_headers <- "--write-headers" %in% commandArgs(trailingOnly = TRUE)

problems <- 0L
check_tab <- function(tab, want) {
  range <- utils::URLencode(paste0(tab, "!1:1"), reserved = TRUE)
  resp <- httr2::request(sprintf("%s/%s/values/%s", FW_SHEETS_API, FWISE_FORMS_SHEET_ID, range)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status != 200) {
    cat("  ", tab, ": ", fw_sheets_message(tab, status), "\n", sep = "")
    problems <<- problems + 1L
    return(invisible())
  }
  got <- unlist(httr2::resp_body_json(resp)$values[[1]] %||% list())
  if (!length(got) && write_headers) {
    put <- httr2::request(sprintf("%s/%s/values/%s", FW_SHEETS_API, FWISE_FORMS_SHEET_ID,
                                  utils::URLencode(paste0(tab, "!A1"), reserved = TRUE))) |>
      httr2::req_url_query(valueInputOption = "RAW") |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_method("PUT") |>
      httr2::req_body_json(list(values = list(as.list(want)))) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(put) == 200) {
      cat("  ", tab, ": headers written\n", sep = "")
      got <- want
    } else {
      cat("  ", tab, ": could not write headers (HTTP ", httr2::resp_status(put), ")\n", sep = "")
    }
  }
  head <- got[seq_len(min(length(got), length(want)))]
  if (identical(head, want)) {
    extra <- setdiff(got, want)
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

cat("\n")
if (problems > 0L) stop(problems, " tab(s) need attention", call. = FALSE)
cat("The sheet is ready.\n")
