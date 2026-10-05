# tracking.R
# Usage tracking, in two layers. What is recorded and why is in TRACKING.md.
#
#   1. GoatCounter counts ARRIVALS: one script in the page head, nothing more.
#      No click tracking and no calls from the app's own JavaScript.
#   2. A SERVER-SIDE EVENT LOG for actions inside the app, one row per event
#      in the `events` tab of a Google Sheet. The funders' figures come from
#      this.
#
# BUFFERED PER SESSION, WRITTEN ONCE. Events collect in memory and go to the
# sheet in one append when the session ends. The write is synchronous, so it
# holds up every other session on the same process for as long as it takes;
# hence a short timeout, no retry and no sleep. A row lost to a failure costs
# a figure, a retry loop costs every visitor on the process.
#
# NOTHING HERE MAY REACH A VISITOR. Every public function is wrapped so that a
# failure is swallowed: the batch is dropped, as the forms write nothing when
# storage is unavailable.
#
# NO PERSONAL DATA, BY CONSTRUCTION. Each event's detail is built here from
# controlled inputs, checked against the values the page offered, and picked
# by name. No IP address, user agent, referrer URL, cookie or free text is ever
# read. The session id is Shiny's random per-session token.
#
# THE SHEET WRITE REUSES THE FORMS' SIGN-IN: fw_google_token(), FW_SHEETS_API
# and fw_sheet_cells() in R/forms_store.R. The one difference is the request:
# many rows, and no retry.

# The only events there are. Add a name here, and to TRACKING.md, before
# logging it.
FW_TRACK_EVENTS <- c("session_start", "report_built", "download",
                     "contact_click", "form_submit", "session_end")

# The tab and its columns. Row 1 of the tab holds exactly these headers.
FW_TRACK_TAB <- "events"
FW_TRACK_COLUMNS <- c("timestamp", "session_id", "event", "detail", "app_version")

# Seconds before the write is abandoned. Shorter than the forms' 10: a form
# write is someone waiting on their own answer, this is a session that has
# already gone.
FW_TRACK_TIMEOUT_S <- 5

# A ceiling on one session's buffer, so a page left building reports all day
# cannot grow the process's memory without limit. Events past it are dropped.
FW_TRACK_MAX_EVENTS <- 500L

# The query parameter carrying where a visitor came from (?ref=webinar).
# GoatCounter reads the same one as the visit's source.
FW_TRACK_SOURCE_PARAM <- "ref"

# ---- Mode --------------------------------------------------------------------

#' "sheet", "console" or "off"
#'
#' Off unless asked for, so a local run never writes to the live sheet. "sheet"
#' without both the sheet id and the key is off too, silently.
fw_track_mode <- function(mode = FWISE_LOG_MODE, sheet_id = FWISE_LOG_SHEET_ID,
                          key = GS4_SA_KEY_B64) {
  attr(fw_track_mode_why(mode, sheet_id, key), "mode")
}

#' The mode, and in words why, for the startup line
#'
#' Names which setting is missing or wrong, never a value other than the mode
#' itself, so a deployment that says "off" says why in the same line. Quotes
#' pasted around the mode in a console are tolerated.
fw_track_mode_why <- function(mode = FWISE_LOG_MODE, sheet_id = FWISE_LOG_SHEET_ID,
                              key = GS4_SA_KEY_B64) {
  say <- function(m, why) structure(if (nzchar(why)) paste0(m, " (", why, ")") else m,
                                    mode = m)
  if (is.null(mode)) return(say("off", "FWISE_LOG_MODE is not set"))
  m <- tolower(gsub("^[\"' ]+|[\"' ]+$", "", mode))
  if (!m %in% c("sheet", "console", "off")) {
    return(say("off", paste0("FWISE_LOG_MODE is '", substr(m, 1, 20),
                             "', not sheet, console or off")))
  }
  if (m == "sheet" && is.null(sheet_id)) return(say("off", "FWISE_LOG_SHEET_ID is not set"))
  if (m == "sheet" && is.null(key)) return(say("off", "GS4_SA_KEY_B64 is not set"))
  say(m, "")
}

# ---- GoatCounter -------------------------------------------------------------

#' The GoatCounter script for the page head, or NULL while no code is set
#'
#' count.js skips localhost on its own, so a local run never counts a visit.
fw_goatcounter_tag <- function(code = FW_GOATCOUNTER_CODE) {
  if (is.null(code) || !nzchar(code) || identical(code, "GOATCOUNTER_CODE")) {
    return(NULL)
  }
  tags$script(
    `data-goatcounter` = sprintf("https://%s.goatcounter.com/count", code),
    async = NA,
    src = "//gc.zgo.at/count.js"
  )
}

# ---- Details -----------------------------------------------------------------

#' Where the visitor came from, from ?ref=
#'
#' Lowercased, then kept only if it is letters, digits and hyphens, 30 at
#' most. "direct" when there is none; "other" when there is one that fails.
#'
#' @param url_search the page's query string, e.g. "?ref=webinar"
fw_track_source <- function(url_search) {
  q <- shiny::parseQueryString(url_search %||% "")
  v <- q[[FW_TRACK_SOURCE_PARAM]]
  if (is.null(v) || !nzchar(v[1])) return("direct")
  v <- tolower(v[1])
  if (grepl("^[a-z0-9-]{1,30}$", v)) v else "other"
}

#' The report builder's filters, checked against what the page offered
#'
#' EVERY MULTI-SELECT IS AN ARRAY, wrapped in I() so jsonlite never unboxes a
#' single choice to a bare string, and empty when nothing was picked. A value
#' the picker did not offer is dropped. Sliders are kept only while numeric
#' and inside their own range.
#'
#' @param f   fw_filter_state() output
#' @param ids the page's filter ids
#' @param ch  fw_filter_choices(data)
#' @return a named list ready for jsonlite
fw_track_filters <- function(f, ids, ch) {
  out <- list()
  for (id in ids) {
    if (!identical(FW_FILTERS[[id]]$kind, "multi")) next
    v <- as.character(f[[id]] %||% character(0))
    out[[id]] <- I(v[v %in% unname(ch[[id]])])
  }

  in_range <- function(x, lo, hi) {
    is.numeric(x) && length(x) == 2 && !anyNA(x) &&
      is.numeric(c(lo, hi)) && length(c(lo, hi)) == 2 &&
      all(x >= min(lo, hi) - 1e-9 & x <= max(lo, hi) + 1e-9)
  }

  if ("years" %in% ids) {
    yrs <- suppressWarnings(as.numeric(c(f[["year_from"]] %||% NA, f[["year_to"]] %||% NA)))
    if (in_range(yrs, ch$year_min, ch$year_max)) {
      out$year_from <- as.integer(yrs[1])
      out$year_to <- as.integer(yrs[2])
    }
    out$include_no_year <- isTRUE(f[["include_no_year"]])
  }

  if ("size" %in% ids) {
    for (unit in FW_SIZE_UNITS) {
      v <- f[[paste0("size_", unit)]]
      full <- f[[paste0("size_full_", unit)]]
      if (!is.null(v) && in_range(as.numeric(v), full[1], full[2])) {
        out[[paste0("size_", unit)]] <- round(as.numeric(v), 2)
      }
    }
    out$include_no_size <- isTRUE(f[["include_no_size"]])
  }
  out
}

#' The download event's detail
#'
#' @param parts what the reader ticked; read through fw_bundle_parts(), the
#'   same function that decides what the file holds
#' @param n the number of records in the report
fw_track_download_detail <- function(parts, n) {
  parts <- fw_bundle_parts(parts)
  type <- if (length(parts) > 1) "zip"
          else unname(c(pdf = "pdf", records = "html", xlsx = "xlsx")[parts])
  list(type = type, parts = I(parts), n_records = as.integer(n))
}

# ---- Rows --------------------------------------------------------------------

#' One event as a row in FW_TRACK_COLUMNS order
#'
#' An empty detail is written as {} rather than [], so every detail parses as
#' an object.
fw_track_row <- function(session_id, event, detail = list(), now = Sys.time()) {
  stopifnot(length(event) == 1, event %in% FW_TRACK_EVENTS)
  if (length(detail) == 0) detail <- stats::setNames(list(), character(0))
  c(
    format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    as.character(session_id),
    event,
    as.character(jsonlite::toJSON(detail, auto_unbox = TRUE, digits = NA)),
    FW_APP_VERSION
  )
}

#' The append request for a session's rows, built but not sent
#'
#' Separate so dev/plan_test.R can check it with no network or key. Unlike
#' fw_sheet_request() there is NO req_retry: the write blocks the process.
fw_track_request <- function(rows, token, sheet_id = FWISE_LOG_SHEET_ID) {
  range <- utils::URLencode(paste0(FW_TRACK_TAB, "!A1"), reserved = TRUE)
  values <- lapply(rows, function(r) as.list(unname(fw_sheet_cells(r))))
  httr2::request(sprintf("%s/%s/values/%s:append", FW_SHEETS_API, sheet_id, range)) |>
    httr2::req_url_query(valueInputOption = "RAW", insertDataOption = "INSERT_ROWS") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(values = values)) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(FW_TRACK_TIMEOUT_S)
}

# ---- The per-session logger ----------------------------------------------------

#' A logger for one session
#'
#' @param session_id the session's token
#' @param mode fw_track_mode()
#' @return list(log = function(event, detail), flush = function())
fw_tracker <- function(session_id, mode = fw_track_mode()) {
  buf <- new.env(parent = emptyenv())
  buf$rows <- list()
  buf$flushed <- FALSE

  log <- function(event, detail = list()) {
    if (mode == "off" || buf$flushed) return(invisible(NULL))
    if (length(buf$rows) >= FW_TRACK_MAX_EVENTS) return(invisible(NULL))
    buf$rows[[length(buf$rows) + 1L]] <- fw_track_row(session_id, event, detail)
    invisible(NULL)
  }

  # ONCE, and the buffer is emptied first, so a second call writes nothing.
  flush <- function() {
    if (mode == "off" || buf$flushed) return(invisible(NULL))
    buf$flushed <- TRUE
    rows <- buf$rows
    buf$rows <- list()
    if (!length(rows)) return(invisible(NULL))
    if (mode == "console") {
      for (r in rows) message("FWISE event: ", paste(r, collapse = " | "))
      return(invisible(NULL))
    }
    resp <- httr2::req_perform(fw_track_request(rows, fw_google_token()))
    status <- httr2::resp_status(resp)
    # The server log only, and the status only: never a row's contents.
    if (status != 200) message("FWISE event log: write dropped (HTTP ", status, ")")
    invisible(NULL)
  }

  list(log = fw_track_quiet(log), flush = fw_track_quiet(flush))
}

#' Wrap a function so nothing it does can reach the visitor
#'
#' Errors and warnings alike end it quietly. One line goes to the server log,
#' naming the condition's class only.
fw_track_quiet <- function(fn) {
  function(...) {
    tryCatch(
      fn(...),
      error = function(e) {
        message("FWISE event log: dropped (", class(e)[1], ")")
        invisible(NULL)
      },
      warning = function(w) {
        message("FWISE event log: dropped (", class(w)[1], ")")
        invisible(NULL)
      }
    )
  }
}

#' Start tracking a session: the call at the top of the server function
#'
#' Logs session_start and registers the session_end write. The logger lives in
#' session$userData, which module sessions share with the root one, so a
#' module logs with fw_track(session, ...) and needs no extra argument.
fw_track_session <- function(session) {
  tryCatch({
    tracker <- fw_tracker(session$token)
    session$userData$fw_tracker <- tracker
    tracker$log("session_start", list(
      source = fw_track_source(isolate(session$clientData$url_search))
    ))
    session$onSessionEnded(function() {
      tracker$log("session_end")
      tracker$flush()
    })
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Log one event for this session. Does nothing when tracking is off.
fw_track <- function(session, event, detail = list()) {
  tryCatch({
    tracker <- session$userData$fw_tracker
    if (!is.null(tracker)) tracker$log(event, detail)
  }, error = function(e) NULL)
  invisible(NULL)
}
