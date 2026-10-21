# tracking.R
# Usage tracking (details in TRACKING.md):
#   1. GoatCounter counts arrivals: one script in the page head.
#   2. A server-side event log, one row per event in the `events` tab.
#
# Events buffer per session and are written in one append at session end. The
# write blocks the process, so a short timeout and no retry. Failures are
# swallowed: tracking never affects a visitor.
#
# Blocker to ensure no personal data: details are built from checked inputs
# only. No IP, user agent, referrer, cookie or free text. The session id is
# Shiny's random token. Uses the forms' Google sign-in (forms_store.R).

# The allowed events. Add new ones here and in TRACKING.md.
FW_TRACK_EVENTS = c("session_start", "report_built", "download",
                     "contact_click", "form_submit", "session_end")

# The sheet tab. Row 1: timestamp, session_id, event, detail, app_version.
FW_TRACK_TAB = "events"

# Write timeout, shorter than the forms': nobody is waiting on it.
FW_TRACK_TIMEOUT_S = 5

# Max events buffered per session; extras are dropped.
FW_TRACK_MAX_EVENTS = 500L

# Source parameter, e.g. ?campaign=webinar (GoatCounter Campaigns). Not
# ?ref=, which GoatCounter ignores on its own.
FW_TRACK_SOURCE_PARAM = "campaign"

# ---- Mode --------------------------------------------------------------------

#' "sheet", "console" or "off"
#'
#' Off by default. "sheet" without the sheet id and key is off too.
fw_track_mode = function(mode = FWISE_LOG_MODE, sheet_id = FWISE_LOG_SHEET_ID,
                          key = GS4_SA_KEY_B64) {
  attr(fw_track_mode_why(mode, sheet_id, key), "mode")
}

#' The mode, and in words why, for the startup line
#'
#' Names the missing setting, never its value. Tolerates pasted quotes.
fw_track_mode_why = function(mode = FWISE_LOG_MODE, sheet_id = FWISE_LOG_SHEET_ID,
                              key = GS4_SA_KEY_B64) {
  say = function(m, why) structure(if (nzchar(why)) paste0(m, " (", why, ")") else m,
                                    mode = m)
  if (is.null(mode)) return(say("off", "FWISE_LOG_MODE is not set"))
  m = tolower(gsub("^[\"' ]+|[\"' ]+$", "", mode))
  if (!m %in% c("sheet", "console", "off")) {
    # Never echo the value (it could be a misplaced key).
    return(say("off", "FWISE_LOG_MODE must be sheet, console or off"))
  }
  if (m == "sheet" && is.null(sheet_id)) return(say("off", "FWISE_LOG_SHEET_ID is not set"))
  if (m == "sheet" && is.null(key)) return(say("off", "GS4_SA_KEY_B64 is not set"))
  say(m, "")
}

# ---- GoatCounter -------------------------------------------------------------

#' The GoatCounter script, or NULL with no code. count.js skips localhost.
fw_goatcounter_tag = function(code = FW_GOATCOUNTER_CODE) {
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

#' Where the visitor came from, from ?campaign=
#'
#' Lowercased; letters, digits and hyphens, max 30. "direct" if none,
#' "other" if invalid.
#'
#' @param url_search the page's query string, e.g. "?campaign=webinar"
fw_track_source = function(url_search) {
  q = shiny::parseQueryString(url_search %||% "")
  v = q[[FW_TRACK_SOURCE_PARAM]]
  if (is.null(v) || !nzchar(v[1])) return("direct")
  v = tolower(v[1])
  if (grepl("^[a-z0-9-]{1,30}$", v)) v else "other"
}

#' The report builder's filters, checked against what the page offered
#'
#' Multi-selects are arrays (I(), so jsonlite doesn't unbox); unoffered
#' values are dropped. Sliders kept only if numeric and in range.
#'
#' @param f   fw_filter_state() output
#' @param ids the page's filter ids
#' @param ch  fw_filter_choices(data)
#' @return a named list ready for jsonlite
fw_track_filters = function(f, ids, ch) {
  out = list()
  for (id in ids) {
    if (!identical(FW_FILTERS[[id]]$kind, "multi")) next
    v = as.character(f[[id]] %||% character(0))
    out[[id]] = I(v[v %in% unname(ch[[id]])])
  }

  in_range = function(x, lo, hi) {
    is.numeric(x) && length(x) == 2 && !anyNA(x) &&
      is.numeric(c(lo, hi)) && length(c(lo, hi)) == 2 &&
      all(x >= min(lo, hi) - 1e-9 & x <= max(lo, hi) + 1e-9)
  }

  if ("years" %in% ids) {
    yrs = suppressWarnings(as.numeric(c(f[["year_from"]] %||% NA, f[["year_to"]] %||% NA)))
    if (in_range(yrs, ch$year_min, ch$year_max)) {
      out$year_from = as.integer(yrs[1])
      out$year_to = as.integer(yrs[2])
    }
    out$include_no_year = isTRUE(f[["include_no_year"]])
  }

  if ("size" %in% ids) {
    for (unit in FW_SIZE_UNITS) {
      v = f[[paste0("size_", unit)]]
      full = f[[paste0("size_full_", unit)]]
      if (!is.null(v) && in_range(as.numeric(v), full[1], full[2])) {
        out[[paste0("size_", unit)]] = round(as.numeric(v), 2)
      }
    }
    out$include_no_size = isTRUE(f[["include_no_size"]])
  }
  out
}

#' The download event's detail
#'
#' @param parts what the reader ticked; read through fw_bundle_parts(), the
#'   same function that decides what the file holds
#' @param n the number of records in the report
fw_track_download_detail = function(parts, n) {
  parts = fw_bundle_parts(parts)
  type = if (length(parts) > 1) "zip"
          else unname(c(pdf = "pdf", records = "html", xlsx = "xlsx")[parts])
  list(type = type, parts = I(parts), n_records = as.integer(n))
}

# ---- Rows --------------------------------------------------------------------

#' One event as a row, in the tab's column order
#'
#' Empty detail is {} so every detail parses as an object.
fw_track_row = function(session_id, event, detail = list(), now = Sys.time()) {
  stopifnot(length(event) == 1, event %in% FW_TRACK_EVENTS)
  if (length(detail) == 0) detail = stats::setNames(list(), character(0))
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
#' Separate so dev/plan_test.R can check it offline. No retry.
fw_track_request = function(rows, token, sheet_id = FWISE_LOG_SHEET_ID) {
  range = utils::URLencode(paste0(FW_TRACK_TAB, "!A1"), reserved = TRUE)
  values = lapply(rows, function(r) as.list(unname(fw_sheet_cells(r))))
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
fw_tracker = function(session_id, mode = fw_track_mode()) {
  buf = new.env(parent = emptyenv())
  buf$rows = list()
  buf$flushed = FALSE

  log = function(event, detail = list()) {
    if (mode == "off" || buf$flushed) return(invisible(NULL))
    if (length(buf$rows) >= FW_TRACK_MAX_EVENTS) return(invisible(NULL))
    buf$rows[[length(buf$rows) + 1L]] = fw_track_row(session_id, event, detail)
    invisible(NULL)
  }

  # Empty the buffer first, so a second call writes nothing.
  flush = function() {
    if (mode == "off" || buf$flushed) return(invisible(NULL))
    buf$flushed = TRUE
    rows = buf$rows
    buf$rows = list()
    if (!length(rows)) return(invisible(NULL))
    if (mode == "console") {
      for (r in rows) message("FWISE event: ", paste(r, collapse = " | "))
      return(invisible(NULL))
    }
    resp = httr2::req_perform(fw_track_request(rows, fw_google_token()))
    status = httr2::resp_status(resp)
    # Log the status only, never row contents.
    if (status != 200) message("FWISE event log: write dropped (HTTP ", status, ")")
    invisible(NULL)
  }

  list(log = fw_track_quiet(log), flush = fw_track_quiet(flush))
}

#' Wrap a function so nothing it does can reach the visitor
#'
#' Errors and warnings end it quietly, logging only the condition class.
fw_track_quiet = function(fn) {
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
#' Logs session_start and registers the session_end write. The logger is in
#' session$userData, shared with module sessions.
fw_track_session = function(session) {
  tryCatch({
    tracker = fw_tracker(session$token)
    session$userData$fw_tracker = tracker
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
fw_track = function(session, event, detail = list()) {
  tryCatch({
    tracker = session$userData$fw_tracker
    if (!is.null(tracker)) tracker$log(event, detail)
  }, error = function(e) NULL)
  invisible(NULL)
}
