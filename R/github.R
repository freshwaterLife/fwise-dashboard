# github.R
# The GitHub API client. THE ONLY FILE THAT KNOWS HOW TO TALK TO GITHUB.
#
# It exists because the data repository is private. A raw.githubusercontent.com
# URL cannot carry a credential - the raw host answers with a redirect chain that
# drops the Authorization header. So reads go through the contents endpoint on
# api.github.com, which means one host, one credential and one place to look
# when something 404s.
#
# READ ONLY since Oct 2026: submissions go to the inbox sheet (forms_store.R),
# so nothing here writes to the repository any more.
#
# ONE SECRET. FWISE_DATA_TOKEN is the only thing the deployment sets. The
# repository and the branch are constants in config.R because neither is secret
# and neither varies. Leave the token unset and nothing in this file is ever
# called - see fw_data_mode() in data_load.R.

FW_GH_API = "https://api.github.com"

#' The contents endpoint for one path inside the data repository
#'
#' Paths here are always code-generated - a table name - so there is nothing
#' user-supplied to escape.
fw_gh_url = function(path) {
  sprintf("%s/repos/%s/contents/%s", FW_GH_API, FW_DATA_REPO, path)
}

#' A request carrying the token
#'
#' req_error() is disabled throughout. Every caller in this file inspects the
#' status itself, because a 404 means "not there" in some places and "the token
#' cannot see the repository" in others, and httr2's default error would collapse
#' that distinction into one unhelpful abort.
fw_gh_request = function(path, accept = "application/vnd.github+json") {
  httr2::request(fw_gh_url(path)) |>
    httr2::req_headers(
      Authorization = paste("Bearer", FWISE_DATA_TOKEN),
      Accept = accept,
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    # A TIMEOUT AND ONE RETRY. These run on the session's own thread, so a
    # GitHub call that hangs holds up every reader on the same process until
    # the proxy gives up on them - and they see "Disconnected from the server"
    # (client, 29 Sept 2026). Only transient failures are retried.
    httr2::req_timeout(FW_GH_TIMEOUT_S) |>
    httr2::req_retry(max_tries = 2)
}

# Seconds before a GitHub call is abandoned. See fw_gh_request().
FW_GH_TIMEOUT_S = 15

#' What a failed call actually means, in words someone can act on
#'
#' These messages end up in the Connect Cloud log, which is the only diagnostic
#' anyone deploying this will have. The 404 case earns its length: GitHub returns
#' 404 rather than 403 for a private repository a token cannot see, so the
#' obvious reading of it is the wrong one.
fw_gh_message = function(verb, path, status) {
  hint = switch(
    as.character(status),
    "401" = "FWISE_DATA_TOKEN is not a valid token, or it has expired.",
    "403" = paste("The token is valid but lacks permission. It needs Contents:",
                  "Read on the repository."),
    "404" = paste("Either the file is not there, or the token cannot see the",
                  "repository at all - GitHub answers 404 rather than 403 for a",
                  "private repository it will not admit exists. Check the token's",
                  "resource owner is the organisation rather than a personal",
                  "account, and that this repository is one of the selected ones."),
    paste0("Unexpected HTTP ", status, ".")
  )
  paste0("GitHub ", verb, " failed for ", FW_DATA_REPO, "/", path,
         " (HTTP ", status, "). ", hint)
}

#' Fetch one file, returning a local path, or NULL if it is not there
#'
#' The body is streamed straight to a temporary file rather than through memory,
#' and the temporary path is what callers hand to read_csv() or fromJSON(). That
#' keeps every reader in data_load.R identical across local and remote reads -
#' they all open a path.
fw_gh_download = function(path, ref = FW_DATA_REF) {
  dest = tempfile(fileext = paste0(".", tools::file_ext(path)))
  resp = fw_gh_request(path, accept = "application/vnd.github.raw") |>
    httr2::req_url_query(ref = ref) |>
    httr2::req_perform(path = dest)

  status = httr2::resp_status(resp)
  if (status == 404) return(NULL)
  if (status != 200) stop(fw_gh_message("read", path, status), call. = FALSE)
  dest
}
