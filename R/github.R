# github.R
# Read-only GitHub API client for the private data repo, via the contents
# endpoint (raw.githubusercontent.com drops the auth header on redirect).
# Only used when FWISE_DATA_TOKEN is set (see fw_data_mode()).

FW_GH_API = "https://api.github.com"

#' The contents endpoint for one path inside the data repository
#'
#' Paths are code-generated, so nothing to escape.
fw_gh_url = function(path) {
  sprintf("%s/repos/%s/contents/%s", FW_GH_API, FW_DATA_REPO, path)
}

#' A request carrying the token
#'
#' req_error() is off: callers read the status themselves, since a 404 can
#' mean "missing" or "token can't see the repo".
fw_gh_request = function(path, accept = "application/vnd.github+json") {
  httr2::request(fw_gh_url(path)) |>
    httr2::req_headers(
      Authorization = paste("Bearer", FWISE_DATA_TOKEN),
      Accept = accept,
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    httr2::req_user_agent("fwise-dashboard") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    # Timeout and one retry on transient failures: a hung call blocks every
    # session on the process.
    httr2::req_timeout(FW_GH_TIMEOUT_S) |>
    httr2::req_retry(max_tries = 2)
}

# Seconds before a GitHub call is abandoned. See fw_gh_request().
FW_GH_TIMEOUT_S = 15

#' What a failed call actually means, in words someone can act on
#'
#' For the Connect Cloud log. GitHub returns 404, not 403, for a private repo
#' the token can't see.
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
#' Streamed to a temp file, so every reader just opens a path.
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
