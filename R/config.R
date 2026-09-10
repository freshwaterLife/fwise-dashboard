# config.R
# Central place for paths, environment variables and constants.
# If you need to change where data comes from or where submissions go, change it
# here rather than hunting through the modules.

# ---- Paths -------------------------------------------------------------------

# THE DATA LIVES IN A SEPARATE REPOSITORY. fwise-data/ is a sibling of this
# directory, not a subdirectory of it, so that the client can publish new data by
# pushing to that repository without touching, rebuilding or redeploying the app.
#
# Nothing in here should ever assume the two are checked out together beyond the
# development default below. In production the data arrives over the GitHub API
# instead, and submissions are written back the same way. See fw_data_mode() in
# data_load.R and R/github.R.

FW_ROOT       <- getwd()
FW_DEV_DIR    <- file.path(FW_ROOT, "dev")

# The development default: the sibling checkout. Overridden by FWISE_DATA_SOURCE.
FW_DATA_DIR   <- normalizePath(file.path(FW_ROOT, "..", "fwise-data"),
                               mustWork = FALSE)
FW_SCHEMA_DIR <- file.path(FW_DATA_DIR, "schema")

# The raw flat export from the client. data_prep.R reads this and never writes to
# it. Update the filename when a newly cleaned export is dropped in.
FW_SOURCE_CSV <- file.path(FW_DATA_DIR, "fwise_2026-09-06.csv")

# ---- Environment -------------------------------------------------------------

# Small helper so a missing or empty environment variable behaves the same way.
fw_env <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) default else value
}

# WHERE THE DATA COMES FROM.
#
# THE DATA REPOSITORY IS A CONSTANT, NOT A SETTING. It is private, it is not
# going to move, and its name is not a secret. Making it something the client has
# to type into a deployment console would add a way to get the app wrong without
# adding anything you can do with it.
FW_DATA_REPO <- "freshwaterLife/fwise-data"
FW_DATA_REF  <- "main"

# THE ONE SECRET, and the switch. Set FWISE_DATA_TOKEN and the app reads the
# repository above over the GitHub API and writes submissions back to it. Leave
# it unset and everything stays on the local sibling checkout, making no network
# calls at all - a developer on a train gets the same app as a developer at a
# desk, and a GitHub outage does not stop local work.
#
# It is a fine-grained personal access token with Contents: Read and write,
# resource owner freshwaterLife, scoped to that one repository. IT EXPIRES, and
# when it does the app stops reading data as well as accepting submissions.
FWISE_DATA_TOKEN <- fw_env("FWISE_DATA_TOKEN", default = NULL)

# THE OVERRIDE, and it is not needed in production. Set it to point a test deploy
# at a fork, a branch or a local path without touching code. Three shapes, all
# resolved by fw_data_mode() in data_load.R:
#
#   unset            the token decides - API if it is set, ../fwise-data/ if not
#   /some/path       that directory, read as files
#   https://...      that base URL, read over plain HTTPS with NO credential
#
# Point it at the directory holding metadata.json and schema/, not at schema/
# itself. A trailing slash is tolerated.
FWISE_DATA_SOURCE <- fw_env("FWISE_DATA_SOURCE", default = NULL)

# ---- Data visualisation palette ----------------------------------------------

# Wong (2011) colourblind-safe palette. The associated academic paper uses this,
# so the app must match it for figures to stay consistent across the paper, the
# app and exported reports.
#
# These are DATA colours. Never use them as interface chrome, and never use the
# interface teals to encode data.
FW_PALETTE <- c(
  successful = "#009E73",
  failed     = "#E69F00",
  ongoing    = "#56B4E9",
  unknown    = "#CC79A7",
  neutral    = "#999999",
  emphasis   = "#0072B2",
  vermilion  = "#D55E00"
)

# Outcome values as they appear in the data, mapped to palette keys. Keeping the
# mapping explicit means a renamed outcome category fails loudly here rather than
# silently falling through to grey.
FW_OUTCOME_COLOURS <- c(
  "Successful" = unname(FW_PALETTE["successful"]),
  "Failed"     = unname(FW_PALETTE["failed"]),
  "Ongoing"    = unname(FW_PALETTE["ongoing"]),
  "Unknown"    = unname(FW_PALETTE["unknown"])
)

# ---- Constants ---------------------------------------------------------------

# Coordinates are shown and stored at six decimal places, roughly 0.1m. More
# precision than that is false confidence for a treated waterbody.
FW_COORD_DP <- 6
