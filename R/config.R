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
# development default below. In production the data arrives over HTTPS instead.
# See fw_data_source() in data_load.R.

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

# WHERE THE DATA COMES FROM. One variable, two modes, resolved by
# fw_data_source() in data_load.R:
#
#   unset            ../fwise-data/ - the sibling checkout, for development
#   https://...      a raw GitHub base URL, for Posit Connect Cloud
#
# Local development therefore makes NO network calls. That is deliberate: a
# developer on a train should get the same app as a developer at a desk, and a
# GitHub outage should not stop local work.
#
# Point the remote value at the directory holding metadata.json and schema/, not
# at schema/ itself. A trailing slash is tolerated.
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
