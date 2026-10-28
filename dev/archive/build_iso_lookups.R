# build_iso_lookups.R
# ARCHIVED. Built fwise-data/lookup_iso3166.csv and lookup_iso3166_2.csv for
# the form's country and region pickers, dev/qa.R and fw_validate_geography().
# Rerun only if the ISO standard changes.
#
#     Rscript dev/archive/build_iso_lookups.R
#
# Sources (pinned below):
#   Countries    lukes/ISO-3166-Countries-with-Regional-Codes (public domain)
#   Subdivisions olahol/iso-3166-2.js via npm (MIT)

suppressMessages({
  library(dplyr)
  library(readr)
})

# brand.R first: config.R reads FW_COLOURS from it.
source("R/brand.R")
source("R/config.R")

FW_ISO_COUNTRIES_URL = paste0(
  "https://raw.githubusercontent.com/lukes/",
  "ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
)
FW_ISO_SUBDIV_TGZ = "https://registry.npmjs.org/iso-3166-2/-/iso-3166-2-1.0.0.tgz"

# ---- Countries ---------------------------------------------------------------

# Common names where they differ from ISO's ("United States"), matching the
# data. The ISO name is kept in country_iso_name.
FW_COUNTRY_ALIAS = c(
  "United States of America" = "United States",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "Russian Federation" = "Russia",
  "Korea, Republic of" = "South Korea",
  "Korea, Democratic People's Republic of" = "North Korea",
  "Iran, Islamic Republic of" = "Iran",
  "Syrian Arab Republic" = "Syria",
  "Tanzania, United Republic of" = "Tanzania",
  "Venezuela, Bolivarian Republic of" = "Venezuela",
  "Bolivia, Plurinational State of" = "Bolivia",
  "Viet Nam" = "Vietnam",
  "Lao People's Democratic Republic" = "Laos",
  "Moldova, Republic of" = "Moldova",
  "Taiwan, Province of China" = "Taiwan",
  "Micronesia, Federated States of" = "Micronesia",
  "Congo, Democratic Republic of the" = "Democratic Republic of the Congo",
  "Brunei Darussalam" = "Brunei",
  "Cabo Verde" = "Cape Verde",
  "Czechia" = "Czechia",
  "Turkiye" = "Turkey",
  "Palestine, State of" = "Palestine",
  "Netherlands, Kingdom of the" = "Netherlands"
)

message("Fetching ISO 3166-1 country list")
countries = read_csv(FW_ISO_COUNTRIES_URL, show_col_types = FALSE,
                      progress = FALSE) |>
  transmute(
    country_iso_name = name,
    country   = ifelse(name %in% names(FW_COUNTRY_ALIAS),
                       unname(FW_COUNTRY_ALIAS[name]), name),
    iso2      = `alpha-2`,
    iso3      = `alpha-3`,
    # North/South America come from `intermediate-region`, not `sub-region`
    # (which lumps everything south of the US together).
    continent = case_when(
      `sub-region` == "Northern America" ~ "North America",
      `intermediate-region` %in% c("Central America", "Caribbean") ~ "North America",
      `intermediate-region` == "South America" ~ "South America",
      region == "Africa"   ~ "Africa",
      region == "Asia"     ~ "Asia",
      region == "Europe"   ~ "Europe",
      region == "Oceania"  ~ "Oceania",
      name == "Antarctica" ~ "Antarctica",
      # The UN dataset gives Taiwan no region at all.
      name == "Taiwan, Province of China" ~ "Asia",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(iso3)) |>
  select(country, country_iso_name, iso2, iso3, continent) |>
  arrange(country)

stopifnot(nrow(countries) > 240, !any(duplicated(countries$iso3)))
# Every country gets a continent, or the import would write NA into a row
# and the app would refuse it at the next load.
if (any(is.na(countries$continent))) {
  stop("No continent derived for: ",
       paste(countries$country[is.na(countries$continent)], collapse = ", "),
       call. = FALSE)
}

# ---- Subdivisions ------------------------------------------------------------
#
# The package ships its data as a JavaScript object literal rather than JSON -
# unquoted keys - so it is converted here rather than parsed directly.

message("Fetching ISO 3166-2 subdivisions")
tmp = tempfile(fileext = ".tgz")
utils::download.file(FW_ISO_SUBDIV_TGZ, tmp, quiet = TRUE, mode = "wb")
exdir = tempfile()
utils::untar(tmp, exdir = exdir)

js = readLines(file.path(exdir, "package", "iso3166.min.js"), warn = FALSE)
js = paste(js, collapse = "\n")

start = regexpr("var data=", js, fixed = TRUE) + nchar("var data=")
depth = 0L
end = NA_integer_
chars = strsplit(substring(js, start), "")[[1]]
for (i in seq_along(chars)) {
  if (chars[i] == "{") depth = depth + 1L
  else if (chars[i] == "}") {
    depth = depth - 1L
    if (depth == 0L) { end = i; break }
  }
}
literal = paste(chars[seq_len(end)], collapse = "")

# Quote the bare keys, skipping anything inside a string literal - a naive
# regular expression corrupts subdivision names that contain a comma or a colon.
quote_keys = function(x) {
  ch = strsplit(x, "")[[1]]
  out = character(0)
  i = 1L; n = length(ch); in_str = FALSE; q = ""
  while (i <= n) {
    c = ch[i]
    if (in_str) {
      out = c(out, c)
      if (c == "\\") { out = c(out, ch[i + 1L]); i = i + 2L; next }
      if (c == q) in_str = FALSE
      i = i + 1L; next
    }
    if (c == '"' || c == "'") {
      in_str = TRUE; q = c; out = c(out, '"'); i = i + 1L; next
    }
    if (c == "{" || c == ",") {
      out = c(out, c); i = i + 1L
      rest = paste(ch[i:min(n, i + 60L)], collapse = "")
      m = regmatches(rest, regexec("^\\s*([A-Za-z_$][A-Za-z0-9_$]*)\\s*:", rest))[[1]]
      if (length(m) == 2) {
        out = c(out, paste0('"', m[2], '":'))
        i = i + nchar(m[1])
      }
      next
    }
    out = c(out, c); i = i + 1L
  }
  paste(out, collapse = "")
}

data = jsonlite::fromJSON(quote_keys(literal), simplifyVector = FALSE)

subdivisions = bind_rows(lapply(names(data), function(iso2) {
  subs = data[[iso2]]$sub
  if (is.null(subs) || !length(subs)) return(NULL)
  tibble(
    iso2 = iso2,
    subdivision_code = names(subs),
    subdivision_name = vapply(subs, function(x) x$name %||% NA_character_, character(1)),
    subdivision_type = vapply(subs, function(x) x$type %||% NA_character_, character(1))
  )
})) |>
  left_join(select(countries, iso2, iso3), by = "iso2") |>
  filter(!is.na(iso3), !is.na(subdivision_name)) |>
  select(iso3, iso2, subdivision_code, subdivision_name, subdivision_type) |>
  arrange(iso3, subdivision_name)

`%||%` = function(x, y) if (is.null(x) || length(x) == 0) y else x

stopifnot(nrow(subdivisions) > 4000)

# ---- Write -------------------------------------------------------------------

write_csv(countries, file.path(FW_DATA_DIR, "lookup_iso3166.csv"), na = "")
write_csv(subdivisions, file.path(FW_DATA_DIR, "lookup_iso3166_2.csv"), na = "")

message("Written to ", FW_DATA_DIR)
message("  lookup_iso3166.csv    ", nrow(countries), " countries")
message("  lookup_iso3166_2.csv  ", nrow(subdivisions), " subdivisions across ",
        n_distinct(subdivisions$iso3), " countries")

# The countries FWISE already holds attempts for must all be present, or the
# contribute form would offer a list that cannot represent the existing data.
attempts = FW_ATTEMPTS_CSV
if (file.exists(attempts)) {
  have = unique(read_csv(attempts, show_col_types = FALSE, progress = FALSE)$country)
  missing = setdiff(have, countries$country)
  if (length(missing)) {
    warning("Countries in the data that are not in the ISO list: ",
            paste(missing, collapse = ", "),
            "\n  Add an alias to FW_COUNTRY_ALIAS above so the two agree.",
            call. = FALSE)
  } else {
    message("  all ", length(have), " countries in the data are in the ISO list")
  }
}
