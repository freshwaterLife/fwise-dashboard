# build_from_export.R
# ARCHIVED, pre-launch only. Rebuilt the data from the client's export in
# fwise-data/source/: attempts.csv, species.csv, contacts.csv, metadata.json
# and the row-to-id map. After launch ids are permanent, so don't rerun it;
# data changes go through dev/qa.R.
#
#     Rscript dev/archive/build_from_export.R
#
# Then dev/reconcile_source.R and dev/fetch_species_images.R.
#
# Carries over, by exact name: species ids, taxa, family, IUCN and photos;
# contact ids (by name + organisation; redact_email becomes contact_public
# FALSE). Unmatched species get new ids and are listed for checking. Uncited
# species and contacts are dropped.
#
# Transformations: trim, \r\n to \n, blank to absent; float noise to 15
# significant digits; "Country (Region)" split (ISO territories are countries);
# water_regime corrected by FW_REGIME_BY_TYPE.

suppressPackageStartupMessages({ library(readr) })
source(file.path("R", "brand.R")); source(file.path("R", "config.R"))
source(file.path("R", "data_load.R")); source(file.path("R", "submit.R"))

today  = format(Sys.Date())
QA_DIR = file.path(FW_DATA_DIR, "qa")
src_dir = file.path(FW_DATA_DIR, "source")

# ---- Read ----------------------------------------------------------------------
src_path = list.files(src_dir, pattern = "^fwise_.*[0-9]\\.(csv|xlsx)$", full.names = TRUE)
if (length(src_path) != 1) stop("Expected exactly one export in ", src_dir, ", found: ",
                                paste(basename(src_path), collapse = ", "), call. = FALSE)
export_date = sub("^fwise_(.*)\\.(csv|xlsx)$", "\\1", basename(src_path))
ids_path = file.path(src_dir, sub("\\.(csv|xlsx)$", "_ids.csv", basename(src_path)))

# Gotcha: openxlsx leaves XML escapes ("&amp;") in inline-string cells.
xml_unescape = function(x) {
  if (!is.character(x)) return(x)
  ok = !is.na(x) & grepl("&#", x, fixed = TRUE)
  for (form in list(c("&#([0-9]+);", "10"), c("&#x([0-9a-fA-F]+);", "16"))) {
    hit = gregexpr(form[1], x[ok], perl = TRUE)
    regmatches(x[ok], hit) = lapply(regmatches(x[ok], hit), function(m)
      vapply(m, function(e) intToUtf8(strtoi(gsub("[&#x;]", "", e), as.integer(form[2]))), ""))
  }
  for (e in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&apos;", "'"), c("&amp;", "&")))
    x = gsub(e[1], e[2], x, fixed = TRUE)
  x
}

raw = if (grepl("\\.xlsx$", src_path)) {
  x = openxlsx::read.xlsx(src_path, sheet = 1, check.names = FALSE, na.strings = c("", "NA"))
  x[] = lapply(x, xml_unescape)
  x
} else {
  utils::read.csv(src_path, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, encoding = "UTF-8")
}

# Excel stores doubles, and a number in a text column arrives as its full
# binary expansion. 15 significant digits is what Excel itself displays.
fmt_num = function(v) vapply(v, function(z) if (is.na(z)) NA_character_ else
  format(z, digits = 15, scientific = FALSE, trim = TRUE, drop0trailing = TRUE), "")
NUM_RE = "^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$"
noisy  = function(x) !is.na(x) & grepl(NUM_RE, x) &
  (grepl("[eE]", x) | nchar(gsub("[^0-9]", "", sub("[eE].*$", "", x))) > 15)

tidied = list()
for (col in names(raw)) {
  x = raw[[col]]
  if (is.numeric(x)) { raw[[col]] = unname(fmt_num(x)); next }
  x = trimws(gsub("\r\n?", "\n", as.character(x)))
  x[!is.na(x) & x == ""] = NA
  hit = which(noisy(x))
  if (length(hit)) {
    new = unname(fmt_num(as.numeric(x[hit])))
    tidied[[col]] = data.frame(row = hit, from = x[hit], to = new)
    x[hit] = new
  }
  raw[[col]] = x
}
n = nrow(raw)

EXPECTED = c(
  "site_name", "country", "waterbody_type", "water_regime", "latitude", "longitude",
  "area_treated", "area_unit", "area_notes", paste0("invasive_species_", 1:8),
  "invasion_year", "start_year", "end_year", "duration_days", "eradication_or_control",
  "reason", paste0("beneficiary_species_", 1:8), "outcome",
  paste0(rep(c("method_", "method_notes_"), 3), rep(1:3, each = 2)),
  "method_description", "labour_person_days", "depth_m", "depth_notes", "volume_m3",
  "volume_notes", "max_flow_m3s", "water_temp_c", "water_temp_notes",
  "target_ingredient_basis", "toxin_conc_target_mg_l", "conc_target_notes",
  "toxin_conc_measured_mg_l", "conc_measured_notes", "neutralising_agent",
  "neutralising_notes", "verification_method", "verification_notes",
  paste0(rep(c("primary_contact_", "secondary_contact_"), each = 3), c("name", "email", "organisation")),
  "source", "sent", "reference", "reference_link", "redact_email"
)
missing = setdiff(EXPECTED, names(raw)); extra = setdiff(names(raw), EXPECTED)
if (length(missing) || length(extra)) stop(
  "The export's columns are not the ones this build knows.",
  if (length(missing)) paste0("\n  missing: ", paste(missing, collapse = ", ")),
  if (length(extra))   paste0("\n  new: ", paste(extra, collapse = ", ")),
  "\nA column with no home is how data goes missing: give it one here first.", call. = FALSE)

rd = function(p) utils::read.csv(p, colClasses = "character", na.strings = c("", "NA"),
                                  check.names = FALSE, encoding = "UTF-8")
old_sp = rd(FW_SPECIES_CSV); old_co = rd(FW_CONTACTS_CSV)
iso = rd(file.path(FW_DATA_DIR, "lookup_iso3166.csv"))
# Everything the build could not place or had to choose, one row per problem,
# written to qa/build_issues_<date>.csv for fixing in the export.
issues = data.frame(source_row = integer(0), issue = character(0), detail = character(0))
issue = function(rows, what, detail) issues <= rbind(issues, data.frame(
  source_row = as.integer(rows), issue = what, detail = detail, stringsAsFactors = FALSE))

# ---- Species -------------------------------------------------------------------
old_sp$label = fw_species_label(old_sp)$label
slot_cols = grep("^(invasive|beneficiary)_species_[0-9]$", names(raw), value = TRUE)
names_used = unique(na.omit(unlist(raw[, slot_cols], use.names = FALSE)))
new_names = setdiff(names_used, old_sp$label)

parse_name = function(x) {
  sci = regmatches(x, regexec("\\(([^()]*)\\)\\s*$", x))
  sci = vapply(sci, function(m) if (length(m) == 2) trimws(m[2]) else NA_character_, "")
  com = trimws(sub("\\s*\\(([^()]*)\\)\\s*$", "", x))
  com[com == ""] = NA
  list(common = com, sci = sci)
}
nm = parse_name(new_names)
added_sp = data.frame(
  species_id = fw_mint_ids("SP", length(new_names), exclude = old_sp$species_id),
  common_name = nm$common, scientific_name = nm$sci, stringsAsFactors = FALSE)
for (col in setdiff(FW_SPECIES_COLUMNS, names(added_sp))) added_sp[[col]] = rep(NA_character_, nrow(added_sp))

# SAME SCIENTIFIC NAME, SAME SPECIES. The export recases or renames some common
# names ("Silver Carp" -> "Silver carp", "Squawfish" -> "Northern pikeminnow").
# Where the binomial matches exactly one held species that the export no longer
# cites under its old name, that row carries over - id, taxa, family, photo -
# under the export's spelling.
free = old_sp[!old_sp$label %in% names_used & !is.na(old_sp$scientific_name), ]
renamed = data.frame(from = character(0), to = character(0))
for (i in seq_len(nrow(added_sp))) {
  j = which(tolower(free$scientific_name) == tolower(added_sp$scientific_name[i]))
  if (length(j) != 1) next
  keep = setdiff(FW_SPECIES_COLUMNS, c("common_name", "scientific_name"))
  added_sp[i, keep] = free[j, keep]
  renamed = rbind(renamed, data.frame(from = free$label[j], to = new_names[i]))
  free = free[-j, ]
}
added_sp$label = fw_species_label(added_sp)$label
stopifnot(identical(added_sp$label, new_names))  # the name round-trips exactly

species = rbind(old_sp[old_sp$label %in% names_used, c(FW_SPECIES_COLUMNS, "label")],
                 added_sp[, c(FW_SPECIES_COLUMNS, "label")])
sp_id = setNames(species$species_id, species$label)

pack_species = function(prefix) {
  cols = paste0(prefix, 1:8)
  apply(raw[, cols], 1, function(v) {
    v = v[!is.na(v)]
    if (!length(v)) NA_character_ else paste(unname(sp_id[v]), collapse = FW_MULTI_SEP)
  })
}

# ---- Contacts ------------------------------------------------------------------
# WHO A CONTACT IS. A person is their name and organisation. A slot with no
# name is the organisation itself (Alex, 24 Sept 2026: "they should sit as
# organisations"), with contact_name blank; with neither, the email is all
# there is to go on.
contact_key = function(name, org, email) {
  z = function(x) ifelse(is.na(x), "", x)
  ifelse(!is.na(name), paste(name, z(org), sep = "|"),
         ifelse(!is.na(org), paste0("org|", org),
                ifelse(!is.na(email), paste0("email|", tolower(email)), NA_character_)))
}
old_co$key = contact_key(old_co$contact_name, old_co$organisation, old_co$contact_email)
people = do.call(rbind, lapply(c("primary", "secondary"), function(p) data.frame(
  row = seq_len(n), role = p,
  name = raw[[paste0(p, "_contact_name")]], email = raw[[paste0(p, "_contact_email")]],
  org = raw[[paste0(p, "_contact_organisation")]], stringsAsFactors = FALSE)))
people$key = contact_key(people$name, people$org, people$email)
people = people[!is.na(people$key), ]
redacted = !is.na(raw$redact_email) & tolower(raw$redact_email) %in% c("yes", "true", "y", "1")

keys = unique(people$key)
contacts = do.call(rbind, lapply(keys, function(k) {
  p = people[people$key == k, ]
  emails = unique(na.omit(p$email))
  if (length(emails) > 1) issue(p$row[!is.na(p$email) & p$email != emails[1]],
                                "Contact has more than one email; the first was kept",
                                paste0(if (is.na(p$name[1])) p$org[1] else p$name[1], ": kept ", emails[1], ", dropped ",
                                       paste(emails[-1], collapse = ", ")))
  data.frame(key = k, contact_name = p$name[1], contact_email = emails[1] %||% NA_character_,
             organisation = p$org[1], contact_public = if (any(redacted[p$row])) "FALSE" else "TRUE",
             stringsAsFactors = FALSE)
}))
contacts$contact_id = old_co$contact_id[match(contacts$key, old_co$key)]
fresh = is.na(contacts$contact_id)
contacts$contact_id[fresh] = fw_mint_ids("CO", sum(fresh), exclude = old_co$contact_id)
co_id = setNames(contacts$contact_id, contacts$key)
contact_ref = function(p) {
  k = contact_key(raw[[paste0(p, "_contact_name")]], raw[[paste0(p, "_contact_organisation")]],
                   raw[[paste0(p, "_contact_email")]])
  ifelse(is.na(k), NA_character_, unname(co_id[k]))
}

# ---- Methods -------------------------------------------------------------------
methods = character(n); method_notes = character(n)
for (r in seq_len(n)) {
  m  = unlist(raw[r, paste0("method_", 1:3)]); nt = unlist(raw[r, paste0("method_notes_", 1:3)])
  keep = !is.na(m)
  if (!any(keep)) {
    # No method: any slot note is kept verbatim, as the migration did.
    methods[r] = NA; nt = nt[!is.na(nt)]
    method_notes[r] = if (length(nt)) paste(nt, collapse = FW_NOTES_SEP) else NA
    next
  }
  # A note in a slot with no method has no "Method: note" entry to live in.
  # Left out of the data and listed in the issues file (Alex, 24 Sept 2026):
  # the fix is a method in that slot of the export.
  for (k in which(!keep & !is.na(nt))) issue(r, "Method note with no method", paste0("slot ", k, ": ", nt[k]))
  methods[r] = paste(m[keep], collapse = FW_MULTI_SEP)
  method_notes[r] = if (all(is.na(nt[keep]))) NA else
    paste(paste0(m[keep], ": ", ifelse(is.na(nt[keep]), "", nt[keep])), collapse = FW_NOTES_SEP)
}
off_list = setdiff(unique(na.omit(unlist(raw[, paste0("method_", 1:3)]))), FW_METHODS$method_name)
for (m in off_list) issue(which(apply(raw[, paste0("method_", 1:3)], 1, function(v) m %in% v)),
                          "Method is not one of the seven in FW_METHODS; it loads as class 'other'", m)

# ---- Place ---------------------------------------------------------------------
paren   = regmatches(raw$country, regexec("^(.*?)\\s*\\(([^()]*)\\)\\s*$", raw$country))
base    = vapply(seq_len(n), function(i) if (length(paren[[i]]) == 3) paren[[i]][2] else raw$country[i], "")
inner   = vapply(paren, function(m) if (length(m) == 3) m[3] else NA_character_, "")
is_terr = !is.na(inner) & inner %in% iso$country
country = ifelse(is_terr, inner, base)
region  = ifelse(is_terr, NA_character_, inner)
unknown = which(!is.na(country) & !country %in% iso$country)
if (length(unknown)) issue(unknown, "Country not in lookup_iso3166.csv, so no iso3 or continent", country[unknown])

regime = ifelse(raw$waterbody_type %in% names(FW_REGIME_BY_TYPE),
                 unname(FW_REGIME_BY_TYPE[raw$waterbody_type]), raw$water_regime)
n_regime = sum(!((is.na(regime) & is.na(raw$water_regime)) | (!is.na(regime) & !is.na(raw$water_regime) & regime == raw$water_regime)))

# ---- Attempt ids ---------------------------------------------------------------
# Same export, same ids: a re-run reuses the map it wrote last time.
if (file.exists(ids_path) && nrow(prev = rd(ids_path)) == n) {
  attempt_id = prev$attempt_id[order(as.integer(prev$source_row))]
} else {
  attempt_id = fw_mint_ids("FW", n)
}

# ---- Assemble ------------------------------------------------------------------
same_name = intersect(names(raw), FW_ATTEMPT_COLUMNS)
attempts = data.frame(attempt_id = attempt_id, stringsAsFactors = FALSE)
for (col in FW_ATTEMPT_COLUMNS[-1]) attempts[[col]] = NA_character_
for (col in same_name) attempts[[col]] = raw[[col]]
attempts$status       = "approved"
attempts$last_updated = export_date
attempts$country = country; attempts$region = region
attempts$iso3      = iso$iso3[match(country, iso$country)]
attempts$continent = iso$continent[match(country, iso$country)]
attempts$water_regime = regime
attempts$driver = raw$reason
attempts$invasive_species    = pack_species("invasive_species_")
attempts$beneficiary_species = pack_species("beneficiary_species_")
attempts$methods = methods; attempts$method_notes = method_notes
attempts$primary_contact_id   = contact_ref("primary")
attempts$secondary_contact_id = contact_ref("secondary")

fw_validate_files(attempts[, FW_ATTEMPT_COLUMNS], species[, FW_SPECIES_COLUMNS], contacts[, FW_CONTACT_COLUMNS])

# ---- Species to check ----------------------------------------------------------
fish = !is.na(species$taxa) & species$taxa == "Fish"
check = species[is.na(species$taxa) | (fish & is.na(species$family)), ]
if (nrow(check)) {
  sci_l = tolower(old_sp$scientific_name); lab_l = tolower(old_sp$label)
  hint = vapply(seq_len(nrow(check)), function(i) {
    j = which(lab_l == tolower(check$label[i]) |
               (!is.na(check$scientific_name[i]) & sci_l == tolower(check$scientific_name[i])))
    if (length(j)) j[1] else NA_integer_
  }, 1L)
  out = data.frame(
    species_id = check$species_id, common_name = check$common_name,
    scientific_name = check$scientific_name, taxa = check$taxa, family = check$family,
    missing = ifelse(is.na(check$taxa), "taxa and family", "family"),
    similar_existing = old_sp$label[hint], suggested_taxa = old_sp$taxa[hint],
    suggested_family = old_sp$family[hint],
    attempts = vapply(check$species_id, function(id) sum(grepl(id, c(attempts$invasive_species,
                                                                    attempts$beneficiary_species), fixed = TRUE)), 1L))
  dir.create(QA_DIR, showWarnings = FALSE)
  check_path = file.path(QA_DIR, paste0("species_to_check_", today, ".csv"))
  write_csv(out, check_path, na = "")
}

# ---- Write ---------------------------------------------------------------------
write_csv(attempts[, FW_ATTEMPT_COLUMNS], FW_ATTEMPTS_CSV, na = "")
write_csv(species[, FW_SPECIES_COLUMNS],  FW_SPECIES_CSV,  na = "")
write_csv(contacts[, FW_CONTACT_COLUMNS], FW_CONTACTS_CSV, na = "")
write_csv(data.frame(source_row = seq_len(n), attempt_id = attempt_id), ids_path)
meta = list(release = export_date, source_export = basename(src_path),
             generated_by = "dev/archive/build_from_export.R",
             row_counts = list(attempts = n, species = nrow(species), contacts = nrow(contacts)))
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), file.path(FW_DATA_DIR, "metadata.json"))

# ---- Report --------------------------------------------------------------------
message("Built from ", basename(src_path), ":")
message("  attempts.csv  ", n, " rows")
message("  species.csv   ", nrow(species), " (", nrow(species) - nrow(added_sp) + nrow(renamed),
        " carried over, of which ", nrow(renamed), " renamed; ", nrow(added_sp) - nrow(renamed), " new; ",
        sum(!old_sp$species_id %in% species$species_id), " no longer cited)")
for (i in seq_len(nrow(renamed))) message("      ", renamed$from[i], " -> ", renamed$to[i])
message("  contacts.csv  ", nrow(contacts), " (", sum(!fresh), " carried over, ", sum(fresh), " new, ",
        sum(!old_co$contact_id %in% contacts$contact_id), " no longer cited)")
message("  regions split ", sum(!is.na(region)), ", territories as their own country ", sum(is_terr),
        ", water_regime corrections ", n_regime)
if (length(tidied)) {
  message("  float noise tidied: ", sum(vapply(tidied, nrow, 1L)), " text cells in ",
          paste(names(tidied), collapse = ", "))
  ex = do.call(rbind, tidied); ex = ex[!duplicated(ex$from), ]
  for (i in seq_len(min(nrow(ex), 5))) message("      ", ex$from[i], " -> ", ex$to[i])
}
if (nrow(check)) message("\n", nrow(check), " species need taxa/family filled in species.csv: ", check_path)
if (nrow(issues)) {
  # The export row is its spreadsheet row minus the header, so both are given.
  issues = issues[order(issues$source_row), ]
  issues = data.frame(source_row = issues$source_row, excel_row = issues$source_row + 1L,
                       attempt_id = attempt_id[issues$source_row], site_name = raw$site_name[issues$source_row],
                       issue = issues$issue, detail = issues$detail)
  dir.create(QA_DIR, showWarnings = FALSE)
  issues_path = file.path(QA_DIR, paste0("build_issues_", today, ".csv"))
  write_csv(issues, issues_path, na = "")
  message("\n", nrow(issues), " issue(s) to fix in the export: ", issues_path)
  tab = table(issues$issue)
  for (k in names(tab)) message("  ", tab[[k]], "  ", k)
}
message("\nNext: Rscript dev/reconcile_source.R, then dev/fetch_species_images.R.")
