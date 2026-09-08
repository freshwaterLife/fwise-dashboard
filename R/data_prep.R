# data_prep.R
# ONE-WAY transform: flat client export -> six star-schema tables in data/schema/.
#
# This is NOT part of the running app. It is a script you re-run whenever the
# client drops in a newly cleaned export. Run it with:
#
#     Rscript R/data_prep.R
#
# It never writes to the source CSV. If you need to correct a value, correct it
# in the source export and re-run this, so the cleaning stays in one place.
#
# IMPORTANT. Shiny automatically sources every file in R/ when the app starts, so
# the whole transform is wrapped in fw_build_schema() and only runs when this
# file is executed directly. Without that guard, starting the app would silently
# rebuild data/schema/ on every boot. If you add another build script to R/, give
# it the same guard.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

source(file.path("R", "config.R"))

# ==============================================================================
# PERMANENT IDENTIFIERS
# ==============================================================================
#
# An id, once assigned, is NEVER reassigned. That is the whole contract, and
# everything below exists to keep it.
#
# WHY IT MATTERS. attempt_id, species_id and contact_id are the join key for QA
# writeback, for Zenodo versioning, and for linking an attempt to its reference.
# An id that moved between rebuilds would silently corrupt all three - not with
# an error, but with rows quietly pointing at the wrong thing.
#
# WHAT THIS REPLACES. Ids used to be row positions: sprintf("FW%04d",
# row_number()) for attempts, and for species and contacts a row number assigned
# AFTER an alphabetical sort. So adding one species beginning with "A" shifted the
# id of every species after it, and re-sorting the export renumbered everything.
#
# HOW IT WORKS NOW. A registry in fwise-data/id_registry/ maps a natural key to
# the id it was given. A rebuild looks each row up, reuses the id it already has,
# and mints a new one only for a genuinely new row. The registry is committed, so
# it is as durable as the data.
#
# A pure hash of row content was the alternative and was rejected: it re-mints an
# id whenever QA corrects any field feeding it, which is the same fragility
# wearing a different hat.

# Ambiguous characters are left out - no I, O, 0 or 1 - because these ids get
# read aloud, retyped and pasted into spreadsheets by people.
FW_ID_ALPHABET <- c(as.character(2:9), setdiff(LETTERS, c("I", "O")))

#' Mint n ids that collide with nothing already in use
#'
#' Format: PREFIX-YYYYMMDD-XXXXXX, e.g. FW-20260908-7K3QX9. The date is when the
#' id was first assigned, so ids sort roughly by when a record entered FWISE.
fw_mint_ids <- function(prefix, n, exclude = character(0), date = Sys.Date()) {
  if (n == 0) return(character(0))
  stamp <- format(date, "%Y%m%d")
  out <- character(0)
  # 32^6 is about a billion, so this loop effectively never runs twice. It is
  # here so that a collision is impossible rather than merely unlikely.
  while (length(out) < n) {
    cand <- replicate(n - length(out), paste0(
      prefix, "-", stamp, "-",
      paste(sample(FW_ID_ALPHABET, 6, replace = TRUE), collapse = "")
    ))
    out <- setdiff(unique(c(out, cand)), exclude)
  }
  out[seq_len(n)]
}

fw_registry_path <- function(name) {
  file.path(FW_DATA_DIR, "id_registry", paste0(name, ".csv"))
}

#' Resolve natural keys to permanent ids, minting only what is new
#'
#' @param natural_key character vector, one per row, already disambiguated so
#'   that two different records never share a key.
#' @param prefix "FW", "SP" or "CO".
#' @param registry_name file stem under fwise-data/id_registry/.
#' @return ids aligned to natural_key.
fw_assign_ids <- function(natural_key, prefix, registry_name) {
  path <- fw_registry_path(registry_name)

  reg <- if (file.exists(path)) {
    read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
  } else {
    tibble(natural_key = character(), id = character(), first_seen = character())
  }

  known <- setNames(reg$id, reg$natural_key)
  ids <- unname(known[natural_key])

  new_keys <- unique(natural_key[is.na(ids)])
  if (length(new_keys) > 0) {
    new_ids <- fw_mint_ids(prefix, length(new_keys), exclude = reg$id)
    lookup <- setNames(new_ids, new_keys)
    ids[is.na(ids)] <- unname(lookup[natural_key[is.na(ids)]])

    # Appended, never rewritten. Existing rows keep their position and their id.
    reg <- bind_rows(reg, tibble(
      natural_key = new_keys, id = new_ids,
      first_seen  = format(Sys.Date())
    ))
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    write_csv(reg, path, na = "")
    message("  ", registry_name, ": minted ", length(new_ids), " new id(s)")
  }

  # The invariant, checked rather than trusted. If a key ever resolves to an id
  # other than the one the registry holds, the build stops here rather than
  # shipping silently renumbered data.
  check <- setNames(reg$id, reg$natural_key)[natural_key]
  if (!identical(unname(check), ids)) {
    stop("Registry conflict in ", registry_name,
         ": a natural key resolved to a different id than the registry holds.",
         call. = FALSE)
  }
  ids
}

#' Make a natural key unique by numbering repeats
#'
#' Two rows CAN legitimately share every identifying field: the Quailing Pool
#' rows are two separate rotenone treatments of the same pool in the same year,
#' distinguishable only by their free-text method description. Numbering the
#' repeats keeps their ids apart without dragging a paragraph of prose into the
#' key, where a QA typo would re-mint the id.
#'
#' The residual risk is that reordering two rows that share a key would swap
#' their ids. That affects 4 rows in 914, and populating the source `Key` column
#' from key_backfill.csv removes it entirely.
fw_disambiguate <- function(key) {
  paste0(key, "#", ave(seq_along(key), key, FUN = seq_along))
}

fw_build_schema <- function() {


  message("Reading source: ", FW_SOURCE_CSV)
  raw <- read_csv(FW_SOURCE_CSV, show_col_types = FALSE, progress = FALSE)
  message("  ", nrow(raw), " rows, ", ncol(raw), " columns")

  # Treat empty strings as missing throughout. The export uses "" rather than NA.
  blank_to_na <- function(x) {
    if (!is.character(x)) return(x)
    x <- str_trim(x)
    if_else(x == "", NA_character_, x)
  }
  raw <- raw |> mutate(across(where(is.character), blank_to_na))

  # ---- Attempt identifiers -----------------------------------------------------

  # `Key` is the source spreadsheet's own identifier and is authoritative wherever
  # it is filled in. It is empty on every row of this export, so the natural-key
  # registry carries the load until the client pastes key_backfill.csv into the
  # master sheet. After that this branch takes over and the natural key stops
  # mattering. See the PERMANENT IDENTIFIERS block at the top of this file.
  attempt_key <- fw_disambiguate(paste(
    raw$`Location Name`, raw$Country,
    raw$`Latitude (decimal degrees)`, raw$`Longitude (decimal degrees)`,
    raw$`Eradication Start Year`, raw$`Primary Method`,
    raw$`Eradication End Year`, raw$Outcome,
    raw$`Invasive Species Eradicated 1`, raw$`Area Treated (size)`,
    sep = "|"
  ))

  from_registry <- fw_assign_ids(attempt_key, "FW", "attempt_ids")
  raw <- raw |> mutate(
    attempt_id = if_else(!is.na(Key) & nzchar(Key), Key, from_registry)
  )

  # ---- Country, region, continent, ISO3 ----------------------------------------

  # The source has no continent or ISO code, and encodes territories as a
  # parenthetical, e.g. "United States (Hawaii)". lookup_country.csv splits those
  # into country plus region and assigns each territory its own continent, so Guam
  # maps to Oceania rather than inheriting North America from the United States.
  lookup_country <- read_csv(
    file.path(FW_DATA_DIR, "lookup_country.csv"),
    show_col_types = FALSE, progress = FALSE
  ) |> mutate(across(everything(), blank_to_na))

  missing_countries <- setdiff(unique(raw$Country), lookup_country$country_raw)
  if (length(missing_countries) > 0) {
    stop(
      "Country values not present in data/lookup_country.csv:\n  ",
      paste(missing_countries, collapse = "\n  "),
      "\nAdd a row for each before re-running."
    )
  }

  # ---- Long species table (invasive and beneficiary) ---------------------------

  # Species arrive as "Common name (Scientific name)" across eight numbered slots.
  # Parse both halves so the dropdowns can be searched by either.
  parse_species <- function(x) {
    scientific <- str_match(x, "\\(([^()]*)\\)\\s*$")[, 2]
    common <- str_trim(str_remove(x, "\\s*\\(([^()]*)\\)\\s*$"))
    tibble(
      common_name = if_else(is.na(common) | common == "", NA_character_, common),
      scientific_name = str_trim(scientific)
    )
  }

  # Pull the eight slots into long form, keeping the slot number so taxa and family
  # can be aligned back onto it positionally.
  pivot_slots <- function(df, prefix, role) {
    df |>
      select(attempt_id, all_of(paste0(prefix, " ", 1:8))) |>
      pivot_longer(
        -attempt_id,
        names_to = "slot", values_to = "raw_name", values_drop_na = TRUE
      ) |>
      mutate(
        slot = as.integer(str_extract(slot, "\\d+$")),
        role = role
      )
  }

  invasive_long <- pivot_slots(raw, "Invasive Species Eradicated", "invasive")
  benefic_long  <- pivot_slots(raw, "Eradication Beneficiary", "beneficiary")

  # Taxa are underscore-joined and align one-to-one with the species slots.
  taxa_long <- function(df, taxa_col, role) {
    df |>
      select(attempt_id, taxa_string = all_of(taxa_col)) |>
      filter(!is.na(taxa_string)) |>
      mutate(taxa = str_split(taxa_string, "_")) |>
      select(-taxa_string) |>
      unnest(taxa) |>
      group_by(attempt_id) |>
      mutate(slot = row_number(), role = role) |>
      ungroup() |>
      mutate(taxa = str_trim(taxa))
  }

  # Fish family is also underscore-joined, but it lists ONLY the fish entries, in
  # order. So family n attaches to the nth slot whose taxa is "Fish", not to slot n.
  family_long <- function(df, family_col, taxa, role_name) {
    fam <- df |>
      select(attempt_id, family_string = all_of(family_col)) |>
      filter(!is.na(family_string)) |>
      mutate(family = str_split(family_string, "_")) |>
      select(-family_string) |>
      unnest(family) |>
      group_by(attempt_id) |>
      mutate(fish_index = row_number()) |>
      ungroup() |>
      mutate(family = str_trim(family))

    fish_slots <- taxa |>
      filter(role == role_name, taxa == "Fish") |>
      arrange(attempt_id, slot) |>
      group_by(attempt_id) |>
      mutate(fish_index = row_number()) |>
      ungroup() |>
      select(attempt_id, slot, fish_index)

    fish_slots |>
      inner_join(fam, by = c("attempt_id", "fish_index")) |>
      mutate(role = role_name) |>
      select(attempt_id, slot, role, family)
  }

  taxa_all <- bind_rows(
    taxa_long(raw, "Invasive Taxa", "invasive"),
    taxa_long(raw, "Beneficiary Taxa", "beneficiary")
  )

  family_all <- bind_rows(
    family_long(raw, "Invasive Fish Family", taxa_all, "invasive"),
    family_long(raw, "Beneficiary Fish Family", taxa_all, "beneficiary")
  )

  species_mentions <- bind_rows(invasive_long, benefic_long)

  species_long <- species_mentions |>
    bind_cols(parse_species(species_mentions$raw_name)) |>
    left_join(taxa_all, by = c("attempt_id", "slot", "role")) |>
    left_join(family_all, by = c("attempt_id", "slot", "role"))

  message("  species mentions: ", nrow(species_long),
          " (invasive ", sum(species_long$role == "invasive"),
          ", beneficiary ", sum(species_long$role == "beneficiary"), ")")

  # ---- species.csv -------------------------------------------------------------

  # One row per distinct species name. Where the same name appears with different
  # taxa or family across attempts, take the most frequent, which smooths over the
  # occasional inconsistency in the source without silently dropping anything.
  most_common <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    names(sort(table(x), decreasing = TRUE))[1]
  }

  species <- species_long |>
    group_by(raw_name) |>
    summarise(
      scientific_name = first(scientific_name),
      common_name     = first(common_name),
      taxa            = most_common(taxa),
      family          = most_common(family),
      .groups = "drop"
    ) |>
    # NOT SORTED. Sorting here is what used to make species_id positional: an
    # alphabetical arrange followed by row_number() meant one new species
    # beginning with "A" renumbered every species after it. Display order is the
    # reader's concern and belongs in data_load.R, not in the id.
    mutate(
      # The species name IS the natural key - it is already what the group_by
      # above collapses on, so there is nothing else to build a key from.
      species_id = fw_assign_ids(raw_name, "SP", "species_ids"),
      # Not present in this export. The client's taxonomy work will fill these in;
      # the columns exist now so the contract does not change when it does.
      iucn_status  = NA_character_,
      image_url    = NA_character_,
      image_credit = NA_character_
    ) |>
    select(species_id, scientific_name, common_name, taxa, family,
           iucn_status, image_url, image_credit, raw_name)

  # ---- attempt_species.csv -----------------------------------------------------

  attempt_species <- species_long |>
    left_join(select(species, species_id, raw_name), by = "raw_name") |>
    distinct(attempt_id, species_id, role)

  # ---- method.csv and attempt_method.csv ---------------------------------------

  # Methods are an unbounded any-of set, not a ranked hierarchy. The source stores
  # them in primary/secondary/tertiary slots, so they are unpivoted into a bridge.
  # method_order is retained only as insurance for reproducing the paper's figures;
  # do not treat it as a ranking anywhere in the app.
  method_slots <- tribble(
    ~method_order, ~method_col,        ~notes_col,
    1L,            "Primary Method",   "Primary Methods Notes",
    2L,            "Secondary Method", "Secondary Methods Notes",
    3L,            "Tertiary Method",  "Tertiary Methods Notes"
  )

  attempt_method_raw <- pmap_dfr(method_slots, function(method_order, method_col, notes_col) {
    raw |>
      select(attempt_id,
             method_name = all_of(method_col),
             method_notes = all_of(notes_col)) |>
      filter(!is.na(method_name)) |>
      mutate(method_order = method_order)
  })

  method <- attempt_method_raw |>
    distinct(method_name) |>
    arrange(method_name) |>
    mutate(
      method_id = sprintf("ME%02d", row_number()),
      # Drives the conditional chemical-detail section on the contribute form, so
      # a new chemical arriving in the data must be classified here.
      method_class = case_when(
        method_name %in% c("Rotenone", "Antimycin-A", "Other chemical") ~ "chemical",
        method_name %in% c("Netting / Trapping", "Electrofishing",
                           "Draining", "Other mechanical")              ~ "mechanical",
        TRUE                                                            ~ "other"
      )
    ) |>
    select(method_id, method_name, method_class)

  attempt_method <- attempt_method_raw |>
    left_join(method, by = "method_name") |>
    select(attempt_id, method_id, method_order, method_notes)

  # ---- contact.csv -------------------------------------------------------------

  # Contacts are public by default. `Redact Email` is the only visibility control:
  # blank means the address may be shown. It is entirely blank in this export, so
  # every contact is currently public.
  contact_long <- bind_rows(
    raw |> select(attempt_id,
                  contact_name  = `Primary Contact Name`,
                  contact_email = `Primary Contact Email`,
                  organisation  = `Primary Contact Organisation`,
                  redact        = `Redact Email`) |> mutate(rank = "primary"),
    raw |> select(attempt_id,
                  contact_name  = `Secondary Contact Name`,
                  contact_email = `Secondary Contact Email`,
                  organisation  = `Secondary Contact Organisation`,
                  redact        = `Redact Email`) |> mutate(rank = "secondary")
  ) |>
    filter(!is.na(contact_name))

  contact <- contact_long |>
    group_by(contact_name, organisation) |>
    summarise(
      contact_email = most_common(contact_email),
      # Any non-blank redact flag on any of a contact's rows redacts them
      # everywhere. Erring towards privacy is the right default here.
      email_public = !any(!is.na(redact) &
                            str_to_lower(redact) %in% c("yes", "true", "y", "1")),
      .groups = "drop"
    ) |>
    # NOT SORTED, for the same reason as species above.
    mutate(contact_id = fw_assign_ids(
      paste(contact_name, coalesce(organisation, ""), sep = "|"),
      "CO", "contact_ids"
    )) |>
    # `country` on the contact dimension stays empty on purpose. Country is derived
    # from the attempts a contact is attached to, because a contact can be active
    # in more than one. See fw_contacts_summary() in data_load.R.
    mutate(country = NA_character_) |>
    select(contact_id, contact_name, contact_email, organisation, country, email_public)

  contact_key <- contact |>
    mutate(join_key = paste(contact_name, coalesce(organisation, ""), sep = "|")) |>
    select(contact_id, join_key)

  attempt_contact_ids <- contact_long |>
    mutate(join_key = paste(contact_name, coalesce(organisation, ""), sep = "|")) |>
    left_join(contact_key, by = "join_key") |>
    select(attempt_id, rank, contact_id) |>
    distinct(attempt_id, rank, .keep_all = TRUE) |>
    pivot_wider(names_from = rank, values_from = contact_id)

  # Guard against a source export with no secondary contacts at all, which would
  # leave the pivot without that column.
  for (nm in c("primary", "secondary")) {
    if (!nm %in% names(attempt_contact_ids)) attempt_contact_ids[[nm]] <- NA_character_
  }
  attempt_contact_ids <- attempt_contact_ids |>
    select(attempt_id,
           primary_contact_id = primary,
           secondary_contact_id = secondary)

  # ---- attempt.csv -------------------------------------------------------------

  num <- function(x) suppressWarnings(as.numeric(x))

  attempt <- raw |>
    left_join(lookup_country, by = c("Country" = "country_raw")) |>
    left_join(attempt_contact_ids, by = "attempt_id") |>
    transmute(
      attempt_id,
      site_name      = `Location Name`,
      country,
      region,
      iso3,
      continent,
      latitude       = num(`Latitude (decimal degrees)`),
      longitude      = num(`Longitude (decimal degrees)`),
      waterbody_type = System,
      water_regime   = `System Simple`,
      area_treated   = num(`Area Treated (size)`),
      area_unit      = `Area Treated (unit)`,
      area_notes     = `Area Treated Notes`,
      depth_m        = num(`Depth (m)`),
      volume_m3      = num(`Volume (m3)`),
      max_flow_m3s   = num(`Max Flow (m3/s)`),
      water_temp_c   = num(`Water Temperature (C)`),
      invasion_year  = num(`Year of Invasion`),
      start_year     = num(`Eradication Start Year`),
      end_year       = num(`Eradication End Year`),
      duration_days  = num(`Duration (days)`),
      driver         = `Eradication Reason`,
      outcome        = Outcome,
      verification_method = `Eradication Verification Method`,
      verification_notes  = `Eradication Verification Notes`,
      toxin_conc_mg_l     = num(`Target Toxin Concentration (mg/L)`),
      conc_target_notes   = `Target Toxin Concentration Notes`,
      conc_measured_notes = `Measured Toxin Concentration Notes`,
      neutralising_agent  = `Neutralizing Agent`,
      neutralising_notes  = `Neutralizing Agent Notes`,
      method_description  = `Method Description`,
      labour_person_days  = num(`Labor Effort (person days)`),
      # Not collected in this export. The contribute form asks for it, so new
      # submissions will populate these from the next QA cycle onwards.
      cost_estimate  = NA_real_,
      cost_currency  = NA_character_,
      reference      = `Eradication Reference`,
      reference_link = `Eradication Link`,
      source         = Source,
      # Collected by the contribute form and previously dropped here, so a
      # contributor's note had nowhere to land. Empty on all 914 existing rows;
      # the column has to exist before new submissions can fill it. It is a Notes
      # field, so it is record-detail content and NEVER a filter.
      notes_for_fwise = `Notes for FWISE`,
      primary_contact_id,
      secondary_contact_id,
      # Everything already in the database has passed the client's review.
      # Permitted values are pending | approved | rejected - see FW_STATUS in
      # data_load.R, which refuses to load anything else.
      status         = "approved",
      last_updated   = as.Date("2026-09-06")
    )

  # ---- Validate ----------------------------------------------------------------

  stopifnot(
    "attempt_id must be unique"       = !any(duplicated(attempt$attempt_id)),
    "every attempt needs a country"   = !any(is.na(attempt$country)),
    "every attempt needs a continent" = !any(is.na(attempt$continent)),
    "species_id must be unique"       = !any(duplicated(species$species_id)),
    "contact_id must be unique"       = !any(duplicated(contact$contact_id)),
    "bridge species must resolve"     = !any(is.na(attempt_species$species_id)),
    "bridge methods must resolve"     = !any(is.na(attempt_method$method_id))
  )

  orphans <- setdiff(attempt_species$attempt_id, attempt$attempt_id)
  if (length(orphans) > 0) stop("Orphaned attempt_species rows: ", length(orphans))

  # ---- Close the loop back to the source ---------------------------------------

  # A pasteable Key column for the client's master spreadsheet, in source row
  # order with enough context to check the alignment by eye before pasting. Once
  # `Key` is populated the natural-key registry becomes a fallback and the
  # duplicate-row caveat in fw_disambiguate() goes away.
  key_backfill <- tibble(
    source_row     = seq_len(nrow(raw)),
    Key            = attempt$attempt_id,
    `Location Name`= raw$`Location Name`,
    Country        = raw$Country,
    `Eradication Start Year` = raw$`Eradication Start Year`
  )
  dir.create(file.path(FW_DATA_DIR, "id_registry"),
             showWarnings = FALSE, recursive = TRUE)
  write_csv(key_backfill, file.path(FW_DATA_DIR, "id_registry", "key_backfill.csv"),
            na = "")

  # ---- Write -------------------------------------------------------------------

  dir.create(FW_SCHEMA_DIR, showWarnings = FALSE, recursive = TRUE)
  write_csv(attempt, file.path(FW_SCHEMA_DIR, "attempt.csv"), na = "")
  write_csv(select(species, -raw_name), file.path(FW_SCHEMA_DIR, "species.csv"), na = "")
  write_csv(attempt_species, file.path(FW_SCHEMA_DIR, "attempt_species.csv"), na = "")
  write_csv(method, file.path(FW_SCHEMA_DIR, "method.csv"), na = "")
  write_csv(attempt_method, file.path(FW_SCHEMA_DIR, "attempt_method.csv"), na = "")
  write_csv(contact, file.path(FW_SCHEMA_DIR, "contact.csv"), na = "")

  # ---- metadata.json -----------------------------------------------------------

  # Written on every build rather than maintained by hand, so the row counts can
  # never drift away from the tables they describe. The app reads `release` for
  # the footer's "last updated" line.
  meta <- list(
    release       = format(Sys.Date()),
    source_export = basename(FW_SOURCE_CSV),
    generated_by  = "R/data_prep.R",
    generated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    row_counts    = list(
      attempt         = nrow(attempt),
      species         = nrow(species),
      attempt_species = nrow(attempt_species),
      method          = nrow(method),
      attempt_method  = nrow(attempt_method),
      contact         = nrow(contact)
    )
  )
  writeLines(
    jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
    file.path(FW_DATA_DIR, "metadata.json")
  )

  message("\nid registry: ", file.path(FW_DATA_DIR, "id_registry"))
  message("metadata:    ", file.path(FW_DATA_DIR, "metadata.json"))
  message("\nWritten to ", FW_SCHEMA_DIR, ":")
  for (f in c("attempt", "species", "attempt_species", "method", "attempt_method", "contact")) {
    d <- get(f)
    message(sprintf("  %-20s %5d rows  %2d cols", paste0(f, ".csv"), nrow(d), ncol(d)))
  }

}

# Run the transform ONLY when this file is executed directly, e.g.
#     Rscript R/data_prep.R
# sys.nframe() is 0 at the top level of an Rscript run, and greater than 0 when
# the file arrives here through source(), which is how Shiny loads R/.
if (sys.nframe() == 0L) {
  fw_build_schema()
}
