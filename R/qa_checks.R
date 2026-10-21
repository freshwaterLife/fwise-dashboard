# qa_checks.R
# THE RULES A SUBMISSION MUST MEET BEFORE IT JOINS THE DATA, and the step that
# turns a reviewed sheet row into a database row. Used by dev/qa.R, which the
# reviewer steps through in RStudio. In R/ rather than dev/ so that anything
# later built on the same inbox (a bulk import) applies exactly these rules.
#
# PURE FUNCTIONS, APART FROM TWO. fw_qa_read_db() reads the data checkout, and
# fw_qa_write_import() is the import's one write - to the data files and then
# the sheet's `imported` column. Everything else decides and returns. dev/qa.R
# calls them in order.
#
# THE SHAPE IT WORKS ON is the inbox sheet's (FW_INBOX_COLUMNS in submit.R):
# species as labels, the contact as name / organisation / email / public with
# an optional contact_id. fw_inbox_resolve() maps those to ids.
#
# ERRORS BLOCK, WARNINGS DO NOT. An error stops that row being imported. A
# warning is something the reviewer should look at; setting qa_status to
# "done" is their acceptance of it.
#
# A FUZZY MATCH NEVER MERGES ANYTHING. fw_contact_matches() and
# fw_species_matches() suggest; a contact is only reused on an exact match of
# name and organisation, or on the contact_id the reviewer typed, and a species
# only on its exact label.

# Sheet cells hold "; "-separated lists, but a reviewer typing ";" without the
# space should not mint a species called "Carp;Pike", so the inbox is split on
# the semicolon alone.
fw_qa_split = function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  out = trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  out[nzchar(out)]
}

fw_qa_blank_na = function(df) {
  df[] = lapply(df, function(x) {
    x = trimws(as.character(x))
    x[!is.na(x) & !nzchar(x)] = NA_character_
    x
  })
  df
}

#' The three data files and the ISO lookup, from a data checkout, as text
#'
#' @param tidy TRUE trims every value and makes blanks NA, for checking. FALSE
#'   reads the files exactly as they are (no trimming, only empty cells as NA),
#'   which write_csv(na = "") writes back byte for byte - what dev/qa.R uses
#'   when it appends, so the rows it did not touch do not change.
fw_qa_read_db = function(dir = FW_DATA_DIR, tidy = TRUE) {
  rd = function(name) {
    path = file.path(dir, name)
    if (!file.exists(path)) stop("Not found: ", path, call. = FALSE)
    d = readr::read_csv(path, col_types = readr::cols(.default = "c"),
                         progress = FALSE, na = "", trim_ws = FALSE)
    if (tidy) fw_qa_blank_na(d) else d
  }
  db = list(attempts = rd(FW_ATTEMPTS_FILE), species = rd(FW_SPECIES_FILE),
             contacts = rd(FW_CONTACTS_FILE), iso = rd("lookup_iso3166.csv"))
  fw_validate_files(db$attempts, db$species, db$contacts)
  db
}

# ---- Normalising for the fuzzy match -----------------------------------------

#' Lower case, accents gone, punctuation to spaces, spaces squeezed
fw_qa_norm = function(x) {
  x = stringi::stri_trans_general(ifelse(is.na(x), "", x), "Latin-ASCII")
  x = tolower(x)
  x = gsub("[^a-z0-9@.]+", " ", x)
  x = gsub("(^| )\\.+|\\.+( |$)", " ", x)
  trimws(gsub(" +", " ", x))
}

#' A person's name with its words sorted, so "Bird, Chris" == "Chris Bird"
fw_qa_name_key = function(x) {
  vapply(strsplit(gsub(".", " ", fw_qa_norm(x), fixed = TRUE), " ", fixed = TRUE),
         function(w) paste(sort(w[nzchar(w)]), collapse = " "), character(1))
}

#' The last word of a name, and the first letter of the first
fw_qa_surname = function(x) {
  vapply(strsplit(gsub(".", " ", fw_qa_norm(x), fixed = TRUE), " ", fixed = TRUE),
         function(w) { w = w[nzchar(w)]; if (length(w)) w[length(w)] else "" }, character(1))
}
fw_qa_initial = function(x) substr(fw_qa_norm(x), 1, 1)

#' Two strings are "similar" when an edit or two apart, scaled to their length
#'
#' Two edits in a ten-letter name is a typo; two edits in "Li" is a different
#' person, so the allowance is a quarter of the shorter string, at most two.
fw_qa_similar = function(a, b) {
  if (!nzchar(a) || !nzchar(b) || identical(a, b)) return(FALSE)
  d = utils::adist(a, b)[1, 1]
  d <= min(2, floor(min(nchar(a), nchar(b)) / 4))
}

# Email providers anyone can use: sharing one says nothing about two people.
FW_QA_OPEN_DOMAINS = c("gmail.com", "googlemail.com", "hotmail.com", "outlook.com",
                        "yahoo.com", "icloud.com", "live.com", "hotmail.co.uk",
                        "yahoo.co.uk", "btinternet.com", "protonmail.com", "me.com")

#' Why two contacts might be one person, or NA
#'
#' @param a,b list(name, org, email), raw text
fw_qa_contact_reason = function(a, b) {
  ea = tolower(trimws(a$email %||% "")); eb = tolower(trimws(b$email %||% ""))
  ea[is.na(ea)] = ""; eb[is.na(eb)] = ""
  if (nzchar(ea) && identical(ea, eb)) return("same email")
  na = fw_qa_name_key(a$name); nb = fw_qa_name_key(b$name)
  if (nzchar(na) && identical(na, nb)) return("same name")
  if (fw_qa_similar(na, nb)) return("similar name")
  sa = fw_qa_surname(a$name); sb = fw_qa_surname(b$name)
  if (nzchar(sa) && identical(sa, sb) && identical(fw_qa_initial(a$name), fw_qa_initial(b$name)))
    return("same surname and initial")
  oa = fw_qa_norm(a$org); ob = fw_qa_norm(b$org)
  close_surname = nzchar(sa) && nzchar(sb) && (identical(sa, sb) || fw_qa_similar(sa, sb))
  if (nzchar(oa) && identical(oa, ob) && close_surname) return("same organisation, similar surname")
  da = sub("^.*@", "", ea); db = sub("^.*@", "", eb)
  if (nzchar(ea) && nzchar(eb) && identical(da, db) && !da %in% FW_QA_OPEN_DOMAINS && close_surname)
    return("same email domain, similar surname")
  NA_character_
}

# ---- Which rows are open -----------------------------------------------------

#' Rows not yet imported or rejected, and not already in attempts.csv
fw_inbox_open = function(inbox, db) {
  inbox$imported %in% "no" & !inbox$attempt_id %in% db$attempts$attempt_id
}

#' The contact a sheet row names, as list(name, org, email)
fw_inbox_contact = function(inbox, i) {
  list(name = inbox$contact_name[i], org = inbox$contact_organisation[i],
       email = inbox$contact_email[i])
}

#' The contact in contacts.csv with exactly this name and organisation, or NA
fw_qa_exact_contact = function(contacts, name, org) {
  if (is.na(name) && is.na(org)) return(NA_character_)
  hit = which(contacts$contact_name %in% name & contacts$organisation %in% org &
                 is.na(contacts$contact_name) == is.na(name) &
                 is.na(contacts$organisation) == is.na(org))
  if (length(hit)) contacts$contact_id[hit[1]] else NA_character_
}

# ---- Fuzzy matches -----------------------------------------------------------

#' Contacts in the open rows that may be someone we already hold, or the same
#' person typed differently in another open row
#'
#' Rows whose contact_id is filled are settled and skipped, and so is an exact
#' match on name and organisation (it is reused anyway).
fw_contact_matches = function(inbox, db) {
  open = which(fw_inbox_open(inbox, db) & !inbox$status %in% "rejected")
  out = list()
  con = db$contacts
  pending = open[is.na(inbox$contact_id[open]) &
                    !(is.na(inbox$contact_name[open]) & is.na(inbox$contact_organisation[open]) &
                        is.na(inbox$contact_email[open]))]
  for (i in pending) {
    a = fw_inbox_contact(inbox, i)
    exact = fw_qa_exact_contact(con, a$name, a$org)
    if (!is.na(exact)) next
    row = function(candidate, cand, reason) tibble::tibble(
      attempt_id = inbox$attempt_id[i], sheet_row = inbox$sheet_row[i],
      contact_name = a$name, contact_organisation = a$org, contact_email = a$email,
      candidate = candidate, candidate_name = cand$name,
      candidate_organisation = cand$org, candidate_email = cand$email, reason = reason)
    for (k in seq_len(nrow(con))) {
      cand = list(name = con$contact_name[k], org = con$organisation[k], email = con$contact_email[k])
      why = fw_qa_contact_reason(a, cand)
      if (!is.na(why)) out[[length(out) + 1]] = row(con$contact_id[k], cand, why)
    }
    for (j in setdiff(pending, i)) {
      cand = fw_inbox_contact(inbox, j)
      # Identical spellings become one contact at import; only differences matter.
      if (identical(unlist(lapply(a, function(v) v %||% NA)), unlist(lapply(cand, function(v) v %||% NA)))) next
      why = fw_qa_contact_reason(a, cand)
      if (!is.na(why)) out[[length(out) + 1]] = row(paste("sheet row of", inbox$attempt_id[j]),
                                                    cand, paste(why, "(another open row)"))
    }
  }
  if (!length(out)) return(tibble::tibble(
    attempt_id = character(0), sheet_row = integer(0), contact_name = character(0),
    contact_organisation = character(0), contact_email = character(0), candidate = character(0),
    candidate_name = character(0), candidate_organisation = character(0),
    candidate_email = character(0), reason = character(0)))
  dplyr::bind_rows(out)
}

#' Species labels in the open rows that we do not hold but may already have
#' under a slightly different name
fw_species_matches = function(inbox, db) {
  open = which(fw_inbox_open(inbox, db) & !inbox$status %in% "rejected")
  held = fw_species_label(db$species)
  held_sci = fw_qa_norm(held$scientific_name); held_com = fw_qa_norm(held$common_name)
  out = list()
  for (i in open) for (col in c("invasive_species", "beneficiary_species")) {
    for (lab in fw_qa_split(inbox[[col]][i])) {
      if (lab %in% held$label) next
      nm = fw_split_species_label(lab)
      sci = fw_qa_norm(nm$scientific); com = fw_qa_norm(nm$common)
      for (k in seq_len(nrow(held))) {
        why = if (nzchar(sci) && identical(sci, held_sci[k])) "same scientific name"
               else if (nzchar(sci) && fw_qa_similar(sci, held_sci[k])) "similar scientific name"
               else if (nzchar(com) && identical(com, held_com[k])) "same common name"
               else if (nzchar(com) && fw_qa_similar(com, held_com[k])) "similar common name"
               else if (identical(fw_qa_norm(lab), fw_qa_norm(held$label[k]))) "same label, different case or punctuation"
               else NA_character_
        if (is.na(why)) next
        out[[length(out) + 1]] = tibble::tibble(
          attempt_id = inbox$attempt_id[i], sheet_row = inbox$sheet_row[i], column = col,
          label = lab, candidate = held$species_id[k], candidate_label = held$label[k],
          reason = why)
      }
    }
  }
  if (!length(out)) return(tibble::tibble(
    attempt_id = character(0), sheet_row = integer(0), column = character(0), label = character(0),
    candidate = character(0), candidate_label = character(0), reason = character(0)))
  dplyr::bind_rows(out)
}

# ---- The checks --------------------------------------------------------------

#' Every problem in the inbox, one row per problem
#'
#' @param inbox       the submissions tab, from fw_inbox_read()
#' @param new_species the new_species tab
#' @param db          fw_qa_read_db()
#' @return a tibble: attempt_id, sheet_row, column, value, level, problem
fw_inbox_check = function(inbox, new_species, db) {
  out = list()
  flag = function(i, column, level, problem, value = NA_character_) {
    if (!length(i)) return(invisible())
    out[[length(out) + 1]] <= tibble::tibble(
      attempt_id = if (is.null(inbox$attempt_id)) NA_character_ else inbox$attempt_id[i],
      sheet_row = inbox$sheet_row[i], column = column,
      value = rep_len(as.character(value), length(i)), level = level, problem = problem)
  }
  done = function() {
    if (!length(out)) return(tibble::tibble(
      attempt_id = character(0), sheet_row = integer(0), column = character(0),
      value = character(0), level = character(0), problem = character(0)))
    dplyr::bind_rows(out) |> dplyr::arrange(.data$sheet_row, dplyr::desc(.data$level == "error"))
  }

  missing = setdiff(FW_INBOX_COLUMNS, names(inbox))
  if (length(missing)) {
    out[[1]] = tibble::tibble(attempt_id = NA_character_, sheet_row = 1L,
                               column = paste(missing, collapse = ", "), value = NA_character_,
                               level = "error",
                               problem = "the submissions tab is missing these columns; nothing else was checked")
    return(done())
  }
  if (!nrow(inbox)) return(done())
  if (is.null(new_species) || !nrow(new_species)) {
    new_species = as.data.frame(stats::setNames(rep(list(character(0)), length(FW_NEW_SPECIES_COLUMNS)),
                                                 FW_NEW_SPECIES_COLUMNS))
  }

  # ---- The reviewer's columns, and the row's place in the data ----
  i = which(!inbox$qa_status %in% FW_QA_STATUS)
  flag(i, "qa_status", "error", paste0("must be ", paste(sQuote(FW_QA_STATUS, FALSE), collapse = " or ")), inbox$qa_status[i])
  i = which(!inbox$imported %in% FW_IMPORTED)
  flag(i, "imported", "error", paste0("must be ", paste(sQuote(FW_IMPORTED, FALSE), collapse = ", ")), inbox$imported[i])
  i = which(is.na(inbox$attempt_id))
  flag(i, "attempt_id", "error", "blank; every row needs the id the form gave it")
  dup = !is.na(inbox$attempt_id) & duplicated(inbox$attempt_id) | duplicated(inbox$attempt_id, fromLast = TRUE)
  i = which(dup & !is.na(inbox$attempt_id))
  flag(i, "attempt_id", "error", "the same attempt_id is on more than one row", inbox$attempt_id[i])
  in_db = inbox$attempt_id %in% db$attempts$attempt_id
  i = which(inbox$imported %in% "yes" & !in_db)
  flag(i, "imported", "error", "marked imported but not in attempts.csv - is fwise-data up to date?", "yes")
  i = which(inbox$imported %in% "no" & in_db)
  flag(i, "imported", "warning", "already in attempts.csv; section 5 will mark it imported", "no")

  # ---- The record itself: open rows that are not rejected ----
  open = fw_inbox_open(inbox, db) & !inbox$status %in% "rejected" & !dup & !is.na(inbox$attempt_id)

  i = which(fw_inbox_open(inbox, db) & !inbox$status %in% FW_STATUS)
  flag(i, "status", "error", paste0("must be ", paste(sQuote(FW_STATUS, FALSE), collapse = ", ")), inbox$status[i])
  i = which(open & inbox$qa_status %in% "done" & inbox$status %in% "pending")
  flag(i, "status", "error", "QA is done but status is still pending: set approved or rejected", "pending")
  i = which(open & !inbox$consent_data_use %in% "yes")
  flag(i, "consent_data_use", "error", "the contributor did not consent to their record being used", inbox$consent_data_use[i])

  # Place
  if (!is.null(db$iso)) {
    i = which(open & !inbox$country %in% db$iso$country)
    flag(i, "country", "error", "not a country in lookup_iso3166.csv (check the spelling against the list)", inbox$country[i])
    i = which(open & !is.na(inbox$region) & inbox$region %in% db$iso$country)
    flag(i, "region", "error", "is a country in its own right: put it in country and clear region", inbox$region[i])
  }
  for (col in c("latitude", "longitude")) {
    lim = if (col == "latitude") 90 else 180
    v = suppressWarnings(as.numeric(inbox[[col]]))
    i = which(open & is.na(inbox[[col]]))
    flag(i, col, "warning", "blank: the record will not appear on the map")
    i = which(open & !is.na(inbox[[col]]) & (is.na(v) | abs(v) > lim))
    flag(i, col, "error", paste0("must be a number between -", lim, " and ", lim), inbox[[col]][i])
  }

  # Years and the other numbers
  for (col in c("invasion_year", "start_year", "end_year")) {
    i = which(open & !is.na(inbox[[col]]) & !grepl("^[0-9]{4}$", inbox[[col]]))
    flag(i, col, "error", "a year is four digits", inbox[[col]][i])
  }
  i = which(open & is.na(inbox$start_year))
  flag(i, "start_year", "warning", "blank: the record drops out of every chart by year")
  sy = suppressWarnings(as.integer(inbox$start_year)); ey = suppressWarnings(as.integer(inbox$end_year))
  i = which(open & !is.na(sy) & !is.na(ey) & ey < sy)
  flag(i, "end_year", "warning", "ends before it starts", inbox$end_year[i])
  for (col in setdiff(FW_ATTEMPT_NUMERIC, c("latitude", "longitude", "invasion_year", "start_year", "end_year"))) {
    i = which(open & !is.na(inbox[[col]]) & is.na(suppressWarnings(as.numeric(inbox[[col]]))))
    flag(i, col, "warning", "not a plain number: kept as typed, but charts treat it as blank", inbox[[col]][i])
  }

  # Species
  held = fw_species_label(db$species)$label
  ns_label = new_species$label
  for (col in c("invasive_species", "beneficiary_species")) {
    for (r in which(open)) for (lab in fw_qa_split(inbox[[col]][r])) {
      if (lab %in% held) next
      k = match(lab, ns_label)
      if (is.na(k)) {
        flag(r, col, "error", "not a species we hold, and not in the new_species tab: correct it to a held label (see species_matches) or add a new_species row", lab)
      } else if (is.na(new_species$taxa[k])) {
        flag(r, col, "error", "new species with no taxa: fill taxa in the new_species tab", lab)
      } else if (new_species$taxa[k] %in% "Fish" && is.na(new_species$family[k])) {
        flag(r, col, "warning", "new fish with no family: section 4 tries GBIF, or fill it in new_species", lab)
      }
    }
  }
  i = which(open & is.na(inbox$invasive_species))
  flag(i, "invasive_species", "error", "every attempt targets at least one invasive species")

  # Contact
  has_contact = !(is.na(inbox$contact_name) & is.na(inbox$contact_organisation) & is.na(inbox$contact_email))
  i = which(open & !is.na(inbox$contact_id) & !inbox$contact_id %in% db$contacts$contact_id)
  flag(i, "contact_id", "error", "not a contact_id in contacts.csv", inbox$contact_id[i])
  i = which(open & is.na(inbox$contact_id) & !has_contact)
  flag(i, "contact_name", "warning", "no contact: the record will have no one to ask about it")
  i = which(open & (has_contact | !is.na(inbox$contact_id)) & !inbox$contact_public %in% c("yes", "no"))
  flag(i, "contact_public", "error", "must be yes or no", inbox$contact_public[i])
  i = which(open & !is.na(inbox$contact_email) & !fw_is_email(inbox$contact_email))
  flag(i, "contact_email", "warning", "does not look like an email address", inbox$contact_email[i])
  for (r in which(open & is.na(inbox$contact_id) & has_contact)) {
    ex = fw_qa_exact_contact(db$contacts, inbox$contact_name[r], inbox$contact_organisation[r])
    if (is.na(ex)) next
    held_email = db$contacts$contact_email[match(ex, db$contacts$contact_id)]
    if (!identical(tolower(held_email %||% NA), tolower(inbox$contact_email[r] %||% NA)) && !is.na(inbox$contact_email[r]))
      flag(r, "contact_email", "warning",
           paste0("name and organisation match ", ex, ", which holds ", held_email %||% "no email",
                  "; the record will use ", ex, " and the email typed here is not stored"),
           inbox$contact_email[r])
  }

  # Vocabularies: a value the data has never held may be a typo
  i = which(open & vapply(inbox$methods, function(m) any(!fw_qa_split(m) %in% FW_METHODS$method_name), TRUE))
  flag(i, "methods", "warning", "a method not in FW_METHODS: it loads as class 'other'", inbox$methods[i])
  for (col in c("driver", "waterbody_type", "neutralising_agent", "area_unit", "water_regime", "outcome")) {
    vocab = unique(stats::na.omit(db$attempts[[col]]))
    i = which(open & !is.na(inbox[[col]]) & !inbox[[col]] %in% vocab)
    flag(i, col, "warning", "a value the data has not held before", inbox[[col]][i])
  }

  # Possible duplicates of a record we hold, or of another open row
  key = function(site, year) paste(fw_qa_norm(site), year)
  in_key = key(inbox$site_name, inbox$start_year)
  i = which(open & !is.na(inbox$site_name) & in_key %in% key(db$attempts$site_name, db$attempts$start_year))
  flag(i, "site_name", "warning", "same site name and start year as a record we hold: a duplicate?", inbox$site_name[i])
  i = which(open & !is.na(inbox$site_name) & (duplicated(in_key) | duplicated(in_key, fromLast = TRUE)))
  flag(i, "site_name", "warning", "same site name and start year as another open row: a duplicate?", inbox$site_name[i])

  # The fuzzy matches, pointed at from here so the reviewer opens the tibble
  cm = fw_contact_matches(inbox, db)
  i = match(unique(cm$attempt_id), inbox$attempt_id)
  flag(i, "contact_name", "warning", "possible match for this contact: see contact_matches", inbox$contact_name[i])
  sm = fw_species_matches(inbox, db)
  for (r in unique(sm$attempt_id)) {
    labs = unique(sm$label[sm$attempt_id == r])
    flag(match(r, inbox$attempt_id), "species", "warning",
         "a typed species may be one we hold: see species_matches", paste(labs, collapse = "; "))
  }

  # THE GATE. Rows with no error so far are resolved, as if approved, and run
  # through the loader the app uses at startup. Anything it would refuse is an
  # error here rather than a broken app after the push.
  so_far = done()
  clean = which(open & !inbox$attempt_id %in% so_far$attempt_id[so_far$level == "error"])
  if (length(clean)) {
    trial = inbox[clean, , drop = FALSE]
    trial$status = "approved"
    res = tryCatch({ fw_inbox_resolve(trial, new_species, db); NULL },
                    error = function(e) conditionMessage(e))
    if (!is.null(res)) flag(clean, "(whole row)", "error",
                            paste("the app would refuse to load this:", res))
  }

  done()
}

# ---- Resolving rows to the database's shape ----------------------------------

#' A new species' two names: the new_species tab's, blanks filled from the label
#'
#' A BARE LABEL IS NOT A COMMON NAME WHEN A SCIENTIFIC NAME IS GIVEN. Split, a
#' label with no "(...)" is all common name, so "Oreochromis grahami" with
#' scientific_name filled used to gain the common name "Oreochromis grahami"
#' too, and failed to rebuild its own label (21 Oct 2026). A bare label fills
#' common_name only when the tab gives neither name.
fw_new_species_names = function(labels, common, scientific) {
  nm = fw_split_species_label(labels)
  bare = is.na(nm$scientific)
  list(common = dplyr::coalesce(common, ifelse(bare & !is.na(scientific), NA_character_, nm$common)),
       scientific = dplyr::coalesce(scientific, nm$scientific))
}

#' Turn reviewed sheet rows into attempts rows, plus the species and contacts
#' they add
#'
#' @param rows        the sheet rows to import (approved, checked)
#' @param new_species the new_species tab
#' @param db          fw_qa_read_db()
#' @param today       the date stamped on last_updated and in new ids
#' @return list(to_add, species_add, contacts_add, contacts_private) where
#'   contacts_private are ids of held contacts a submission asked to hide
fw_inbox_resolve = function(rows, new_species, db, today = Sys.Date()) {
  species = db$species; contacts = db$contacts
  if (!nrow(rows)) return(list(to_add = db$attempts[0, ], species_add = species[0, ],
                               contacts_add = contacts[0, ], contacts_private = character(0)))
  labelled = fw_species_label(species)
  sp_id = stats::setNames(labelled$species_id, labelled$label)

  # ---- Species: every label is held or becomes one new row ----
  labels = unique(unlist(lapply(c(rows$invasive_species, rows$beneficiary_species), fw_qa_split)))
  new_labels = setdiff(labels, names(sp_id))
  species_add = species[0, ]
  if (length(new_labels)) {
    k = match(new_labels, new_species$label)
    if (anyNA(k)) stop("No new_species row for: ", paste(new_labels[is.na(k)], collapse = "; "), call. = FALSE)
    ns = new_species[k, , drop = FALSE]
    if (anyNA(ns$taxa)) stop("New species with no taxa: ", paste(new_labels[is.na(ns$taxa)], collapse = "; "), call. = FALSE)
    ids = fw_mint_ids("SP", length(new_labels), exclude = species$species_id, date = today)
    nm = fw_new_species_names(new_labels, ns$common_name, ns$scientific_name)
    species_add = tibble::tibble(
      species_id = ids,
      common_name = nm$common, scientific_name = nm$scientific,
      taxa = ns$taxa, family = ns$family)
    for (col in setdiff(FW_SPECIES_COLUMNS, names(species_add))) species_add[[col]] = NA_character_
    species_add = species_add[, FW_SPECIES_COLUMNS]
    # The label the new row will carry must be the one typed, or the next
    # import would not recognise it as held.
    relabel = fw_species_label(species_add)$label
    if (!identical(relabel, new_labels))
      stop("New species whose names do not rebuild their label: ",
           paste(new_labels[relabel != new_labels], collapse = "; "),
           ". Fix common_name/scientific_name in new_species.", call. = FALSE)
    sp_id = c(sp_id, stats::setNames(ids, new_labels))
  }
  to_ids = function(cell) {
    items = fw_qa_split(cell)
    if (!length(items)) NA_character_ else paste(unname(sp_id[items]), collapse = FW_MULTI_SEP)
  }

  # ---- Contacts: an id, an exact match, or one new row per distinct person ----
  contact_add = contacts[0, ]
  private = character(0)
  contact_of = character(nrow(rows))
  new_key = character(0); new_id = character(0)
  for (r in seq_len(nrow(rows))) {
    nm = rows$contact_name[r]; og = rows$contact_organisation[r]; em = rows$contact_email[r]
    public = rows$contact_public[r] %in% "yes"
    id = rows$contact_id[r]
    if (is.na(id) && is.na(nm) && is.na(og) && is.na(em)) { contact_of[r] = NA_character_; next }
    if (is.na(id)) id = fw_qa_exact_contact(contacts, nm, og)
    if (!is.na(id)) {
      if (!id %in% contacts$contact_id) stop("Unknown contact_id ", id, call. = FALSE)
      if (!public) private = union(private, id)
      contact_of[r] = id
      next
    }
    key = paste(nm %||% "", og %||% "", tolower(em %||% ""), sep = "\r")
    k = match(key, new_key)
    if (is.na(k)) {
      id = fw_mint_ids("CO", 1, exclude = c(contacts$contact_id, new_id), date = today)
      new_key = c(new_key, key); new_id = c(new_id, id)
      contact_add = dplyr::bind_rows(contact_add, tibble::tibble(
        contact_id = id, contact_name = nm, contact_email = em, organisation = og,
        contact_public = if (public) "TRUE" else "FALSE"))
    } else {
      id = new_id[k]
      if (!public) contact_add$contact_public[contact_add$contact_id == id] = "FALSE"
    }
    contact_of[r] = id
  }

  # ---- The attempts rows ----
  to_add = as.data.frame(stats::setNames(rep(list(rep(NA_character_, nrow(rows))), length(FW_ATTEMPT_COLUMNS)),
                                          FW_ATTEMPT_COLUMNS), stringsAsFactors = FALSE)
  shared = intersect(FW_ATTEMPT_COLUMNS, names(rows))
  for (col in shared) to_add[[col]] = as.character(rows[[col]])
  to_add$invasive_species    = vapply(rows$invasive_species, to_ids, "", USE.NAMES = FALSE)
  to_add$beneficiary_species = vapply(rows$beneficiary_species, to_ids, "", USE.NAMES = FALSE)
  to_add$primary_contact_id  = contact_of
  to_add$secondary_contact_id = NA_character_
  # DERIVED, NOT REVIEWED. iso3 and continent are properties of the country
  # (fw_validate_geography() in data_load.R).
  m = match(to_add$country, db$iso$country)
  if (anyNA(m)) stop("Country not in lookup_iso3166.csv: ",
                     paste(unique(to_add$country[is.na(m)]), collapse = ", "), call. = FALSE)
  to_add$iso3 = db$iso$iso3[m]
  to_add$continent = db$iso$continent[m]
  terr = !is.na(to_add$region) & to_add$region %in% db$iso$country
  if (any(terr)) stop("Region is a country in its own right: ",
                      paste(unique(to_add$region[terr]), collapse = ", "), call. = FALSE)
  to_add$last_updated = format(today)
  to_add = tibble::as_tibble(to_add)

  # Nothing goes into attempts.csv that would stop the app at its next load.
  contacts_after = contacts
  contacts_after$contact_public[contacts_after$contact_id %in% private] = "FALSE"
  all_attempts = dplyr::bind_rows(db$attempts, to_add)
  fw_validate_files(all_attempts, dplyr::bind_rows(species, species_add),
                    dplyr::bind_rows(contacts_after, contact_add))
  tables = fw_unpack(all_attempts, dplyr::bind_rows(species, species_add),
                      dplyr::bind_rows(contacts_after, contact_add))
  fw_filter_approved(tables)

  list(to_add = to_add, species_add = tibble::as_tibble(species_add),
       contacts_add = tibble::as_tibble(contact_add),
       contacts_private = setdiff(private, contacts$contact_id[contacts$contact_public %in% "FALSE"]))
}

# ---- Choosing and writing ------------------------------------------------------

#' Which rows an import takes: QA done, not yet imported, and approved (to add)
#' or rejected (to mark)
#'
#' @param import_ids NULL for every ready row, or the ids to limit it to
fw_inbox_ready = function(inbox, db, import_ids = NULL) {
  ready = fw_inbox_open(inbox, db) & inbox$qa_status %in% "done" &
    (is.null(import_ids) | inbox$attempt_id %in% import_ids)
  list(ready = ready, take = ready & inbox$status %in% "approved",
       reject = ready & inbox$status %in% "rejected")
}

#' Write an import: the data files, metadata.json, then the sheet
#'
#' THE DATA FILES FIRST, THEN THE SHEET. If the sheet write fails, a second
#' call adds nothing (every id, species and contact already written is
#' skipped) and marks every open row whose id is now in attempts.csv - so a
#' failed run is repaired by running it again.
#'
#' @param resolved    fw_inbox_resolve()
#' @param species_add the new species rows, enriched or not
#' @param snapshot    the sheet rows (FW_INBOX_COLUMNS) the import was resolved
#'   from; the write stops if the sheet no longer says the same
#' @return invisibly, a list of what was written
fw_qa_write_import = function(resolved, species_add, snapshot, dir = FW_DATA_DIR,
                               today = Sys.Date()) {
  inbox_now = fw_inbox_read(FW_INBOX_TABS$submissions)
  now = inbox_now[match(snapshot$attempt_id, inbox_now$attempt_id), FW_INBOX_COLUMNS, drop = FALSE]
  rownames(now) = NULL; rownames(snapshot) = NULL
  if (!identical(now, snapshot)) stop("The sheet has changed since the import was resolved. ",
                                      "Run sections 3 and 4 again.", call. = FALSE)

  # Read raw, so the rows this does not touch are written back byte for byte.
  raw = fw_qa_read_db(dir, tidy = FALSE)
  add = resolved$to_add[!resolved$to_add$attempt_id %in% raw$attempts$attempt_id, ]
  sp  = species_add[!species_add$species_id %in% raw$species$species_id, FW_SPECIES_COLUMNS]
  co  = resolved$contacts_add[!resolved$contacts_add$contact_id %in% raw$contacts$contact_id, ]
  reject_ids = snapshot$attempt_id[snapshot$status %in% "rejected" & snapshot$imported %in% "no"]

  attempts = dplyr::bind_rows(raw$attempts, add)[, FW_ATTEMPT_COLUMNS]
  species  = dplyr::bind_rows(raw$species, sp)[, FW_SPECIES_COLUMNS]
  contacts = dplyr::bind_rows(raw$contacts, co)[, FW_CONTACT_COLUMNS]
  hide = contacts$contact_id %in% resolved$contacts_private & !contacts$contact_public %in% "FALSE"
  contacts$contact_public[hide] = "FALSE"
  # The last guard: the app's own loader, on exactly what is about to be written.
  fw_validate_files(attempts, species, contacts)
  fw_filter_approved(fw_unpack(fw_qa_blank_na(attempts), fw_qa_blank_na(species),
                               fw_qa_blank_na(contacts)))

  readr::write_csv(attempts, file.path(dir, FW_ATTEMPTS_FILE), na = "")
  readr::write_csv(species,  file.path(dir, FW_SPECIES_FILE),  na = "")
  readr::write_csv(contacts, file.path(dir, FW_CONTACTS_FILE), na = "")
  meta_path = file.path(dir, "metadata.json")
  meta = if (file.exists(meta_path)) jsonlite::fromJSON(meta_path) else list()
  meta$release = format(today)
  meta$generated_by = "dev/qa.R"
  meta$row_counts = list(attempts = nrow(attempts), species = nrow(species), contacts = nrow(contacts))
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), meta_path)

  inbox_now = fw_inbox_read(FW_INBOX_TABS$submissions)
  done_ids = inbox_now$attempt_id[inbox_now$imported %in% "no" &
                                     inbox_now$attempt_id %in% attempts$attempt_id]
  fw_inbox_set(FW_INBOX_TABS$submissions, done_ids, "imported", "yes")
  fw_inbox_set(FW_INBOX_TABS$submissions, reject_ids, "imported", "rejected")

  invisible(list(attempts_added = add$attempt_id, species_added = sp$species_id,
                 contacts_added = co$contact_id, contacts_hidden = contacts$contact_id[hide],
                 marked_imported = done_ids, marked_rejected = reject_ids,
                 attempts_total = nrow(attempts)))
}
