# value_test.R
# THE NUMBERS ARE RIGHT. Every figure the reader sees - the KPI strip, the
# caveat percentages, the coverage line, every chart's bars, the export's rows -
# recomputed here in plain base R from the loaded tables and compared with what
# the app renders. If a chart ever counts an attempt twice, or a filter drops a
# row it should keep, this is what says so.
#
#     Rscript dev/value_test.R
#
# Exits non-zero on any failure. It deliberately does NOT use dplyr for the
# recomputation: the app does, and a test that repeats the app's own joins would
# only prove the joins agree with themselves.

suppressPackageStartupMessages(library(shiny))
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)
d  <- fw_load_data()
m  <- fw_load_metadata()
ch <- fw_filter_choices(d)
ch_all <- fw_startup_choices(d)

failures <- 0L
ok <- function(lbl, got, want = TRUE) {
  pass <- isTRUE(all.equal(got, want, check.attributes = FALSE))
  if (!pass) failures <<- failures + 1L
  cat(sprintf("  %-58s %s\n", lbl,
              if (pass) "PASS"
              else paste0("*** FAIL *** got ", paste(format(got), collapse = "|"),
                          " want ", paste(format(want), collapse = "|"))))
}
strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x$html %||% x)))
traces <- function(p) plotly::plotly_build(p)$x$data
label_name <- function(lbl) sub("  \\([0-9]+\\)$", "", as.character(lbl))
label_n    <- function(lbl) as.integer(sub("^.*  \\(([0-9]+)\\)$", "\\1", as.character(lbl)))

# ---- The raw tables, and a few plain lookups --------------------------------
a   <- d$attempt
asp <- d$attempt_species
am  <- d$attempt_method
method_name <- stats::setNames(d$method$method_name, d$method$method_id)
outcome_of  <- stats::setNames(ifelse(is.na(a$outcome), "Unknown", a$outcome),
                               as.character(a$attempt_id))
# One row per (attempt, method), as every chart counts them.
am1 <- unique(data.frame(attempt_id = as.character(am$attempt_id),
                         method = unname(method_name[as.character(am$method_id)]),
                         stringsAsFactors = FALSE))

plan_ids <- fw_plan_filter_ids()
base <- fw_filter_state(list(), plan_ids, ch = ch)
base$year_from <- ch$year_min; base$year_to <- ch$year_max; base$include_no_year <- TRUE
# The second "include unrecorded" checkbox, as the browser paints it: ON. The
# state reads isTRUE(input$...), which is FALSE before the control exists.
base$include_no_size <- TRUE
all_sel <- fw_filter_apply(d, base)

# The report builder no longer offers a method filter (see mod_plan.R), so the
# selections below that used to be built with one are built on the DASHBOARD's
# id list, where the bridge code under test is identical.
all_ids <- fw_filter_ids()
base_all <- fw_filter_state(list(), all_ids, ch = ch)
base_all$year_from <- ch$year_min; base_all$year_to <- ch$year_max
base_all$include_no_year <- TRUE; base_all$include_no_size <- TRUE

# ==============================================================================
cat("\n-- numbers on screen --\n")

s <- fw_headline_stats(d)
ok("headline: attempts",  s$attempts,  nrow(a))
ok("headline: countries", s$countries, length(unique(a$country)))
ok("headline: earliest year", s$earliest_year, min(a$start_year, na.rm = TRUE))
ok("headline: invasive species", s$species,
   length(unique(asp$species_id[asp$role == "invasive"])))
ok("headline: successful", s$successful, sum(a$outcome == "Successful", na.rm = TRUE))

# The report builder's summary strip, for one real filter.
top_methods <- names(sort(table(am1$method), decreasing = TRUE))
f1 <- base_all; f1$method <- top_methods[1]
sel1 <- fw_filter_apply(d, f1)
ids_m1 <- unique(am1$attempt_id[am1$method == top_methods[1]])
ok("filter: one method selects exactly its attempts", nrow(sel1), length(ids_m1))
ps <- fw_plan_summary(d, sel1)
ok("summary: attempts",  ps$attempts,  nrow(sel1))
ok("summary: countries", ps$countries, length(unique(sel1$country)))
ok("summary: species",   ps$species,
   length(unique(asp$species_id[asp$role == "invasive" & asp$attempt_id %in% sel1$attempt_id])))
# SUCCESSFUL ATTEMPTS ONLY (client, 23 Sept 2026). A species on an attempt
# that failed was not protected by it, so the strip's figure narrows to the
# ones that worked - the same rule fw_headline_stats()$protected has always
# applied on the Welcome page.
won1 <- sel1$attempt_id[sel1$outcome %in% "Successful"]
ok("summary: species protected counts successful attempts only", ps$beneficiaries,
   length(unique(asp$species_id[asp$role == "beneficiary" & asp$attempt_id %in% won1])))
ok("summary: and that is fewer than every attempt would give",
   ps$beneficiaries <=
     length(unique(asp$species_id[asp$role == "beneficiary" &
                                    asp$attempt_id %in% sel1$attempt_id])))
ok("summary: no methods count any more", is.null(ps$methods))
ok("summary: the strip shows protected as a floor",
   grepl(paste0("&gt;", fw_fmt_num(ps$beneficiaries)), as.character(fw_plan_summary_ui(ps)), fixed = TRUE))
yrs <- sel1$start_year[!is.na(sel1$start_year)]
ok("summary: year span", ps$year_span, paste0(min(yrs), "-", max(yrs)))

# The footer.
ok("footer: last updated is the release date", fw_last_updated(d, m), as.Date(m$release))
pending <- attr(d, "status_counts")[["pending"]]
inbox <- tryCatch(fw_pending_submissions(), error = function(e) data.frame())
in_inbox <- if (nrow(inbox) == 0) 0L else if ("status" %in% names(inbox)) sum(inbox$status == "pending") else nrow(inbox)
ok("footer: in-review count", fw_review_count(d), as.integer(pending + in_inbox))
foot_html <- as.character(fw_footer(as.Date(m$release)))
fwise_a <- regmatches(foot_html, regexpr('<a[^>]*>\\s*<img src="[^"]*" alt="FWISE[^"]*"', foot_html))
ok("footer: the FWISE logo is the long SIMPLE wordmark",
   grepl(paste0('src="', FW_LOGO$mark_web, '"'), fwise_a, fixed = TRUE))
ok("footer: the FWISE logo goes to The solution, in the same tab",
   grepl("fw_nav_to&#39;, &#39;home&#39;", fwise_a, fixed = TRUE) && !grepl("_blank", fwise_a, fixed = TRUE))

# The caveats.
#
# [PLACEHOLDER] THE CLIENT IS WRITING THESE (24 Sept 2026), so what is asserted
# here is the machinery, not the words: that the placeholder is the only thing
# in there, that a block with no heading prints no heading, and that the number
# substitution still works. WHEN THE REAL TEXT ARRIVES, put back the per-number
# checks that stood here - each computed figure appearing in some body, and no
# {placeholder} surviving - because a caveat carrying a stale number reads as
# precision and is worse than no caveat at all.
blocks <- fw_caveat_blocks(d)
bodies <- vapply(blocks, `[[`, character(1), "body")
ok("caveats: the client's placeholder is the whole of it",
   bodies, "[PLACEHOLDER - ANABELL TO PROVIDE CAVEATS FOR FWISE]")
ok("caveats: the placeholder block carries no heading",
   vapply(blocks, function(b) fw_caveat_title(b$heading), ""), "")
ok("caveats: flat vector is body only while there is no heading",
   length(fw_caveats(d)), length(blocks))

# The numbers a caveat can quote are still computed and still substituted, so
# the client's text can use them the day it lands. Asserted on a body of our
# own rather than on the copy deck, which currently has nothing to fill.
local({
  filled <- fw_fill("{successful} of {no_start}",
                    successful = format(sum(a$outcome == "Successful", na.rm = TRUE), big.mark = ","),
                    no_start = format(sum(is.na(a$start_year)), big.mark = ","))
  ok("caveats: the number substitution still works", !grepl("\\{", filled))
})

# The closing section of every export: methods first, then the caveats.
closing <- fw_closing_blocks(d)
ok("closing section: methods block leads",
   closing[[1]]$heading, fw_t("export", "methods_heading"))
ok("closing section: the caveats follow it", length(closing), length(blocks) + 1L)
ok("closing section: the workbook text carries both",
   all(c(toupper(fw_t("export", "methods_heading")),
         "[PLACEHOLDER - ANABELL TO PROVIDE CAVEATS FOR FWISE]") %in% fw_methods_caveats_text(d)))

# The About page renders end to end. Its section keys are built with paste0(),
# which dev/check_literals.R cannot see, so a missing key only shows up here.
about_html <- NA_character_
try(testServer(mod_about_server, args = list(data = d, meta = m), {
  about_html <<- as.character(output$body$html)
}), silent = TRUE)
ok("about: the page renders", !is.na(about_html) && nchar(about_html) > 1000)
# Every heading on the page, whether it is an always-visible section or the
# summary of one of the six disclosures. Add a section, add it here. NO
# "database": the opening prose section held Lorem Ipsum and came out on
# 24 Sept 2026 - put it back here when the client's copy arrives.
ok("about: every section heading is present",
   all(vapply(c("signup", "cite", "caveats", "method", "glossary",
                "related", "other", "images", "licence", "links"),
              function(k) grepl(fw_t("about", paste0(k, "_heading")), about_html, fixed = TRUE),
              logical(1))))
# The panels are click-to-open, and a <details> that lost its <summary> is a
# block of prose nobody can close.
ok("about: the six panels are disclosures",
   lengths(regmatches(about_html, gregexpr("fw-disclosure__summary", about_html)))[[1]], 6L)

# THE GLOSSARY NAMES THE METHODS THE DATA ACTUALLY HOLDS (client, 23 Sept
# 2026). A term that does not match a value in attempts.csv$methods sends a
# reader looking for something the charts never say.
glossary_terms <- vapply(fw_t("about", "glossary_items"), function(it) it$term, "")
ok("about: every method in the data has a glossary entry",
   all(d$method$method_name %in% sub(" methods$", "", glossary_terms)))
ok("about: and every glossary entry is drawn",
   all(vapply(glossary_terms, function(t) grepl(t, about_html, fixed = TRUE),
              logical(1))))
# The preamble moved to Contribute only. If it comes back here, the page opens
# on scope rules again.
ok("about: no eradication preamble",
   !grepl(fw_t("contribute", "preamble", "heading"), about_html, fixed = TRUE))

# The networking coverage line, as the module renders it.
cov <- NA_character_
try(testServer(mod_networking_server, args = list(data = d), {
  cov <<- strip(output$coverage)
}), silent = TRUE)
contacts <- fw_contacts_summary(d)
with_email <- contacts$contact_id[!is.na(contacts$contact_email)]
reachable <- sum(a$primary_contact_id %in% with_email | a$secondary_contact_id %in% with_email)
no_contact <- sum(is.na(a$primary_contact_id) & is.na(a$secondary_contact_id))
ok("coverage: renders", !is.na(cov))
ok("coverage: reachable of total",
   grepl(paste0("covers ", fw_fmt_num(reachable), " of the ", fw_fmt_num(nrow(a))), cov, fixed = TRUE))
ok("coverage: no-email of contacts",
   grepl(paste0(fw_fmt_num(sum(is.na(contacts$contact_email))), " of the ", fw_fmt_num(nrow(contacts))), cov, fixed = TRUE))
ok("coverage: no-contact count", grepl(paste0(fw_fmt_num(no_contact), " attempts have no contact"), cov, fixed = TRUE))

# THE NETWORKING FILTERS. Recomputed from attempts.csv and species.csv on disk:
# the attempts matching place AND species-in-role, then every contact named on
# them in either contact column.
net_a  <- read.csv(FW_ATTEMPTS_CSV, colClasses = "character", na.strings = "")
net_a  <- net_a[net_a$status == "approved", ]
net_sp <- read.csv(FW_SPECIES_CSV, colClasses = "character", na.strings = "")
net_label <- stats::setNames(fw_species_label(net_sp)$label, net_sp$species_id)
net_has <- function(col, labels) vapply(
  strsplit(ifelse(is.na(net_a[[col]]), "", net_a[[col]]), ";"),
  function(ids) any(net_label[trimws(ids)] %in% labels), logical(1))
net_expect <- function(rows) {
  ids <- c(net_a$primary_contact_id[rows], net_a$secondary_contact_id[rows])
  sort(intersect(unique(ids[!is.na(ids)]), contacts$contact_id))
}
net_got <- function(...) sort(fw_networking_filter(d, contacts, list(...))$contact_id)
# The most-recorded protected species, and the country with most of its attempts.
ben_ids <- trimws(unlist(strsplit(stats::na.omit(net_a$beneficiary_species), ";")))
net_sp1 <- unname(net_label[names(sort(table(ben_ids), decreasing = TRUE))[1]])
net_c1 <- names(sort(table(net_a$country[net_has("beneficiary_species", net_sp1)]),
                     decreasing = TRUE))[1]
ok("networking: no filter lists everyone", net_got(), sort(contacts$contact_id))
ok(paste0("networking: ", net_c1, " alone"),
   net_got(country = net_c1), net_expect(which(net_a$country == net_c1)))
ok("networking: species as protected",
   net_got(species = net_sp1, category = "beneficiary"),
   net_expect(which(net_has("beneficiary_species", net_sp1))))
ok("networking: species as either role",
   net_got(species = net_sp1, category = "either"),
   net_expect(which(net_has("beneficiary_species", net_sp1) |
                      net_has("invasive_species", net_sp1))))
ok("networking: species AND country on the same attempt",
   net_got(species = net_sp1, category = "beneficiary", country = net_c1),
   net_expect(which(net_has("beneficiary_species", net_sp1) & net_a$country == net_c1)))
ok("networking: a protected species searched as invasive finds only invasive records",
   net_got(species = net_sp1, category = "invasive"),
   net_expect(which(net_has("invasive_species", net_sp1))))
net_org <- names(sort(table(contacts$organisation), decreasing = TRUE))[1]
ok("networking: organization",
   net_got(organisation = net_org),
   sort(contacts$contact_id[!is.na(contacts$organisation) & contacts$organisation == net_org]))

# The species tiles.
inv_rows <- unique(data.frame(attempt_id = as.character(asp$attempt_id[asp$role == "invasive"]),
                              species_id = asp$species_id[asp$role == "invasive"], stringsAsFactors = FALSE))
counts <- sort(table(inv_rows$species_id), decreasing = TRUE)
# FW_PLAN_SPECIES_N, the limit the report builder passes - fewer than the
# FW_TOP_N the ranked bar charts use, because a tile is a photograph rather than
# a line. See fw_species_tiles_ui().
top <- fw_species_top_n(d, all_sel, "invasive", limit = FW_PLAN_SPECIES_N)
ok("tiles: top species count", top$n[1], as.integer(counts[1]))
ok("tiles: row count is the limit", nrow(top), min(FW_PLAN_SPECIES_N, length(counts)))
ok("tiles: outcome splits sum to n", all(rowSums(top[, FW_OUTCOME_LEVELS]) == top$n))
sp1 <- top$species_id[1]
oc <- table(factor(outcome_of[inv_rows$attempt_id[inv_rows$species_id == sp1]], levels = FW_OUTCOME_LEVELS))
ok("tiles: top species outcome split", as.integer(unlist(top[1, FW_OUTCOME_LEVELS])), as.integer(oc))

# ==============================================================================
cat("\n-- the explore page --\n")

# THE RECORD BROWSER. Its database panel, its selection strip, its list and its
# paging, read out of the module itself and recomputed here from the raw tables.
exp_ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))
ok("explore: offers exactly its six filters", sort(exp_ids), sort(FW_EXPLORE_FILTERS))
ok("explore: and no year range", "years" %in% exp_ids, FALSE)

# THE FISH FAMILY FILTERS. Recomputed from the CSVs on disk - the packed
# species ids in attempts.csv and the family column of species.csv - not from
# the loaded bridge table the engine reads.
raw_a  <- read.csv(FW_ATTEMPTS_CSV, colClasses = "character", na.strings = "")
raw_a  <- raw_a[raw_a$status == "approved", ]
raw_sp <- read.csv(FW_SPECIES_CSV, colClasses = "character", na.strings = "")
fam_of <- stats::setNames(raw_sp$family, raw_sp$species_id)
raw_family_hits <- function(col, fam) {
  has <- vapply(strsplit(ifelse(is.na(raw_a[[col]]), "", raw_a[[col]]), ";"),
                function(ids) any(fam_of[trimws(ids)] %in% fam), logical(1))
  sort(raw_a$attempt_id[has])
}
fam_apply <- function(...) {
  st <- list(...); st$.ids <- exp_ids
  sort(as.character(fw_filter_apply(d, st)$attempt_id))
}
ok("family: only fish carry a family",
   unique(raw_sp$taxa[!is.na(raw_sp$family)]), "Fish")
fam_inv <- names(sort(table(fam_of[unlist(lapply(strsplit(raw_a$invasive_species, ";"), trimws))]),
                      decreasing = TRUE))[1]
ok("family: choices are the invasive side's fish families",
   ch$family, sort(unique(stats::na.omit(fam_of[trimws(unlist(strsplit(raw_a$invasive_species, ";")))]))))
ok(paste0("family: ", fam_inv, " (invasive) selects its attempts"),
   fam_apply(taxa = "Fish", family = fam_inv), raw_family_hits("invasive_species", fam_inv))
fam_ben <- names(sort(table(fam_of[trimws(unlist(strsplit(stats::na.omit(raw_a$beneficiary_species), ";")))]),
                      decreasing = TRUE))[1]
ok(paste0("family: ", fam_ben, " (protected) selects its attempts"),
   fam_apply(taxa_beneficiary = "Fish", family_beneficiary = fam_ben),
   raw_family_hits("beneficiary_species", fam_ben))
ok("family: two families are any-of",
   fam_apply(taxa = "Fish", family = ch$family[1:2]),
   raw_family_hits("invasive_species", ch$family[1:2]))
# Hidden means not applied: a family left set after Fish is deselected must not
# narrow anything. Read through fw_filter_state(), which is where that is done.
st_hidden <- fw_filter_state(list(taxa = "Crayfish", family = fam_inv), exp_ids)
ok("family: ignored while Fish is not picked", st_hidden$family, character(0))
st_shown <- fw_filter_state(list(taxa = c("Crayfish", "Fish"), family = fam_inv), exp_ids)
ok("family: applied while Fish is picked", st_shown$family, fam_inv)

ben_rows <- unique(data.frame(
  attempt_id = as.character(asp$attempt_id[asp$role == "beneficiary"]),
  species_id = asp$species_id[asp$role == "beneficiary"], stringsAsFactors = FALSE))

# The database panel. Never filtered, so these are the whole-table counts.
ok("explore db: beneficiary species", s$beneficiaries, length(unique(ben_rows$species_id)))
won_all <- a$attempt_id[a$outcome %in% "Successful"]
ok("explore db: protected counts only successful attempts", s$protected,
   length(unique(ben_rows$species_id[ben_rows$attempt_id %in% as.character(won_all)])))
ok("explore db: and that is fewer than every beneficiary recorded",
   s$protected < s$beneficiaries)
ok("explore db: latest year", s$latest_year, max(a$start_year, na.rm = TRUE))
db <- strip(fw_explore_db_panel(s, 7L))
for (v in list(c("attempts", nrow(a)), c("countries", length(unique(a$country))),
               c("invasive", s$species),
               # SUCCESSFUL ATTEMPTS ONLY (client, 23 Sept 2026): the tile
               # shows s$protected, not s$beneficiaries. The two differ, and
               # the assertion below checks they still do - otherwise this
               # would pass against either.
               c("protected", s$protected),
               c("in review", 7L))) {
  ok(paste("explore db: panel names the", v[1], "count"),
     grepl(fw_fmt_num(as.integer(v[2])), db, fixed = TRUE))
}
# AND THE SUCCESS COUNT IS NOT THERE. Removed at the client's request: it was
# the only outcome in a strip of sizes, and printed beside the total it is a
# success RATE with the division left to the reader - the one headline this app
# does not publish. Asserted as an absence so it cannot quietly come back.
# ASSERTED AGAINST A LITERAL, and it has to be. This read fw_t("explore",
# "db_successful") - the key of the tile it is checking for the absence of -
# which meant that removing the tile's copy, which is exactly what "removed at
# the client's request" means, made fw_t() raise "No copy defined" and took the
# rest of this file with it. An absence test cannot depend on the thing being
# absent still existing.
ok("explore db: the panel does NOT name a success count",
   !grepl("success", db, ignore.case = TRUE))
# NO YEAR-SPAN SENTENCE (client, 24 Sept 2026): "Attempts recorded from ... to
# ..." came off the panel.
ok("explore db: the year span sentence is gone",
   grepl(paste0(min(a$start_year, na.rm = TRUE), " to ", max(a$start_year, na.rm = TRUE)), db, fixed = TRUE),
   FALSE)
ok("explore db: the protected figure carries its >",
   grepl(paste0("&gt;", fw_fmt_num(s$protected)), as.character(fw_explore_db_panel(s, 7L)), fixed = TRUE))
ok("explore db: no placeholder left", !grepl("\\{[a-z_]+\\}", db))

# Beneficiary taxa: the new filter, matched against the bridge directly.
taxa_of <- stats::setNames(d$species$taxa, d$species$species_id)
ben_taxa <- table(taxa_of[ben_rows$species_id])
pick_taxa <- names(sort(ben_taxa, decreasing = TRUE))[2]
want_taxa_ids <- unique(ben_rows$attempt_id[taxa_of[ben_rows$species_id] %in% pick_taxa])
fb <- fw_filter_state(list(), exp_ids); fb$taxa_beneficiary <- pick_taxa
ok(paste0("explore filter: beneficiary taxa '", pick_taxa, "' selects its attempts"),
   sort(fw_filter_apply(d, fb)$attempt_id), sort(want_taxa_ids))

# AN ATTEMPT WITH NO COORDINATES STILL HAS A RECORD. It has no marker, so
# fw_map_points() cannot reach it, but building its record must still work.
no_coord_id <- a$attempt_id[is.na(a$latitude) | is.na(a$longitude)][1]
no_coord_rec <- fw_attempt_records(d, a[a$attempt_id == no_coord_id, ])
ok("explore: an unlocated attempt is in the record frame", nrow(no_coord_rec), 1L)
ok("explore: and not in the map's points",
   no_coord_id %in% fw_map_points(d, a)$attempt_id, FALSE)
no_coord_html <- fw_record_detail_html(no_coord_rec[1, ], d$species)
ok("explore: its record panel names the site",
   grepl(a$site_name[a$attempt_id == no_coord_id][1], no_coord_html, fixed = TRUE))
# PREVIOUS AND NEXT ARE GONE, and this asserts their absence rather than simply
# not testing for it: the buttons, their script branch and the server observer
# were removed together, and a stray one left behind is a control that looks
# live and does nothing.
ok("explore: the panel carries no previous/next buttons",
   grepl("data-fw-record-step", no_coord_html, fixed = TRUE), FALSE)
ok("explore: the marker's template is that same panel, wrapped",
   fw_map_detail_html(no_coord_rec[1, ], d$species),
   paste0('<template class="fw-popup__detail">',
          fw_record_detail_html(no_coord_rec[1, ], d$species), "</template>"))

# ---- The hover card ---------------------------------------------------------
#
# TWO PHOTOGRAPHS ARE IN IT NOW - the first invasive species and the first
# beneficiary - both at the client's request and both against the reasoning that
# made the card cheap in the first place. These pin the things that keep it
# affordable, because the cost was the whole argument against the picture.
map_pts <- fw_map_points(d, all_sel)
thumbs <- fw_map_thumb_cache(d, map_pts)
cards <- vapply(seq_len(nrow(map_pts)), function(i)
  fw_map_popup(map_pts[i, ], d$species, detail = "lazy", thumbs = thumbs),
  character(1))
# CARDS THAT CARRY AN ACTUAL PHOTOGRAPH, which is no longer the same set as
# cards with a figure block: every card has the block now, and a role with no
# cached image gets a blank tile inside it. The obligations below - lazy
# loading, the credit - are about <img>s, so they are asserted against the cards
# that have one.
with_img <- cards[grepl("fw-species-figure__img", cards, fixed = TRUE)]

first_of <- function(col) vapply(col, function(x) {
  ids <- fw_popup_parts(x); if (length(ids)) ids[1] else NA_character_
}, character(1), USE.NAMES = FALSE)
# ONE ENTRY PER DISTINCT SPECIES, ACROSS BOTH ROLES - and keyed by species, so a
# species that leads an attempt in one role and another attempt in the other is
# rendered once, not twice.
ok("hover: the cache holds one entry per distinct first species of either role",
   length(thumbs),
   length(unique(stats::na.omit(c(first_of(map_pts$inv_ids),
                                  first_of(map_pts$ben_ids))))))
# TWO IMAGES, NOT SIXTEEN. The detail panel still shows every species in both
# roles; if this ever counts more than two per card, the cost argument that
# allowed the pictures back has quietly stopped holding.
ok("hover: no card carries more than two photographs",
   max(lengths(regmatches(cards, gregexpr("fw-species-figure__img", cards,
                                          fixed = TRUE)))), 2L)
ok("hover: most cards have at least one",
   length(with_img) > 0.5 * length(cards))
# BOTH SLOTS ON EVERY CARD, at the client's request. The block used to be
# omitted when neither role had a photograph and to go one-up when only one did,
# so the rows underneath started at a different height card to card.
ok("hover: every card carries the figure block",
   all(grepl("fw-popup__figure", cards, fixed = TRUE)))
ok("hover: and every one of them is two tiles wide",
   all(grepl("data-fw-figures=\"2\"", cards, fixed = TRUE)))
# EACH PICTURE IS LABELLED WITH ITS ROLE. Two unlabelled photographs side by
# side do not say which is the target and which is the beneficiary.
ok("hover: a two-photograph card labels both roles",
   all(vapply(cards[grepl("data-fw-figures=\"2\"", cards, fixed = TRUE)],
              function(x) grepl(fw_t("species", "fig_invasive"), x, fixed = TRUE) &&
                          grepl(fw_t("species", "fig_beneficiary"), x, fixed = TRUE),
              logical(1))))
# EVERY IMAGE IS DEFERRED. The card markup sits in the marker's popup string
# from the start, so an eager <img> would have the browser fetching hundreds of
# thumbnails before anybody hovered anything.
ok("hover: every photograph is lazy and async",
   all(grepl('loading="lazy"', with_img, fixed = TRUE)) &&
     all(grepl('decoding="async"', with_img, fixed = TRUE)))
# THE CREDIT IS A CONDITION OF USE, not decoration. See R/species_images.R.
ok("hover: every photograph carries its credit",
   all(grepl("fw-species-figure__credit", with_img, fixed = TRUE)))
# A ROLE WITH NO PHOTOGRAPH GETS A BLANK TILE, and this used to assert the
# opposite - no tile at all. The client asked for the slot to be held open and
# captioned, because an absent tile tells the reader nothing about whether a
# beneficiary was recorded, which is the question the card is being read for.
ok("hover: a role with no photograph gets the blank tile",
   any(grepl("fw-species-figure--none", cards, fixed = TRUE)))
ok("hover: and the blank tile says so in words",
   all(vapply(cards[grepl("fw-species-figure--none", cards, fixed = TRUE)],
              function(x) grepl(fw_t("species", "fig_none"), x, fixed = TRUE) ||
                          grepl(fw_t("species", "p_none"), x, fixed = TRUE),
              logical(1))))
# A ROLE WITH NO SPECIES SAYS NOT RECORDED, in the text rows' words, on the
# hover tile and in the detail panel alike. Picked from the data, not by id.
no_ben <- map_pts[is.na(map_pts$ben_ids), ]
if (nrow(no_ben)) {
  nb <- no_ben[1, ]
  ok("hover: a role with no species gets the not-recorded tile",
     grepl(fw_popup_thumb_unrecorded(), fw_popup_thumb(nb, thumbs), fixed = TRUE))
  ok("detail: a role with no species gets the not-recorded tile",
     grepl(fw_popup_thumb_unrecorded(),
           fw_record_detail_html(nb, d$species), fixed = TRUE))
}
# THE CARD'S FIELDS, IN THE CLIENT'S ORDER (23 Sept 2026), EVERY ONE DRAWN.
# Location and country unlabelled, then the labelled rows. Recorded by is
# gone from the card. A blank field says "Not noted".
#
# KIND OF WATER NOW SITS ABOVE METHOD(S) - the setting a reader is matching
# their own site against comes before what was done about it - and carries the
# treated size in brackets. Both are asserted below.
hover_keys <- c("p_targeted", "p_protected", "p_outcome", "p_began",
                "p_duration", "p_waterbody", "p_methods")
key_pos <- function(html, key) regexpr(paste0('fw-popup__key">', fw_t("species", key), "<"),
                                       html, fixed = TRUE)
ok("hover: every card has every field, in order",
   all(vapply(cards, function(x) {
     pos <- vapply(hover_keys, function(k) key_pos(x, k), integer(1))
     all(pos > 0) && !is.unsorted(pos)
   }, logical(1))))
ok("hover: every card names its country, unlabelled",
   all(grepl('class="fw-popup__place', cards, fixed = TRUE)))
ok("hover: no card carries a Recorded by row",
   any(grepl("Recorded by", cards, fixed = TRUE)), FALSE)
# THE KIND OF WATER CARRIES THE TREATED SIZE, in brackets after it.
sized <- map_pts[!is.na(map_pts$area_treated) & !is.na(map_pts$waterbody_type), ]
if (nrow(sized)) {
  sz <- sized[1, ]
  ok("hover: kind of water carries the size and its unit",
     grepl(paste0(sz$waterbody_type, " (", fw_popup_area(sz), ")"),
           fw_map_hover_html(sz), fixed = TRUE))
  ok("hover: and the size is the same string the record panel prints",
     grepl(fw_popup_area(sz), fw_record_detail_html(sz, d$species), fixed = TRUE))
}
# No size recorded: the kind of water stands alone, with no empty brackets.
nosize <- map_pts[is.na(map_pts$area_treated) & !is.na(map_pts$waterbody_type), ]
if (nrow(nosize)) {
  ok("hover: no size recorded leaves the kind of water bare",
     fw_popup_waterbody(nosize[1, ]), nosize$waterbody_type[1])
}

blank_row <- map_pts[1, ]
blank_row$waterbody_type <- NA; blank_row$area_treated <- NA
blank_row$method_names <- NA
blank_row$duration_days <- NA; blank_row$start_year <- NA
blank_row$inv_list <- NA; blank_row$country <- NA
blank_card <- fw_map_hover_html(blank_row)
# Six fields blanked above: the country line and five rows.
ok("hover: blank fields say Not noted",
   lengths(regmatches(blank_card, gregexpr(fw_t("species", "p_not_noted"), blank_card, fixed = TRUE))), 6L)
ok("hover: and the rows are still all there",
   all(vapply(hover_keys, function(k) key_pos(blank_card, k) > 0, logical(1))))

# THE RECORD: EVERY FIELD DRAWN FOR EVERY ATTEMPT, so every record has the
# same labels. Verification notes are gone; contacts are one bulleted row.
detail_keys <- c("p_species", "p_beneficiary", "p_outcome", "p_began",
                 "p_duration", "p_driver", "p_waterbody", "p_area",
                 "p_methods", "p_method_desc", "p_verified", "p_contacts",
                 "p_reference")
recs_all <- fw_attempt_records(d, a)
details <- vapply(seq_len(nrow(recs_all)), function(i)
  fw_record_detail_html(recs_all[i, ], d$species, figure_cache = character(0)),
  character(1))
ok("detail: every record has every field, in order",
   all(vapply(details, function(x) {
     pos <- vapply(detail_keys, function(k) key_pos(x, k), integer(1))
     all(pos > 0) && !is.unsorted(pos)
   }, logical(1))))
ok("detail: every record has the download pointer",
   all(grepl(fw_t("species", "p_download_hint"), details, fixed = TRUE)))
# THE SOURCE BUTTON, ONLY WHERE THERE IS A LINK (21 Sept 2026). No link, no
# line at all - the "Read the source: Not noted" line is gone.
has_src <- !is.na(recs_all$reference_link) & nzchar(recs_all$reference_link)
ok("detail: a source button exactly where the record has a link",
   identical(grepl("fw-popup-detail__link-btn", details, fixed = TRUE), has_src))
ok("detail: no record says Read the source: Not noted",
   any(grepl(paste0(fw_t("species", "p_read_source"), ": "), details, fixed = TRUE)), FALSE)
ok("detail: some records have a link and some do not",
   any(has_src) && !all(has_src))
# By its row, not its text: one attempt repeats its note in What was done.
ok("detail: no record has a verification notes row",
   any(grepl('fw-popup__key">Verification<', details, fixed = TRUE)), FALSE)
unk <- which(recs_all$verification_method == "Unknown")[1]
ok("detail: an Unknown verification is shown as Unknown",
   grepl(paste0(fw_t("species", "p_verified"), '</span><span class="fw-popup__val">Unknown<'),
         details[unk], fixed = TRUE))
two_c <- which(!is.na(recs_all$primary_contact_name) & !is.na(recs_all$secondary_contact_name))[1]
if (!is.na(two_c)) {
  ok("detail: two contacts are two bullets",
     lengths(regmatches(sub(".*Contact\\(s\\)", "", details[two_c]),
                        gregexpr("<li>", sub(".*Contact\\(s\\)", "", details[two_c]), fixed = TRUE))), 2L)
}

# METHODS BY NAME ONLY, at most three, recomputed from attempts.csv. No note
# text reaches a card or a record.
am_sorted <- am[order(am$attempt_id, am$method_order), ]
want_names <- vapply(as.character(a$attempt_id), function(id) {
  nm <- unname(method_name[as.character(am_sorted$method_id[am_sorted$attempt_id == id])])
  paste(unique(nm[!is.na(nm)]), collapse = FW_POPUP_SEP)
}, character(1), USE.NAMES = FALSE)
got_names <- recs_all$method_names[match(a$attempt_id, recs_all$attempt_id)]
got_names[is.na(got_names)] <- ""
ok("methods: the record's method names match the bridge, in order", got_names, want_names)
before_contacts <- sub("Contact\\(s\\).*", "", details)
ok("methods: no record lists more than three",
   max(lengths(regmatches(before_contacts, gregexpr("<li>", before_contacts, fixed = TRUE)))) <= 3L)

# THE PLAN SPECIES TITLES name the tiles shown and the role's full count.
for (role in c("invasive", "beneficiary")) {
  n_role <- length(unique(asp$species_id[asp$role == role & asp$attempt_id %in% sel1$attempt_id]))
  ttl <- fw_species_top_title(d, sel1, role)
  ok(paste0("species title (", role, "): names the total"),
     grepl(paste0(if (role == "beneficiary") ">" else "(of ", fw_fmt_num(n_role), " total)"), ttl, fixed = TRUE))
  ok(paste0("species title (", role, "): names the tiles shown"),
     grepl(paste0("Top ", fw_num_word(min(FW_PLAN_SPECIES_N, n_role)), " "), ttl, fixed = TRUE))
}
# STRUCTURALLY, not by searching for note text: the notes repeat inside What
# was done and elsewhere, so a text search proves nothing. The Method(s) row's
# bullets must be exactly the attempt's method names from the bridge table.
methods_row <- function(html) {
  m <- regmatches(html, regexpr('Method\\(s\\)</span><span class="fw-popup__val[^"]*">.*?</span></div>', html))
  if (!length(m)) return(NA_character_)
  items <- regmatches(m, gregexpr("<li>.*?</li>", m))[[1]]
  paste(gsub("</?li>", "", items), collapse = FW_POPUP_SEP)
}
got_rows <- vapply(details, methods_row, character(1), USE.NAMES = FALSE)
want_rows <- vapply(want_names[match(recs_all$attempt_id, a$attempt_id)],
                    function(x) paste(htmltools::htmlEscape(fw_popup_parts(x)), collapse = FW_POPUP_SEP),
                    character(1), USE.NAMES = FALSE)
ok("methods: every record's bullets are its method names and nothing else", got_rows, want_rows)
ok("methods: every hover card's bullets are too",
   vapply(cards, methods_row, character(1), USE.NAMES = FALSE),
   vapply(map_pts$attempt_id, function(id) paste(htmltools::htmlEscape(fw_popup_parts(
     want_names[match(id, a$attempt_id)])), collapse = FW_POPUP_SEP), character(1), USE.NAMES = FALSE))

# DURATION, recomputed by hand: days under a year, years to 1dp from 365 on.
ok("duration: 1 day",    fw_popup_duration(1),   "1 day")
ok("duration: 20 days",  fw_popup_duration(20),  "20 days")
ok("duration: 364 days", fw_popup_duration(364), "364 days")
ok("duration: 365 is 1.0 years", fw_popup_duration(365), "1.0 years")
ok("duration: 400 is 1.1 years", fw_popup_duration(400), "1.1 years")
ok("duration: 10220 is 28.0 years", fw_popup_duration(10220), "28.0 years")
ok("duration: none is NA", fw_popup_duration(NA), NA_character_)
ok("years: a range", fw_popup_years(1998, 2004), "1998-2004")
ok("years: start only", fw_popup_years(1998, NA), "1998")
dur_i <- which(!is.na(recs_all$duration_days))[1]
ok("duration: a record shows its own",
   grepl(fw_popup_duration(recs_all$duration_days[dur_i]), details[dur_i], fixed = TRUE))

# THE BASEMAPS. No place-label layer, and no null overlay - leaflet.js turns a
# null into a checkbox named null in the layer switcher.
bm <- fw_add_basemaps(fw_leaflet())
bm_calls <- bm$x$calls
bm_ctrl <- Filter(function(cl) cl$method == "addLayersControl", bm_calls)[[1]]
ok("map: the layer switcher has no null overlay",
   !is.null(bm_ctrl$args[[2]]) && length(bm_ctrl$args[[2]]) == 0)
ok("map: no place-label tiles",
   any(grepl("only_labels", unlist(bm_calls), fixed = TRUE)), FALSE)
ok("map: zoom out is capped", bm$x$options$minZoom, FW_MAP$min_zoom)
# CACHE ONLY. If a live lookup ever crept in, a build would reach Wikimedia
# once per uncached species while rendering - which is the thing the cache
# exists to prevent. Asserted by building with an empty species image column:
# every card still draws its two slots, and not one of them holds an <img>.
#
# THIS IS WHAT MAKES FORCING BOTH SLOTS FREE. A blank tile is a div with a line
# of text in it, so holding the slot open costs no request - which is the whole
# reason the cost argument that allowed the pictures back still holds.
blank <- d; blank$species$image_url <- NA_character_
blank_cards <- vapply(seq_len(min(20L, nrow(map_pts))), function(i)
  fw_map_popup(map_pts[i, ], blank$species, detail = "lazy",
               thumbs = fw_map_thumb_cache(blank, map_pts)), character(1))
ok("hover: with nothing cached, no card carries a photograph",
   any(grepl("fw-species-figure__img", blank_cards, fixed = TRUE)), FALSE)
ok("hover: but every card still holds both slots open",
   all(grepl("data-fw-figures=\"2\"", blank_cards, fixed = TRUE)))

# ON A PAGE THE PHOTOGRAPHS TRAVEL ONCE. A lazy map's cards name their two
# species and the card script fills them in from a dictionary sent with the
# widget. These pin that the cards carry no figure markup, that the dictionary
# covers every id they name, and that the payload actually shrank - the 2.5 MB
# a full map used to send is the reason this exists.
ref_cards <- vapply(seq_len(nrow(map_pts)), function(i)
  fw_map_popup(map_pts[i, ], d$species, detail = "lazy", thumbs = thumbs,
               thumb_ref = TRUE), character(1))
ok("hover: a by-reference card carries no figure markup",
   any(grepl("fw-species-figure", ref_cards, fixed = TRUE)), FALSE)
ok("hover: but still holds both slots, by id",
   all(grepl('data-fw-figures="2" data-fw-thumbs="', ref_cards, fixed = TRUE)))
ref_ids <- unlist(strsplit(sub('.*data-fw-thumbs="([^"]*)".*', "\\1", ref_cards),
                           FW_POPUP_SEP, fixed = TRUE))
ok("hover: every id a card names is in the whole-database dictionary",
   all(setdiff(ref_ids, "") %in% names(fw_map_thumbs_all(d))))
full_map <- fw_add_attempt_markers(leaflet::leaflet(), d, all_sel,
                                   detail = "lazy", detail_input = "x")
full_json <- htmlwidgets:::toJSON(htmlwidgets:::createPayload(full_map))
# 2.49 MB with the figures inline, 1.47 MB by reference on 16 September 2026.
ok("map: a full lazy map is under 1.6 MB", nchar(full_json, "bytes") < 1.6e6)
ok("map: and hands its dictionary to the card script",
   length(full_map$jsHooks$render[[1]]$data$thumbs), length(thumbs))
ok("map: an embedded map hands none, its cards carry their own",
   is.null(fw_plan_map(d, a[1:20, ], detail = "embed")$jsHooks$render[[1]]$data))
ok("hover: the card script fills a by-reference slot",
   grepl("fillThumbs(body)", fw_map_card_js("x"), fixed = TRUE))

# THE YEARS ARE ON THE CARD, at the client's request - a reader deciding whether
# to open a record wants to know whether it is from this decade or the eighties.
dated <- map_pts[!is.na(map_pts$start_year), ][1, ]
ok("hover: the card names the years",
   grepl(paste0(">", fw_popup_years(dated$start_year, dated$end_year), "<"),
         fw_map_popup(dated, d$species, detail = "lazy"), fixed = TRUE))

# "SELECT FOR THE FULL RECORD" IS A BUTTON NOW, not a line of quiet text.
ok("hover: the open affordance is styled as a button",
   all(grepl("fw-popup__more-btn", cards, fixed = TRUE)))
# A REAL <button>, AND THE ONLY CLICK TARGET IN THE CARD. The client asked for
# the record to open from the marker or this button, not the whole card, so
# the button carries the data-fw-open hook the card script listens for.
ok("hover: and is a real button carrying the open hook",
   all(grepl('<button type="button" class="fw-popup__more-btn" data-fw-open>',
             cards, fixed = TRUE)))
# The card script opens only from that hook, and through the opener of the map
# that showed the card - a listener wired once with the first map's opener
# sent Explore's clicks to the Plan page's input.
card_js <- fw_map_card_js("x-map_detail")
ok("hover: the card listens for the button, not for any click",
   grepl("closest('[data-fw-open]')", card_js, fixed = TRUE))
ok("hover: and opens through the showing map's own opener",
   grepl("card.fwOpen = panelOpen", card_js, fixed = TRUE) &&
     grepl("card.fwOpen(card.fwLayer)", card_js, fixed = TRUE))

# THE fw_record_neighbour() TESTS USED TO SIT HERE - five of them, covering
# wrapping at both ends and the two unanswerable questions. The function went
# with the previous/next buttons it served.

# THE CARD SCRIPT MUST BE WIRED EVEN WHEN THE MAP IS EMPTY. It is what puts the
# record panel on <body>, and the list can open a record for an attempt that has
# no coordinates - Pakistan's single attempt is exactly that case. Without the
# onRender on the empty path the panel never exists and no card opens.
empty_map <- fw_add_attempt_markers(leaflet::leaflet(), d, a[0, ],
                                    detail = "lazy", detail_input = "x")
ok("explore: an empty map still installs the record panel",
   length(empty_map$jsHooks$render), 1L)
ok("explore: and still falls back to the world view",
   !is.null(empty_map$x$setView))
ok("explore: a populated map installs it too",
   length(fw_add_attempt_markers(leaflet::leaflet(), d, a[1:5, ],
                                 detail = "lazy", detail_input = "x")$jsHooks$render), 1L)

# THE CARD RENDERER IS GONE, and so is the table that replaced it. The
# assertions that used to live here - one button per card, the placeholder for a
# species with no photograph, the map link only where there are coordinates -
# have nowhere left to point: the Explore page renders no list at all.

# The module, driven.
exp_kpis <- exp_sum <- NA_character_
exp_eu <- NA_character_
eu_n <- 0L
au_n <- 0L
try(testServer(mod_explore_server, args = list(data = d, in_review = 7L), {
  exp_kpis  <<- strip(output$kpis)
  exp_sum   <<- strip(output$summary)
  # An attempt with no coordinates has no marker, so nothing on the page can
  # reach it any more - but it is still IN the selection, which is what keeps
  # it in the counts and the charts. Asking for its record must not error.
  ok("explore: an unlocated attempt is still in the selection",
     no_coord_id %in% sel()$attempt_id)
  session$setInputs(map_detail = no_coord_id)
  session$setInputs(continent = "Europe")
  exp_eu <<- strip(output$summary)
  eu_n <<- nrow(sel())
  # The linkage itself is asserted against fw_geo_allowed() below - a mock
  # session does not expose the update messages an observer sends, and the
  # arithmetic is the part worth pinning anyway.
  session$setInputs(continent = character(0), country = "Australia")
  au_n <<- nrow(sel())
}), silent = TRUE)

ok("explore: the database panel renders", !is.na(exp_kpis) && nchar(exp_kpis) > 50)
ok("explore: the strip counts the whole database unfiltered",
   grepl(paste0(fw_fmt_num(nrow(a)), " attempts"), exp_sum, fixed = TRUE))
# THE TABLE'S ASSERTIONS ARE GONE WITH THE TABLE. They covered one row per
# attempt, the credit line on every thumbnail, the map link only where there
# were coordinates, and both species columns on every row. Nothing on the
# Explore page renders any of that now; the record panel's own markup is
# asserted further up, against fw_record_detail_html().

# ---- The geography filters, pinned ------------------------------------------
#
# THE RECOMPUTING BUG THIS GUARDS AGAINST. The client reported the summary strip
# disagreeing with the country and continent filters. What was actually wrong
# was that the two filters could contradict each other - Europe AND Australia
# matches nothing, correctly - and a blank page is indistinguishable from a
# broken one. The linkage in mod_explore.R makes that unreachable; these pin the
# arithmetic underneath it so a future change to either cannot drift.
#
# Counted from the attempt table directly, never from the filter engine, so this
# cannot agree with itself.
geo_ids <- fw_filter_ids(drop = setdiff(names(FW_FILTERS), FW_EXPLORE_FILTERS))
geo_apply <- function(...) {
  st <- list(...); st$.ids <- geo_ids
  fw_filter_apply(d, st)
}
one_country <- names(sort(table(a$country), decreasing = TRUE))[1]
one_cont <- a$continent[a$country == one_country][1]
sel_c <- geo_apply(country = one_country)
sel_k <- geo_apply(continent = one_cont)
ok("geo: a country filter selects exactly that country's attempts",
   nrow(sel_c), sum(a$country == one_country))
ok("geo: and the strip counts one country",
   fw_plan_summary(d, sel_c)$countries, 1L)
ok("geo: a continent filter selects exactly that continent's attempts",
   nrow(sel_k), sum(a$continent == one_cont))
ok("geo: and the strip counts the countries actually in the selection",
   fw_plan_summary(d, sel_k)$countries,
   dplyr::n_distinct(a$country[a$continent == one_cont]))
# The two together are an AND, and a country inside its own continent must not
# be narrowed away by it.
ok("geo: continent AND its own country is the country",
   nrow(geo_apply(continent = one_cont, country = one_country)), nrow(sel_c))
# The contradiction still resolves to nothing - the linkage prevents a reader
# REACHING it, it does not change what the engine means.
other_cont <- setdiff(unique(a$continent), one_cont)[1]
ok("geo: a country outside the chosen continent still matches nothing",
   nrow(geo_apply(continent = other_cont, country = one_country)), 0L)
# THE STRIP NEVER COUNTS AN ABSENT COUNTRY. n_distinct() counts NA as a level,
# so a blank country column would inflate this the moment one appeared.
ok("geo: every attempt has a country and a continent",
   sum(is.na(a$country) | is.na(a$continent)), 0L)

# ---- The linkage, as arithmetic ---------------------------------------------
#
# What the two observers in mod_explore.R compute. Pinned here rather than
# through the module because a mock session does not expose the update messages
# they send, and this is the part that would be wrong if either were.
geo_pairs <- unique(a[, c("continent", "country")])
ok("link: choosing a continent narrows the country picker",
   fw_geo_allowed(geo_pairs, "continent", "country", "Europe", ch_all$country),
   sort(unique(a$country[a$continent == "Europe"])))
ok("link: choosing a country narrows the continent picker",
   fw_geo_allowed(geo_pairs, "country", "continent", "Australia", ch_all$continent),
   unique(a$continent[a$country == "Australia"]))
# SEVERAL CONTINENTS IS A UNION, not an intersection - the picker is multi-select
# and two continents must offer the countries of both.
two <- c("Europe", "Oceania")
ok("link: two continents offer the countries of both",
   fw_geo_allowed(geo_pairs, "continent", "country", two, ch_all$country),
   sort(unique(a$country[a$continent %in% two])))
# CLEARING RESTORES THE FULL LIST. If an empty selection narrowed to nothing,
# clearing the continent would leave the country picker permanently empty -
# which is the failure mode this whole linkage exists to avoid.
ok("link: clearing the continent restores every country",
   fw_geo_allowed(geo_pairs, "continent", "country", character(0), ch_all$country),
   ch_all$country)
ok("link: and a NULL selection is treated the same as an empty one",
   fw_geo_allowed(geo_pairs, "continent", "country", NULL, ch_all$country),
   ch_all$country)
# The narrowed list is always a subset of what the picker offered to begin with,
# so the linkage can never invent a country the filter engine cannot match.
ok("link: the narrowed list never leaves the picker's own choices",
   all(fw_geo_allowed(geo_pairs, "continent", "country", "Europe", ch_all$country)
       %in% ch_all$country))

ok("explore: filtering to Europe narrows the strip", eu_n, sum(a$continent == "Europe"))
ok("explore: and the strip says so",
   grepl(paste0(fw_fmt_num(sum(a$continent == "Europe")), " attempts"), exp_eu, fixed = TRUE))

# ==============================================================================
cat("\n-- chart maths --\n")

# Cumulative attempts, stacked by outcome.
dated <- all_sel[!is.na(all_sel$start_year), ]
tr <- traces(fw_chart_cumulative(all_sel))
finals <- 0L
for (t in tr) {
  o <- t$name
  ok(paste("cumulative: final total for", o), tail(t$y, 1),
     sum(outcome_of[as.character(dated$attempt_id)] == o))
  ok(paste("cumulative: never decreases for", o), all(diff(t$y) >= 0))
  finals <- finals + tail(t$y, 1)
}
ok("cumulative: outcomes sum to every dated attempt", finals, nrow(dated))

# Outcome by method, count and share.
me <- am1[am1$attempt_id %in% as.character(all_sel$attempt_id), ]
me$outcome <- outcome_of[me$attempt_id]
tr <- traces(fw_chart_method(d, all_sel, "count"))
mism <- 0L; hidden_ok <- TRUE
for (t in tr) {
  for (i in seq_along(t$y)) {
    mname <- label_name(t$y[i])
    if (sum(me$method == mname & me$outcome == t$name) != t$x[i]) mism <- mism + 1L
    share <- 100 * t$x[i] / label_n(t$y[i])
    if ((t$text[i] == "") != (share < FW_CHART$label_min_share)) hidden_ok <- FALSE
  }
}
ok("method: every segment is the direct count", mism, 0L)
ok("method: in-bar count hidden exactly under the share floor", hidden_ok)
# THE HOVER, in both modes. It once read the in-bar label, which is blank under
# the share floor, so the narrow segments - the ones a reader hovers to find
# out about - said "Unknown:  of 567". The count and the total are recomputed
# here and the hover string has to carry both, hidden label or not.
# AND IT IS THE SPECIES TILE'S SENTENCE, IN BOTH MODES (client, 24 Sept 2026):
# "Successful: 25% (3 of 12)", the same plan$r_tile_seg the tiles fill, so the
# reader meets one wording wherever they hover. The mode no longer changes it.
# Recomputed here from the raw rows, so the assertion cannot be satisfied by
# the chart reading its own drawn value.
#
# THE OUTCOME IS PART OF THE ASSERTION, and deliberately. It is a loop variable
# on the R side, and a ~formula would have plotly resolve it after the loop had
# finished - giving every trace the last outcome. That bug is invisible unless
# something checks the name against the trace it came from.
hover_want <- function(t, i, rows, group, name_of, mode = "count") {
  mname <- label_name(t$y[i])
  n <- sum(group == mname & name_of == t$name)
  total <- sum(group == mname)
  fw_fill(fw_t("plan", "r_tile_seg"), outcome = t$name,
          pc = sprintf("%.0f", round(100 * n / total)),
          n = fw_fmt_num(n), total = fw_fmt_num(total))
}
for (mode in c("count", "share")) {
  tr <- traces(fw_chart_method(d, all_sel, mode))
  all_ok <- TRUE; hidden_ok <- TRUE; n_hidden <- 0L
  for (t in tr) for (i in seq_along(t$y)) {
    want <- hover_want(t, i, me, me$method, me$outcome, mode)
    if (t$customdata[i] != want) all_ok <- FALSE
    if (t$text[i] == "") { n_hidden <- n_hidden + 1L; if (t$customdata[i] != want) hidden_ok <- FALSE }
    # plotly_build() repeats the template per point; one is enough to read.
    if (!grepl("%{customdata}", t$hovertemplate[1], fixed = TRUE) ||
        grepl("%{text}", t$hovertemplate[1], fixed = TRUE)) all_ok <- FALSE
  }
  ok(sprintf("method (%s): every hover is the species tile's sentence, outcome included",
             mode), all_ok)
  ok(sprintf("method (%s): the %d hidden-label segments still hover a count", mode, n_hidden),
     hidden_ok && n_hidden > 0L)
}
labels <- unique(unlist(lapply(tr, function(t) as.character(t$y))))
ok("method: the (n) in each label is the method total",
   all(vapply(labels, function(l) label_n(l) == sum(me$method == label_name(l)), logical(1))))
tr_share <- traces(fw_chart_method(d, all_sel, "share"))
sums <- tapply(unlist(lapply(tr_share, `[[`, "x")),
               unlist(lapply(tr_share, function(t) as.character(t$y))), sum)
ok("method: shares sum to 100 per method", all(abs(sums - 100) < 1e-9))

# THE IN-BAR LABEL FOLLOWS THE MODE. It used to print the count in both, so a
# 100% stacked bar carried numbers that summed to the method's total instead of
# to the 100% its own axis promised. A reader trusting the numbers over the axis
# read the chart backwards, which is the worst way for a chart to be wrong.
label_text <- function(tr) {
  x <- unlist(lapply(tr, `[[`, "text"))
  x[!is.na(x) & nzchar(x)]
}
ok("method: share mode labels every visible segment as a percentage",
   all(grepl("%$", label_text(tr_share))))
ok("method: and there is at least one to check",
   length(label_text(tr_share)) > 0)
# tr is the SHARE traces here - the loop above leaves it on its last mode - so
# the count chart is built fresh rather than reusing a name that moved.
tr_count <- traces(fw_chart_method(d, all_sel, "count"))
ok("method: count mode labels stay counts",
   all(grepl("^[0-9]+$", label_text(tr_count))))

# THE VERTICAL RULES ARE DRAWN OVER THE BARS (client, 23 Sept 2026: "add light
# grey vertical lines for each %"). They have to be SHAPES: plotly draws every
# gridline under every trace, and a 100% stack spans the whole axis, so the
# share view's grid was painted over end to end and showed only in the gaps
# between rows. xaxis.layer = "above traces" does not fix it - by plotly's own
# definition it lifts the axis line and the labels and leaves the grid where it
# was. What is pinned here is that the shapes exist, that they sit above the
# data, and that every one of them lands on a tick the axis labels.
bar_layout <- function(p) plotly::plotly_build(p)$x$layout
for (mode in c("count", "share")) {
  for (nm in c("method", "waterbody")) {
    lay <- bar_layout(if (nm == "method") fw_chart_method(d, all_sel, mode)
                      else fw_chart_waterbody(all_sel, mode))
    want_at <- if (mode == "share") seq(0, 100, FW_CHART$share_dtick) else 0
    ok(sprintf("%s (%s): one rule per labelled break", nm, mode),
       length(lay$shapes), length(want_at))
    ok(sprintf("%s (%s): each rule is at a break the axis names", nm, mode),
       vapply(lay$shapes, function(s) s$x0, numeric(1)), want_at)
    ok(sprintf("%s (%s): and drawn above the bars, full height", nm, mode),
       all(vapply(lay$shapes, function(s)
         identical(s$layer, "above") && identical(s$yref, "paper") &&
           identical(s$line$color, FW_COLOURS$border), logical(1))))
    # THE AXIS DRAWS NO GRID OF ITS OWN IN SHARE MODE, or every rule would have
    # a half-hidden twin under the bars. The count view keeps its gridlines:
    # its bars stop short of the right-hand edge, so they read there, and only
    # the rule at zero needed adding.
    ok(sprintf("%s (%s): the axis grid is off exactly in share mode", nm, mode),
       identical(lay$xaxis$showgrid, FALSE), mode == "share")
    if (mode == "share") {
      ok(sprintf("%s: the share axis states its own tick spacing", nm),
         lay$xaxis$dtick, FW_CHART$share_dtick)
    } else {
      ok(sprintf("%s: the count grid is the interface hairline grey", nm),
         lay$xaxis$gridcolor, FW_COLOURS$border)
    }
  }
}
# THE TWO "OTHER" METHODS SIT AT THE BOTTOM whatever their counts. plotly draws
# the first category at the bottom, so they must come FIRST in the level order.
# Asserted on the ids in FW_METHOD_OTHER rather than the display names, which is
# the whole reason that constant is held as ids.
lv <- levels(tr_count[[1]]$y)
other_names <- d$method$method_name[match(FW_METHOD_OTHER, d$method$method_id)]
other_names <- other_names[!is.na(other_names)]
is_other_lv <- vapply(lv, function(l) label_name(l) %in% other_names, logical(1))
ok("method: the Other methods are pinned to the bottom of the order",
   all(which(is_other_lv) <= sum(is_other_lv)))
ok("method: and Other mechanical sits below Other chemical",
   identical(label_name(lv[1]), "Other mechanical"))
# Everything that is NOT an Other is still ordered by frequency, ascending.
real_lv <- lv[!is_other_lv]
ok("method: the real methods are still ordered by frequency",
   !is.unsorted(vapply(real_lv, label_n, numeric(1))))

# Duration. SINGLE-METHOD ATTEMPTS ONLY - fw_duration_sel() drops any attempt
# that records more than one method, because duration_days belongs to the
# attempt and this chart puts it against a method. The expected frame is built
# the same way, from the bridge rather than from the chart, so a change to the
# filter shows up here as a failure rather than as agreement with itself.
dur <- stats::setNames(all_sel$duration_days, as.character(all_sel$attempt_id))
n_methods <- table(unique(d$attempt_method[, c("attempt_id", "method_id")])$attempt_id)
solo <- names(n_methods)[n_methods == 1L]
dm <- me; dm$dur <- dur[dm$attempt_id]
dm <- dm[!is.na(dm$dur) & dm$dur > 0 & dm$attempt_id %in% solo, ]
tr <- traces(fw_chart_duration(d, all_sel))
box <- Filter(function(t) identical(t$type, "box"), tr)[[1]]
pts <- Filter(function(t) identical(t$type, "scatter"), tr)
# THE FILTER COSTS REAL ROWS and the test says how many rather than leaving it
# to be discovered. Multi-method attempts are a third of those with a duration.
ok("duration: the filter drops the multi-method attempts",
   nrow(dm) < sum(!is.na(dm$dur)) || nrow(dm) < sum(!is.na(dur) & dur > 0))
ok("duration: the box holds every positive single-method duration",
   length(box$x), nrow(dm))
ok("duration: one point per (attempt, method) with a duration",
   sum(vapply(pts, function(t) length(t$x), integer(1))), nrow(dm))
ok("duration: per-outcome point counts",
   all(vapply(pts, function(t) length(t$x) == sum(dm$outcome == t$name), logical(1))))
dur_y <- plotly::plotly_build(fw_chart_duration(d, all_sel))$x$layout$yaxis
dur_rows <- as.character(dur_y$ticktext)
ok("duration: the (n) in each label counts durations, not attempts",
   all(vapply(dur_rows, function(l) label_n(l) == sum(dm$method == label_name(l)), logical(1))))
# THE DOTS ARE SPREAD ACROSS THEIR ROW, and never far enough to be read as the
# next one. Each dot's row is recovered from its y by rounding, and the method
# on that row must be the dot's own - checked against the per-method counts,
# recomputed from dm rather than read from the chart.
sw <- FW_CHART$duration_swarm
pt_y <- unlist(lapply(pts, function(t) t$y))
ok("duration: every dot is within its row's spread",
   all(abs(pt_y - round(pt_y)) <= sw$spread + 1e-9))
ok("duration: dots per row equal that method's durations",
   as.integer(table(factor(round(pt_y), levels = seq_along(dur_rows)))),
   vapply(dur_rows, function(l) sum(dm$method == label_name(l)), integer(1)))
ok("duration: identical durations are drawn apart, not on top of each other",
   {
     key <- paste(round(pt_y), unlist(lapply(pts, function(t) t$x)))
     dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
     !any(duplicated(paste(key, pt_y))) && any(dup)
   })
ok("duration: the dots carry no hover",
   all(vapply(pts, function(t) all(t$hoverinfo == "skip"), logical(1))))
# THE ROW ORDER IS THE BAR CHARTS' RULE (client, 23 Sept 2026): ordered by the
# label's own n, with the two "Other" methods pinned to the bottom whatever
# their counts. Asserted here the way the method chart's order is asserted
# above - on the ids in FW_METHOD_OTHER, not on the display names, which is the
# whole reason that constant is held as ids. dur_rows is bottom to top.
dur_other <- d$method$method_name[match(FW_METHOD_OTHER, d$method$method_id)]
dur_other <- dur_other[!is.na(dur_other)]
dur_is_other <- vapply(dur_rows, function(l) label_name(l) %in% dur_other, logical(1))
ok("duration: the Other methods are pinned to the bottom of the order",
   all(which(dur_is_other) <= sum(dur_is_other)))
ok("duration: and every other row is still ordered by how many durations it has",
   label_n(dur_rows[!dur_is_other]), sort(label_n(dur_rows[!dur_is_other])))
ok("duration: the pinning is doing something in this selection",
   sum(dur_is_other) > 0L)
# A single-method attempt contributes exactly one (attempt, method) row, so the
# points and the attempts drawn are the same number. That is the whole point of
# the filter: every point on this chart is a duration of the method it sits on.
ok("duration: one point per attempt, because each has one method",
   nrow(dm), nrow(fw_duration_sel(d, all_sel)))

# THE BACKGROUND IS PLAIN NOW, and these used to be four assertions about the
# alternating tinted bands that stood behind the data. The client replaced them
# with a dotted line on each unit break, so what is pinned here is their
# absence: a chart that quietly regrows shapes is the fault these replaced.
dur_shapes <- plotly::plotly_build(fw_chart_duration(d, all_sel))$x$layout$shapes
ok("duration: nothing is drawn behind the data", length(dur_shapes), 0L)

# THE AXIS RANGE IS THE ONE THING THAT IS log10, and it is set explicitly
# because plotly's autorange for a horizontal box trace on a log scale returned
# 10^-67.5 to 10^8.1 - which squashed every point into a sliver at one edge and
# left the rest of the chart as one large empty panel.
dur_x <- plotly::plotly_build(fw_chart_duration(d, all_sel))$x$layout$xaxis
ok("duration: the axis sets its own range rather than letting plotly guess",
   length(dur_x$range), 2L)
ok("duration: and that range is in log10, close around the data",
   {
     span <- log10(range(fw_duration_sel(d, all_sel)$duration_days))
     dur_x$range[1] < span[1] && dur_x$range[2] > span[2] &&
       (span[1] - dur_x$range[1]) < 1 && (dur_x$range[2] - span[2]) < 1
   })
# THE TICKS ARE THE THIRD SPACE: data units, logged by plotly itself. Wrapping
# them in log10() bunches them into the left tenth of the axis and loses "1 day"
# entirely, because log10(0) is -Inf.
dur_days <- fw_duration_sel(d, all_sel)$duration_days
ok("duration: the tick values are day counts, not log10 of them",
   identical(dur_x$tickvals, fw_duration_ticks(dur_days)$vals))
# THE AXIS REACHES THE LAST DOT (client, 23 Sept 2026). The fixed breaks stop
# at ten years, so a longer selection used to draw dots past the final label.
ok("duration: the last tick is the longest attempt drawn",
   dur_x$tickvals[length(dur_x$tickvals)], max(dur_days))
ok("duration: and every tick is labelled",
   length(dur_x$tickvals), length(dur_x$ticktext))
# No fixed break is left sitting past the data, where it would label empty
# axis, and none is left crowding the terminal tick.
ok("duration: no tick sits beyond the data", all(dur_x$tickvals <= max(dur_days)))
ok("duration: no two ticks overprint",
   all(diff(log10(dur_x$tickvals)) >= FW_CHART$duration_tick_gap))
# NO AXIS TITLE (client, 23 Sept 2026): the named ticks say what it measures.
ok("duration: the x axis carries no title", dur_x$title, "")
# THE UNIT BREAKS, which is what replaced the bands. They are the axis's own
# gridlines rather than shapes, so they land on tickvals and nowhere else - one
# dotted vertical on a day, a week, a month, a year, five years and ten.
ok("duration: the unit breaks are dotted", dur_x$griddash, "dot")
# THE CLIENT COULD NOT SEE THEM at the border grey and 1px, so they are pinned
# to the ink and the configured weight.
ok("duration: and drawn in the off-black ink", dur_x$gridcolor, FW_COLOURS$ink)
ok("duration: at the configured weight", dur_x$gridwidth, FW_CHART$duration_grid)
ok("duration: and they are drawn at all", isTRUE(dur_x$showgrid))
# tickmode "array" is what confines the gridlines to the named ticks. Without it
# plotly picks its own decades and the breaks stop being units.
ok("duration: the ticks are the only thing the axis draws a line at",
   dur_x$tickmode, "array")
# No horizontal rules through the boxes: the y axis is method names, so a
# gridline there is a reference to nothing.
ok("duration: the method axis draws no gridlines",
   isFALSE(plotly::plotly_build(fw_chart_duration(d, all_sel))$x$layout$yaxis$showgrid))

# EVERY CHART IS A PICTURE, NOT A CANVAS (Sept 2026 user testing): the key
# cannot switch a series off, nothing zooms or pans, the only button is
# plotly's own PNG camera, and the PNG is print-ready at FW_CHART$export_dpi.
# Checked on every chart builder the app draws, through fw_plotly_style().
all_charts <- list(
  methods = fw_chart_method(d, all_sel, "count"),
  duration = fw_chart_duration(d, all_sel),
  waterbody = fw_chart_waterbody(all_sel, "count"),
  cumulative = fw_chart_cumulative(all_sel)
)
for (nm in names(all_charts)) {
  bx <- plotly::plotly_build(all_charts[[nm]])$x
  ok(paste0("plotly ", nm, ": legend entries cannot be toggled"),
     isFALSE(bx$layout$legend$itemclick) && isFALSE(bx$layout$legend$itemdoubleclick))
  ok(paste0("plotly ", nm, ": axes cannot be zoomed or panned"),
     isTRUE(bx$layout$xaxis$fixedrange) && isTRUE(bx$layout$yaxis$fixedrange) &&
       isFALSE(bx$layout$dragmode) && isFALSE(bx$config$scrollZoom) &&
       isFALSE(bx$config$doubleClick))
  ok(paste0("plotly ", nm, ": the camera is the only button"),
     unlist(bx$config$modeBarButtons), "toImage")
  ok(paste0("plotly ", nm, ": PNG export at ", FW_CHART$export_dpi, " dpi"),
     bx$config$toImageButtonOptions$scale * 96, FW_CHART$export_dpi)
  ok(paste0("plotly ", nm, ": the button is plotly's default (hover)"),
     is.null(bx$config$displayModeBar))
}
# BARE AXES, WITH ROOM. The A/B test is over (Sept 2026): no chart draws an
# axis line, and every axis keeps its labels FW_CHART$tick_gap off the axis
# with ticks that are there only as spacing - long enough, and no colour.
for (nm in names(all_charts)) {
  bx <- plotly::plotly_build(all_charts[[nm]])$x$layout
  for (ax in c("xaxis", "yaxis")) {
    axl <- bx[[ax]]
    ok(paste0("axis ", nm, " ", ax, ": no axis line"), isTRUE(axl$showline), FALSE)
    ok(paste0("axis ", nm, " ", ax, ": labels ", FW_CHART$tick_gap, "px off the axis"),
       c(identical(axl$ticks, "outside"), identical(axl$ticklen, FW_CHART$tick_gap)),
       c(TRUE, TRUE))
    ok(paste0("axis ", nm, " ", ax, ": the spacing ticks are not drawn"),
       axl$tickcolor, FW_TRANSPARENT)
  }
}

# A category chart with Other.
wb <- all_sel$waterbody_type[!is.na(all_sel$waterbody_type)]
wb_out <- outcome_of[as.character(all_sel$attempt_id[!is.na(all_sel$waterbody_type)])]
totals <- sort(table(wb), decreasing = TRUE)
b <- plotly::plotly_build(fw_chart_waterbody(all_sel, "count"))
tr <- b$x$data
labels <- unique(unlist(lapply(tr, function(t) as.character(t$y))))
n_named <- min(FW_TOP_N, length(totals)); has_other <- length(totals) > FW_TOP_N
ok("category: top-n named plus Other", length(labels), n_named + has_other)
per_label <- tapply(unlist(lapply(tr, `[[`, "x")),
                    unlist(lapply(tr, function(t) as.character(t$y))), sum)
named <- per_label[!grepl(paste0("^", fw_t("charts", "other")), names(per_label))]
ok("category: named totals match the data",
   all(vapply(names(named), function(l) named[[l]] == totals[[label_name(l)]], logical(1))))
if (has_other) {
  ok("category: Other is the sum of the tail",
     unname(per_label[grepl(paste0("^", fw_t("charts", "other")), names(per_label))]),
     sum(totals[-seq_len(FW_TOP_N)]))
  ok("category: Other is drawn first (at the bottom)",
     grepl(paste0("^", fw_t("charts", "other")), b$x$layout$yaxis$categoryarray[1]))
}
wb_named <- if (has_other) ifelse(wb %in% names(totals)[seq_len(FW_TOP_N)], wb, fw_t("charts", "other")) else wb
ok("category: every segment is the direct count",
   all(unlist(lapply(tr, function(t) vapply(seq_along(t$y), function(i)
     sum(wb_named == label_name(t$y[i]) & wb_out == t$name) == t$x[i], logical(1))))))

# THE SHARE VIEW IS OF EACH BAR, NOT OF THE SELECTION. Every kind of water
# reaches 100%, which is what makes it the outcome mix rather than the
# selection's composition - the two read the same on a one-category chart and
# quite differently on a real one.
tr_wb_share <- plotly::plotly_build(fw_chart_waterbody(all_sel, "share"))$x$data
wb_sums <- tapply(unlist(lapply(tr_wb_share, `[[`, "x")),
                  unlist(lapply(tr_wb_share, function(t) as.character(t$y))), sum)
ok("category: shares sum to 100 per kind of water", all(abs(wb_sums - 100) < 1e-9))

# THE HOVER IS THE SPECIES TILE'S SENTENCE, IN BOTH MODES (client, 24 Sept
# 2026) - "Successful: 25% (3 of 12)". It used to read the drawn value, so
# share mode would have offered "Successful: 33.33333"; then it read the count
# only, and later the count with a percentage in share mode alone. One template
# now, plan$r_tile_seg. Both numbers are recomputed here from the attempts, and
# the outcome is checked against the trace it came from - see the note on the
# method chart's hover above for why that matters.
for (mode in c("count", "share")) {
  tr_h <- plotly::plotly_build(fw_chart_waterbody(all_sel, mode))$x$data
  all_ok <- TRUE
  for (t in tr_h) {
    if (grepl("%{x}", t$hovertemplate[1], fixed = TRUE)) all_ok <- FALSE
    for (i in seq_along(t$y)) {
      lab <- label_name(t$y[i])
      n <- sum(wb_named == lab & wb_out == t$name)
      total <- sum(wb_named == lab)
      want <- fw_fill(fw_t("plan", "r_tile_seg"), outcome = t$name,
                      pc = sprintf("%.0f", round(100 * n / total)),
                      n = fw_fmt_num(n), total = fw_fmt_num(total))
      if (!identical(as.character(t$customdata[i]), want)) all_ok <- FALSE
    }
  }
  ok(paste0("category ", mode, ": hover is the species tile's sentence"), all_ok)
}

# THE SEGMENTS CARRY LABELS IN BOTH MODES (client, 21 Sept 2026: the 100% view
# had none). Recomputed from the attempts: the count, or round(100 * n / bar
# total) with a %, and blank only under the share floor.
for (mode in c("count", "share")) {
  tr_l <- plotly::plotly_build(fw_chart_waterbody(all_sel, mode))$x$data
  all_ok <- TRUE; n_shown <- 0
  for (t in tr_l) {
    for (i in seq_along(t$y)) {
      lab <- label_name(t$y[i])
      n <- sum(wb_named == lab & wb_out == t$name)
      share <- 100 * n / sum(wb_named == lab)
      want <- if (share < FW_CHART$label_min_share) "" else
        if (mode == "share") paste0(round(share), "%") else as.character(n)
      if (!identical(as.character(t$text[i]), want)) all_ok <- FALSE
      if (nzchar(want)) n_shown <- n_shown + 1
    }
  }
  ok(paste0("category ", mode, ": segment labels match the recomputed values"),
     all_ok)
  ok(paste0("category ", mode, ": some segments are labelled"), n_shown > 0)
}

# The caption under the method chart: attempts with no method row at all.
ok("method: the no-method caption count",
   fw_n_no_method(d, all_sel),
   sum(!as.character(all_sel$attempt_id) %in% as.character(am$attempt_id)))

# A slice with a missing outcome still shows four.
oc <- fw_outcome_counts(all_sel[all_sel$outcome %in% c("Successful", "Failed"), ])
ok("outcome counts: four rows however empty", nrow(oc), 4L)
ok("outcome counts: the empty level is zero", oc$n[oc$outcome == "Ongoing"], 0L)

# ==============================================================================
cat("\n-- filter maths --\n")

ok("filters: nothing set returns every attempt", nrow(all_sel), nrow(a))
f <- base_all; f$method <- top_methods[1:2]
ok("filters: two methods is the union",
   nrow(fw_filter_apply(d, f)), length(unique(am1$attempt_id[am1$method %in% top_methods[1:2]])))
sp_lab <- fw_species_label(d$species)
top_sp <- names(counts)[1:2]
f <- base; f$species <- sp_lab$label[match(top_sp, sp_lab$species_id)]
ok("filters: two species is the union, invasive role only",
   nrow(fw_filter_apply(d, f)),
   length(unique(asp$attempt_id[asp$role == "invasive" & asp$species_id %in% top_sp])))
yr <- as.integer(names(sort(table(a$start_year), decreasing = TRUE))[1])
f <- base; f$year_from <- yr; f$year_to <- yr; f$include_no_year <- FALSE
ok("filters: a one-year range is inclusive at both ends",
   nrow(fw_filter_apply(d, f)), sum(a$start_year == yr, na.rm = TRUE))
f <- base; f$year_from <- yr - 5L; f$year_to <- yr + 5L; f$include_no_year <- FALSE
ok("filters: a range keeps both bounds",
   nrow(fw_filter_apply(d, f)), sum(a$start_year >= yr - 5L & a$start_year <= yr + 5L, na.rm = TRUE))
f <- base; f$include_no_year <- FALSE
ok("filters: the no-year toggle adds exactly the undated rows",
   nrow(all_sel) - nrow(fw_filter_apply(d, f)), sum(is.na(a$start_year)))
f <- base; f$taxa <- "No such kind of animal"
ok("filters: an impossible value empties the result", nrow(fw_filter_apply(d, f)), 0L)
ok("filters: the zero hint names only the filter that is set",
   fw_filter_zero_hints(d, f), fw_filter_label("taxa"))

# ==============================================================================
cat("\n-- exports and the report --\n")

export <- fw_export_frame(d, sel1$attempt_id)
ok("export: one row per selected attempt", nrow(export), nrow(sel1))
ok("export: every column, in order", names(export), FW_EXPORT_COLUMNS)

xlsx <- tempfile(fileext = ".xlsx")
# A PLAN-SHAPED filter state, because that is what the page hands the workbook.
# f1 above is the dashboard's shape - it carries a method, which this page no
# longer offers - and writing the sheet from it would record a filter the reader
# could not have set. The rows are the same selection either way.
f1_plan <- modifyList(base, list(country = unique(sel1$country)))
fw_write_workbook(xlsx, d, export, f1_plan, m)
sheet_names <- unname(unlist(fw_t("export", "sheets")))
ok("workbook: the four sheets, named from the copy", openxlsx::getSheetNames(xlsx), sheet_names)
ok("workbook: attempts sheet rows", nrow(openxlsx::read.xlsx(xlsx, sheet_names[1])), nrow(sel1))
# BY NAME, not by position. The sheet list has changed before, and an index
# that silently pointed at the wrong sheet is how this assertion stopped
# meaning anything the first time.
defs <- openxlsx::read.xlsx(xlsx, fw_t("export", "sheets")$definitions)
ok("workbook: dictionary is exactly the export columns",
   defs[[1]], FW_EXPORT_COLUMNS)
fs <- openxlsx::read.xlsx(xlsx, fw_t("export", "sheets")$filters)
# grepl, not %in%: size contributes one row per unit, each labelled
# "<label> (<unit>)", so an exact match would miss it.
ok("workbook: every report-builder filter is recorded",
   all(vapply(plan_ids, function(id) {
     any(grepl(fw_filter_label(id), fs[[1]], fixed = TRUE))
   }, logical(1))))
# f1 carries a method, which this page no longer filters on - so the sheet must
# NOT claim it did. A filter the reader could not have set has no business in
# the record of what they selected.
ok("workbook: a filter this page does not offer is not recorded",
   any(grepl(fw_filter_label("method"), fs[[1]], fixed = TRUE)), FALSE)
ok("workbook: the chosen countries are recorded",
   all(vapply(unique(sel1$country), function(c) {
     any(grepl(c, fs[[2]], fixed = TRUE))
   }, logical(1))))
wbk <- openxlsx::loadWorkbook(xlsx)
fills <- toupper(unique(unlist(lapply(wbk$styleObjects, function(s) s$style$fill$fillFg))))
ok("workbook: header fill is the teal text token",
   any(grepl(toupper(sub("^#", "", FW_COLOURS$teal_text)), fills)))

# ---- The PDF report's static twins ---------------------------------------
#
# THE SAME NUMBERS AS THE PAGE. Each ggplot twin in R/charts_static.R is drawn
# from the counting function its plotly original uses; this recomputes each
# total in base R and checks the page's traces and the PDF's drawn bars
# against it, in both modes, for the whole database and for one method's slice.
gg_total <- function(p) {
  if (is.null(p)) return(0)
  ld <- ggplot2::layer_data(p, 1)
  sum(ld$xmax - ld$xmin)
}
pl_total <- function(p) {
  if (is.null(p)) return(0)
  b <- plotly::plotly_build(p)$x$data
  sum(unlist(lapply(b, function(t) if (identical(t$type, "bar")) t$x else NULL)))
}
pairs_of <- function(s) {
  am_s <- am[as.character(am$attempt_id) %in% as.character(s$attempt_id), ]
  unique(paste(am_s$attempt_id, am_s$method_id))
}
for (nm in c("all", "slice")) {
  s <- if (nm == "all") all_sel else sel1
  n_pairs <- length(pairs_of(s))
  n_wb <- sum(!is.na(s$waterbody_type))
  ok(sprintf("pdf twin (%s): methods, page = recount", nm),
     pl_total(fw_chart_method(d, s, "count")), n_pairs)
  ok(sprintf("pdf twin (%s): methods, pdf = recount", nm),
     gg_total(fw_gg_method(d, s, "count")), n_pairs)
  ok(sprintf("pdf twin (%s): kind of water, page = pdf = recount", nm),
     c(pl_total(fw_chart_waterbody(s, "count")), gg_total(fw_gg_waterbody(s, "count"))),
     c(n_wb, n_wb))
  # In share mode every bar is 100, so the drawn total is 100 per bar.
  md <- fw_method_data(d, s, "share")
  if (!is.null(md)) {
    ok(sprintf("pdf twin (%s): share bars each reach 100", nm),
       gg_total(fw_gg_method(d, s, "share")), 100 * length(md$order_lv))
  }
  # One dot per single-method attempt with a duration, the page's caption count.
  dur <- fw_gg_duration(d, s)
  if (!is.null(dur)) {
    ok(sprintf("pdf twin (%s): one duration dot per counted attempt", nm),
       nrow(ggplot2::layer_data(dur, 3)), nrow(fw_duration_sel(d, s)))
  }
  # One dot per located attempt on the map.
  #
  # FOUND BY ITS DATA, NOT BY INDEX. The map gained an ocean layer and a lakes
  # layer on 23 Sept 2026 and the points moved from index 2 to index 4, which
  # this read as "176 dots" - the country count - and passed nothing. Every
  # layer here is a geom_sf, so the geom cannot tell them apart; the points
  # are the only layer whose data carries an outcome.
  mp <- fw_gg_map(d, s)
  pt_layer <- which(vapply(mp$plot$layers,
                           function(l) "outcome" %in% names(l$data), logical(1)))
  ok(sprintf("pdf twin (%s): the map has exactly one point layer", nm),
     length(pt_layer), 1L)
  ok(sprintf("pdf twin (%s): one map dot per located attempt", nm),
     nrow(ggplot2::layer_data(mp$plot, pt_layer)),
     sum(!is.na(s$latitude) & !is.na(s$longitude)))
}
# THE PDF FOLLOWS THE READER'S TOGGLES, each chart on its own: the axis title
# is the one the page shows in that mode.
ok("pdf twin: the methods chart follows its mode",
   c(fw_gg_method(d, sel1, "count")$labels$x, fw_gg_method(d, sel1, "share")$labels$x),
   c(fw_t("charts", "x_attempts"), fw_t("charts", "x_share")))

# ---- The PDF's size estimate ----------------------------------------------
#
# THE WARNING IS ONLY AS GOOD AS THE ESTIMATE. Rendered for real on three
# selections - the whole database, one country, one attempt - and the estimate
# must land within 30% of each file, or the picker warns about the wrong ones.
if (!fw_pdf_available()) {
  cat("  *** PDF SIZE ASSERTIONS SKIPPED: quarto not found (set QUARTO_PATH) ***\n")
} else {
  one_country <- names(sort(table(all_sel$country), decreasing = TRUE))[2]
  cases <- list(world = all_sel,
                country = all_sel[all_sel$country %in% one_country, ],
                single = all_sel[1, ])
  for (nm in names(cases)) {
    s <- cases[[nm]]
    out <- tempfile(fileext = ".pdf")
    fw_write_pdf_report(out, d, s, filters = base, meta = m)
    est <- fw_pdf_size_estimate(d, s)
    real_mb <- file.size(out) / 1e6
    real_pages <- fw_pdf_page_count(out)
    cat(sprintf("    %-8s %4d attempts: %.2f MB / %d pages, estimated %.2f MB / %d pages\n",
                nm, nrow(s), real_mb, real_pages, est$mb, est$pages))
    ok(sprintf("pdf size (%s): estimate within 30%% of the file", nm),
       abs(est$mb - real_mb) / real_mb <= 0.3, TRUE)
    ok(sprintf("pdf pages (%s): estimate within 30%% of the file", nm),
       abs(est$pages - real_pages) / real_pages <= 0.3, TRUE)
  }
}

ct <- fw_report_country_table(all_sel)
n_c <- length(unique(all_sel$country))
ok("country table: row count is the cap plus a remainder row",
   nrow(ct), min(n_c, FW_REPORT_COUNTRY_ROWS) + as.integer(n_c > FW_REPORT_COUNTRY_ROWS))
ok("country table: attempts sum to the selection",
   sum(as.integer(gsub(",", "", ct[[2]]))), nrow(all_sel))
ok("country table: headings from the copy",
   names(ct), c(fw_t("export", "col_country"), fw_t("export", "col_attempts")))
if (n_c > FW_REPORT_COUNTRY_ROWS) {
  ok("country table: the remainder row says how many it gathers",
     ct[[1]][nrow(ct)], fw_fill(fw_t("export", "other_countries"), n = n_c - FW_REPORT_COUNTRY_ROWS))
}

private <- d$contact$contact_email[!d$contact$email_public & !is.na(d$contact$contact_email)]
if (length(private)) {
  ok("redaction: a private address is refused",
     inherits(try(fw_assert_export_safe(data.frame(primary_contact_email = private[1]), d),
                  silent = TRUE), "try-error"))
}
ok("redaction: the real export passes", isTRUE(fw_assert_export_safe(export, d)))

q_txt <- fw_questions_text(ch_all)
required_keys <- c("site_name", "country", "water_regime", "waterbody_type", "start_year",
                   "driver", "method_main", "outcome", "contact_name", "contact_email")
req_labels <- vapply(required_keys, function(k) fw_t("contribute", "fields", k), character(1))
ok("question list (text): every required question is present",
   all(vapply(req_labels, grepl, logical(1), q_txt, fixed = TRUE)))
ok("question list (text): required questions are tagged",
   lengths(regmatches(q_txt, gregexpr(fw_t("questions", "required_text"), q_txt, fixed = TRUE))) >= length(required_keys))
ok("question list (text): long option lists are capped",
   grepl(sub("\\{n\\}.*$", "", fw_t("questions", "more_options")), q_txt, fixed = TRUE))
q_docx <- paste(officer::docx_summary(fw_questions_docx(ch_all))$text, collapse = "\n")
ok("question list (docx): every required question is present",
   all(vapply(req_labels, grepl, logical(1), q_docx, fixed = TRUE)))
ok("question list (docx): required questions are tagged",
   lengths(regmatches(q_docx, gregexpr(fw_t("questions", "required_docx"), q_docx, fixed = TRUE))) >= length(required_keys))

# ==============================================================================
cat("\n-- the map, and place --\n")

# A note with no method reaches the export and the popup as itself.
orphan_ids <- a$attempt_id[!a$attempt_id %in% am$attempt_id & !is.na(a$method_notes)]
full_export <- fw_export_frame(d)
ok("export: a method note with no method still travels",
   all(!is.na(full_export$method_notes[match(orphan_ids, full_export$attempt_id)])))
ok("export: and the methods column beside it is NA",
   all(is.na(full_export$methods[match(orphan_ids, full_export$attempt_id)])))
mp <- fw_map_points(d, a)
ok("map: every located attempt is a point", nrow(mp), sum(!is.na(a$latitude) & !is.na(a$longitude)))
# THE RECORD FRAME IS THE SAME FRAME. The map draws the located subset of it;
# the Explore list draws all of it. A record must not differ between the two.
recs <- fw_attempt_records(d, a)
ok("map: the record frame holds every attempt, located or not", nrow(recs), nrow(a))
ok("map: and the located ones are byte-identical to the map's",
   identical(as.data.frame(recs[match(mp$attempt_id, recs$attempt_id), ], row.names = NULL),
             as.data.frame(mp, row.names = NULL)))
# The orphan notes no longer reach the map: it lists method names only
# (client, 21 Sept 2026), and those notes stay in the export asserted above.
ok("map: an attempt with only a note lists no method",
   all(is.na(mp$method_names[match(intersect(orphan_ids, mp$attempt_id), mp$attempt_id)])))

# Cached figures are the figures. Rendering once per species must give the same
# panel as rendering per marker, or the cache has changed what the reader sees.
sample_rows <- head(which(!is.na(mp$inv_ids)), 5)
cache <- fw_map_figure_cache(d, mp[sample_rows, ])
ok("map: the figure cache holds every species in the sample",
   all(unique(unlist(lapply(mp$inv_ids[sample_rows], fw_popup_parts))) %in% names(cache)))
ok("map: a cached detail panel is byte-identical to a fresh one",
   all(vapply(sample_rows, function(i) identical(
     fw_map_detail_html(mp[i, ], d$species, figure_cache = cache),
     fw_map_detail_html(mp[i, ], d$species)), logical(1))))

# The two ways of carrying the record.
one <- mp[1, ]
ok("map: an embedded popup carries the detail template",
   grepl("<template class=\"fw-popup__detail\">", fw_map_popup(one, d$species, detail = "embed"), fixed = TRUE))
lazy <- fw_map_popup(one, d$species, detail = "lazy")
ok("map: a lazy popup carries no template", !grepl("<template", lazy, fixed = TRUE))
ok("map: but does carry the attempt id", grepl(paste0('data-fw-id="', one$attempt_id, '"'), lazy, fixed = TRUE))
ok("map: lazy popups are a fraction of embedded ones",
   nchar(lazy) * 3 < nchar(fw_map_popup(one, d$species, detail = "embed")))
ok("map: lazy markers need somewhere to report a click",
   inherits(try(fw_add_attempt_markers(leaflet::leaflet(), d, a, detail = "lazy"), silent = TRUE), "try-error"))
w <- fw_plan_map(d, a[1:20, ], detail = "embed")
calls <- vapply(w$x$calls, function(x) x$method, character(1))
ok("map: markers are added with cluster options", "addCircleMarkers" %in% calls &&
   !is.null(w$x$calls[[which(calls == "addCircleMarkers")]]$args[[which(vapply(w$x$calls[[which(calls == "addCircleMarkers")]]$args, function(z) is.list(z) && !is.null(z$maxClusterRadius), logical(1)))[1]]]))
# A selection on one coordinate (Belgium, Austria) fitted at infinite zoom and
# drew a grey map. Every fit carries the cap.
one_site <- fw_map_points(d, a[a$country == "Belgium", ])
ok("map: Belgium's attempts share one coordinate",
   nrow(unique(one_site[, c("latitude", "longitude")])), 1L)
fit <- fw_fit_points(leaflet::leaflet(), one_site)$x$fitBounds
ok("map: a fit is capped at FW_MAP$fit_max_zoom", fit[[5]]$maxZoom, FW_MAP$fit_max_zoom)
ok("map: the cap still shows a stack as one group",
   FW_MAP$fit_max_zoom < FW_MAP$cluster$fine_zoom)

# Place: the stored geography follows the ISO lookup.
iso <- fw_read_lookup("lookup_iso3166.csv")
mi <- match(a$country, iso$country)
ok("place: every country is in the ISO list", !any(is.na(mi)))
ok("place: iso3 is the lookup's for every row", all(a$iso3 == iso$iso3[mi]))
ok("place: continent is the lookup's for every row", all(a$continent == iso$continent[mi]))
ok("place: no region is itself a country", !any(a$region %in% iso$country))
ok("place: the lookup gives every country a continent", !any(is.na(iso$continent)))
ok("place: a row that breaks the rule stops the load",
   inherits(try(fw_validate_geography(transform(as.data.frame(a)[1, ], continent = "Asia")), silent = TRUE), "try-error"))
ok("place: a territory filed under its state stops the load",
   inherits(try(fw_validate_geography(transform(as.data.frame(a)[1, ], country = "United States", region = "Guam", iso3 = "USA", continent = "North America")), silent = TRUE), "try-error"))

# ==============================================================================
cat("\n-- welcome page --\n")

home_html <- as.character(mod_home_ui("home", fw_headline_stats(d)))
# Recomputed from the tables, not from fw_headline_stats().
ok_ids <- as.character(d$attempt$attempt_id)[as.character(d$attempt$outcome) %in% "Successful"]
as_ben <- d$attempt_species[as.character(d$attempt_species$role) == "beneficiary", ]
n_protected <- length(unique(as_ben$species_id[as.character(as_ben$attempt_id) %in% ok_ids]))
n_successful <- length(ok_ids)
ok("welcome: species protected = beneficiaries of successful attempts",
   fw_headline_stats(d)$protected, n_protected)
ok("welcome: successful attempts counted from the outcomes",
   fw_headline_stats(d)$successful, n_successful)
# Both figures in the sentence, each in its own bold span, attempts first.
kpi_html <- regmatches(home_html, regexpr('(?s)<p class="fw-home-kpi">.*?</p>', home_html, perl = TRUE))
kpi_bold <- regmatches(kpi_html, gregexpr("<strong>[^<]*</strong>", kpi_html))[[1]]
ok("welcome: the sentence is on the page", length(kpi_html), 1L)
# THE ">" IS INSIDE THE SECOND BOLD (client, 24 Sept 2026), so it is teal and
# in the numeric face with the figure it qualifies rather than in plain ink
# beside it.
ok("welcome: the sentence carries both figures in bold, attempts then species",
   kpi_bold, paste0("<strong>", c(fw_fmt_num(n_successful),
                                  paste0("&gt;", fw_fmt_num(n_protected))), "</strong>"))
ok("welcome: the sentence reads as the client wrote it",
   grepl(paste0("<strong>", fw_fmt_num(n_successful),
                "</strong> successful eradications recorded so far have protected <strong>&gt;",
                fw_fmt_num(n_protected), "</strong> species."), kpi_html, fixed = TRUE))
# The map's instruction is the caption under the map, in the map colours, and
# not in the bar.
cap_html <- regmatches(home_html, regexpr('(?s)<figcaption class="fw-compare__caption">.*?</figcaption>', home_html, perl = TRUE))
ok("welcome: the map line is under the map, both states in their map colours",
   c(length(cap_html) == 1L,
     grepl('<span class="fw-compare__now">successes (blue)</span>', cap_html, fixed = TRUE),
     grepl('<span class="fw-compare__later">opportunities (yellow)</span>', cap_html, fixed = TRUE)),
   c(TRUE, TRUE, TRUE))
ok("welcome: the bar holds only the sentence", grepl("fw-compare__", kpi_html, fixed = TRUE), FALSE)
# The title's ** pairs are bold, not literal asterisks.
title_html <- regmatches(home_html, regexpr('(?s)<h1 class="fw-page-header__title">.*?</h1>', home_html, perl = TRUE))
ok("welcome: the title's ** pairs render as bold",
   c(grepl("**", title_html, fixed = TRUE), grepl("<strong>", title_html, fixed = TRUE)),
   c(FALSE, TRUE))
ok("welcome: no more protected than beneficiaries overall",
   n_protected <= fw_headline_stats(d)$beneficiaries)
ok("welcome: no unfilled slot left in the page", !grepl("\\{[a-z_]+\\}", home_html))
ok("welcome: the tab is called The solution", fw_t("nav", "home"), "The solution")
ok("welcome: story copy and pictures are keyed alike, in order",
   names(fw_t("home", "stories")), names(FW_HOME_IMG$stories))
ok("welcome: stories run A-Z by continent",
   { cn <- vapply(fw_t("home", "stories"), `[[`, "", "continent"); identical(cn, sort(cn)) })
card_ids <- sub('.*id="', "", regmatches(home_html, gregexpr('id="fw-home-story-[a-z_]+" popover', home_html))[[1]])
card_ids <- sub('".*', "", card_ids)
opens <- unique(sub('.*="', "", sub('"$', "", regmatches(home_html, gregexpr('popovertarget="fw-home-story-[a-z_]+"', home_html))[[1]])))
ok("welcome: one card per story", card_ids, paste0("fw-home-story-", names(fw_t("home", "stories"))))
ok("welcome: one picture button per story",
   lengths(regmatches(home_html, gregexpr('class="fw-home-species"', home_html))),
   length(fw_t("home", "stories")))
ok("welcome: the page order names every story once",
   setequal(FW_HOME_ORDER, names(fw_t("home", "stories"))) && !anyDuplicated(FW_HOME_ORDER))
# The buttons, in the order they are in the page - the grid fills by column,
# so this is down the left then down the right.
tile_order <- sub(".*fw-home-story-", "", regmatches(home_html,
  gregexpr('class="fw-home-species" popovertarget="fw-home-story-[a-z_]+', home_html))[[1]])
ok("welcome: pictures run Apache, Valcheta, redfin | grebe, galaxias, mussel",
   vapply(tile_order, function(k) fw_t("home", "stories", k)$beneficiary$name, ""),
   c("Apache trout", "Valcheta frog", "Fiery redfin",
     "Little grebe", "Golden galaxias", "Freshwater pearl mussel"))
ok("welcome: every button opens a card that exists", setequal(opens, card_ids))
pics <- na.omit(unlist(c(FW_HOME_IMG$stories, FW_HOME_IMG$stories_named,
                         FW_HOME_IMG[c("map_now", "map_next")])))
ok("welcome: every picture named exists under www/", all(file.exists(file.path("www", pics))))
ok("welcome: every story has its beneficiary picture", !any(is.na(FW_HOME_IMG$stories)))
ok("welcome: placeholders drawn for each missing picture",
   lengths(regmatches(home_html, gregexpr('class="fw-story__placeholder"', home_html))),
   sum(is.na(FW_HOME_IMG$stories)))
ok("welcome: the cards show the beneficiary only",
   !grepl("fw-story__figure--invasive", home_html, fixed = TRUE) &&
     !grepl("_greyscale.png", home_html, fixed = TRUE))
ok("welcome: one picture per card",
   lengths(regmatches(home_html, gregexpr('class="fw-story__figure"', home_html))),
   length(fw_t("home", "stories")))

# THE TILE AND THE CARD SHOW DIFFERENT PLATES, and which way round matters.
# The card gets the lettered one because that is where the reader asked for the
# story; the strip keeps the unlettered drawing because six lettered plates at
# tile size are six pieces of unreadable text. Swapping them looks harmless in
# a diff and is the whole of this feature.
ok("welcome: every story card carries the lettered plate",
   all(vapply(FW_HOME_IMG$stories_named,
              function(p) grepl(paste0('src="', p, '"'), home_html, fixed = TRUE),
              TRUE)))
ok("welcome: the tile strip keeps the unlettered drawing",
   all(vapply(FW_HOME_IMG$stories,
              function(p) grepl(paste0('src="', p, '"'), home_html, fixed = TRUE),
              TRUE)))
# Both sets are in the page at once, so the lettered plates must not be fetched
# until a card opens. They are inside a closed popover, which is display:none,
# and it is loading="lazy" that stops the browser fetching them anyway.
ok("welcome: every story picture is lazy",
   lengths(regmatches(home_html, gregexpr('loading="lazy"', home_html))) >=
     length(FW_HOME_IMG$stories) + length(FW_HOME_IMG$stories_named))
nav_to <- regmatches(home_html, gregexpr("fw_nav_to&#39;,&#39;[a-z_]+", home_html))[[1]]
nav_to <- sub(".*&#39;", "", nav_to)
ok("welcome: header links go to explore, plan, contribute, networking",
   nav_to, c("explore", "plan", "contribute", "networking"))
app_src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
ok("welcome: every link target is a real tab",
   all(vapply(nav_to, function(v) grepl(paste0('value = "', v, '"'), app_src, fixed = TRUE), TRUE)))
ok("welcome: no swatch legend left", !grepl("fw-compare__swatch", home_html, fixed = TRUE))
ok("welcome: the caption names both map states in colour",
   grepl('class="fw-compare__now"', home_html, fixed = TRUE) &&
     grepl('class="fw-compare__later"', home_html, fixed = TRUE))
ok("welcome: the reveal starts fully on current work",
   grepl('value="0"', home_html) && grepl("--fw-pos: 0%", home_html, fixed = TRUE))

# ==============================================================================
cat("\n-- design values --\n")

rem <- function(x) as.numeric(sub("rem$", "", x))
# THE THREE EXEMPTIONS ARE NAMED, so a fourth cannot be added by accident: a
# new size_* token under 1rem fails this until someone writes it down here and
# says why. size_popup is for transient overlays; size_credit is the photo
# credits in the dashboard's attempts table; size_fine is the Welcome page's
# evidence footnote, the Mercator note under both maps, and the report's
# filters table on paper (client, 23 Sept 2026). All three are recorded at
# FW_TYPE in R/brand.R and in the floor note in www/scss/_tokens.scss.
FW_TYPE_FLOOR_EXEMPT <- c("size_popup", "size_credit", "size_fine")
sizes <- FW_TYPE[grepl("^size_", names(FW_TYPE)) &
                   !names(FW_TYPE) %in% FW_TYPE_FLOOR_EXEMPT]
ok("type: nothing on the page is under the 1rem floor", all(vapply(sizes, rem, numeric(1)) >= 1))
ok("type: the floor's exemptions are the three that are written down",
   sum(vapply(FW_TYPE[grepl("^size_", names(FW_TYPE))], rem, numeric(1)) < 1), 3L)
# AND THE EXEMPTION HAS EXACTLY THE CALLERS IT IS WRITTEN DOWN FOR. $fw-size-fine
# is the one token that may go under the floor on a page that stays put, so the
# stylesheet is checked for who actually uses it.
scss <- paste(vapply(list.files("www/scss", pattern = "[.]scss$", full.names = TRUE),
                     function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
                     character(1)), collapse = "\n")
# DECLARATIONS, not mentions: the token is named several times in the comments
# that explain it, and those are not callers.
fine_users <- length(gregexpr("font-size:\\s*\\$fw-size-fine", scss)[[1]])
ok("type: the fine size has the three page callers it is permitted", fine_users, 3L)
ok("type: the plotly floor is the rem floor in pixels", FW_TYPE$floor_px, 16L)
vars <- fw_sass_variables()
ok("tokens: no empty Sass variable", !any(vapply(vars, function(v) is.null(v) || is.na(v) || !nzchar(v), logical(1))))
ok("tokens: no white surface",
   !any(tolower(unlist(FW_COLOURS)) %in% c("#fff", "#ffffff")))
ok("charts: duration tick labels match the tick values",
   length(fw_t("charts", "duration_ticks")), length(FW_CHART$duration_ticks))
# THE PRINTED MAP'S OWN COLOURS. Four, all named, none of them white - the
# report's page is a tint and a white lake would read as a hole in it.
ok("map print: every colour is a hex value",
   all(grepl("^#[0-9a-fA-F]{6}$", unlist(FW_MAP_PRINT))))
ok("map print: none of them is white",
   !any(tolower(unlist(FW_MAP_PRINT)) %in% c("#fff", "#ffffff")))

cat("\n")
if (failures > 0L) stop(failures, " value assertion(s) failed", call. = FALSE)
cat("All value tests passed.\n")
