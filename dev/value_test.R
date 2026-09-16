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
ok("summary: methods",   ps$methods,
   length(unique(am$method_id[am$attempt_id %in% sel1$attempt_id])))
yrs <- sel1$start_year[!is.na(sel1$start_year)]
ok("summary: year span", ps$year_span, paste0(min(yrs), "-", max(yrs)))

# The footer.
ok("footer: last updated is the release date", fw_last_updated(d, m), as.Date(m$release))
pending <- attr(d, "status_counts")[["pending"]]
inbox <- tryCatch(fw_pending_submissions(), error = function(e) data.frame())
in_inbox <- if (nrow(inbox) == 0) 0L else if ("status" %in% names(inbox)) sum(inbox$status == "pending") else nrow(inbox)
ok("footer: in-review count", fw_review_count(d), as.integer(pending + in_inbox))

# About's scale sentence.
n_contrib <- sum(!is.na(d$contact$contact_name) & nzchar(d$contact$contact_name))
scale_txt <- strip(fw_about_scale(s, n_contrib, NS("x")))
ok("about: names the attempt count",  grepl(fw_fmt_num(nrow(a)), scale_txt, fixed = TRUE))
ok("about: names the contributor count", grepl(fw_fmt_num(n_contrib), scale_txt, fixed = TRUE))
ok("about: no placeholder left", !grepl("\\{[a-z_]+\\}", scale_txt))

# The caveats.
blocks <- fw_caveat_blocks(d)
n <- nrow(a)
successful <- sum(a$outcome == "Successful", na.rm = TRUE)
unverified <- sum(a$outcome == "Successful" &
                    (is.na(a$verification_notes) | a$verification_notes == ""), na.rm = TRUE)
no_size <- sum(is.na(a$area_treated)); no_start <- sum(is.na(a$start_year)); no_end <- sum(is.na(a$end_year))
pct <- function(x) paste0(round(100 * x / n), "%")
bodies <- vapply(blocks, `[[`, character(1), "body")
ok("caveats: successful count",  any(grepl(paste0(format(successful, big.mark = ","), " attempts recorded as successful"), bodies, fixed = TRUE)))
ok("caveats: unverified count",  any(grepl(paste0(format(unverified, big.mark = ","), " carry no verification"), bodies, fixed = TRUE)))
ok("caveats: no-size count and share",
   any(grepl(paste0(format(no_size, big.mark = ","), " attempts (", pct(no_size), ")"), bodies, fixed = TRUE)))
ok("caveats: no-start count and share",
   any(grepl(paste0(format(no_start, big.mark = ","), " (", pct(no_start), ")"), bodies, fixed = TRUE)))
ok("caveats: no-end count and share",
   any(grepl(paste0(format(no_end, big.mark = ","), " (", pct(no_end), ")"), bodies, fixed = TRUE)))
ok("caveats: no placeholder left", !any(grepl("\\{[a-z_]+\\}", bodies)))
ok("caveats: flat vector is heading, body, blank",
   length(fw_caveats(d)), 3L * length(blocks) - 1L)

# The About page renders end to end. Its section keys are built with paste0(),
# which dev/check_literals.R cannot see, so a missing key only shows up here.
about_html <- NA_character_
try(testServer(mod_about_server, args = list(data = d, meta = m), {
  about_html <<- as.character(output$body$html)
}), silent = TRUE)
ok("about: the page renders", !is.na(about_html) && nchar(about_html) > 1000)
# Every heading on the page, whether it is an always-visible section or the
# summary of one of the four disclosures. Add a section, add it here.
ok("about: every section heading is present",
   all(vapply(c("database", "signup", "cite", "caveats", "method", "stories",
                "related", "other", "images", "licence", "links"),
              function(k) grepl(fw_t("about", paste0(k, "_heading")), about_html, fixed = TRUE),
              logical(1))))
# The panels are click-to-open, and a <details> that lost its <summary> is a
# block of prose nobody can close.
ok("about: the four panels are disclosures",
   lengths(regmatches(about_html, gregexpr("fw-disclosure__summary", about_html)))[[1]], 4L)
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

ben_rows <- unique(data.frame(
  attempt_id = as.character(asp$attempt_id[asp$role == "beneficiary"]),
  species_id = asp$species_id[asp$role == "beneficiary"], stringsAsFactors = FALSE))

# The database panel. Never filtered, so these are the whole-table counts.
ok("explore db: beneficiary species", s$beneficiaries, length(unique(ben_rows$species_id)))
ok("explore db: latest year", s$latest_year, max(a$start_year, na.rm = TRUE))
db <- strip(fw_explore_db_panel(s, 7L))
for (v in list(c("attempts", nrow(a)), c("countries", length(unique(a$country))),
               c("invasive", s$species), c("beneficiaries", s$beneficiaries),
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
ok("explore db: the year span is filled in",
   grepl(paste0(min(a$start_year, na.rm = TRUE), " to ", max(a$start_year, na.rm = TRUE)), db, fixed = TRUE))
ok("explore db: no placeholder left", !grepl("\\{[a-z_]+\\}", db))

# Beneficiary taxa: the new filter, matched against the bridge directly.
taxa_of <- stats::setNames(d$species$taxa, d$species$species_id)
ben_taxa <- table(taxa_of[ben_rows$species_id])
pick_taxa <- names(sort(ben_taxa, decreasing = TRUE))[2]
want_taxa_ids <- unique(ben_rows$attempt_id[taxa_of[ben_rows$species_id] %in% pick_taxa])
fb <- fw_filter_state(list(), exp_ids); fb$taxa_beneficiary <- pick_taxa
ok(paste0("explore filter: beneficiary taxa '", pick_taxa, "' selects its attempts"),
   sort(fw_filter_apply(d, fb)$attempt_id), sort(want_taxa_ids))

# The sort orders. Undated attempts go last under either year order.
undated <- which(is.na(all_sel$start_year))
o_new <- fw_explore_order(all_sel, "newest")
o_old <- fw_explore_order(all_sel, "oldest")
ok("explore sort: newest starts at the latest year",
   all_sel$start_year[o_new[1]], max(a$start_year, na.rm = TRUE))
ok("explore sort: oldest starts at the earliest year",
   all_sel$start_year[o_old[1]], min(a$start_year, na.rm = TRUE))
ok("explore sort: undated go last under newest",
   all(tail(o_new, length(undated)) %in% undated))
ok("explore sort: and last under oldest too",
   all(tail(o_old, length(undated)) %in% undated))
ok("explore sort: every order is a permutation of the selection",
   all(vapply(FW_EXPLORE_SORTS, function(x)
     identical(sort(fw_explore_order(all_sel, x)), seq_len(nrow(all_sel))), logical(1))))

# AN ATTEMPT WITH NO COORDINATES STILL HAS A RECORD. It has no marker, so
# fw_map_points() cannot reach it; the list shows it and opening it must work.
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
              function(x) grepl(fw_t("species", "fig_none"), x, fixed = TRUE),
              logical(1))))
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

# THE YEARS ARE ON THE CARD, at the client's request - a reader deciding whether
# to open a record wants to know whether it is from this decade or the eighties.
dated <- map_pts[!is.na(map_pts$start_year), ][1, ]
ok("hover: the card names the years",
   grepl(fw_popup_years(dated$start_year, dated$end_year),
         fw_map_popup(dated, d$species, detail = "lazy"), fixed = TRUE))

# "SELECT FOR THE FULL RECORD" IS A BUTTON NOW, not a line of quiet text.
ok("hover: the open affordance is styled as a button",
   all(grepl("fw-popup__more-btn", cards, fixed = TRUE)))
# NOT a real <button>: the whole card is the click target, and a button inside
# it would swallow the click it advertises.
ok("hover: and is not a focusable control inside the card's own click target",
   any(grepl("<button", cards, fixed = TRUE)), FALSE)

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
  # sorted() is what previous/next steps through. THE LIST IT WAS NAMED FOR IS
  # GONE - the client removed the table - so this is now the only thing that
  # pins the order a reader walks records in, and it still has to be
  # most-recent-first rather than whatever order the filter left behind.
  ok("explore: the record order is newest first",
     sorted()$start_year[1], max(a$start_year, na.rm = TRUE))
  # An attempt with no coordinates has no marker, so nothing on the page can
  # reach it any more - but it is still IN the selection, which is what keeps
  # it in the counts and the charts. Asking for its record must not error.
  ok("explore: an unlocated attempt is still in the selection",
     no_coord_id %in% sorted()$attempt_id)
  session$setInputs(map_detail = no_coord_id)
  session$setInputs(map_detail_step = list(id = sorted()$attempt_id[1], step = -1L))
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
hover_of <- fw_t("charts", "hover_of")
hover_want <- function(t, i, rows, group, name_of) {
  mname <- label_name(t$y[i])
  paste0(sum(group == mname & name_of == t$name), hover_of, sum(group == mname))
}
for (mode in c("count", "share")) {
  tr <- traces(fw_chart_method(d, all_sel, mode))
  all_ok <- TRUE; hidden_ok <- TRUE; n_hidden <- 0L
  for (t in tr) for (i in seq_along(t$y)) {
    want <- hover_want(t, i, me, me$method, me$outcome)
    if (t$customdata[i] != want) all_ok <- FALSE
    if (t$text[i] == "") { n_hidden <- n_hidden + 1L; if (t$customdata[i] != want) hidden_ok <- FALSE }
    # plotly_build() repeats the template per point; one is enough to read.
    if (!grepl("%{customdata}", t$hovertemplate[1], fixed = TRUE) ||
        grepl("%{text}", t$hovertemplate[1], fixed = TRUE)) all_ok <- FALSE
  }
  ok(sprintf("method (%s): every hover reads 'n of total'", mode), all_ok)
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
# The same fix, the same bug, the other chart.
ok("method by waterbody: share mode labels are percentages too",
   all(grepl("%$", label_text(traces(fw_chart_method_waterbody(d, all_sel, "share"))))))

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
ok("duration: the (n) in each label counts durations, not attempts",
   all(vapply(unique(as.character(box$y)), function(l) label_n(l) == sum(dm$method == label_name(l)), logical(1))))
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
ok("duration: the tick values are day counts, not log10 of them",
   identical(dur_x$tickvals, FW_CHART$duration_ticks))
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

# A category chart with Other.
wb <- all_sel$waterbody_type[!is.na(all_sel$waterbody_type)]
wb_out <- outcome_of[as.character(all_sel$attempt_id[!is.na(all_sel$waterbody_type)])]
totals <- sort(table(wb), decreasing = TRUE)
b <- plotly::plotly_build(fw_chart_waterbody(all_sel))
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

# Methods within each kind of water.
wbt <- stats::setNames(all_sel$waterbody_type, as.character(all_sel$attempt_id))
mw <- me; mw$wb <- wbt[mw$attempt_id]; mw <- mw[!is.na(mw$wb), ]
wb_totals <- sort(table(mw$wb), decreasing = TRUE)
if (length(wb_totals) > FW_TOP_N) {
  mw$wb[!mw$wb %in% names(wb_totals)[seq_len(FW_TOP_N)]] <- fw_t("charts", "other")
}
tr <- traces(fw_chart_method_waterbody(d, all_sel, "count"))
ok("method x water: every segment counts (attempt, method) once",
   all(unlist(lapply(tr, function(t) vapply(seq_along(t$y), function(i)
     sum(mw$wb == label_name(t$y[i]) & mw$method == t$name) == t$x[i], logical(1))))))
tr_share <- traces(fw_chart_method_waterbody(d, all_sel, "share"))
sums <- tapply(unlist(lapply(tr_share, `[[`, "x")),
               unlist(lapply(tr_share, function(t) as.character(t$y))), sum)
ok("method x water: shares sum to 100 per kind of water", all(abs(sums - 100) < 1e-9))
for (mode in c("count", "share")) {
  tr <- traces(fw_chart_method_waterbody(d, all_sel, mode))
  all_ok <- TRUE; hidden_ok <- TRUE; n_hidden <- 0L
  for (t in tr) for (i in seq_along(t$y)) {
    want <- hover_want(t, i, mw, mw$wb, mw$method)
    if (t$customdata[i] != want) all_ok <- FALSE
    if (t$text[i] == "") { n_hidden <- n_hidden + 1L; if (t$customdata[i] != want) hidden_ok <- FALSE }
    if (grepl("%{text}", t$hovertemplate[1], fixed = TRUE)) all_ok <- FALSE
  }
  ok(sprintf("method x water (%s): every hover reads 'n of total'", mode), all_ok)
  ok(sprintf("method x water (%s): the %d hidden-label segments still hover a count", mode, n_hidden),
     hidden_ok && n_hidden > 0L)
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
ok("workbook: the five sheets, named from the copy", openxlsx::getSheetNames(xlsx), sheet_names)
ok("workbook: attempts sheet rows", nrow(openxlsx::read.xlsx(xlsx, sheet_names[1])), nrow(sel1))
# BY NAME, not by position. The sheet list gained Contacts, and an index that
# silently pointed at the wrong sheet is how this assertion stopped meaning
# anything the first time.
defs <- openxlsx::read.xlsx(xlsx, fw_t("export", "sheets")$definitions)
ok("workbook: dictionary opens with exactly the export columns",
   defs[[1]][seq_along(FW_EXPORT_COLUMNS)], FW_EXPORT_COLUMNS)
ok("workbook: and then defines the contacts sheet's own columns",
   all(names(fw_t("export", "dictionary_contacts")) %in% defs[[1]]), TRUE)
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

html <- tempfile(fileext = ".html")
fw_write_html_report(html, d, sel1, export, f1, m)
doc <- fw_html_read_text(html)
tables <- regmatches(doc, gregexpr('(?s)<table class="fw-table">.*?</table>', doc, perl = TRUE))[[1]]
# THE ATTEMPTS TABLE IS NOT IN THE DOCUMENT ANY MORE. It used to be asserted
# here as "one row per selected attempt"; the client removed it from the page
# and this file followed, so the assertion is now that it is absent. Written
# against the CONTACTS heading as the thing that should still be there, so a
# report that renders no tables at all fails rather than passes.
# Named by what each remaining table is FOR, so this says which survive rather
# than only how many.
#
# MATCHED ON HEADER CELLS ONLY. Matching anywhere in the table finds "Invasive
# species" in the FILTERS table, where it is the name of a filter the reader
# set rather than a column of attempts - which is exactly the false positive
# that made the first version of the absence test below fail.
headings <- function(t) gsub("<[^>]*>", "",
                             regmatches(t, gregexpr("<th[^>]*>[^<]*</th>", t))[[1]])
has_table <- function(heading) any(vapply(tables, function(t)
  heading %in% headings(t), logical(1)))
ok("report: the contacts table is still there",
   has_table(fw_t("plan", "col_contact_name")))
ok("report: the attempts-by-country table is still there",
   has_table(fw_t("export", "col_country")))
# THE ROW-PER-ATTEMPT TABLE IS GONE. Asserted on a literal rather than a copy
# key, because the keys it used were deleted with it - and an absence test that
# depends on the absent thing still existing is how dev/value_test.R broke once
# already (see the success-count assertion further up).
ok("report: the row-per-attempt table is gone",
   !has_table("Site") && !has_table("Invasive species"))
# It is only acceptable to drop it because every row still leaves the building
# in the CSV below, which the next assertion is what pins.
# EVERY RECORD MUST STILL LEAVE THE BUILDING. Dropping the table from the
# document is only acceptable because the CSV below carries the same rows -
# that is asserted a few lines down, and the two belong together.
payload <- function(id) {
  one <- regmatches(doc, regexpr(paste0('<script id="', id, '".*?</script>'), doc, perl = TRUE))
  jsonlite::base64_dec(sub("</script>$", "", sub("^<script[^>]*>", "", one)))
}
csv_path <- tempfile(fileext = ".csv"); writeBin(payload("fw-file-csv"), csv_path)
ok("report: the csv inside matches the selection",
   nrow(utils::read.csv(csv_path, check.names = FALSE, encoding = "UTF-8")), nrow(sel1))
# The no-method caption under the method chart, present with the base-R count
# when there is one and absent when there is none.
n_nm <- sum(!as.character(sel1$attempt_id) %in% as.character(am$attempt_id))
ok(sprintf("report: the no-method caption is %s (%d)",
           if (n_nm > 0) "present" else "absent", n_nm),
   grepl(fw_fill(fw_t("plan", "r_method_missing"), n = fw_fmt_num(n_nm)), doc, fixed = TRUE),
   n_nm > 0)
# The waterbody chart's own mode reaches the document independently.
html2 <- tempfile(fileext = ".html")
fw_write_html_report(html2, d, sel1, export, f1, m, method_mode = "count", method_wb_mode = "share")
doc2 <- fw_html_read_text(html2)
ok("report: the waterbody chart follows its own mode",
   grepl(fw_t("charts", "x_share_uses"), doc2, fixed = TRUE) &&
     !grepl(fw_t("charts", "x_share"), doc2, fixed = TRUE))
styles <- regmatches(doc, gregexpr("(?s)<style[^>]*>.*?</style>", doc, perl = TRUE))[[1]]
ours <- styles[grepl(".fw-container", styles, fixed = TRUE) | grepl(".fw-report{", styles, fixed = TRUE)]
ok("report: both of our stylesheets are inlined", length(ours), 2L)
ok("report: our stylesheets carry no pure white",
   !any(grepl("#fff\\b|#ffffff", ours, ignore.case = TRUE)))

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
ok("map: the orphan notes reach the popup frame",
   all(!is.na(mp$method_pairs[match(intersect(orphan_ids, mp$attempt_id), mp$attempt_id)])))

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
cat("\n-- design values --\n")

rem <- function(x) as.numeric(sub("rem$", "", x))
# THE TWO EXEMPTIONS ARE NAMED, so a third cannot be added by accident: a new
# size_* token under 1rem fails this until someone writes it down here and says
# why. size_popup is for transient overlays; size_credit is the photo credits in
# the dashboard's attempts table. Both are recorded at FW_TYPE in R/brand.R.
FW_TYPE_FLOOR_EXEMPT <- c("size_popup", "size_credit")
sizes <- FW_TYPE[grepl("^size_", names(FW_TYPE)) &
                   !names(FW_TYPE) %in% FW_TYPE_FLOOR_EXEMPT]
ok("type: nothing on the page is under the 1rem floor", all(vapply(sizes, rem, numeric(1)) >= 1))
ok("type: the floor's exemptions are the two that are written down",
   sum(vapply(FW_TYPE[grepl("^size_", names(FW_TYPE))], rem, numeric(1)) < 1), 2L)
ok("type: the plotly floor is the rem floor in pixels", FW_TYPE$floor_px, 16L)
vars <- fw_sass_variables()
ok("tokens: no empty Sass variable", !any(vapply(vars, function(v) is.null(v) || is.na(v) || !nzchar(v), logical(1))))
ok("tokens: no white surface",
   !any(tolower(unlist(FW_COLOURS)) %in% c("#fff", "#ffffff")))
ok("charts: duration tick labels match the tick values",
   length(fw_t("charts", "duration_ticks")), length(FW_CHART$duration_ticks))

cat("\n")
if (failures > 0L) stop(failures, " value assertion(s) failed", call. = FALSE)
cat("All value tests passed.\n")
