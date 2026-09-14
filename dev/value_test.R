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
base <- fw_filter_state(list(), plan_ids)
base$year_from <- ch$year_min; base$year_to <- ch$year_max; base$include_no_year <- TRUE
all_sel <- fw_filter_apply(d, base)

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
f1 <- base; f1$method <- top_methods[1]
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
ok("about: every section heading is present",
   all(vapply(c("database", "method", "images", "cite", "licence", "links"),
              function(k) grepl(fw_t("about", paste0(k, "_heading")), about_html, fixed = TRUE),
              logical(1))))

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
top <- fw_species_top_n(d, all_sel, "invasive")
ok("tiles: top species count", top$n[1], as.integer(counts[1]))
ok("tiles: row count is the limit", nrow(top), min(FW_TOP_N, length(counts)))
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
               c("successful", sum(a$outcome == "Successful", na.rm = TRUE)),
               c("in review", 7L))) {
  ok(paste("explore db: panel names the", v[1], "count"),
     grepl(fw_fmt_num(as.integer(v[2])), db, fixed = TRUE))
}
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
no_coord_html <- fw_record_detail_html(no_coord_rec[1, ], d$species, nav = TRUE)
ok("explore: its record panel names the site",
   grepl(a$site_name[a$attempt_id == no_coord_id][1], no_coord_html, fixed = TRUE))
ok("explore: the panel carries previous and next when a server can answer them",
   length(gregexpr("data-fw-record-step", no_coord_html, fixed = TRUE)[[1]]), 2L)
ok("explore: and carries none when nothing can",
   grepl("data-fw-record-step",
         fw_record_detail_html(no_coord_rec[1, ], d$species), fixed = TRUE), FALSE)
ok("explore: the marker's template is that same panel, wrapped",
   fw_map_detail_html(no_coord_rec[1, ], d$species),
   paste0('<template class="fw-popup__detail">',
          fw_record_detail_html(no_coord_rec[1, ], d$species), "</template>"))

# Stepping through the list. Wrapping at both ends, and nothing for a question
# that cannot be answered.
step_ids <- head(all_sel$attempt_id, 5)
ok("explore step: next from the first is the second",
   fw_record_neighbour(step_ids, step_ids[1], 1L), step_ids[2])
ok("explore step: previous from the first wraps to the last",
   fw_record_neighbour(step_ids, step_ids[1], -1L), step_ids[5])
ok("explore step: next from the last wraps to the first",
   fw_record_neighbour(step_ids, step_ids[5], 1L), step_ids[1])
ok("explore step: an id outside the list is unanswerable",
   fw_record_neighbour(step_ids, "FW-NOPE", 1L), NULL)
ok("explore step: so is a step that is not a number",
   fw_record_neighbour(step_ids, step_ids[1], "sideways"), NULL)

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

# A card with no coordinates offers no "show on map" link - the map cannot go
# there, and a link that does nothing is worse than no link.
card_located <- as.character(fw_record_card(
  fw_attempt_records(d, a[!is.na(a$latitude), ][1, ])[1, ],
  detail_input = "ex-map_detail", locate_input = "ex-locate"))
card_unlocated <- as.character(fw_record_card(
  no_coord_rec[1, ], detail_input = "ex-map_detail", locate_input = "ex-locate"))
ok("explore card: a located attempt offers the map link",
   grepl("ex-locate", card_located, fixed = TRUE))
ok("explore card: an unlocated one does not",
   grepl("ex-locate", card_unlocated, fixed = TRUE), FALSE)
ok("explore card: but both ask for the record through the same input",
   all(grepl("ex-map_detail", c(card_located, card_unlocated), fixed = TRUE)))
ok("explore card: the whole card is one button",
   grepl('<button type="button" class="fw-record__open"', card_located, fixed = TRUE))
ok("explore card: a card with no photograph gets the placeholder, not a gap",
   grepl("fw-species-figure--none",
         as.character(fw_record_card(no_coord_rec[1, ], figure = NULL,
                                     detail_input = "x")), fixed = TRUE))

# The module, driven.
exp_kpis <- exp_sum <- exp_count <- exp_pager <- exp_cards <- NA_character_
exp_eu <- NA_character_
eu_n <- 0L
try(testServer(mod_explore_server, args = list(data = d, in_review = 7L), {
  session$setInputs(sort = "newest", list_size = "20")
  exp_kpis  <<- strip(output$kpis)
  exp_sum   <<- strip(output$summary)
  exp_count <<- strip(output$list_count)
  exp_pager <<- strip(output$records_pager)
  exp_cards <<- as.character(output$records$html)
  # The list order is what previous/next steps through, so the detail server
  # must be reading the SORTED selection and not the raw one.
  ok("explore: the list is sorted newest first",
     sorted()$start_year[1], max(a$start_year, na.rm = TRUE))
  # An unlocated attempt is in the list, and asking for its record does not
  # error even though it has no marker.
  ok("explore: an unlocated attempt is in the list",
     no_coord_id %in% sorted()$attempt_id)
  session$setInputs(map_detail = no_coord_id)
  session$setInputs(map_detail_step = list(id = sorted()$attempt_id[1], step = -1L))
  session$setInputs(locate = sorted()$attempt_id[1])
  session$setInputs(continent = "Europe")
  exp_eu <<- strip(output$summary)
  eu_n <<- nrow(sel())
}), silent = TRUE)

ok("explore: the database panel renders", !is.na(exp_kpis) && nchar(exp_kpis) > 50)
ok("explore: the strip counts the whole database unfiltered",
   grepl(paste0(fw_fmt_num(nrow(a)), " attempts"), exp_sum, fixed = TRUE))
ok("explore: the list count agrees with the strip",
   grepl(paste0(fw_fmt_num(nrow(a)), " attempts"), exp_count, fixed = TRUE))
ok("explore: the pager shows the first page of 20",
   grepl(paste0("Showing 1 - 20 of ", fw_fmt_num(nrow(a))), exp_pager, fixed = TRUE))
ok("explore: one card per attempt on the page",
   length(gregexpr('class="fw-record"', exp_cards, fixed = TRUE)[[1]]), 20L)
ok("explore: a card asks for the record through the map's own input",
   length(gregexpr("map_detail", exp_cards, fixed = TRUE)[[1]]), 20L)
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

# Duration.
dur <- stats::setNames(all_sel$duration_days, as.character(all_sel$attempt_id))
dm <- me; dm$dur <- dur[dm$attempt_id]; dm <- dm[!is.na(dm$dur) & dm$dur > 0, ]
tr <- traces(fw_chart_duration(d, all_sel))
box <- Filter(function(t) identical(t$type, "box"), tr)[[1]]
pts <- Filter(function(t) identical(t$type, "scatter"), tr)
ok("duration: the box holds every positive duration", length(box$x), nrow(dm))
ok("duration: one point per (attempt, method) with a duration",
   sum(vapply(pts, function(t) length(t$x), integer(1))), nrow(dm))
ok("duration: per-outcome point counts",
   all(vapply(pts, function(t) length(t$x) == sum(dm$outcome == t$name), logical(1))))
ok("duration: the (n) in each label counts durations, not attempts",
   all(vapply(unique(as.character(box$y)), function(l) label_n(l) == sum(dm$method == label_name(l)), logical(1))))

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
f <- base; f$method <- top_methods[1:2]
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
fw_write_workbook(xlsx, d, export, f1, m)
sheet_names <- unname(unlist(fw_t("export", "sheets")))
ok("workbook: the four sheets, named from the copy", openxlsx::getSheetNames(xlsx), sheet_names)
ok("workbook: attempts sheet rows", nrow(openxlsx::read.xlsx(xlsx, sheet_names[1])), nrow(sel1))
defs <- openxlsx::read.xlsx(xlsx, sheet_names[3])
ok("workbook: dictionary covers exactly the export columns", defs[[1]], FW_EXPORT_COLUMNS)
fs <- openxlsx::read.xlsx(xlsx, sheet_names[4])
non_range <- Filter(function(id) !identical(FW_FILTERS[[id]]$kind, "range"), plan_ids)
ok("workbook: every report-builder filter is recorded",
   all(c(vapply(non_range, fw_filter_label, character(1)), fw_filter_label("years")) %in% fs[[1]]))
ok("workbook: the chosen method is recorded",
   any(grepl(top_methods[1], fs[[2]], fixed = TRUE)))
wbk <- openxlsx::loadWorkbook(xlsx)
fills <- toupper(unique(unlist(lapply(wbk$styleObjects, function(s) s$style$fill$fillFg))))
ok("workbook: header fill is the teal text token",
   any(grepl(toupper(sub("^#", "", FW_COLOURS$teal_text)), fills)))

html <- tempfile(fileext = ".html")
fw_write_html_report(html, d, sel1, export, f1, m)
doc <- fw_html_read_text(html)
tables <- regmatches(doc, gregexpr('(?s)<table class="fw-table">.*?</table>', doc, perl = TRUE))[[1]]
attempts_table <- tables[grepl(paste0(">", fw_t("plan", "col_site"), "<"), tables)][1]
n_rows <- lengths(regmatches(attempts_table, gregexpr("<tr>", attempts_table, fixed = TRUE))) - 1L
ok("report: the attempts table lists every selected attempt", n_rows, nrow(sel1))
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
sizes <- FW_TYPE[grepl("^size_", names(FW_TYPE)) & names(FW_TYPE) != "size_popup"]
ok("type: nothing on the page is under the 1rem floor", all(vapply(sizes, rem, numeric(1)) >= 1))
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
