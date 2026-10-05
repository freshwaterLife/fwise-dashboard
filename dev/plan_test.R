# plan_test.R
# Checks the report builder: the deliberate build gate, the three result states,
# any-of filter semantics, and that the export cannot disagree with itself.
#
#     Rscript dev/plan_test.R
#
# Exits non-zero on any failure, so it is usable from CI or a pre-commit hook.

library(shiny)
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)

# THE CLOCK IS PINNED. Download names carry the time to the second (client,
# 30 Sept 2026), and the assertions below compare a name made after a download
# - the PDF takes seconds - with the one the download was given. See
# fw_file_stamp() in R/export.R.
options(fw.now = as.POSIXct("2026-09-30 14:21:05", tz = "UTC"))
d <- fw_load_data(); m <- fw_load_metadata(); ch <- fw_filter_choices(d)

failures <- 0L
ok <- function(lbl, got, want = TRUE) {
  pass <- identical(got, want)
  if (!pass) failures <<- failures + 1L
  cat(sprintf("  %-52s %-6s %s\n", lbl, format(got), if (pass) "PASS" else "*** FAIL ***"))
}
strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x$html %||% x)))

# The report builder's own filter set: everything in FW_FILTERS except outcome
# and method. Built from the registry rather than typed out, so this test
# exercises the same id list the page does.
plan_ids <- fw_plan_filter_ids()
base <- fw_filter_state(list(), plan_ids, ch = ch)
base$year_from <- ch$year_min
base$year_to <- ch$year_max
# The two "include unrecorded" checkboxes as the browser paints them: ON. The
# state reads isTRUE(input$...), which is FALSE before the control exists, and
# these tests describe a panel the reader is looking at.
base$include_no_year <- TRUE
base$include_no_size <- TRUE

cat("\n-- the copy deck --\n")
# [PLACEHOLDER] What the caveats say while the client writes the real ones.
# When they land, these assertions should name a phrase from their text instead.
PLACEHOLDER_CAVEAT <- "[PLACEHOLDER - ANABELL TO PROVIDE CAVEATS FOR FWISE]"

ok("no section is defined in two copy files",
   anyDuplicated(names(fw_copy_all())), 0L)

cat("\n-- filters --\n")
ok("no filters returns everything", nrow(fw_filter_apply(d, base)), nrow(d$attempt))

# ANY-OF, not all-of, and no attempt counted twice. If the bridge join leaked
# duplicates this would exceed the sum of the two individual counts. Exercised
# on SPECIES rather than method, which this page no longer offers - the bridge
# code is the same either way.
sp_a <- ch$species[1]; sp_b <- ch$species[2]
f_a <- modifyList(base, list(species = sp_a))
f_b2 <- modifyList(base, list(species = sp_b))
f_ab <- modifyList(base, list(species = c(sp_a, sp_b)))
n_a <- nrow(fw_filter_apply(d, f_a)); n_b <- nrow(fw_filter_apply(d, f_b2))
n_ab <- nrow(fw_filter_apply(d, f_ab))
ok("two species is a union, not an intersection", n_ab >= max(n_a, n_b))
ok("and does not double-count",                   n_ab <= n_a + n_b)

# The 53 undated attempts must never vanish without the user asking.
ok("undated attempts kept by default",
   nrow(fw_filter_apply(d, base)) - nrow(fw_filter_apply(d, modifyList(base, list(include_no_year = FALSE)))),
   ch$n_no_year)

# A missing slider value means "no bound", not "nothing matches". The filter
# panel is a renderUI, so this is the state on first paint.
ok("absent year bounds do not empty the result",
   nrow(fw_filter_apply(d, modifyList(base, list(year_from = NULL, year_to = NULL)))),
   nrow(d$attempt))

# The two filters this page deliberately does not offer, and the reasoning for
# each is at the top of mod_plan.R: outcome is an answer the reader must not be
# able to pre-select, and method is the thing they came here to learn.
ok("outcome is not a report-builder filter", "outcome" %in% plan_ids, FALSE)
ok("method is not a report-builder filter",  "method" %in% plan_ids, FALSE)
# The fish family pair came back on 24 Sept 2026 (client), shown only while
# Fish is picked - see fw_filter_when_panel() in R/filters.R.
ok("and those two are all that is dropped",
   sort(setdiff(fw_filter_ids(), plan_ids)), sort(c("method", "outcome")))
ok("the fish family pair is a report-builder filter",
   all(c("family", "family_beneficiary") %in% plan_ids), TRUE)
# Outcome is now filterable NOWHERE. It used to be the dashboard's alone; the
# client's decision is that it is an answer on both pages. It stays visible in
# every chart, the map and the table - it is just never used to narrow.
ok("the dashboard does not offer outcome either",
   "outcome" %in% FW_EXPLORE_FILTERS, FALSE)
# Still vs flowing joined them on 21 Sept 2026 (client).
ok("the dashboard's filters are place, water regime, animal and fish family",
   sort(FW_EXPLORE_FILTERS),
   sort(c("continent", "country", "regime", "taxa", "taxa_beneficiary",
          "family", "family_beneficiary")))

cat("\n-- the size filter --\n")
# TWO UNITS THAT MUST NOT MIX. Hectares for still water, kilometres for flowing,
# and an attempt is only ever compared against the slider for its own unit.
ha_full <- fw_size_log_range(ch$size$ha)
km_full <- fw_size_log_range(ch$size$km)
n_ha <- sum(d$attempt$area_unit == "ha", na.rm = TRUE)
n_km <- sum(d$attempt$area_unit == "km", na.rm = TRUE)

sized <- modifyList(base, list(size_ha = ha_full, size_km = km_full))
ok("sliders at full range change nothing",
   nrow(fw_filter_apply(d, sized)), nrow(d$attempt))

# The unmeasured attempts are the checkbox's decision ALONE. This regressed once
# already: the match was OR-ed in, which could only ever add rows back, so
# unticking the box removed nothing.
ok("unticking 'include unrecorded size' drops exactly the unsized",
   nrow(fw_filter_apply(d, sized)) -
     nrow(fw_filter_apply(d, modifyList(sized, list(include_no_size = FALSE)))),
   ch$size$n_no_size)

big_ha <- modifyList(sized, list(size_ha = c(fw_size_log(100), ha_full[2])))
sel_ha <- fw_filter_apply(d, big_ha)
ok("narrowing hectares leaves every kilometre attempt alone",
   sum(sel_ha$area_unit == "km", na.rm = TRUE), n_km)
ok("and keeps only hectare attempts inside the bound",
   all(sel_ha$area_treated[which(sel_ha$area_unit == "ha")] >= 100), TRUE)

big_km <- modifyList(sized, list(size_km = c(fw_size_log(50), km_full[2])))
sel_km <- fw_filter_apply(d, big_km)
ok("narrowing kilometres leaves every hectare attempt alone",
   sum(sel_km$area_unit == "ha", na.rm = TRUE), n_ha)

# An absent slider is "this unit is not being constrained", not "nothing
# matches". The size cell is a renderUI, so this is the state on first paint.
ok("absent size bounds do not empty the result",
   nrow(fw_filter_apply(d, modifyList(base, list(size_ha = NULL, size_km = NULL)))),
   nrow(d$attempt))

# WHICH SLIDERS A REGIME OFFERS. Still water is an area, flowing water is a
# length, so choosing one leaves the other unit's slider with nothing to say.
# That is the client's rule and fw_size_units() is a straight map.
ok("still water offers hectares alone", fw_size_units("Lentic"), "ha")
ok("flowing water offers kilometres alone", fw_size_units("Lotic"), "km")
ok("no regime offers both", fw_size_units(character(0)), FW_SIZE_UNITS)
ok("both regimes offer both", fw_size_units(c("Lentic", "Lotic")), FW_SIZE_UNITS)

# THE HIDDEN SLIDER MUST NOT GO ON FILTERING, and this is the assertion the
# whole design rests on. Shiny KEEPS the value of an input whose UI has been
# removed, so a slider dropped by a regime change goes on reporting the last
# bounds the reader gave it. Without the matching drop in fw_filter_state() it
# narrows the result invisibly - measured, not theorised: it silently dropped
# six attempts.
#
# Note the mock input carries BOTH sliders, exactly as a live session does after
# the reader has moved one and then changed the regime.
mock <- list(regime = "Lentic",
             size_ha = fw_size_log_range(ch$size$ha),
             size_km = c(fw_size_log(500), fw_size_log(715)),
             include_no_size = TRUE, include_no_year = TRUE,
             years = c(ch$year_min, ch$year_max))
state <- fw_filter_state(mock, plan_ids, ch = ch)
ok("still water snapshots the hectare slider", !is.null(state$size_ha))
ok("and drops the kilometre slider it cannot see", is.null(state$size_km))
ok("so a stale bound left on the hidden slider changes nothing",
   nrow(fw_filter_apply(d, state)),
   nrow(fw_filter_apply(d, modifyList(state, list(size_km = NULL)))))

# THE COST OF THE FIXED MAP, ASSERTED SO IT IS ON THE RECORD rather than
# discovered. 40 attempts carry a unit that disagrees with their regime:
#
#     regime    ha    km   (none)
#     Lentic   497     6       81
#     Lotic     34   203       87
#
# With still water chosen, those six kilometre-measured attempts have no slider
# on the page and so are not size-filtered at all. That is the honest reading of
# a hidden control - the alternative is judging them against bounds the reader
# cannot see. It goes away on its own as the client's cleaning lands; until then
# this test says how many rows it applies to, so a change in that number is
# noticed. It went away in the 23 Sept 2026 export: none are left.
mismatch <- sum(d$attempt$water_regime == "Lentic" &
                  d$attempt$area_unit == "km", na.rm = TRUE)
ok("no still-water attempt is measured in kilometres", mismatch, 0L)
tight <- modifyList(state, list(size_ha = c(fw_size_log(1), fw_size_log(2))))
ok("a narrow hectare bound does not touch the mismatched kilometre rows",
   sum(fw_filter_apply(d, tight)$area_unit == "km" &
         fw_filter_apply(d, tight)$water_regime == "Lentic", na.rm = TRUE),
   mismatch)

# A slider left where it started is not a filter, and must not be recorded as a
# bound the reader chose - the slider ends are widened to whole log steps and so
# do not match the real minimum and maximum.
size_rows <- function(f) {
  r <- Filter(function(x) grepl(fw_filter_label("size"), x$setting, fixed = TRUE),
              fw_filter_summary(f))
  vapply(r, function(x) x$value, character(1))
}
ok("an untouched size slider records All",
   unique(size_rows(sized)), fw_t("export", "filter_all"))
ok("and a moved one records real units, not logarithms",
   any(grepl("100", size_rows(big_ha), fixed = TRUE)), TRUE)

# The registry drives the panel, the state, the hints and the workbook. If a
# filter is offered on the page it must appear in the sheet that records what
# the reader selected.
sheet_settings <- fw_filters_sheet(base, 10L, 914L)$Setting
# grepl, not %in%: size contributes one row PER UNIT, each labelled
# "<label> (<unit>)", so an exact match would miss it.
ok("every filter reaches the workbook sheet",
   all(vapply(setdiff(plan_ids, "years"), function(i) {
     any(grepl(fw_filter_label(i), sheet_settings, fixed = TRUE))
   }, logical(1))))
ok("size reaches it once per unit",
   sum(grepl(fw_filter_label("size"), sheet_settings, fixed = TRUE)),
   length(FW_SIZE_UNITS))
ok("and the two dropped filters do not",
   any(c(fw_filter_label("outcome"), fw_filter_label("method")) %in% sheet_settings),
   FALSE)

cat("\n-- the export cannot disagree with itself --\n")
# The filtered download and the future Zenodo release are the SAME function.
full <- fw_export_frame(d)
sub  <- fw_export_frame(d, d$attempt$attempt_id[d$attempt$country == "Norway"])
both <- merge(full, sub, by = "attempt_id", suffixes = c(".a", ".b"))
mismatch <- sum(vapply(setdiff(names(sub), "attempt_id"), function(c) {
  a <- both[[paste0(c, ".a")]]; b <- both[[paste0(c, ".b")]]
  sum(!(is.na(a) & is.na(b)) & (is.na(a) | is.na(b) | a != b))
}, integer(1)))
ok("full and filtered exports agree cell for cell", mismatch, 0L)
ok("no list columns survive", any(vapply(full, is.list, logical(1))), FALSE)
ok("multi-values are semicolon-delimited", any(grepl(";", full$invasive_species)), TRUE)
ok("no numbered species columns", any(grepl("_[1-8]$", names(full))), FALSE)
ok("no underscore-nested taxa",
   any(grepl("_", full$invasive_taxa[!is.na(full$invasive_taxa)])), FALSE)
ok("contributor notes are not exported", "notes_for_fwise" %in% names(full), FALSE)

cat("\n-- the size scale (29 Sept 2026) --\n")
# log10(1 + x), from 0 to a round number above the data maximum, in even steps
# - never values from the data.
ok("size: the scale round-trips",
   isTRUE(all.equal(fw_size_unlog(fw_size_log(c(0, 0.0014, 3.4, 237500))),
                    c(0, 0.0014, 3.4, 237500))))
ok("size: both sliders start at 0", c(ha_full[1], km_full[1]), c(0, 0))
ok("size: the top is at or above the data max",
   fw_size_unlog(ha_full[2]) >= ch$size$ha[2] && fw_size_unlog(km_full[2]) >= ch$size$km[2])
ok("size: the top is a 1-2-5 round number",
   all(vapply(fw_size_unlog(c(ha_full[2], km_full[2])), function(x) {
     x <- round(x)
     m <- x / 10^floor(log10(x))
     isTRUE(any(abs(m - c(1, 2, 5)) < 1e-6))
   }, logical(1))))
ok("size: nice ceiling", vapply(c(3, 12345, 237500, 500, 0.004), fw_nice_ceiling, numeric(1)),
   c(5, 20000, 500000, 500, 0.005))
ok("size: the step divides the range into equal positions",
   isTRUE(all.equal(fw_size_log_step(ha_full) * FW_SIZE_POSITIONS, ha_full[2])))
ok("size: a record sitting on the top end is kept",
   nrow(fw_filter_apply(list(attempt = data.frame(
     attempt_id = 1, area_treated = 20000, area_unit = "ha")),
     list(.ids = "size", size_ha = c(0, fw_size_log(20000)), include_no_size = TRUE))), 1L)
ok("size: the bottom label is 0", fw_size_label(fw_size_unlog(0)), fw_fmt_num(0))

cat("\n-- clear and the default state --\n")
# Clear builds from fw_filter_defaults(), because the reset controls only
# reach the browser after the flush. It has to be the state a freshly reset
# page would report.
reset_input <- list(years = c(ch$year_min, ch$year_max),
                    size_ha = ha_full, size_km = km_full,
                    include_no_size = TRUE, include_no_year = TRUE)
ok("defaults match a freshly reset page",
   identical(fw_filter_defaults(plan_ids, ch), fw_filter_state(reset_input, plan_ids, ch = ch)))
ok("defaults build the whole database",
   nrow(fw_filter_apply(d, fw_filter_defaults(plan_ids, ch))), nrow(d$attempt))

cat("\n-- species pickers follow kind and family (29 Sept 2026) --\n")
sp_lab <- fw_species_label(d$species)
lab_taxa <- function(l) unique(sp_lab$taxa[sp_lab$label %in% l])
lab_family <- function(l) unique(sp_lab$family[sp_lab$label %in% l])
cray <- fw_species_allowed(sp_lab, "Crayfish", character(0), ch$species)
ok("Crayfish offers crayfish only", length(cray) > 0 && identical(lab_taxa(cray), "Crayfish"))
salm <- fw_species_allowed(sp_lab, "Fish", "Salmonidae", ch$species)
ok("Salmonidae offers salmonids only", length(salm) > 0 && identical(lab_family(salm), "Salmonidae"))
ok("nothing chosen offers everything, in the same order",
   identical(fw_species_allowed(sp_lab, character(0), character(0), ch$species), ch$species))
# THE PROTECTED SIDE FOLLOWS EVERYTHING ELSE (client, 30 Sept 2026): with
# invasive Salmonidae chosen, the protected pickers list only what attempts
# against salmonids protected - recomputed here from the bridge table.
salm_f <- modifyList(base, list(taxa = "Fish", family = "Salmonidae"))
salm_ids <- fw_filter_apply(d, salm_f)$attempt_id
want_ben <- unique(d$attempt_species$species_id[d$attempt_species$role == "beneficiary" &
                                                  d$attempt_species$attempt_id %in% salm_ids])
prot <- fw_protected_allowed(d, sp_lab, salm_ids, character(0), character(0), ch)
ok("salmonids: protected species are exactly those protected by salmonid attempts",
   setequal(prot$beneficiary, intersect(ch$beneficiary, sp_lab$label[sp_lab$species_id %in% want_ben])))
ok("salmonids: and that is fewer than everything",
   length(prot$beneficiary) > 0 && length(prot$beneficiary) < length(ch$beneficiary))
ok("salmonids: protected animals come from those species only",
   setequal(prot$taxa_beneficiary,
            intersect(ch$taxa_beneficiary, sp_lab$taxa[sp_lab$species_id %in% want_ben])))
ok("protected: a protected animal narrows the protected species further",
   {
     t1 <- prot$taxa_beneficiary[1]
     p2 <- fw_protected_allowed(d, sp_lab, salm_ids, t1, character(0), ch)
     length(p2$beneficiary) > 0 && all(lab_taxa(p2$beneficiary) == t1)
   })
ok("protected: no other filter offers every protected species",
   identical(fw_protected_allowed(d, sp_lab, d$attempt$attempt_id, character(0),
                                  character(0), ch)$beneficiary, ch$beneficiary))
testServer(mod_plan_server, args = list(data = d, meta = m), {
  session$setInputs(taxa = "Crayfish")
  ok("live: picking Crayfish narrows the species picker",
     length(fw_species_allowed(sp_lab, input$taxa, character(0), ch$species)) < length(ch$species))
})

cat("\n-- the build gate and the three states --\n")
# THE RESULTS ARE A STATIC SKELETON now, shown by a conditionalPanel that reads
# output$state, with small HTML-only uiOutputs for what varies. The charts and
# the map used to sit inside one renderUI rebuilt on every Build, and some of
# them kept the previous selection's figures - see fw_plan_results_ui().
#
# THE EMPTY STATE IS EMPTY. The page's introduction lives in the page header,
# so before a build nothing renders at all - not a prompt.
is_empty <- function(x) is.null(x) || !nzchar(trimws(paste(strip(x), collapse = "")))
skeleton <- as.character(fw_plan_results_ui(NS("plan")))
# What a reader sees in the results state: the skeleton plus what fills it.
shown <- function(output) {
  paste(strip(list(html = skeleton)), strip(output$summary), strip(output$species),
        strip(output$outcome_bars))
}
testServer(mod_plan_server, args = list(data = d, meta = m), {
  ok("state 1: nothing asked yet", output$state, "none")
  ok("state 1: empty before any build", is_empty(output$zero), TRUE)

  # THE GATE. Results must not appear just because a filter moved.
  session$setInputs(country = "Norway", years = c(1934, 2025), include_no_year = TRUE)
  ok("changing a filter does not render results", output$state, "none")

  session$setInputs(build = 1)
  ok("state 3: results render on build", output$state, "results")
  ok("and the zero message does not", is_empty(output$zero), TRUE)
  h <- shown(output)
  ok("the results heading is there",
     grepl("What the matching attempts show", h), TRUE)
  ok("all four outcomes stay visible",
     all(vapply(c("Successful", "Failed", "Ongoing", "Unknown"), grepl, logical(1), h)), TRUE)
  # THE CAVEATS ARE NOT ON THIS PAGE ANY MORE. They moved to About, because they
  # describe the whole database rather than the selection, and under a freshly
  # built result they read as qualifications of that selection alone. They still
  # travel inside every download - asserted further down.
  ok("caveats do NOT sit beside the results", grepl(PLACEHOLDER_CAVEAT, h, fixed = TRUE), FALSE)
  ok("the contacts block does", grepl(fw_t("plan", "r_contacts"), h, fixed = TRUE), TRUE)
  ok("the cumulative chart has left this page",
     grepl("How the record has", h), FALSE)
  ok("export matches the selection",
     nrow(report()$export), nrow(report()$sel))

  # A REBUILD REDRAWS EVERY FIGURE. The reported bug: after changing country,
  # some charts still showed the last country. Each figure's data is compared
  # with the same figure computed independently for the new selection.
  chart_x <- function(json) {
    j <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    as.character(unlist(lapply(j$x$data, function(t) t$x)))
  }
  expect_x <- function(p) {
    b <- plotly::plotly_build(p)
    as.character(unlist(lapply(b$x$data, function(t) t$x)))
  }
  for (cc in c("Italy", "Sweden", "Norway")) {
    session$setInputs(country = cc, build = input$build + 1)
    # The selection recomputed from the recorded filters, not read back off
    # report(), and it has to be this country and nothing else.
    s <- fw_filter_apply(d, report()$filters)
    ok(paste0(cc, ": the build is of ", cc),
       nrow(s) > 0 && all(s$country == cc) &&
         setequal(report()$sel$attempt_id, s$attempt_id))
    ok(paste0(cc, ": method chart redrawn"),
       identical(chart_x(output$methods), expect_x(fw_chart_method(d, s, mode = "count"))))
    ok(paste0(cc, ": waterbody chart redrawn"),
       identical(chart_x(output$chart_waterbody),
                 expect_x(fw_chart_waterbody(s, mode = "count"))))
    ok(paste0(cc, ": duration chart redrawn"),
       identical(chart_x(output$duration), expect_x(fw_chart_duration(d, s))))
    ok(paste0(cc, ": summary counts the new selection"),
       grepl(fw_fmt_num(nrow(s)), strip(output$summary), fixed = TRUE))
  }

  session$setInputs(taxa = "Turtle", build = input$build + 1)
  ok("state 2: nothing matched", output$state, "zero")
  h <- strip(output$zero)
  ok("state 2: zero result says so plainly",
     grepl("No attempts match those filters", h), TRUE)
  ok("and names a filter to relax", grepl("Try relaxing one of these first", h), TRUE)
  ok("the zero state does not carry caveats either",
     grepl(PLACEHOLDER_CAVEAT, h, fixed = TRUE), FALSE)

  # ---- The download -------------------------------------------------------
  #
  # ONE HANDLER AND A PICKER. The methods-and-caveats text used to travel
  # whatever else was ticked; it is gone (client, 24 Sept 2026) and each
  # document carries the section itself, so one tick downloads one file.
  session$setInputs(taxa = character(0), build = input$build + 1)
  ok("back from zero to results", output$state, "results")

  # ---- Where the download lives now ---------------------------------------
  #
  # AT THE TOP, AND IN AN OVERLAY. It used to be a block at the foot of the
  # page, below two tables; the client's objection was that a reader could not
  # tell any of this was exportable without scrolling past all of it.
  head_html <- skeleton
  ok("the download button is in the results head",
     grepl("fw-plan__download-open", head_html, fixed = TRUE))
  # THE COUNTS ARE NOT IN THE RESULTS ANY MORE (client, 29 Sept 2026). They
  # sit under Build and Clear with the applied-filter line, filters first,
  # then what was built from them, then the counts, then the results.
  page_html <- as.character(mod_plan_ui("plan"))
  ok("the counts left the results skeleton",
     grepl('id="plan-summary"', head_html, fixed = TRUE), FALSE)
  ok("page order: filters, applied filters, counts, results",
     all(diff(vapply(c('id="plan-filters"', 'id="plan-filters_summary"',
                       'id="plan-summary"', 'id="plan-results_anchor"'),
                     function(x) regexpr(x, page_html, fixed = TRUE)[1],
                     numeric(1))) > 0))
  ok("the filter panel is not a disclosure",
     grepl("<details", as.character(fw_plan_filters_ui(NS("plan"), ch)), fixed = TRUE),
     FALSE)
  ok("the applied-filter line says what was built",
     grepl(fw_t("plan", "f_applied"), strip(output$filters_summary), fixed = TRUE))
  # The slider sends doubles; an untouched range must still read as untouched.
  session$setInputs(country = character(0), years = as.numeric(c(ch$year_min, ch$year_max)),
                    build = input$build + 1)
  ok("an untouched year slider is not listed as a filter",
     grepl(fw_filter_label("years"), strip(output$filters_summary), fixed = TRUE), FALSE)

  # CLEAR RESETS AND REBUILDS (client, 29 Sept 2026). It used to empty the
  # controls and leave the last report on screen.
  session$setInputs(country = "Norway", build = input$build + 1)
  ok("clear: a narrowed build first", nrow(report()$sel) < nrow(d$attempt))
  session$setInputs(clear = 1)
  ok("clear: rebuilds the whole database", nrow(report()$sel), nrow(d$attempt))
  ok("clear: and the applied-filter line says All",
     grepl(fw_t("filters", "all"), strip(output$filters_summary), fixed = TRUE))
  ok("clear: rebuilt from the defaults",
     identical(report()$filters, fw_filter_defaults(plan_ids, ch)))
  # EXACTLY ONE PICKER EXISTS AT A TIME. Every input in this module shares one
  # DOM id space, so a copy on the page AND a copy in the modal would put two
  # controls called download_parts in the document and the handler would read
  # whichever Shiny bound last. The page must carry none.
  ok("the picker is not on the page",
     grepl("download_parts", head_html, fixed = TRUE), FALSE)
  # THE MATCHING-ATTEMPTS TABLE IS GONE and the contacts table is not.
  ok("the attempts table is gone from the page",
     grepl("table_body", head_html, fixed = TRUE), FALSE)
  ok("the contacts table is still there",
     grepl("contacts_body", head_html, fixed = TRUE))
  # THE FIGURES ARE IN THE SKELETON, and nothing that renders HTML for the
  # results carries an output of its own - that nesting was the bug.
  ok("every figure is a static output",
     all(vapply(c("plan-map", "plan-methods", "plan-duration", "plan-chart_waterbody"),
                function(id) grepl(sprintf('id="%s"', id), head_html, fixed = TRUE),
                logical(1))))
  ok("no output nested in a results renderUI",
     any(vapply(list(output$summary, output$species, output$outcome_bars,
                     output$map_missing, output$method_caption,
                     output$duration_missing, output$zero),
                function(x) grepl("shiny-(html|text|plot)-output|html-widget-output",
                                  as.character(x$html %||% "")),
                logical(1))), FALSE)
  unzip_names <- function(p) utils::unzip(p, list = TRUE)$Name

  # WHAT A READER CAN TICK (client, 23 Sept 2026): the PDF report, the attempts
  # file and the spreadsheet, in that order. The CSV went in the same round -
  # it was the spreadsheet's rows a second time - and the interactive HTML
  # report before it.
  ok("the three parts, in order", FW_BUNDLE_PARTS, c("pdf", "records", "xlsx"))
  # A RETIRED PART IS DROPPED AND THE SELECTION FALLS TO THE SPREADSHEET. It
  # used to be dropped to nothing, which was safe while the methods text made
  # every download non-empty; with that gone, an empty selection has no file to
  # name, so fw_bundle_parts() has a floor - see the note there.
  ok("the old html part is not recognised", fw_bundle_parts("html"), "xlsx")
  ok("and the csv is not recognised either", fw_bundle_parts("csv"), "xlsx")
  picker <- as.character(fw_plan_download_ui(session$ns, pdf = TRUE))
  ok("the picker offers all three",
     all(vapply(c('value="pdf"', 'value="records"', 'value="xlsx"'),
                grepl, logical(1), x = picker, fixed = TRUE)), TRUE)
  ok("and no longer offers the csv",
     grepl('value="csv"', picker, fixed = TRUE), FALSE)
  ok("the picker lists them in FW_BUNDLE_PARTS order",
     order(vapply(FW_BUNDLE_PARTS,
                  function(v) regexpr(paste0('value="', v, '"'), picker, fixed = TRUE),
                  integer(1))),
     seq_along(FW_BUNDLE_PARTS))
  ok("and ticks the spreadsheet and the PDF",
     lengths(regmatches(picker, gregexpr('checked="checked"', picker, fixed = TRUE))), 2L)
  # NO PDF BOX WHERE NO PDF CAN BE MADE, and a sentence saying so instead.
  no_pdf <- as.character(fw_plan_download_ui(session$ns, pdf = FALSE))
  ok("without Quarto the PDF box is not drawn",
     grepl('value="pdf"', no_pdf, fixed = TRUE), FALSE)
  ok("and the picker says why",
     grepl(fw_t("plan", "pdf_unavailable"), no_pdf, fixed = TRUE), TRUE)
  local({
    old <- Sys.getenv(c("QUARTO_PATH", "PATH"))
    Sys.setenv(QUARTO_PATH = tempfile(), PATH = "/nonexistent")
    on.exit(Sys.setenv(QUARTO_PATH = old[[1]], PATH = old[[2]]))
    ok("and the bundle skips a PDF it cannot make",
       fw_bundle_parts(c("xlsx", "pdf")), "xlsx")
  })

  session$setInputs(download_parts = c("xlsx", "records"))
  path <- output$download
  ok("the bundle downloads", file.exists(path), TRUE)
  ok("and is a zip when more than one file was asked for",
     grepl("[.]zip$", fw_bundle_filename(c("xlsx", "records"))), TRUE)
  inside <- unzip_names(path)
  ok("it carries the spreadsheet", any(grepl("[.]xlsx$", inside)), TRUE)
  ok("and the attempts file",      fw_records_filename() %in% inside, TRUE)
  # THE MANDATORY .txt IS GONE (client, 24 Sept 2026) and the zip holds exactly
  # what was ticked. It used to carry a methods-and-caveats file whatever else
  # was in there; each document closes on that section itself now.
  ok("and nothing else",
     sort(inside), sort(c(fw_export_filename(), fw_records_filename())))

  # ONE TICK, ONE FILE, NOT A ZIP OF ONE. This is the change the client asked
  # for: the travelling text file made every download a zip of two things, so
  # ticking the report handed back an archive to unpack.
  session$setInputs(download_parts = "records")
  ok("one thing ticked downloads that one thing",
     fw_bundle_filename("records"), fw_records_filename())
  ok("and it is not a zip",
     grepl("^<!DOCTYPE html", readLines(output$download, n = 1)), TRUE)

  # AN UNRECOGNISED PART IS DROPPED, NOT REFUSED. "csv" was a real part until
  # 23 Sept 2026, so a stale bookmark or a re-sent form can still name it. With
  # nothing left to write, fw_bundle_parts() falls to the spreadsheet rather
  # than naming a file it cannot produce - the picker's own Download button is
  # disabled long before a reader could get here (fw_download_guard()).
  session$setInputs(download_parts = "csv")
  ok("a bundle asking for the retired csv falls back to the spreadsheet",
     fw_bundle_filename("csv"), fw_export_filename())

  session$setInputs(download_parts = character(0))
  ok("and so does one asking for nothing at all",
     fw_bundle_filename(character(0)), fw_export_filename())

  # DATE AND TIME IN EVERY NAME (client, 30 Sept 2026), in UTC.
  ok("download names carry the date and the time",
     fw_bundle_filename(c("xlsx", "records")), "fwise-report_20260930-142105.zip")
  ok("and so does each file on its own",
     c(fw_export_filename(), fw_records_filename(), fw_pdf_filename()),
     c("fwise-attempts_20260930-142105.xlsx",
       "fwise-detailed-report_20260930-142105.html",
       "fwise-report_20260930-142105.pdf"))

  # THE PROGRESS BAR (client, 21 Sept 2026). Every step reports, the bar never
  # goes backwards, it ends at 1, and every step has words from the copy deck.
  # The zip step exists only where there is more than one file to pack, so a
  # single-part bundle reports its own step and the finish and nothing else.
  for (parts in list("xlsx", c("xlsx", "records"))) {
    seen <- list()
    fw_write_bundle(tempfile(), parts, d, report()$sel, report()$export,
                    report()$filters, m,
                    progress = function(v, detail) seen[[length(seen) + 1]] <<- list(v, detail))
    vals <- vapply(seen, `[[`, numeric(1), 1)
    lbl <- paste0("progress (", paste(parts, collapse = "+"), "): ")
    ok(paste0(lbl, "one report per step plus the finish"),
       length(vals), length(parts) + (length(parts) > 1) + 1L)
    ok(paste0(lbl, "never goes backwards"), all(diff(vals) >= 0))
    ok(paste0(lbl, "starts at 0 and ends at 1"), c(vals[1], utils::tail(vals, 1)), c(0, 1))
    ok(paste0(lbl, "every step is worded"),
       all(nzchar(vapply(seen, `[[`, "", 2))))
  }

  # ---- The workbook -------------------------------------------------------
  # Ticked on its own, so the download IS the .xlsx - no archive to open.
  session$setInputs(download_parts = "xlsx")
  path <- tempfile(fileext = ".xlsx")
  file.copy(output$download, path, overwrite = TRUE)
  sheets <- openxlsx::getSheetNames(path)
  # METHODS AND CAVEATS IS THE LAST TAB (client, 24 Sept 2026), where the PDF
  # and the records HTML also close. Still no contacts sheet.
  ok("all four sheets present, in order, and no contacts sheet",
     sheets, c("Attempts", "Field definitions", "Filters applied",
               fw_t("export", "sheets")$caveats))
  ok("and the last tab carries the methods, the client's caveats and the citation",
     all(c(toupper(fw_t("export", "methods_heading")),
           "[PLACEHOLDER - ANABELL TO PROVIDE CAVEATS FOR FWISE]",
           toupper(fw_t("export", "citation_heading")),
           fw_citation_text(m, nrow(d$attempt))) %in%
           openxlsx::read.xlsx(path, fw_t("export", "sheets")$caveats)[[1]]), TRUE)
  ok("data sheet matches the selection",
     nrow(openxlsx::read.xlsx(path, "Attempts")), nrow(report()$sel))
  ok("field definitions cover every exported column",
     all(names(openxlsx::read.xlsx(path, "Attempts")) %in%
           openxlsx::read.xlsx(path, "Field definitions")$Field), TRUE)

  # THE CONTROL, not the intention. An address belonging to a contact who asked
  # not to be listed must not be anywhere in the workbook.
  private <- d$contact$contact_email[!d$contact$contact_public]
  private <- private[!is.na(private) & nzchar(private)]
  rows <- openxlsx::read.xlsx(path, "Attempts")
  ok("no private address reaches the attempts sheet",
     any(private %in% c(rows$primary_contact_email, rows$secondary_contact_email)),
     FALSE)

  # ---- The attempts file ---------------------------------------------------
  #
  # EVERY ATTEMPT IN THE SELECTION, ONCE, IN THE SPREADSHEET'S ORDER, with every
  # exported field labelled on every card. Read back out of the file the reader
  # gets, not out of the function that wrote it.
  # ONE PART DOWNLOADS AS ITSELF, more than one as a zip (client, 24 Sept
  # 2026). fw_bundle_filename() draws that line and this follows it, so the
  # assertions below read the same file the reader would get either way.
  unpack <- function(parts, pattern) {
    session$setInputs(download_parts = parts)
    name <- fw_bundle_filename(parts)
    if (!grepl("[.]zip$", name)) {
      out <- file.path(tempfile("fw-one-"), name)
      dir.create(dirname(out))
      file.copy(output$download, out, overwrite = TRUE)
      return(out)
    }
    z <- tempfile(fileext = ".zip")
    file.copy(output$download, z, overwrite = TRUE)
    dir <- tempfile(); dir.create(dir)
    utils::unzip(z, exdir = dir)
    list.files(dir, pattern = pattern, full.names = TRUE)[1]
  }
  rec_path <- unpack(c("xlsx", "records"), "[.]html$")
  rec <- paste(readLines(rec_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ids <- regmatches(rec, gregexpr('<article class="fw-rec-card" id="[^"]+"', rec))[[1]]
  ids <- sub('^.*id="', "", sub('"$', "", ids))
  ok("one card per attempt in the selection", length(ids), nrow(report()$sel))
  ok("in the export's order", ids, report()$export$attempt_id)
  # STILL WATER / FLOWING WATER, never the stored Lentic / Lotic (client,
  # 24 Sept 2026) - the words the filters and the form already use.
  ok("water regime reads in plain English",
     grepl("<dd>(Lentic|Lotic)</dd>", rec), FALSE)
  ok("and the plain words are there",
     any(vapply(FW_REGIME_LABELS, function(l) grepl(paste0("<dd>", l, "</dd>"), rec, fixed = TRUE),
                logical(1))), TRUE)
  ok("the records close on the citation",
     grepl(fw_t("export", "citation_heading"), rec, fixed = TRUE), TRUE)
  cards <- strsplit(rec, '<article class="fw-rec-card"', fixed = TRUE)[[1]][-1]
  labels <- fw_t("export", "record_labels")
  ok("the labels cover every exported column",
     setequal(names(labels), FW_EXPORT_COLUMNS), TRUE)
  grouped <- unlist(lapply(fw_t("export", "record_groups"), `[[`, "fields"))
  ok("and every column but the id sits in one group",
     sort(grouped), sort(setdiff(FW_EXPORT_COLUMNS, "attempt_id")))
  n_dt <- vapply(cards, function(x) lengths(regmatches(x, gregexpr("<dt>", x, fixed = TRUE))), 1L)
  # EVERY GROUPED FIELD, MINUS THE CHEMICAL DETAIL WHERE THAT SECTION IS CUT
  # (client, 23 Sept 2026). The expected count per card is recomputed from the
  # export rows through the same predicate the writer uses, so the file and the
  # rule cannot drift; what is asserted independently is that both kinds of card
  # are in this file at all, and that the headings went with the fields.
  chem <- Filter(function(g) identical(g$id, "chemical"),
                 fw_t("export", "record_groups"))[[1]]
  exp_rows <- report()$export
  cut <- vapply(seq_len(nrow(exp_rows)),
                function(i) fw_record_group_drop(chem, as.list(exp_rows[i, ])),
                logical(1))
  # AND MINUS A BLANK REGION (client, 24 Sept 2026). Region is recorded for a
  # handful of countries only, so "Not noted" under an Australian site read as
  # a missing Tasmania. It is the one field that vanishes rather than saying
  # so - see FW_RECORD_OMIT_BLANK - and the expected count follows the same
  # predicate the writer uses.
  no_region <- vapply(exp_rows$region, fw_record_blank, logical(1))
  ok("every card draws every grouped field it keeps",
     all(unname(n_dt) ==
           length(grouped) -
           ifelse(cut, length(chem$fields), 0L) -
           ifelse(no_region, 1L, 0L)),
     TRUE)
  ok("some cards in here have no region, or the rule is untested",
     sum(no_region) > 0)
  ok("a card with no region draws no Region row",
     any(grepl("<dt>Region</dt>", cards[no_region], fixed = TRUE)), FALSE)
  ok("and a card with one still does",
     all(grepl("<dt>Region</dt>", cards[!no_region], fixed = TRUE)), TRUE)
  ok("some cards here have the chemical section cut", sum(cut) > 0)
  ok("and some keep it, or the rule is untested", sum(!cut) > 0)
  ok("a cut card loses the heading with the fields",
     sum(vapply(cards, function(x)
       grepl(paste0("<h3>", chem$heading, "</h3>"), x, fixed = TRUE), logical(1))),
     sum(!cut))
  # THE GUARD ON THE CUT: an attempt with no chemical method but a recorded
  # concentration keeps the section, because the spreadsheet in the same
  # download still carries that value and the two must not disagree. Asserted
  # over the WHOLE database rather than this selection - there are only a
  # handful of such attempts and a country filter can easily hold none, which
  # would leave the guard passing without ever being exercised.
  all_rows <- fw_export_frame(d)
  all_no_chem <- !vapply(strsplit(as.character(all_rows$method_classes), FW_MULTI_SEP,
                                  fixed = TRUE),
                         function(x) "chemical" %in% x, logical(1))
  all_has_data <- vapply(seq_len(nrow(all_rows)), function(i)
    any(!vapply(chem$fields, function(f) fw_record_blank(all_rows[[f]][i]), logical(1))),
    logical(1))
  all_cut <- vapply(seq_len(nrow(all_rows)),
                    function(i) fw_record_group_drop(chem, as.list(all_rows[i, ])),
                    logical(1))
  ok("no attempt with chemical data ever loses the section",
     any(all_cut & all_has_data), FALSE)
  ok("and some attempt with no chemical method is kept by that guard",
     sum(all_no_chem & all_has_data) > 0)
  ok("the cut is exactly no-chemical-method and no chemical data",
     identical(all_cut, unname(all_no_chem & !all_has_data)), TRUE)
  ok("a blank field says Not noted",
     grepl(paste0('<span class="fw-rec-none">', fw_t("species", "p_not_noted")), rec,
           fixed = TRUE), TRUE)
  ok("no private address reaches the attempts file",
     any(vapply(private, grepl, logical(1), x = rec, fixed = TRUE)), FALSE)
  ok("the caveats travel with it", grepl(PLACEHOLDER_CAVEAT, rec, fixed = TRUE), TRUE)
  ok("and the methods statement with them",
     grepl(fw_t("export", "methods_heading"), rec, fixed = TRUE), TRUE)
  ok("nothing in it is fetched",
     grepl('(src|href)="(?!data:|#|https?:|mailto:)[^"]*[.](css|js|png|jpe?g|woff2?)"',
           rec, perl = TRUE), FALSE)
  ok("its fonts are inlined, unwrapped",
     grepl("url(\"data:font/woff2;base64,", rec, fixed = TRUE) &&
       !grepl("data:[a-z/+.-]+;base64,[A-Za-z0-9+/=]*\\n", rec), TRUE)
  # The FWISE mark twice - the masthead and, since 1 Oct 2026, first in the
  # footer - then Weird Fishes and the collaborators.
  ok("every logo the footer carries is in it",
     lengths(regmatches(rec, gregexpr('src="data:image/png', rec, fixed = TRUE))),
     3L + length(FW_LOGO$collab_files))

  # ---- The PDF report -------------------------------------------------------
  #
  # NEEDS QUARTO. Without it the assertions below cannot run, and they say so
  # loudly rather than passing quietly: set QUARTO_PATH to a Quarto 1.4+ CLI.
  if (!fw_pdf_available()) {
    cat("  *** PDF ASSERTIONS SKIPPED: quarto not found (set QUARTO_PATH) ***\n")
  } else {
    pdf_path <- unpack("pdf", "[.]pdf$")
    ok("the PDF is in the bundle", basename(pdf_path), fw_pdf_filename())
    head_bytes <- readBin(pdf_path, "raw", 5)
    ok("and is a PDF", rawToChar(head_bytes), "%PDF-")
    ok("with more than one page", fw_pdf_page_count(pdf_path) > 1L, TRUE)

    # WHAT THE DOCUMENT SAYS is read from its Typst source, which is the text
    # the PDF was set from - the PDF's own text streams are compressed.
    keep <- tempfile()
    fw_write_pdf_report(tempfile(fileext = ".pdf"), d, report()$sel, filters = report()$filters,
                        meta = m, keep = keep)
    typ <- paste(readLines(file.path(keep, "report.typ"), warn = FALSE), collapse = "\n")
    has <- function(txt) grepl(txt, typ, fixed = TRUE)
    ok("the letterhead carries the title", has(fw_t("plan", "report_title")), TRUE)
    ok("the filter selection is recorded", has(fw_t("plan", "report_selection")), TRUE)
    ok("the caveats travel with the document", has(PLACEHOLDER_CAVEAT), TRUE)
    ok("and the methods statement with them",
       has(fw_t("export", "methods_heading")), TRUE)
    # METHODS, CAVEATS, CITATION (client, 24 Sept 2026), each titled. The
    # headless placeholder caveat takes "Caveats" here and nowhere else.
    ok("the caveat placeholder is titled Caveats",
       has(paste0('("', fw_t("export", "caveats_title"), '", "', PLACEHOLDER_CAVEAT, '")')), TRUE)
    ok("and the citation closes the section",
       has(paste0('("', fw_t("export", "citation_heading"), '", ')), TRUE)
    pos <- function(txt) regexpr(txt, typ, fixed = TRUE)
    ok("in the order methods, caveats, citation",
       pos(paste0('("', fw_t("export", "methods_heading"), '"')) <
         pos(paste0('("', fw_t("export", "caveats_title"), '"')) &&
         pos(paste0('("', fw_t("export", "caveats_title"), '"')) <
         pos(paste0('("', fw_t("export", "citation_heading"), '"')), TRUE)
    ok("the contacts carry the PDF's own note",
       has(fw_t("plan", "report_contacts_note")), TRUE)
    ok("the footer no longer points back at the caveats",
       has("Read the caveats above"), FALSE)
    ok("and so do the contacts", has(fw_t("plan", "r_contacts")), TRUE)
    ok("the map is drawn", file.exists(file.path(keep, "map.png")), TRUE)
    # BOTH VERSIONS OF THE TOGGLED CHARTS, one above the other (client, 29
    # Sept 2026), whatever the toggles on screen were set to.
    ok("every chart is drawn",
       all(file.exists(file.path(keep, c("methods-count.png", "methods-share.png",
                                         "duration.png", "waterbody-count.png",
                                         "waterbody-share.png")))), TRUE)
    ok("the number of attempts comes before the success rate",
       pos('"methods-count.png"') < pos('"methods-share.png"') &&
         pos('"waterbody-count.png"') < pos('"waterbody-share.png"'), TRUE)
    ok("and each is labelled",
       has(fw_t("plan", "r_method_share")) && has(fw_t("plan", "r_waterbody_count")), TRUE)
    # AND THE DELETED ONE IS NOT (client, 23 Sept 2026).
    ok("the methods-by-water chart is gone",
       file.exists(file.path(keep, "method-waterbody.png")), FALSE)
    # WHAT THIS REPORT COVERS lists only the filters the reader actually set,
    # and carries the note saying what the report is and is not for.
    ok("the filters table explains itself",
       has(fw_t("plan", "report_selection_note")), TRUE)
    ok("and is set below the print floor", has("size: fw-small"), TRUE)
    ok("no untouched filter is listed as All",
       grepl(paste0('"', fw_t("export", "filter_all"), '"'), typ, fixed = TRUE), FALSE)
    # THE OUTCOME BARS LEAD, above the species plates.
    ok("the outcomes come before the species plates",
       regexpr("#fw-outcome-bars", typ, fixed = TRUE) <
         regexpr("#fw-species-tiles", typ, fixed = TRUE), TRUE)
    # SIX CONTACTS AT MOST, however broad the selection.
    ok("at most six contacts are printed",
       nrow(fw_pdf_contacts_table(fw_plan_contacts(d, report()$sel))) <= FW_PDF$contacts_n,
       TRUE)
    ok("every logo travels",
       all(file.exists(file.path(keep, paste0("logo-", c("mark", "wfa",
                                                         names(FW_LOGO$collab_files)),
                                             ".png")))), TRUE)
    ok("no private address reaches the PDF",
       any(vapply(private, function(e) has(gsub("([@.])", "\\1\u200b", e)) || has(e),
                  logical(1))), FALSE)
    ok("the method caption is there when it should be",
       has(fw_method_caption_text(d, report()$sel) %||% "\u0001"),
       fw_n_no_method(d, report()$sel) + fw_n_multi_method(d, report()$sel) > 0)
  }
})

# ---- The still/flowing chart and the regime filter (client, 30 Sept 2026) ---
#
# One regime picked: the chart would be a single bar restating the filter, so
# the PDF and the Detailed report both leave it out. Both picked, or neither:
# it stays. fw_pdf_body() needs no Quarto, so this runs everywhere.
cat("\n-- the still/flowing chart follows the regime filter --\n")
for (rg in list(character(0), "Lentic", c("Lentic", "Lotic"))) {
  fr <- base; fr$regime <- rg
  sel_r <- fw_filter_apply(d, fr)
  dir_r <- tempfile("fw-regime-"); dir.create(dir_r)
  typ_r <- fw_pdf_body(dir_r, d, sel_r, fr, m)
  rec_r <- as.character(fw_records_summary_ui(d, sel_r, fr))
  want <- length(rg) != 1L
  lbl <- if (length(rg)) paste(rg, collapse = "+") else "none"
  ok(sprintf("PDF: waterbody chart with regime %s", lbl),
     file.exists(file.path(dir_r, "waterbody-count.png")), want)
  ok(sprintf("Detailed report: waterbody block with regime %s", lbl),
     grepl(fw_t("plan", "r_waterbody"), rec_r, fixed = TRUE), want)
  if (!length(rg)) {
    ok("PDF: the duration chart is drawn", file.exists(file.path(dir_r, "duration.png")), TRUE)
    ok("PDF: the method caption is one line",
       grepl(fw_typ_caption(fw_method_caption_text(d, sel_r)), typ_r, fixed = TRUE), TRUE)
    ok("PDF: species tiles are drawn", grepl("#fw-species-tiles", typ_r, fixed = TRUE), TRUE)
  }
  unlink(dir_r, recursive = TRUE)
}
# A species pick drops that role's tiles in the PDF too.
fsp <- base; fsp$species <- ch$species[1]
dir_s <- tempfile("fw-sp-"); dir.create(dir_s)
typ_s <- fw_pdf_body(dir_s, d, fw_filter_apply(d, fsp), fsp, m)
ok("PDF: an invasive species pick drops the invasive tiles",
   grepl("Top [a-z]+ invasive species", typ_s), FALSE)
ok("PDF: and keeps the protected tiles",
   grepl("Top [a-z]+ species protected", typ_s), TRUE)
unlink(dir_s, recursive = TRUE)

# ---- The record search (client, 1 Oct 2026) ----------------------------------
#
# RECOMPUTED, NOT RE-READ: what each search should find is worked out here
# from the export frame's own columns with base R, and the page's table is
# checked against it - match counts, the ten-a-page cut, the last page, and
# that every shown row carries its full card, hidden until opened.
cat("\n-- record search --\n")
ex_all <- fw_export_frame(d, fw_filter_apply(d, base)$attempt_id)
hay <- fw_plan_records_haystack(ex_all)
recount <- function(q) {
  q <- tolower(q)
  fields <- c("attempt_id", "site_name", "country", "region", "invasive_species",
              "beneficiary_species", "methods")
  sum(vapply(seq_len(nrow(ex_all)), function(i) {
    v <- unlist(ex_all[i, fields, drop = FALSE]); v <- v[!is.na(v)]
    any(grepl(q, tolower(v), fixed = TRUE)) || grepl(q, tolower(paste(v, collapse = " ")), fixed = TRUE)
  }, logical(1)))
}
ok("records: blank search matches every attempt",
   length(fw_plan_records_find(hay, "  ")), nrow(ex_all))
q_country <- ex_all$country[!is.na(ex_all$country)][1]
q_species <- ex_all$invasive_species[!is.na(ex_all$invasive_species)][1]
for (q in c(q_country, toupper(q_country), q_species, ex_all$attempt_id[5], "rotenone")) {
  ok(sprintf("records: '%s' finds the recounted number", q),
     length(fw_plan_records_find(hay, q)), recount(q))
}
ok("records: the id search finds exactly that attempt",
   ex_all$attempt_id[fw_plan_records_find(hay, ex_all$attempt_id[5])][1], ex_all$attempt_id[5])
ok("records: nonsense finds nothing", length(fw_plan_records_find(hay, "zzqxj")), 0L)

rows_all <- seq_len(nrow(ex_all))
pg <- function(page) as.character(fw_plan_records_ui(ex_all, rows_all, page))
n_rows_on <- function(h) lengths(regmatches(h, gregexpr('class="fw-records__row"', h, fixed = TRUE)))
last <- fw_plan_pages(nrow(ex_all), FW_PLAN_RECORDS_PAGE_SIZE)
ok("records: ten rows on page one", n_rows_on(pg(1L)), FW_PLAN_RECORDS_PAGE_SIZE)
ok("records: the last page holds the remainder",
   n_rows_on(pg(last)),
   nrow(ex_all) - (last - 1L) * FW_PLAN_RECORDS_PAGE_SIZE)
h1 <- pg(1L)
ok("records: every row has its card, hidden",
   lengths(regmatches(h1, gregexpr('class="fw-records__detail" id="[^"]+" hidden', h1))),
   FW_PLAN_RECORDS_PAGE_SIZE)
ok("records: the cards carry no contents link and no element id",
   identical(c(grepl("fw-rec-contents", h1, fixed = TRUE),
               grepl('<article class="fw-rec-card" id=', h1, fixed = TRUE)), c(FALSE, FALSE)))
ok("records: page one is the export's first ten, in order",
   identical(regmatches(h1, gregexpr('aria-controls="fw-records-[^"]+"', h1))[[1]],
             sprintf('aria-controls="fw-records-%s"', ex_all$attempt_id[1:FW_PLAN_RECORDS_PAGE_SIZE])))
ok("records: no match says so",
   grepl(fw_t("plan", "r_records_none"),
         as.character(fw_plan_records_ui(ex_all, integer(0))), fixed = TRUE), TRUE)

testServer(mod_plan_server, args = list(data = d, meta = m), {
  session$setInputs(build = 1)
  body <- output$records_body$html
  ok("records (server): page one on build", n_rows_on(body), FW_PLAN_RECORDS_PAGE_SIZE)
  session$setInputs(records_q = q_country)
  session$elapse(FW_PLAN_RECORDS_DEBOUNCE_MS + 100)
  ok("records (server): the search narrows the table",
     length(records_rows()), recount(q_country))
  ok("records (server): the pager counts the matches",
     grepl(paste(fw_t("common", "of"), fw_fmt_num(recount(q_country))),
           output$records_pager$html, fixed = TRUE), TRUE)
  session$setInputs(records_q = "zzqxj")
  session$elapse(FW_PLAN_RECORDS_DEBOUNCE_MS + 100)
  ok("records (server): nothing found says so",
     grepl(fw_t("plan", "r_records_none"), output$records_body$html, fixed = TRUE), TRUE)
})

# The waterbody-type filter went on 1 Oct 2026 (client).
ok("filters: no waterbody-type filter", "waterbody" %in% fw_filter_ids(), FALSE)

cat("\n-- email addresses (revealed on a click, 2 Oct 2026) --\n")
# THE HARVESTER'S VIEW: anything shaped like an address, bare or in a mailto.
# Every live-app surface must show none until a click; the downloaded report
# must show every public one. The expected set is recomputed here from the
# contacts table and the contact_public flag, not taken from the renderers.
EMAIL_RX <- "[[:alnum:]._%+-]+@[[:alnum:].-]+[.][[:alpha:]]{2,}"
has_addr <- function(html) grepl(EMAIL_RX, html) || grepl("mailto:", html, fixed = TRUE)
decoded <- function(html) {
  enc <- regmatches(html, gregexpr('data-fw-email="[^"]+"', html))[[1]]
  vapply(sub('^data-fw-email="(.*)"$', "\\1", enc), fw_email_decode, "", USE.NAMES = FALSE)
}
fwise_addr <- fw_t("footer", "contact_email")

ok("email: the encoding round-trips",
   identical(fw_email_decode(fw_email_encode("a.b-c+d@ex.example.org")), "a.b-c+d@ex.example.org"))
ok("email: and for a non-ASCII address",
   identical(fw_email_decode(fw_email_encode("josé@café.example")), "josé@café.example"))
ok("email: the encoded form carries neither the @ nor the domain",
   !grepl("@|fwlife", fw_email_encode(fwise_addr)))
ok("email: the FWISE address is written in the copy deck once",
   sum(grepl(fwise_addr, unlist(fw_copy_all()), fixed = TRUE)), 1L)

foot <- as.character(fw_footer(as.Date("2026-10-02")))
ok("email: the footer serves no address", has_addr(foot), FALSE)
# Not "fwlife" at all: the Freshwater Life logo links to fwlife.org, rightly.
ok("email: nor the old split halves", grepl("data-u=|data-d=", foot), FALSE)
ok("email: the footer's button decodes to the FWISE address", decoded(foot), fwise_addr)

fb <- as.character(fw_feedback_panel())
ok("email: the About band serves no address", has_addr(fb), FALSE)
ok("about: the band's button opens the feedback form (2 Oct 2026)",
   grepl('data-fw-open="feedback"', fb, fixed = TRUE))
ok("about: and no longer reveals the address", grepl("data-fw-email", fb, fixed = TRUE), FALSE)
ok("about: the sign-up card opens the newsletter form",
   grepl('data-fw-open="newsletter"', as.character(fw_about_signup()), fixed = TRUE))
ok("about: and is no longer a link", grepl("<a ", as.character(fw_about_signup()), fixed = TRUE), FALSE)
ok("email: the About page's script sends no mailto any more",
   grepl("fw-mailto", as.character(fw_client_script()), fixed = TRUE), FALSE)

net <- as.character(mod_networking_ui("n"))
ok("email: the Networking page shell serves no address", has_addr(net), FALSE)
ok("email: its outro decodes to the FWISE address", decoded(net), fwise_addr)

contacts_all <- fw_contacts_summary(d)
pub <- contacts_all[!is.na(contacts_all$contact_email), ]
ok("email: the fixture has public addresses to test against", nrow(pub) > 0L)
ok("email: no private address survives the summary",
   any(contacts_all$contact_id %in% d$contact$contact_id[!d$contact$contact_public] &
         !is.na(contacts_all$contact_email)), FALSE)
r1 <- pub[1, ]
live_cell <- as.character(fw_contact_action(r1$contact_email, fw_contact_who(r1)))
ok("email: a directory cell serves no address", has_addr(live_cell), FALSE)
ok("email: and decodes to that contact's address", decoded(live_cell), r1$contact_email)
ok("email: a contact known only by address keeps it out of the aria-label",
   has_addr(as.character(fw_contact_action("x@ex.example.org", "x@ex.example.org"))), FALSE)
ok("email: a contact with no public address gets no button",
   grepl("data-fw-email", as.character(fw_contact_action(NA_character_, "Someone"))), FALSE)

people <- fw_plan_contacts(d, ex_all)
want <- sort(unique(people$contact_email[!is.na(people$contact_email)]))
live_tbl <- as.character(fw_plan_contacts_ui(people, per_page = nrow(people)))
ok("email: the report builder's contacts table serves no address", has_addr(live_tbl), FALSE)
ok("email: and reveals exactly the public addresses",
   identical(sort(unique(decoded(live_tbl))), want))
saved_tbl <- as.character(fw_plan_contacts_ui(people, per_page = nrow(people), reveal = FALSE))
# Matched as written rather than by EMAIL_RX: two addresses in the source data
# carry a stray space ("mark buktenica@...", "andrew.stump@ ky.gov"), which a
# pattern would cut short, and the report shows what the data holds.
ok("email: the downloaded report writes every public address out",
   all(vapply(want, function(e) grepl(paste0(">", htmltools::htmlEscape(e), "<"),
                                      saved_tbl, fixed = TRUE), logical(1))))
ok("email: and no button that would need the app's script",
   grepl("data-fw-email", saved_tbl, fixed = TRUE), FALSE)

recs <- fw_attempt_records(d, fw_filter_apply(d, base))
with_pub <- which(!is.na(recs$primary_contact_email) & nzchar(recs$primary_contact_email))
ok("email: the fixture has a record with a public contact", length(with_pub) > 0L)
rec_html <- fw_record_detail_html(recs[with_pub[1], ], d$species)
ok("email: the map's record panel serves no address", has_addr(rec_html), FALSE)
ok("email: and its button decodes to the record's contact",
   recs$primary_contact_email[with_pub[1]] %in% decoded(rec_html))

# ---- Newsletter, feedback and the privacy page (2 Oct 2026) ------------------
#
# No network and no Google key. The local store is pointed at a temporary
# directory, and the sheet settings are cleared for the duration, so a
# developer whose .Renviron holds the real key cannot write test rows into the
# live sheet by running this.
cat("\n-- newsletter and feedback forms --\n")

saved_sheet <- FWISE_FORMS_SHEET_ID; saved_key <- GS4_SA_KEY_B64; saved_dev <- FW_DEV_DIR
FWISE_FORMS_SHEET_ID <- NULL; GS4_SA_KEY_B64 <- NULL
FW_DEV_DIR <- file.path(tempdir(), "fw-forms-test"); unlink(FW_DEV_DIR, recursive = TRUE)
ok("forms: mode is local with no settings", fw_forms_mode(), "local")

ok("forms: an address with an s in it is an address", fw_is_email("sam.s@example.org"))
ok("forms: a space is not", fw_is_email("sam s@example.org"), FALSE)
ok("forms: no dot after the @ is not", fw_is_email("sam@example"), FALSE)

v <- fw_newsletter_values("  Ada Lovelace ", " Ada.L@Example.ORG ", " ", TRUE)
ok("newsletter: name trimmed", v$name, "Ada Lovelace")
ok("newsletter: email trimmed and lowercased", v$email, "ada.l@example.org")
ok("newsletter: blank organisation is empty", v$organisation, "")
ok("newsletter: a valid sign-up has no problems", length(fw_newsletter_problems(v)), 0L)

empty <- fw_newsletter_problems(fw_newsletter_values(NULL, NULL, NULL, NULL))
ok("newsletter: empty form names exactly the three required fields",
   identical(sort(names(empty)), c("consent", "email", "name")))
lim <- FW_FORM_LIMITS
at_lim  <- fw_newsletter_values(strrep("a", lim$name), "a@b.co", strrep("o", lim$organisation), TRUE)
over    <- fw_newsletter_values(strrep("a", lim$name + 1), "a@b.co", strrep("o", lim$organisation + 1), TRUE)
ok("newsletter: name and organisation AT the limit pass", length(fw_newsletter_problems(at_lim)), 0L)
ok("newsletter: one over the limit fails both",
   identical(sort(names(fw_newsletter_problems(over))), c("name", "organisation")))
long_email <- paste0(strrep("e", lim$email), "@b.co")
ok("newsletter: an over-long email says so, not 'invalid'",
   fw_newsletter_problems(fw_newsletter_values("A", long_email, "", TRUE))$email,
   fw_fill(fw_t("newsletter", "validate", "email_long"), n = lim$email))
ok("newsletter: consent unticked is a problem",
   names(fw_newsletter_problems(fw_newsletter_values("A", "a@b.co", "", FALSE))), "consent")

rec <- fw_newsletter_record(v, "Foot<er>!", now = as.POSIXct("2026-10-02 09:30:00", tz = "UTC"))
ok("newsletter: record carries exactly the sheet's columns", identical(names(rec), FW_NEWSLETTER_COLUMNS))
ok("newsletter: time in UTC ISO form", rec$submitted_at, "2026-10-02T09:30:00Z")
ok("newsletter: consent wording is the ticked sentence", rec$consent_wording,
   fw_t("newsletter", "consent"))
ok("newsletter: terms version is the copy deck's", rec$terms_version, fw_t("privacy", "version"))
ok("newsletter: entry point cleaned", rec$entry_point, "footer")
ok("newsletter: empty entry point is 'unknown'", fw_entry_point(NULL), "unknown")

pages <- fw_feedback_pages()
ok("feedback: six pages plus General and Other", identical(unname(pages),
   c("home", "explore", "plan", "contribute", "networking", "about", "general", "other")))
ok("feedback: labels are the navbar's", names(pages)[3], fw_t("nav", "plan"))
ok("feedback: pre-selects the page the reader is on", fw_feedback_default_page("plan"), "plan")
ok("feedback: from the privacy page, General", fw_feedback_default_page("privacy"), "general")
fv <- fw_feedback_values("plan", "  The legend overlaps.  ", " ")
ok("feedback: message trimmed, blank email allowed", length(fw_feedback_problems(fv)), 0L)
ok("feedback: a page not on the list is refused",
   names(fw_feedback_problems(fw_feedback_values("<script>", "x", ""))), "page")
ok("feedback: message one over the limit is refused",
   names(fw_feedback_problems(fw_feedback_values("plan", strrep("m", lim$message + 1), ""))), "message")
ok("feedback: message AT the limit passes",
   length(fw_feedback_problems(fw_feedback_values("plan", strrep("m", lim$message), ""))), 0L)
ok("feedback: a bad optional email is refused",
   names(fw_feedback_problems(fw_feedback_values("plan", "x", "nope"))), "email")
frec <- fw_feedback_record(fv)
ok("feedback: record carries exactly the sheet's columns", identical(names(frec), FW_FEEDBACK_COLUMNS))

ok("forms: formula starts escaped, the rest untouched",
   identical(fw_sheet_cells(c("=1+1", "+x", "-3", "@a", "ok", "a=b", "")),
             c("'=1+1", "'+x", "'-3", "'@a", "ok", "a=b", "")))
ok("forms: a missing field is a blank cell",
   identical(fw_form_row(list(a = "1", c = NA), c("a", "b", "c")), c("1", "", "")))

ok("forms: an email input is type=email, not 'text email'",
   grepl('type="email"', as.character(fw_form_text("x", type = "email")), fixed = TRUE))
ok("forms: the honeypot is out of the tab order and closed to autofill",
   all(vapply(c('tabindex="-1"', 'autocomplete="off"', 'aria-hidden="true"'), grepl,
              logical(1), as.character(fw_honeypot("hp")), fixed = TRUE)))

# The local store, twice: the header is written once.
res1 <- store_newsletter_signup(rec); res2 <- store_newsletter_signup(rec)
local <- utils::read.csv(file.path(fw_forms_dir(), "newsletter.csv"), colClasses = "character")
ok("forms: local store reports success", res2$success)
ok("forms: local file has the sheet's columns", identical(names(local), FW_NEWSLETTER_COLUMNS))
ok("forms: two writes, two rows", nrow(local), 2L)
ok("forms: local email as recorded", local$email[1], "ada.l@example.org")

# The Sheets request, built and inspected, never sent.
req <- fw_sheet_request("newsletter", c("a", "=b"), token = "TOKEN", sheet_id = "SHEET")
ok("sheets: append URL on the named tab",
   startsWith(req$url, paste0(FW_SHEETS_API, "/SHEET/values/newsletter%21A1:append?")))
ok("sheets: RAW, never USER_ENTERED", grepl("valueInputOption=RAW", req$url, fixed = TRUE))
ok("sheets: rows inserted, not overwritten", grepl("insertDataOption=INSERT_ROWS", req$url, fixed = TRUE))
ok("sheets: POST", httr2::req_get_method(req), "POST")
ok("sheets: body is one row in column order",
   as.character(jsonlite::toJSON(req$body$data, auto_unbox = TRUE)), '{"values":[["a","=b"]]}')

# The service-account token: a throwaway key, signed and verified.
tk <- openssl::rsa_keygen(2048)
key_json <- jsonlite::toJSON(list(type = "service_account", client_email = "sa@p.iam.gserviceaccount.com",
                                  private_key = openssl::write_pem(tk)), auto_unbox = TRUE)
k1 <- fw_google_key(as.character(key_json))
k2 <- fw_google_key(jsonlite::base64_enc(charToRaw(as.character(key_json))))
ok("google: the key reads the same as JSON or base64", identical(k1, k2))
ok("google: token address defaults to Google's", k1$token_uri, "https://oauth2.googleapis.com/token")
ok("google: a key with no private key is refused",
   inherits(tryCatch(fw_google_key('{"client_email":"x"}'), error = identity), "error"))
jwt <- strsplit(fw_google_jwt(k1, now = 1700000000L), ".", fixed = TRUE)[[1]]
b64d <- function(x) { x <- chartr("-_", "+/", x); x <- paste0(x, strrep("=", (4 - nchar(x) %% 4) %% 4)); jsonlite::base64_dec(x) }
claim <- jsonlite::fromJSON(rawToChar(b64d(jwt[2])))
ok("google: three-part token", length(jwt), 3L)
ok("google: RS256", jsonlite::fromJSON(rawToChar(b64d(jwt[1])))$alg, "RS256")
ok("google: the scope is in the claim", claim$scope, FW_SHEETS_SCOPE)
ok("google: issued by the service account", claim$iss, "sa@p.iam.gserviceaccount.com")
ok("google: an hour long", claim$exp - claim$iat, 3600L)
ok("google: the signature verifies against the key",
   isTRUE(openssl::signature_verify(charToRaw(paste(jwt[1:2], collapse = ".")), b64d(jwt[3]),
                                    hash = openssl::sha256, pubkey = tk$pubkey)))

cat("\n-- usage tracking --\n")
ok("tracking: no ref is direct", fw_track_source(""), "direct")
ok("tracking: ref is lowercased", fw_track_source("?ref=Webinar"), "webinar")
ok("tracking: hyphens and digits pass", fw_track_source("?page=privacy&ref=issg-2026"), "issg-2026")
ok("tracking: a space is other", fw_track_source("?ref=a%20b"), "other")
ok("tracking: an address is other", fw_track_source("?ref=a@b.co"), "other")
ok("tracking: 31 characters is other", fw_track_source(paste0("?ref=", strrep("a", 31))), "other")
ok("tracking: 30 characters pass", fw_track_source(paste0("?ref=", strrep("a", 30))), strrep("a", 30))

tf <- base
tf$country <- c(ch$country[1], "Not a country")
tf$continent <- ch$continent[1]
tdet <- fw_track_filters(tf, plan_ids, ch)
tjs <- jsonlite::fromJSON(as.character(jsonlite::toJSON(tdet, auto_unbox = TRUE)), simplifyVector = FALSE)
ok("tracking: an unoffered filter value is dropped", unlist(tjs$country), ch$country[1])
ok("tracking: one choice is still an array",
   grepl('"continent":["', as.character(jsonlite::toJSON(tdet, auto_unbox = TRUE)), fixed = TRUE))
ok("tracking: an empty filter is []",
   grepl('"species":[]', as.character(jsonlite::toJSON(tdet, auto_unbox = TRUE)), fixed = TRUE))
ok("tracking: only filter keys, nothing else",
   all(names(tdet) %in% c(plan_ids, "year_from", "year_to", "include_no_year",
                          paste0("size_", FW_SIZE_UNITS), "include_no_size")))
tf$year_from <- -5
ok("tracking: an out-of-range year is dropped", is.null(fw_track_filters(tf, plan_ids, ch)$year_from))

ok("tracking: one part names its type", fw_track_download_detail("xlsx", 3)$type, "xlsx")
ok("tracking: two parts are a zip", fw_track_download_detail(c("xlsx", "records"), 3)$type, "zip")

trow <- fw_track_row("tok", "session_end")
ok("tracking: a row has the sheet's five columns", length(trow), length(FW_TRACK_COLUMNS))
ok("tracking: an empty detail is {}", trow[4], "{}")
ok("tracking: UTC ISO 8601 timestamp", grepl("^\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\dZ$", trow[1], perl = TRUE))
ok("tracking: an unknown event is refused",
   inherits(tryCatch(fw_track_row("tok", "page_view"), error = identity), "error"))

ok("tracking: unset mode is off", fw_track_mode(NULL, "id", "key"), "off")
ok("tracking: an unknown mode is off", fw_track_mode("verbose", "id", "key"), "off")
ok("tracking: sheet without an id is off", fw_track_mode("sheet", NULL, "key"), "off")
ok("tracking: sheet without a key is off", fw_track_mode("sheet", "id", NULL), "off")
ok("tracking: sheet with both is sheet", fw_track_mode("Sheet", "id", "key"), "sheet")
ok("tracking: console needs nothing", fw_track_mode("console", NULL, NULL), "console")

treq <- fw_track_request(list(trow, trow, trow), "tkn", sheet_id = "SID")
ok("tracking: appends to the events tab", grepl("/SID/values/events%21A1:append", treq$url, fixed = TRUE))
ok("tracking: RAW", grepl("valueInputOption=RAW", treq$url, fixed = TRUE))
ok("tracking: every row in one request", length(treq$body$data$values), 3L)
ok("tracking: no retry", is.null(treq$policies$retry_max_tries))

tmsg <- character(0)
withCallingHandlers({
  tt <- fw_tracker("tok", mode = "console")
  tt$log("session_start", list(source = "direct"))
  tt$log("page_view")
  tt$flush(); tt$flush()
}, message = function(m) { tmsg <<- c(tmsg, conditionMessage(m)); invokeRestart("muffleMessage") })
ok("tracking: console prints the good row once", sum(grepl("FWISE event: .*session_start", tmsg)), 1L)
ok("tracking: a bad event is dropped quietly", any(grepl("page_view", tmsg)), FALSE)
toff <- fw_tracker("tok", mode = "off")
ok("tracking: off logs nothing", is.null(toff$log("session_start")), TRUE)
ok("tracking: no tracker is a no-op", is.null(fw_track(list(userData = new.env()), "download")), TRUE)

ok("tracking: a contributor's reveal carries its id",
   grepl('data-fw-contact="CO-1"', as.character(fw_email_reveal("a@b.co", "x", contact_id = "CO-1")), fixed = TRUE))
ok("tracking: the FWISE address carries none",
   grepl("data-fw-contact", as.character(fw_footer_contact()), fixed = TRUE), FALSE)
ok("tracking: no GoatCounter script while the code is a placeholder", is.null(fw_goatcounter_tag()), TRUE)
ok("tracking: GoatCounter script with a code",
   grepl('data-goatcounter="https://abc.goatcounter.com/count"', as.character(fw_goatcounter_tag("abc")), fixed = TRUE))

FWISE_FORMS_SHEET_ID <- saved_sheet; GS4_SA_KEY_B64 <- saved_key
unlink(FW_DEV_DIR, recursive = TRUE); FW_DEV_DIR <- saved_dev

cat("\n-- the privacy page --\n")

doc <- fw_privacy_html()
md <- paste(readLines(FW_PRIVACY_FILE, warn = FALSE), collapse = "\n")
html <- paste(as.character(doc$intro), as.character(doc$html))
ok("privacy: the opening paragraph sits above the contents",
   grepl("This page explains", as.character(doc$intro), fixed = TRUE))
ok("privacy: every deep-link section has its anchor", all(FW_PRIVACY_SECTIONS %in% doc$toc$id))
ok("privacy: anchors are unique", anyDuplicated(doc$toc$id), 0L)
ok("privacy: every anchor is on a heading in the HTML",
   all(vapply(doc$toc$id, function(id) grepl(sprintf('<h[23] id="%s"', id), html), logical(1))))
toc_html <- as.character(fw_privacy_contents(NS("privacy"), doc$toc))
hrefs <- regmatches(toc_html, gregexpr('href="#[^"]+"', toc_html))[[1]]
ok("privacy: the contents list links every heading, in order",
   identical(sub('href="#(.*)"', "\\1", hrefs), doc$toc$id))
n_mark <- lengths(regmatches(html, gregexpr('<mark class="fw-placeholder">[TO CONFIRM:', html, fixed = TRUE)))
ok("privacy: the file's editing notes are not served", grepl("HEADINGS carry", html, fixed = TRUE), FALSE)
n_md_body <- lengths(regmatches(gsub("(?s)<!--.*?-->", "", md, perl = TRUE),
                                gregexpr("[TO CONFIRM:", gsub("(?s)<!--.*?-->", "", md, perl = TRUE), fixed = TRUE)))
ok("privacy: every [TO CONFIRM: ...] is highlighted", n_mark, n_md_body)
ok("privacy: there are placeholders to confirm", n_mark > 0L)
ok("privacy: no address served in the page", grepl("fwise@", html, fixed = TRUE), FALSE)
ok("privacy: the contact token was replaced", grepl("{contact_email}", html, fixed = TRUE), FALSE)
ok("privacy: the current version has a dated line under Changes",
   grepl(paste0("- ", fw_terms_version(), ","), md, fixed = TRUE))
pui <- as.character(mod_privacy_ui("privacy"))
ok("privacy: the page shows the version", grepl(fw_terms_version(), pui, fixed = TRUE))
ok("privacy: a section not on the list is refused",
   inherits(tryCatch(fw_privacy_href("anything"), error = identity), "error"))
ok("privacy: form links open the right section", fw_privacy_href("feedback"),
   "?page=privacy&section=feedback")

foot <- as.character(fw_footer(as.Date("2026-09-23")))
ok("privacy: the footer has the privacy link", grepl('id="fw_privacy_link"', foot, fixed = TRUE))
ok("feedback: the footer opens the feedback form", grepl('data-fw-open="feedback"', foot, fixed = TRUE))
contrib <- as.character(fw_step_contributor_ui(NS("contribute"), NULL))
ok("contribute: the terms link goes to the submissions section",
   grepl('href="?page=privacy&amp;section=submissions"', contrib, fixed = TRUE))
ok("contribute: and opens in a new tab", grepl('target="_blank" rel="noopener"', contrib, fixed = TRUE))

cat("\n")
if (failures > 0L) stop(failures, " report builder assertion(s) failed", call. = FALSE)
cat("All report builder tests passed.\n")
