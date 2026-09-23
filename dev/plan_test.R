# plan_test.R
# Checks the report builder: the deliberate build gate, the three result states,
# any-of filter semantics, and that the export cannot disagree with itself.
#
#     Rscript dev/plan_test.R
#
# Exits non-zero on any failure, so it is usable from CI or a pre-commit hook.

library(shiny)
for (f in sort(list.files("R", full.names = TRUE), method = "radix")) source(f)
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
ok("and those, plus the Explore-only fish family pair, are all that is dropped",
   sort(setdiff(fw_filter_ids(), plan_ids)),
   sort(c("method", "outcome", "family", "family_beneficiary")))
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
# noticed.
mismatch <- sum(d$attempt$water_regime == "Lentic" &
                  d$attempt$area_unit == "km", na.rm = TRUE)
ok("the regime/unit mismatch is still the 6 that were measured", mismatch, 6L)
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
  ok("caveats do NOT sit beside the results", grepl("laimed is not the same", h), FALSE)
  ok("the contacts block does", grepl("Potential relevant contacts", h), TRUE)
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
     grepl("laimed is not the same", h), FALSE)

  # ---- The download -------------------------------------------------------
  #
  # ONE HANDLER AND A PICKER. The methods-and-caveats text is NOT one of the
  # choices: it travels whatever else is ticked, which is the whole reason the
  # panel could move off the page without the qualifications going with it.
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
  # Ahead of the summary strip, which is the first thing under the heading.
  ok("and it comes before the summary",
     regexpr("download_open", head_html) < regexpr("plan-summary", head_html))
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
                     output$map_missing, output$method_missing,
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
  ok("the old html part is not recognised", fw_bundle_parts("html"), character(0))
  ok("and the csv is not recognised either", fw_bundle_parts("csv"), character(0))
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
  ok("and the methods and caveats", fw_methods_filename() %in% inside, TRUE)

  # AN UNRECOGNISED PART IS DROPPED, NOT REFUSED. "csv" was a real part until
  # 23 Sept 2026, so a stale bookmark or a re-sent form can still name it; it
  # is filtered out by fw_bundle_parts() and the reader gets the text alone,
  # the same as ticking nothing.
  session$setInputs(download_parts = "csv")
  ok("a bundle asking for the retired csv falls back to the text",
     fw_bundle_filename("csv"), fw_methods_filename())
  ok("and it is the real text",
     grepl("HOW FWISE WAS COMPILED", readLines(output$download, n = 1)), TRUE)

  # Nothing ticked is a reasonable thing to want, not an error to refuse: it
  # downloads the methods and caveats on their own, and raw rather than zipped.
  session$setInputs(download_parts = character(0))
  ok("nothing ticked downloads the text alone",
     fw_bundle_filename(character(0)), fw_methods_filename())
  ok("and that is the real text too",
     grepl("HOW FWISE WAS COMPILED", readLines(output$download, n = 1)), TRUE)

  # THE PROGRESS BAR (client, 21 Sept 2026). Every step reports, the bar never
  # goes backwards, it ends at 1, and every step has words from the copy deck.
  for (parts in list(character(0), c("xlsx", "records"))) {
    seen <- list()
    fw_write_bundle(tempfile(), parts, d, report()$sel, report()$export,
                    report()$filters, m,
                    progress = function(v, detail) seen[[length(seen) + 1]] <<- list(v, detail))
    vals <- vapply(seen, `[[`, numeric(1), 1)
    lbl <- paste0("progress (", paste(c("txt", parts), collapse = "+"), "): ")
    ok(paste0(lbl, "one report per step plus the finish"),
       length(vals), length(parts) + 1L + (length(parts) > 0) + 1L)
    ok(paste0(lbl, "never goes backwards"), all(diff(vals) >= 0))
    ok(paste0(lbl, "starts at 0 and ends at 1"), c(vals[1], utils::tail(vals, 1)), c(0, 1))
    ok(paste0(lbl, "every step is worded"),
       all(nzchar(vapply(seen, `[[`, "", 2))))
  }

  # ---- The workbook -------------------------------------------------------
  session$setInputs(download_parts = "xlsx")
  book <- tempfile(fileext = ".zip")
  file.copy(output$download, book, overwrite = TRUE)
  xl <- tempfile(); dir.create(xl)
  utils::unzip(book, exdir = xl)
  path <- list.files(xl, pattern = "[.]xlsx$", full.names = TRUE)[1]
  sheets <- openxlsx::getSheetNames(path)
  ok("all four sheets present, and no contacts sheet",
     sheets, c("Attempts", "Caveats", "Field definitions", "Filters applied"))
  ok("data sheet matches the selection",
     nrow(openxlsx::read.xlsx(path, "Attempts")), nrow(report()$sel))
  ok("field definitions cover every exported column",
     all(names(openxlsx::read.xlsx(path, "Attempts")) %in%
           openxlsx::read.xlsx(path, "Field definitions")$Field), TRUE)

  # THE CONTROL, not the intention. An address belonging to a contact who asked
  # not to be listed must not be anywhere in the workbook.
  private <- d$contact$contact_email[!d$contact$email_public]
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
  unpack <- function(parts, pattern) {
    session$setInputs(download_parts = parts)
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
  # all(), not the two vectors: ok() prints a line per element, and this one is
  # as long as the selection.
  ok("every card draws every grouped field it keeps",
     all(unname(n_dt) ==
           ifelse(cut, length(grouped) - length(chem$fields), length(grouped))),
     TRUE)
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
  ok("the caveats travel with it", grepl("Claimed is not the same", rec, fixed = TRUE), TRUE)
  ok("nothing in it is fetched",
     grepl('(src|href)="(?!data:|#|https?:|mailto:)[^"]*[.](css|js|png|jpe?g|woff2?)"',
           rec, perl = TRUE), FALSE)
  ok("its fonts are inlined, unwrapped",
     grepl("url(\"data:font/woff2;base64,", rec, fixed = TRUE) &&
       !grepl("data:[a-z/+.-]+;base64,[A-Za-z0-9+/=]*\\n", rec), TRUE)
  ok("every logo the footer carries is in it",
     lengths(regmatches(rec, gregexpr('src="data:image/png', rec, fixed = TRUE))),
     2L + length(FW_LOGO$collab_files))

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
                        meta = m, method_mode = "count", keep = keep)
    typ <- paste(readLines(file.path(keep, "report.typ"), warn = FALSE), collapse = "\n")
    has <- function(txt) grepl(txt, typ, fixed = TRUE)
    ok("the letterhead carries the title", has(fw_t("plan", "report_title")), TRUE)
    ok("the filter selection is recorded", has(fw_t("plan", "report_selection")), TRUE)
    ok("the caveats travel with the document", has("Claimed is not the same"), TRUE)
    ok("and so do the contacts", has(fw_t("plan", "r_contacts")), TRUE)
    ok("the map is drawn", file.exists(file.path(keep, "map.png")), TRUE)
    ok("every chart is drawn",
       all(file.exists(file.path(keep, c("methods.png", "duration.png",
                                         "waterbody.png")))), TRUE)
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
    ok("the no-method caption is there when it should be",
       has(fw_fill(fw_t("plan", "r_method_missing"),
                   n = fw_fmt_num(fw_n_no_method(d, report()$sel)))),
       fw_n_no_method(d, report()$sel) > 0)
  }
})

cat("\n")
if (failures > 0L) stop(failures, " report builder assertion(s) failed", call. = FALSE)
cat("All report builder tests passed.\n")
