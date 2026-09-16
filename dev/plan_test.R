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
ok("and those are the only two dropped",
   sort(setdiff(fw_filter_ids(), plan_ids)), c("method", "outcome"))
# Outcome is now filterable NOWHERE. It used to be the dashboard's alone; the
# client's decision is that it is an answer on both pages. It stays visible in
# every chart, the map and the table - it is just never used to narrow.
ok("the dashboard does not offer outcome either",
   "outcome" %in% FW_EXPLORE_FILTERS, FALSE)
ok("the dashboard's four filters are place and animal",
   sort(FW_EXPLORE_FILTERS),
   sort(c("continent", "country", "taxa", "taxa_beneficiary")))

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
# THE EMPTY STATE IS EMPTY. The page's introduction lives in the page header,
# so before a build the results output renders nothing at all - not a prompt.
is_empty <- function(x) is.null(x) || !nzchar(trimws(paste(strip(x), collapse = "")))
testServer(mod_plan_server, args = list(data = d, meta = m), {
  ok("state 1: empty before any build", is_empty(output$results), TRUE)

  # THE GATE. Results must not appear just because a filter moved.
  session$setInputs(country = "Norway", years = c(1934, 2025), include_no_year = TRUE)
  ok("changing a filter does not render results", is_empty(output$results), TRUE)

  session$setInputs(build = 1)
  h <- strip(output$results)
  ok("state 3: results render on build",
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

  session$setInputs(taxa = "Turtle", build = 2)
  h <- strip(output$results)
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
  session$setInputs(taxa = character(0), build = 3)

  # ---- Where the download lives now ---------------------------------------
  #
  # AT THE TOP, AND IN AN OVERLAY. It used to be a block at the foot of the
  # page, below two tables; the client's objection was that a reader could not
  # tell any of this was exportable without scrolling past all of it.
  head_html <- as.character(output$results$html)
  ok("the download button is in the results head",
     grepl("fw-plan__download-open", head_html, fixed = TRUE))
  # Ahead of the summary strip, which is the first thing under the heading.
  ok("and it comes before the summary strip",
     regexpr("download_open", head_html) < regexpr("fw-summary-strip", head_html))
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
  unzip_names <- function(p) utils::unzip(p, list = TRUE)$Name

  session$setInputs(download_parts = c("xlsx", "html"))
  path <- output$download
  ok("the bundle downloads", file.exists(path), TRUE)
  ok("and is a zip when more than one file was asked for",
     grepl("[.]zip$", fw_bundle_filename(c("xlsx", "html"))), TRUE)
  inside <- unzip_names(path)
  ok("it carries the spreadsheet", any(grepl("[.]xlsx$", inside)), TRUE)
  ok("and the report",             any(grepl("[.]html$", inside)), TRUE)
  ok("and the methods and caveats", fw_methods_filename() %in% inside, TRUE)

  session$setInputs(download_parts = "csv")
  ok("a CSV-only bundle still carries the text",
     fw_methods_filename() %in% unzip_names(output$download), TRUE)

  # "pdf" is not a file of its own - it resolves to the HTML report, which
  # carries the print stylesheet. Ticking both must not produce two copies.
  ok("pdf resolves to the report", fw_bundle_parts("pdf"), "html")
  ok("and ticking both does not duplicate it",
     fw_bundle_parts(c("html", "pdf")), "html")

  # Nothing ticked is a reasonable thing to want, not an error to refuse: it
  # downloads the methods and caveats on their own, and raw rather than zipped.
  session$setInputs(download_parts = character(0))
  ok("nothing ticked downloads the text alone",
     fw_bundle_filename(character(0)), fw_methods_filename())
  ok("and it is the real text",
     grepl("HOW FWISE WAS COMPILED", readLines(output$download, n = 1)), TRUE)

  # ---- The workbook -------------------------------------------------------
  session$setInputs(download_parts = "xlsx")
  book <- tempfile(fileext = ".zip")
  file.copy(output$download, book, overwrite = TRUE)
  xl <- tempfile(); dir.create(xl)
  utils::unzip(book, exdir = xl)
  path <- list.files(xl, pattern = "[.]xlsx$", full.names = TRUE)[1]
  sheets <- openxlsx::getSheetNames(path)
  ok("all five sheets present",
     all(c("Attempts", "Contacts", "Caveats", "Field definitions",
           "Filters applied") %in% sheets), TRUE)
  ok("data sheet matches the selection",
     nrow(openxlsx::read.xlsx(path, "Attempts")), nrow(report()$sel))
  ok("field definitions cover every exported column",
     all(names(openxlsx::read.xlsx(path, "Attempts")) %in%
           openxlsx::read.xlsx(path, "Field definitions")$Field), TRUE)
  ok("and the contacts sheet's columns too",
     all(names(openxlsx::read.xlsx(path, "Contacts")) %in%
           openxlsx::read.xlsx(path, "Field definitions")$Field), TRUE)

  # ONE ROW PER PERSON, and the count is the count in front of the reader.
  people <- openxlsx::read.xlsx(path, "Contacts")
  ok("contacts sheet is one row per person",
     anyDuplicated(people$contact_id), 0L)
  ok("and counts attempts within the extract only",
     all(people$attempts_in_extract <= nrow(report()$sel)), TRUE)

  # THE CONTROL, not the intention. An address belonging to a contact who asked
  # not to be listed must not be anywhere in the workbook.
  private <- d$contact$contact_email[!d$contact$email_public]
  private <- private[!is.na(private) & nzchar(private)]
  ok("no private address reaches the contacts sheet",
     any(private %in% people$contact_email), FALSE)

  # ---- The HTML report ------------------------------------------------------
  #
  # Nothing here needs a browser. The Word export it replaced could only be
  # tested by faking the PNGs the browser would have posted back; this one is
  # built entirely on the server, so the file the test reads is the file a
  # reader gets.
  #
  # The assertions are about SELF-CONTAINMENT above all. A report that renders
  # perfectly on the machine that made it and loses its charts when it is
  # forwarded is the failure this format exists to avoid, so the test looks for
  # anything the file would have to fetch.
  # The report now arrives inside the bundle rather than from its own button,
  # so it is unpacked once here and again wherever a toggle has moved.
  report_from_bundle <- function() {
    session$setInputs(download_parts = "html")
    z <- tempfile(fileext = ".zip")
    file.copy(output$download, z, overwrite = TRUE)
    dir <- tempfile(); dir.create(dir)
    utils::unzip(z, exdir = dir)
    list.files(dir, pattern = "[.]html$", full.names = TRUE)[1]
  }

  html <- report_from_bundle()
  ok("html report downloads", file.exists(html), TRUE)

  doc <- fw_html_read_text(html)
  has <- function(txt) grepl(txt, doc, fixed = TRUE)

  ok("the letterhead names the database", has("A world evidence base"), TRUE)
  # STILL HERE, even though the page's own panel has gone to About. A document
  # that leaves the building has to carry its own qualifications.
  ok("the caveats travel with the document", has("Claimed is not the same"), TRUE)
  ok("and so do the contacts", has("Potential relevant contacts"), TRUE)
  ok("the cumulative chart does not", has("How the record has"), FALSE)
  ok("the filter selection is recorded", has("What this report covers"), TRUE)
  ok("the print rules are the PDF export", has("@media print"), TRUE)

  # Every plotly figure and the leaflet map, as widget payloads rather than as
  # pictures of them. If a chart ever stops reaching the document this is what
  # notices - the file still builds, it just quietly loses a figure. The
  # expected count is READ FROM THE DOCUMENT (one figure block per chart, plus
  # the map) rather than written down, so adding a chart does not break this.
  n_widgets <- lengths(regmatches(doc, gregexpr('data-for="htmlwidget', doc, fixed = TRUE)))
  n_figures <- lengths(regmatches(doc, gregexpr('class="fw-report__figure"', doc, fixed = TRUE)))
  n_maps    <- lengths(regmatches(doc, gregexpr('class="fw-report__map"', doc, fixed = TRUE)))
  ok("every figure travels as a live widget", n_widgets, n_figures + n_maps)
  ok("the report carries more than one figure", n_figures > 1L, TRUE)
  ok("plotly is bundled, not linked", has("Plotly.newPlot"), TRUE)
  ok("leaflet is bundled, not linked", has("leaflet-container"), TRUE)

  # NOTHING IS FETCHED FROM DISK. Every asset reference in the file has to be a
  # data: URI; a relative path would resolve against wherever the reader saved
  # the file and find nothing there.
  #
  # Matched on the ASSET EXTENSION rather than on "anything not absolute",
  # because three and a half megabytes of minified plotly contains string
  # literals that look like href= to a regular expression and are not.
  refs <- regmatches(doc, gregexpr('(?:src|href)="[^"]*"', doc, perl = TRUE))[[1]]
  refs <- sub('^[a-z]+="', "", sub('"$', "", refs))
  local_refs <- grep("^(data:|#|https?:|mailto:|blob:)", refs,
                     value = TRUE, invert = TRUE)
  assets <- grep("[.](css|js|png|jpe?g|gif|svg|woff2?|ttf|ico)([?#].*)?$",
                 local_refs, value = TRUE, ignore.case = TRUE)
  ok("nothing in the file is fetched from disk", length(assets), 0L)
  # A newline inside url() is a CSS parse error, which silently costs the file
  # every inlined image. See fw_html_base64().
  ok("no data URI is line-wrapped",
     grepl("data:[a-z/+.-]+;base64,[A-Za-z0-9+/=]*\\n", doc), FALSE)

  # ---- The data carried inside the report ----------------------------------
  payload <- function(id) {
    one <- regmatches(doc, regexpr(
      paste0('<script id="', id, '".*?</script>'), doc, perl = TRUE))
    jsonlite::base64_dec(sub("</script>$", "", sub('^<script[^>]*>', "", one)))
  }

  csv_bytes <- payload("fw-file-csv")
  csv_path <- tempfile(fileext = ".csv")
  writeBin(csv_bytes, csv_path)
  carried <- utils::read.csv(csv_path, check.names = FALSE, encoding = "UTF-8")
  ok("the csv inside the report matches the selection",
     nrow(carried), nrow(report()$sel))
  ok("and carries every exported column",
     identical(names(carried), FW_EXPORT_COLUMNS), TRUE)

  xlsx_bytes <- payload("fw-file-xlsx")
  xlsx_path <- tempfile(fileext = ".xlsx")
  writeBin(xlsx_bytes, xlsx_path)
  ok("the workbook inside the report is the workbook beside it",
     all(c("Attempts", "Contacts", "Caveats", "Field definitions",
           "Filters applied") %in% openxlsx::getSheetNames(xlsx_path)), TRUE)
  ok("and it holds the same rows",
     nrow(openxlsx::read.xlsx(xlsx_path, "Attempts")), nrow(report()$sel))

  # WYSIWYG. The method chart has a share/count toggle and the document takes
  # whichever the reader is looking at, rather than re-deciding for them.
  session$setInputs(method_mode = "count")
  ok("the method chart follows the reader's toggle",
     grepl("Attempts", fw_html_read_text(report_from_bundle()), fixed = TRUE), TRUE)
  # The waterbody chart has its OWN toggle, and the document follows each
  # independently: share below, count above, in the same file.
  session$setInputs(method_wb_mode = "share")
  doc2 <- fw_html_read_text(report_from_bundle())
  ok("the waterbody chart follows its own toggle",
     grepl(fw_t("charts", "x_share_uses"), doc2, fixed = TRUE), TRUE)
  ok("and the method chart above it stays on count",
     grepl(fw_t("charts", "x_attempts"), doc2, fixed = TRUE) &&
       !grepl(fw_t("charts", "x_share"), doc2, fixed = TRUE), TRUE)
  # The caption naming attempts with no method, recomputed in base R. Present
  # with the right number when there are any, absent when there are none.
  n_no_method <- sum(!report()$sel$attempt_id %in% d$attempt_method$attempt_id)
  ok(sprintf("the no-method caption is %s (%d such attempts)",
             if (n_no_method > 0) "present" else "absent", n_no_method),
     grepl(fw_fill(fw_t("plan", "r_method_missing"), n = fw_fmt_num(n_no_method)),
           doc2, fixed = TRUE),
     n_no_method > 0)
})

cat("\n")
if (failures > 0L) stop(failures, " report builder assertion(s) failed", call. = FALSE)
cat("All report builder tests passed.\n")
