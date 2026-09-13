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

# The report builder's own filter set: everything in FW_FILTERS except outcome.
# Built from the registry rather than typed out, so this test exercises the same
# id list the page does.
plan_ids <- fw_plan_filter_ids()
base <- fw_filter_state(list(), plan_ids)
base$year_from <- ch$year_min
base$year_to <- ch$year_max
base$include_no_year <- TRUE

cat("\n-- the copy deck --\n")
ok("no section is defined in two copy files",
   anyDuplicated(names(fw_copy_all())), 0L)

cat("\n-- filters --\n")
ok("no filters returns everything", nrow(fw_filter_apply(d, base)), nrow(d$attempt))

# ANY-OF, not all-of, and no attempt counted twice. If the bridge join leaked
# duplicates this would exceed the sum of the two individual counts.
f_r <- modifyList(base, list(method = "Rotenone"))
f_d <- modifyList(base, list(method = "Draining"))
f_b <- modifyList(base, list(method = c("Rotenone", "Draining")))
n_r <- nrow(fw_filter_apply(d, f_r)); n_d <- nrow(fw_filter_apply(d, f_d))
n_b <- nrow(fw_filter_apply(d, f_b))
ok("two methods is a union, not an intersection", n_b >= max(n_r, n_d))
ok("and does not double-count",                   n_b <= n_r + n_d)

# The 53 undated attempts must never vanish without the user asking.
ok("undated attempts kept by default",
   nrow(fw_filter_apply(d, base)) - nrow(fw_filter_apply(d, modifyList(base, list(include_no_year = FALSE)))),
   ch$n_no_year)

# A missing slider value means "no bound", not "nothing matches". The filter
# panel is a renderUI, so this is the state on first paint.
ok("absent year bounds do not empty the result",
   nrow(fw_filter_apply(d, modifyList(base, list(year_from = NULL, year_to = NULL)))),
   nrow(d$attempt))

# The one place the two pages differ, and it is deliberate. See mod_plan.R.
ok("outcome is not a report-builder filter", "outcome" %in% plan_ids, FALSE)
ok("but the dashboard still offers it",      "outcome" %in% fw_filter_ids(), TRUE)
ok("every other filter is shared",
   setdiff(fw_filter_ids(), plan_ids), "outcome")

# The registry drives the panel, the state, the hints and the workbook. If a
# filter is offered on the page it must appear in the sheet that records what
# the reader selected.
sheet_settings <- fw_filters_sheet(base, 10L, 914L)$Setting
ok("every filter reaches the workbook sheet",
   all(vapply(setdiff(plan_ids, "years"), function(i) {
     fw_filter_label(i) %in% sheet_settings
   }, logical(1))))
ok("and outcome does not", fw_filter_label("outcome") %in% sheet_settings, FALSE)

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
  ok("caveats sit beside the results", grepl("laimed is not the same", h), TRUE)
  # Counted, never hardcoded: every block fw_caveats() defines reaches the panel
  # with its heading, so adding or removing one needs no edit here.
  cav <- fw_caveat_blocks(d)
  ok("every caveat block reaches the panel",
     sum(vapply(cav, function(b) grepl(fw_caveat_title(b$heading), h, fixed = TRUE),
                logical(1))), length(cav))
  ok("export matches the selection",
     nrow(report()$export), nrow(report()$sel))

  session$setInputs(taxa = "Turtle", build = 2)
  h <- strip(output$results)
  ok("state 2: zero result says so plainly",
     grepl("No attempts match those filters", h), TRUE)
  ok("and names a filter to relax", grepl("Try relaxing one of these first", h), TRUE)
  ok("caveats are shown even with no results",
     grepl("laimed is not the same", h), TRUE)

  session$setInputs(taxa = character(0), build = 3)
  path <- output$download
  ok("workbook downloads", file.exists(path), TRUE)
  sheets <- openxlsx::getSheetNames(path)
  ok("all four sheets present",
     all(c("Attempts", "Caveats", "Field definitions", "Filters applied") %in% sheets), TRUE)
  ok("data sheet matches the selection",
     nrow(openxlsx::read.xlsx(path, "Attempts")), nrow(report()$sel))
  ok("field definitions cover every exported column",
     all(names(openxlsx::read.xlsx(path, "Attempts")) %in%
           openxlsx::read.xlsx(path, "Field definitions")$Field), TRUE)

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
  html <- output$download_html
  ok("html report downloads", file.exists(html), TRUE)

  doc <- fw_html_read_text(html)
  has <- function(txt) grepl(txt, doc, fixed = TRUE)

  ok("the letterhead names the database", has("A world evidence base"), TRUE)
  ok("the caveats travel with the document", has("Claimed is not the same"), TRUE)
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
     all(c("Attempts", "Caveats", "Field definitions", "Filters applied") %in%
           openxlsx::getSheetNames(xlsx_path)), TRUE)
  ok("and it holds the same rows",
     nrow(openxlsx::read.xlsx(xlsx_path, "Attempts")), nrow(report()$sel))

  # WYSIWYG. The method chart has a share/count toggle and the document takes
  # whichever the reader is looking at, rather than re-deciding for them.
  session$setInputs(method_mode = "count")
  ok("the method chart follows the reader's toggle",
     grepl("Attempts", fw_html_read_text(output$download_html), fixed = TRUE), TRUE)
  # The waterbody chart has its OWN toggle, and the document follows each
  # independently: share below, count above, in the same file.
  session$setInputs(method_wb_mode = "share")
  doc2 <- fw_html_read_text(output$download_html)
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
