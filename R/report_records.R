# report_records.R
# The attempts download: one self-contained .html file with every attempt in
# the reader's selection written out in full, one card after another.
#
# THE RECORD-BY-RECORD READING THE CLIENT ASKED FOR. The report builder's table
# of attempts went because a table of truncated cells is what the CSV already
# does better; what a reader could not do was sit down and read the attempts
# themselves. This is that: every field of every attempt, labelled, grouped the
# way a person reads a record - where, what water, which animals, when, how,
# what happened, the source, who to ask - and scrollable end to end.
#
# BUILT FROM THE EXPORT FRAME, NOT FROM THE DATABASE. The cards are the rows of
# fw_export_frame() for the same selection, so they carry exactly what the
# spreadsheet in the same download carries, in the same order, with the same
# redaction already applied and asserted (fw_assert_export_safe()). A private
# address cannot reach this file by a route the spreadsheet does not also take.
#
# EVERY FIELD, ALWAYS. A blank field says "Not noted" rather than vanishing -
# the rule the map's record card follows (client, 21 Sept 2026) - so two cards
# can be compared line for line.
#
# WITH TWO EXCEPTIONS. THE CHEMICAL DETAIL (client, 23 Sept 2026): an attempt
# that used no chemical method has nothing to say about target concentrations
# or neutralising agents, and seven "Not noted" lines under a heading about
# chemicals is not a comparison, it is noise. The section is cut from that card
# alone - see fw_record_group_drop(), which also refuses to cut it when any of
# those fields actually holds a value, so the rule above can never lose data.
#
# AND REGION (client, 24 Sept 2026): see FW_RECORD_OMIT_BLANK below. Both
# exceptions drop a field or a section that was never going to be filled; a
# field that COULD have been filled and was not still says "Not noted", which
# is the whole point of the rule.
#
# SELF-CONTAINED. The stylesheet, the fonts and the logos are inlined; there is
# no script beyond the few lines of the find box, and no network request. No
# photographs: they would make the file grow with the selection, and the PDF
# report carries the species pictures.

library(htmltools)

# ---- Inlining ------------------------------------------------------------------

#' The MIME type for an inlined asset, by extension
#'
#' A short closed list rather than a guess: anything not on it is something
#' this file should not be embedding.
FW_HTML_MIME <- c(
  png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
  svg = "image/svg+xml", woff2 = "font/woff2", woff = "font/woff", ttf = "font/ttf"
)

#' Base64 for embedding, on ONE line
#
fw_html_base64 <- function(path) {
  gsub("[\r\n]", "", jsonlite::base64_enc(readBin(path, "raw", file.size(path))))
}

#' A file as a base64 data URI, or NULL if it cannot be embedded
fw_html_data_uri <- function(path) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(NULL)
  mime <- unname(FW_HTML_MIME[tolower(tools::file_ext(path))])
  if (!length(mime) || is.na(mime)) return(NULL)
  paste0("data:", mime, ";base64,", fw_html_base64(path))
}

#' Rewrite every relative url() in a stylesheet to a data URI
#'
#' A stylesheet moved out of its own directory takes its relative references
#' with it: records.scss asks for ../fonts/ubuntu-400.woff2, which resolves
#' against wherever the reader saved the file and finds nothing there.
fw_html_inline_css_urls <- function(css, dir) {
  refs <- regmatches(css, gregexpr("url\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", css))[[1]]
  for (ref in unique(refs)) {
    target <- sub("['\"]?\\s*\\)$", "", sub("^url\\(\\s*['\"]?", "", ref))
    if (grepl("^(data:|https?:|//)", target)) next
    uri <- fw_html_data_uri(file.path(dir, sub("[?#].*$", "", target)))
    if (!is.null(uri)) css <- gsub(ref, paste0("url(\"", uri, "\")"), css, fixed = TRUE)
  }
  css
}

#' The attempts file's stylesheet, compiled and inlined
fw_records_css <- function(dir = "www/scss") {
  fw_html_inline_css_urls(fw_compile_css(file.path(dir, "records.scss")), dir)
}

# ---- A card --------------------------------------------------------------------

fw_record_copy <- function() {
  list(labels = fw_t("export", "record_labels"),
       groups = fw_t("export", "record_groups"),
       none = fw_t("species", "p_not_noted"),
       top = fw_t("export", "records_top"))
}

#' The outcome in words, with its data colour as a swatch beside it
fw_record_outcome <- function(outcome, none) {
  o <- if (is.na(outcome) || !outcome %in% FW_OUTCOME_LEVELS) "Unknown" else outcome
  paste0('<span class="fw-rec-outcome"><span class="fw-rec-outcome__dot" ',
         'aria-hidden="true" style="background:', FW_OUTCOME_COLOURS[[o]], ';"></span>',
         htmlEscape(if (is.na(outcome)) none else outcome), "</span>")
}

#' Whether a field of a record is empty, and so prints as "Not noted"
#'
#' Its own predicate because two things now ask the question: the renderer
#' below, and fw_record_group_drop(), which may not cut a section holding a
#' value. Two spellings of "empty" would be two answers.
fw_record_blank <- function(value) {
  length(value) != 1 || is.na(value) || !nzchar(trimws(as.character(value)))
}

#' Whether this card should leave a group out altogether
#'
#' ONLY THE CHEMICAL DETAIL, and only on an attempt whose methods carry no
#' chemical class - see the note at the head of this file. `method_classes` is
#' the export frame's collapsed list of the classes of the methods recorded on
#' the attempt ("chemical; mechanical"), and an attempt with no method row at
#' all leaves it NA, which counts as not chemical.
#'
#' AND ONLY WHEN THE SECTION IS EMPTY. A handful of records carry a
#' concentration or a neutralising agent against a method the database has
#' classed as mechanical. Cutting on the class alone would drop a recorded
#' value out of the attempts file while the spreadsheet in the same download
#' still carried it, which is a worse fault than the noise this fixes.
fw_record_group_drop <- function(g, row) {
  if (!identical(g$id, "chemical")) return(FALSE)
  classes <- strsplit(as.character(row$method_classes), FW_MULTI_SEP, fixed = TRUE)[[1]]
  if ("chemical" %in% classes) return(FALSE)
  all(vapply(g$fields, function(f) fw_record_blank(row[[f]]), logical(1)))
}

# A FIELD THAT IS DROPPED RATHER THAN DRAWN EMPTY (client, 24 Sept 2026).
#
# Region is a state, province or territory, and only a handful of countries
# record one at all. "Australia" above "Region: Not noted" reads as a missing
# Tasmania - the reader goes looking for a value that was never coming - where
# the other blank fields read as what they are, a gap in the record. So this
# one vanishes when it is empty and prints normally when it is not.
#
# KEEP THIS LIST SHORT. Every name added to it is a line two cards can no
# longer be compared on. The spreadsheet keeps its region column either way: a
# blank cell in a grid is unambiguous in a way a missing row in a card is not.
FW_RECORD_OMIT_BLANK <- c("region")

fw_record_value <- function(field, value, none) {
  if (fw_record_blank(value)) {
    return(paste0('<span class="fw-rec-none">', htmlEscape(none), "</span>"))
  }
  v <- htmlEscape(as.character(value), attribute = TRUE)
  if (field == "reference_link" && grepl("^https?://", value)) {
    return(paste0('<a href="', v, '" target="_blank" rel="noopener noreferrer">', v, "</a>"))
  }
  if (grepl("_email$", field)) return(paste0('<a href="mailto:', v, '">', v, "</a>"))
  if (field == "outcome") return(fw_record_outcome(value, none))
  v
}

#' What the find box matches a card on
fw_record_search <- function(row) {
  x <- c(row$attempt_id, row$site_name, row$country, row$region,
         row$invasive_species, row$beneficiary_species, row$methods)
  tolower(paste(x[!is.na(x)], collapse = " "))
}

#' The years a card covers, "1994-2002", for its contents line
fw_record_years <- function(row) {
  s <- row$start_year; e <- row$end_year
  if (is.na(s) && is.na(e)) return(NULL)
  paste0(if (is.na(s)) "?" else s, "-", if (is.na(e)) "" else e)
}

#' One attempt as a card, as an HTML string
#'
#' @param row one row of fw_export_frame(), as a list
#' @param copy fw_record_copy()
fw_record_card <- function(row, copy = fw_record_copy()) {
  esc <- function(x) htmlEscape(as.character(x))
  place <- c(row$region, row$country)
  place <- paste(place[!is.na(place) & nzchar(place)], collapse = ", ")
  title <- if (is.na(row$site_name) || !nzchar(row$site_name)) copy$none else row$site_name
  # Filtered before the headings are built, so a dropped section takes its <h3>
  # with it rather than leaving an empty <dl> under one.
  shown <- Filter(function(g) !fw_record_group_drop(g, row), copy$groups)
  groups <- vapply(shown, function(g) {
    # Filtered before the pairs are built, so an omitted field takes its <dt>
    # with it rather than leaving a label above an empty <dd>. No group is at
    # risk of emptying out: "Where" also holds the site, country, continent,
    # ISO code and coordinates, all of which always draw.
    keep <- Filter(function(f) {
      !(f %in% FW_RECORD_OMIT_BLANK) || !fw_record_blank(row[[f]])
    }, g$fields)
    fields <- vapply(keep, function(f) {
      paste0("<dt>", esc(copy$labels[[f]]), "</dt><dd>",
             fw_record_value(f, row[[f]], copy$none), "</dd>")
    }, character(1))
    paste0("<h3>", esc(g$heading), '</h3><dl class="fw-rec-fields">',
           paste(fields, collapse = ""), "</dl>")
  }, character(1))
  paste0(
    '<article class="fw-rec-card" id="', esc(row$attempt_id), '" data-search="',
    htmlEscape(fw_record_search(row), attribute = TRUE), '">',
    '<div class="fw-rec-card__head"><div><h2>', esc(title), "</h2>",
    if (nzchar(place)) paste0('<p class="fw-rec-card__place">', esc(place), "</p>") else "",
    '</div><div class="fw-rec-card__meta">', fw_record_outcome(row$outcome, copy$none),
    '<span class="fw-rec-id">', esc(row$attempt_id), "</span></div></div>",
    paste(groups, collapse = ""),
    '<p class="fw-rec-card__top"><a href="#fw-rec-contents">', esc(copy$top), "</a></p>",
    "</article>"
  )
}

# ---- The file ----------------------------------------------------------------------

#' The find box's script
#'
#' Narrows the contents list and the cards to those whose id, site, place,
#' species or methods contain what was typed. Plain DOM, no library, and it
#' works from file:// because it asks nothing of the network.
fw_records_script <- function(total_label) {
  tags$script(HTML(sprintf("
    (function () {
      var box = document.getElementById('fw-rec-find');
      var out = document.getElementById('fw-rec-count');
      var cards = document.querySelectorAll('.fw-rec-card');
      var items = document.querySelectorAll('.fw-rec-toc li');
      var template = %s;
      box.addEventListener('input', function () {
        var q = box.value.trim().toLowerCase(), n = 0;
        for (var i = 0; i < cards.length; i++) {
          var hit = !q || cards[i].getAttribute('data-search').indexOf(q) !== -1;
          cards[i].hidden = !hit;
          if (items[i]) items[i].hidden = !hit;
          if (hit) n++;
        }
        out.textContent = template.replace('{n}', n).replace('{total}', cards.length);
      });
    })();", jsonlite::toJSON(total_label, auto_unbox = TRUE))))
}

#' Write the attempts file
#'
#' @param path    where to write. The bundle's working directory.
#' @param data    the loaded tables, for the caveats and the filter record
#' @param export  fw_export_frame() for the selection: the rows, in order
#' @param filters the filter snapshot taken when Build was pressed
fw_write_records_html <- function(path, data, export, filters, meta = NULL) {
  generated <- format(Sys.time(), "%d %B %Y", tz = "UTC")
  n <- nrow(export)
  # As plain lists: a one-row data frame per card is the slow way to read a
  # field, and there are fifty-five fields a card.
  rows <- lapply(seq_len(n), function(i) as.list(export[i, , drop = FALSE]))
  copy <- fw_record_copy()

  logo <- function(file, alt) {
    uri <- fw_html_data_uri(file)
    if (!is.null(uri)) tags$img(src = uri, alt = alt)
  }
  selection <- fw_filters_sheet(filters, n, nrow(data$attempt), meta)

  body <- div(
    class = "fw-rec",
    tags$header(
      class = "fw-rec-head",
      if (!is.null(fw_html_data_uri(FW_LOGO$mark_file))) {
        tags$img(class = "fw-rec-head__mark", src = fw_html_data_uri(FW_LOGO$mark_file),
                 alt = fw_t("app", "full_title"))
      },
      h1(fw_t("export", "records_title")),
      p(class = "fw-rec-head__subtitle",
        fw_fill(fw_t("export", "records_subtitle"), n = fw_fmt_num(n), date = generated)),
      p(class = "fw-rec-head__lead", fw_t("export", "records_lead"))
    ),

    # What was asked, folded: a record of the question, not the thing read.
    tags$details(
      class = "fw-rec-panel",
      tags$summary(fw_t("export", "records_selection")),
      tags$table(
        class = "fw-rec-table",
        tags$thead(tags$tr(lapply(names(selection), function(h) tags$th(scope = "col", h)))),
        tags$tbody(lapply(seq_len(nrow(selection)), function(i) {
          tags$tr(tags$td(selection[i, 1]), tags$td(selection[i, 2]))
        }))
      )
    ),

    tags$nav(
      class = "fw-rec-panel", id = "fw-rec-contents",
      `aria-labelledby` = "fw-rec-contents-h",
      h2(id = "fw-rec-contents-h", fw_t("export", "records_contents")),
      div(
        class = "fw-rec-find",
        tags$label(`for` = "fw-rec-find", fw_t("export", "records_filter_label")),
        tags$input(id = "fw-rec-find", type = "search", autocomplete = "off",
                   placeholder = fw_t("export", "records_filter_hint")),
        tags$output(id = "fw-rec-count", `for` = "fw-rec-find", `aria-live` = "polite",
                    fw_fill(fw_t("export", "records_showing"),
                            n = fw_fmt_num(n), total = fw_fmt_num(n)))
      ),
      HTML(paste0('<ol class="fw-rec-toc">', paste(vapply(rows, function(r) {
        yrs <- fw_record_years(r)
        paste0('<li><a href="#', htmlEscape(r$attempt_id), '"><span class="fw-rec-id">',
               htmlEscape(r$attempt_id), "</span> ",
               htmlEscape(if (is.na(r$site_name)) copy$none else r$site_name),
               if (!is.na(r$country)) htmlEscape(paste0(", ", r$country)) else "",
               if (!is.null(yrs)) paste0(" (", yrs, ")") else "",
               "</a></li>")
      }, character(1)), collapse = ""), "</ol>"))
    ),

    tags$main(HTML(paste(vapply(rows, fw_record_card, character(1), copy = copy),
                         collapse = "\n"))),

    # HOW IT WAS BUILT AND WHAT TO WATCH FOR, last, and never optional. This
    # file is the one most likely to be forwarded on its own, and since the
    # methods-and-caveats .txt stopped travelling beside it (client, 24 Sept
    # 2026) it is the only thing carrying the section.
    tags$section(
      class = "fw-rec-caveats",
      h2(fw_t("export", "closing_heading")),
      lapply(fw_closing_blocks(data), function(b) {
        title <- fw_caveat_title(b$heading)
        tagList(if (nzchar(title)) h3(title), lapply(b$body, p))
      })
    ),

    tags$footer(
      class = "fw-rec-foot",
      div(class = "fw-rec-foot__logos",
          logo(FW_LOGO$wfa_file, fw_t("footer", "logo_alt_wfa")),
          logo(FW_LOGO$collab_files[["fwl"]], fw_t("footer", "logo_alt_fwl")),
          logo(FW_LOGO$collab_files[["ucsc"]], fw_t("footer", "logo_alt_ucsc")),
          logo(FW_LOGO$collab_files[["scripps"]], fw_t("footer", "logo_alt_scripps")),
          logo(FW_LOGO$collab_files[["issg"]], fw_t("footer", "logo_alt_issg"))),
      p(fw_t("plan", "report_footer"))
    ),

    fw_records_script(fw_t("export", "records_showing"))
  )

  doc <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmlEscape(paste0(fw_t("export", "records_title"), " - ",
                                 fw_t("app", "title"))), "</title>\n",
    "<style>", fw_records_css(), "</style>\n</head>\n<body>\n",
    as.character(body), "\n</body>\n</html>\n"
  )
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(doc)), con)
  invisible(path)
}

#' Filename for the attempts file
fw_records_filename <- function() {
  fw_fill(fw_t("export", "records_filename"), date = format(Sys.Date(), "%Y%m%d"))
}
