# report_html.R
# The report builder's document output: the report the reader is looking at, on
# FWISE letterhead, as ONE self-contained .html file they can email, open on any
# machine, print to PDF, and pull the data back out of.
#
# WHY HTML AND NOT .docx OR A SERVER-RENDERED PDF. This replaced a Word export,
# and the reasons are worth keeping written down because the Word route looks
# cheaper than it is.
#
#   - NO ROUND TRIP TO THE BROWSER. The Word file could not contain a chart
#     unless the browser first photographed every figure with Plotly.toImage(),
#     posted the base64 PNGs back over the websocket into a Shiny input, and the
#     server then clicked a hidden download button on the reader's behalf. That
#     whole dance is gone. This is an ordinary downloadHandler: press the
#     button, get the file.
#   - VECTOR, NOT A SCREENSHOT. The charts travel as live plotly figures rather
#     than flattened PNGs, so they stay crisp at any zoom and at print
#     resolution, and they keep their hover readouts.
#   - THE MAP COMES TOO. Leaflet could never be captured to a PNG - its tiles
#     are cross-origin and taint the canvas - so the Word file had a country
#     table standing in for it. Here the real map travels, and the country table
#     stays beside it for the printed page and for readers with no connection.
#   - THE PAGE'S OWN COMPONENTS. The summary strip, the outcome bars, the
#     attempts table and the caveats panel are the SAME functions the page
#     renders, under the SAME compiled stylesheet. Nothing is translated into
#     Word table primitives, so the report cannot drift away from the screen.
#   - NO NEW INFRASTRUCTURE. No kaleido, no Python, no webshot2, no headless
#     Chrome, no LaTeX. Which is exactly the constraint that deferred PDF output
#     in the first place (HANDOVER.md section 1).
#
# HOW A PDF GETS MADE. The reader presses "Save as PDF" in the report, which
# calls window.print(). The @media print rules below drop the toolbar, force
# background colours to survive, repeat table headers across pages and keep
# figures from splitting. The browser's own print engine is the lightest and the
# highest-fidelity renderer available, and it is already on the reader's
# machine. A bundled html2pdf.js or jsPDF was considered and rejected: both
# rasterise the DOM to a canvas, which throws away the vector output that is
# half the point of this file, and both would add a library to a file that
# currently needs none.
#
# SELF-CONTAINED MEANS SELF-CONTAINED. Every stylesheet, every script, the
# webfonts, the logo and the data all travel inside the one file as inlined text
# or data URIs. Nothing is fetched from a CDN and nothing sits in a sibling
# _files/ directory, because a report that loses its charts when it is forwarded
# as an email attachment is not a report. The one exception is stated on the
# face of the document: the map's tile background is drawn from Carto's servers
# and needs a connection.
#
# The file is around 4 MB, nearly all of it plotly.js. That is the price of
# interactive vector figures that work offline, and it is comparable to the Word
# file it replaces once that file's PNGs were counted.

library(shiny)
library(htmltools)

# ---- Inlining ----------------------------------------------------------------

#' The MIME type for an inlined asset, by extension
#'
#' Deliberately a short closed list rather than a guess: anything not on it is
#' something this report should not be embedding.
FW_HTML_MIME <- c(
  png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
  gif = "image/gif", svg = "image/svg+xml", webp = "image/webp",
  woff2 = "font/woff2", woff = "font/woff", ttf = "font/ttf"
)

fw_html_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  unname(FW_HTML_MIME[ext])
}

#' Base64 for embedding, on ONE line
#'
#' jsonlite rather than base64enc, for the same reason the Word export gave: it
#' is a direct dependency of the data layer and unambiguously in the manifest,
#' where base64enc is only present because something else pulls it in.
#'
#' THE LINE BREAKS HAVE TO GO. jsonlite::base64_enc() wraps its output at 76
#' characters, which is correct for MIME and wrong for a data URI: CSS will not
#' parse a newline inside url(), so an inlined stylesheet keeps its rules and
#' silently loses every image - leaflet's zoom and layer icons come out as empty
#' white boxes with nothing in the console to say why. HTML attributes tolerate
#' the wrapping, which is what makes this a bug that hides.
fw_html_base64 <- function(path) {
  gsub("[\r\n]", "", jsonlite::base64_enc(
    readBin(path, "raw", file.size(path))))
}

#' A file as a base64 data URI, or NULL if it cannot be embedded
fw_html_data_uri <- function(path) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(NULL)
  mime <- fw_html_mime(path)
  if (is.na(mime) || is.null(mime)) return(NULL)
  paste0("data:", mime, ";base64,", fw_html_base64(path))
}

#' Read a text asset as UTF-8, whatever the machine's locale says
fw_html_read_text <- function(path) {
  txt <- readBin(path, "raw", file.size(path))
  out <- rawToChar(txt)
  Encoding(out) <- "UTF-8"
  out
}

#' Rewrite every url() in a stylesheet to a data URI
#'
#' THIS IS WHY INLINING CSS IS NOT JUST PASTING IT. A stylesheet moved out of its
#' own directory takes its relative references with it, and they no longer
#' resolve: leaflet.css asks for images/marker-icon.png, main.css asks for
#' ../fonts/ubuntu-400.woff2. Pasted naively into a <style> block those become
#' requests against wherever the reader saved the file, and the result is a map
#' with no zoom icons and a report set in Arial.
#'
#' Anything that is already absolute - a data: URI, an http(s) URL - is left
#' alone, as is anything that does not resolve to a file on disk.
fw_html_inline_css_urls <- function(css, dir) {
  m <- gregexpr("url\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", css)
  refs <- regmatches(css, m)[[1]]
  if (!length(refs)) return(css)

  for (ref in unique(refs)) {
    target <- sub("^url\\(\\s*['\"]?", "", ref)
    target <- sub("['\"]?\\s*\\)$", "", target)
    if (grepl("^(data:|https?:|//)", target)) next
    target <- sub("[?#].*$", "", target)          # cache-busters and fragments
    path <- file.path(dir, target)
    if (!file.exists(path)) next
    uri <- fw_html_data_uri(path)
    if (is.null(uri)) next
    css <- gsub(ref, paste0("url(\"", uri, "\")"), css, fixed = TRUE)
  }
  css
}

#' Where a resolved html dependency's files actually live
fw_html_dep_dir <- function(dep) {
  path <- dep$src$file
  if (is.null(path)) return(NULL)
  if (!is.null(dep$package)) path <- system.file(path, package = dep$package)
  if (!nzchar(path) || !dir.exists(path)) NULL else path
}

#' A script or style tag whose content cannot close its own element
#'
#' plotly.js is three and a half megabytes of minified JavaScript and nobody has
#' read all of it. If a string literal in there contains the characters
#' "</script", the browser's HTML parser ends the element in the middle of the
#' library. Escaping the slash is valid inside a JS string and impossible
#' outside one, so this is safe in both directions.
fw_html_guard <- function(text, tag) {
  gsub(paste0("</", tag), paste0("<\\/", tag), text, fixed = TRUE)
}

#' Every dependency of a set of widgets, inlined into head tags
#'
#' Order is the order htmltools resolved them in, which is the order the widgets
#' need: htmlwidgets.js before the bindings, plotly.js before plotly's binding
#' runs. Do not sort this.
#'
#' htmlwidgets.js static-renders every widget on the page on DOMContentLoaded
#' when Shiny is absent, which is what makes the figures in a saved file draw
#' themselves with no bootstrapping code of our own. That only works if these
#' tags are in <head>, before the document finishes parsing.
fw_html_dependency_tags <- function(deps) {
  out <- list()
  for (dep in htmltools::resolveDependencies(deps)) {
    dir <- fw_html_dep_dir(dep)
    if (is.null(dir)) next

    for (css in unlist(dep$stylesheet)) {
      path <- file.path(dir, css)
      if (!file.exists(path)) next
      out <- c(out, list(tags$style(
        type = "text/css",
        HTML(fw_html_guard(fw_html_inline_css_urls(
          fw_html_read_text(path), dirname(path)), "style"))
      )))
    }
    for (js in unlist(dep$script)) {
      path <- file.path(dir, js)
      if (!file.exists(path)) next
      out <- c(out, list(tags$script(
        HTML(fw_html_guard(fw_html_read_text(path), "script"))
      )))
    }
    if (!is.null(dep$head)) out <- c(out, list(HTML(dep$head)))
  }
  tagList(out)
}

#' The app's own compiled stylesheet, inlined
#'
#' THE SAME SASS THE APP SERVES, not a second stylesheet written for print. That
#' is the whole reason the report can reuse fw_plan_summary_ui(),
#' fw_outcome_bars_ui() and fw_caveats_ui() directly: the
#' markup and the rules that draw it travel together, so a component restyled in
#' _components.scss is restyled in every report built afterwards, with no second
#' edit and no chance of the two disagreeing.
#'
#' Compiled through fw_compile_css(), so it carries the same tokens as the app.
fw_html_app_css <- function(dir = "www/scss") {
  fw_html_inline_css_urls(fw_compile_css(file.path(dir, "main.scss")), dir)
}

# ---- The report's own stylesheet ---------------------------------------------

#' What the app's stylesheet does not cover: the page frame, and print
#'
#' Lives in www/scss/_report_frame.scss and is compiled through the same
#' fw_compile_css() as the app, so the letterhead, the toolbar and the print
#' rules use the same tokens as everything else. The print rules ARE the PDF
#' export; see the note at the top of that file.
fw_html_report_css <- function() {
  HTML(fw_compile_css("www/scss/report.scss"))
}

# ---- The report's own script -------------------------------------------------

#' Printing, and getting the data back out
#'
#' Two jobs, no library, and both have to work from a file:// URL because that
#' is where this report lives once it has been saved or emailed.
#'
#' THE DOWNLOADS ARE BLOBS, NOT data: HREFS. The spreadsheet inside this file is
#' hundreds of kilobytes; as a data: URI on an <a href> that is a multi-megabyte
#' attribute, and browsers have repeatedly tightened what they will do with
#' those. Decoding to a Blob and handing out an object URL is the route that
#' behaves the same everywhere, including offline and on file://.
#'
#' RESIZE BEFORE PRINTING. plotly and leaflet both size themselves once, against
#' the screen. Neither notices that the page is about to be laid out on A4, so
#' both are told, and told again afterwards so the reader's screen is not left
#' with a figure sized for paper.
fw_html_report_script <- function() {
  tags$script(HTML("
    (function () {
      function payload(id) {
        var el = document.getElementById(id);
        if (!el) return null;
        var bin = atob((el.textContent || '').replace(/\\s+/g, ''));
        var bytes = new Uint8Array(bin.length);
        for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
        return { bytes: bytes, name: el.getAttribute('data-name'),
                 type: el.getAttribute('data-type') };
      }

      window.fwDownload = function (id) {
        var p = payload(id);
        if (!p) return;
        var url = URL.createObjectURL(new Blob([p.bytes], { type: p.type }));
        var a = document.createElement('a');
        a.href = url;
        a.download = p.name;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function () { URL.revokeObjectURL(url); }, 2000);
      };

      function resizeFigures() {
        if (window.Plotly) {
          var plots = document.querySelectorAll('.js-plotly-plot');
          for (var i = 0; i < plots.length; i++) {
            try { Plotly.Plots.resize(plots[i]); } catch (e) {}
          }
        }
        if (window.HTMLWidgets) {
          var maps = document.querySelectorAll('.leaflet.html-widget');
          for (var j = 0; j < maps.length; j++) {
            try {
              var w = HTMLWidgets.getInstance(maps[j]);
              var m = w && (w.getMap ? w.getMap() : w);
              if (m && m.invalidateSize) m.invalidateSize();
            } catch (e) {}
          }
        }
      }

      window.fwPrint = function () {
        resizeFigures();
        // One frame, so the resize above has been laid out before the print
        // dialogue takes its snapshot of the document.
        setTimeout(function () { window.print(); }, 120);
      };

      window.addEventListener('beforeprint', resizeFigures);
      window.addEventListener('afterprint', resizeFigures);
    })();
  "))
}

# ---- Embedded files ----------------------------------------------------------

#' A file carried inside the report, for the reader to pull back out
#'
#' type=\"application/base64\" is not a script type any browser executes, so this
#' is inert markup that happens to hold a payload. Base64's alphabet contains no
#' angle brackets, so nothing in here can close the element early.
fw_html_payload <- function(id, path, filename, mime) {
  tags$script(
    id = id, type = "application/base64",
    `data-name` = filename, `data-type` = mime,
    HTML(fw_html_base64(path))
  )
}

#' The flattened export as a CSV on disk
#'
#' UTF-8 with no byte-order mark. Species names carry accents and a BOM would
#' make Excel read them correctly while breaking a good number of the data tools
#' this CSV is actually for; the .xlsx alongside it is the answer for Excel.
fw_html_write_csv <- function(export, dir) {
  path <- file.path(dir, "fwise-attempts.csv")
  utils::write.csv(export, path, row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  path
}

# ---- Tables ------------------------------------------------------------------

#' A table in the app's own table style
#'
#' @param num columns to set in the mono face and right-align, because they are
#'   figures being compared down a column rather than labels
fw_html_table <- function(df, num = character(0)) {
  if (is.null(df) || !nrow(df)) return(NULL)
  cls <- function(nm) if (nm %in% num) "fw-col-num" else NULL
  tags$table(
    class = "fw-table",
    tags$thead(tags$tr(lapply(names(df), function(nm)
      tags$th(scope = "col", class = cls(nm), nm)))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(names(df), function(nm) {
        v <- df[[nm]][i]
        tags$td(class = cls(nm),
                if (is.na(v) || !nzchar(as.character(v))) fw_t("common", "empty_value")
                else as.character(v))
      }))
    }))
  )
}

#' The filter snapshot as a table
#'
#' fw_filters_sheet() is the workbook's "Filters applied" sheet, built from the
#' same registry that draws the controls. Reused rather than rebuilt, so a filter
#' offered on the page cannot be missing from the document that records what was
#' selected.
fw_html_filters_table <- function(filters, n_rows, n_total, meta = NULL) {
  fw_html_table(fw_filters_sheet(filters, n_rows, n_total, meta))
}

#' Attempts by country
#'
#' Kept from the Word export even though the real map now travels with the
#' document, for two reasons that both still hold: the map's tile background
#' needs a connection, and a reader with the report on paper asking "where has
#' this been tried" wants a list, not a picture. Sorted by count and capped, with
#' the tail gathered rather than dropped, so the total still adds up.
fw_report_country_table <- function(sel, limit = FW_REPORT_COUNTRY_ROWS) {
  if (!nrow(sel)) return(NULL)
  counts <- sort(table(sel$country), decreasing = TRUE)
  keep <- utils::head(counts, limit)
  out <- data.frame(country = names(keep), attempts = as.integer(keep),
                    stringsAsFactors = FALSE)
  rest <- sum(counts) - sum(as.integer(keep))
  if (rest > 0) {
    out <- rbind(out, data.frame(
      country = fw_fill(fw_t("export", "other_countries"),
                        n = length(counts) - length(keep)),
      attempts = rest, stringsAsFactors = FALSE
    ))
  }
  out$attempts <- format(out$attempts, big.mark = ",", trim = TRUE)
  # The headings the reader sees come from the copy deck.
  names(out) <- c(fw_t("export", "col_country"), fw_t("export", "col_attempts"))
  out
}

# ---- Pieces of the document --------------------------------------------------

#' The FWISE letterhead
#'
#' The logo is inlined as a data URI, so the mark survives the file being
#' emailed, saved to a stick, or opened with the network down.
fw_html_letterhead <- function(title, subtitle) {
  logo <- fw_html_data_uri(FW_LOGO$mark_file)
  tags$header(
    class = "fw-report__head",
    if (!is.null(logo)) {
      tags$img(class = "fw-report__mark", src = logo,
               alt = fw_t("app", "full_title"))
    },
    p(class = "fw-report__tagline", fw_t("app", "tagline")),
    h1(class = "fw-report__title", title),
    p(class = "fw-report__subtitle", subtitle)
  )
}

#' The toolbar, and the note that explains what this file is
#'
#' Screen only. The print rules drop it, because a printed page with a "Save as
#' PDF" button on it is a page that has already been saved as a PDF.
fw_html_toolbar <- function() {
  tagList(
    div(
      class = "fw-report__tools",
      tags$button(type = "button", class = "fw-report__tool",
                  onclick = "fwPrint()", fw_t("plan", "html_print")),
      tags$button(type = "button", class = "fw-report__tool fw-report__tool--quiet",
                  onclick = "fwDownload('fw-file-csv')", fw_t("plan", "html_csv")),
      tags$button(type = "button", class = "fw-report__tool fw-report__tool--quiet",
                  onclick = "fwDownload('fw-file-xlsx')", fw_t("plan", "html_xlsx")),
      # The methods and caveats, carried here too. This file is the one piece of
      # the bundle that travels on its own - somebody emails the report, not the
      # zip - so it has to be able to hand back its own qualifications.
      tags$button(type = "button", class = "fw-report__tool fw-report__tool--quiet",
                  onclick = "fwDownload('fw-file-txt')", fw_t("plan", "html_txt")),
      p(fw_t("plan", "html_print_hint"))
    ),
    tags$ul(
      class = "fw-report__about fw-no-print",
      lapply(fw_t("plan", "html_about"), tags$li)
    )
  )
}

#' A titled block with its qualification directly beneath the heading
#'
#' Above, not below - the same rule the page follows, for the same reason. A
#' caveat printed under a chart is read after the reader has drawn their
#' conclusion from it.
fw_html_block <- function(title, note, ...) {
  tagList(
    h2(title),
    if (!is.null(note)) p(class = "fw-report__note", note),
    ...
  )
}

#' A plotly figure, or nothing at all
#'
#' A chart that had too little data to draw returns NULL, and when it does the
#' heading goes with it rather than standing over an empty box.
fw_html_figure <- function(title, note, widget, extra = NULL) {
  if (is.null(widget)) return(NULL)
  tagList(
    fw_html_block(title, note),
    div(class = "fw-report__figure", as.tags(widget, standalone = FALSE)),
    extra
  )
}

# ---- The document ------------------------------------------------------------

#' Write the HTML report
#'
#' @param path        where to write. The download handler's temp file.
#' @param data        the loaded tables, for the caveats and the summary
#' @param sel         the attempt rows this report is about
#' @param export      the flattened export frame for the same selection
#' @param filters     the filter snapshot taken when Build was pressed
#' @param meta        the release metadata, for provenance
#' @param method_mode "share" or "count" - whichever the reader is looking at,
#'   so the document matches the screen rather than re-deciding for them
#' @param method_wb_mode the same for the methods-by-waterbody chart, which has
#'   its own toggle because it counts uses rather than attempts
#' @param waterbody_mode the same again for the kind-of-water chart
#'
#' EVERY PIECE OF TEXT COMES FROM THE SAME PLACE AS THE PAGE. The headings, the
#' notes and the caveats are read from FW_COPY and fw_caveats() rather than
#' rewritten here, for the same reason the workbook does it: a document that
#' turns up in an inbox six months later has to carry its own qualifications,
#' and two copies of a caveat are two caveats that can disagree.
fw_write_html_report <- function(path, data, sel, export, filters,
                                 meta = NULL, method_mode = "count",
                                 method_wb_mode = "count",
                                 waterbody_mode = "count") {
  dir <- tempfile("fw-html-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  generated <- format(Sys.time(), "%d %B %Y", tz = "UTC")
  title <- fw_t("plan", "report_title")
  s <- fw_plan_summary(data, sel)
  n_no_coords <- sum(is.na(sel$latitude) | is.na(sel$longitude))
  n_no_method <- fw_n_no_method(data, sel)
  # THE COUNT THE CHART ACTUALLY DRAWS, not the count with a duration. The
  # chart drops attempts that used more than one method - see
  # fw_duration_sel() - so counting duration alone here would promise the
  # reader more points than they can see.
  n_duration <- nrow(fw_duration_sel(data, sel))
  # FW_PLAN_SPECIES_N, the same limit the page uses, so the document and the
  # screen show the same species rather than the report quietly showing more.
  tiles_invasive <- fw_species_tiles_ui(data, sel, "invasive",
                                        limit = FW_PLAN_SPECIES_N)
  tiles_beneficiary <- fw_species_tiles_ui(data, sel, "beneficiary",
                                           limit = FW_PLAN_SPECIES_N)

  # The two files the reader can pull back out. The workbook is built by the
  # SAME function the spreadsheet button serves, so the copy inside the report
  # and the copy downloaded beside it are byte-for-byte the same four sheets.
  csv_path <- fw_html_write_csv(export, dir)
  xlsx_path <- file.path(dir, fw_export_filename())
  fw_write_workbook(xlsx_path, data, export, filters, meta)
  txt_path <- file.path(dir, fw_methods_filename())
  writeLines(fw_methods_caveats_text(data), txt_path, useBytes = TRUE)

  body <- tagList(
    fw_html_letterhead(
      title,
      fw_fill(fw_t("plan", "report_subtitle"), date = generated)
    ),
    fw_html_toolbar(),

    # ---- What this report is a slice of --------------------------------------
    # First, before any finding. The reader has to know what was asked before
    # they read what came back, and the filters are the question.
    fw_html_block(fw_t("plan", "report_selection"), NULL,
                  fw_html_filters_table(filters, nrow(export),
                                        nrow(data$attempt), meta)),

    # ---- Summary -------------------------------------------------------------
    fw_html_block(fw_t("plan", "r_heading"), NULL, fw_plan_summary_ui(s)),

    # ---- The figures, in the page's order ------------------------------------
    # SAME ORDER AS THE SCREEN, and that is the requirement rather than a
    # preference. A reader who looked at the report builder and then downloaded
    # this file has to find the same argument in the same sequence, or the two
    # read as two different documents about the same selection. The funnel is:
    # where, then what happened, then in what water, then by what means, then to
    # which species - and the two charts about time at the end.

    # ---- Where ---------------------------------------------------------------
    # The real map, not a picture of one. It is the single clearest gain over
    # the Word export this replaced, which could not carry it at all.
    if (nrow(sel) > 0) {
      tagList(
        fw_html_block(fw_t("plan", "r_map"), fw_t("plan", "r_map_note")),
        # detail = "embed": there is no server to ask once the file is saved,
        # so every record travels inside its marker. See fw_add_attempt_markers().
        div(class = "fw-report__map",
            as.tags(fw_plan_map(data, sel, detail = "embed"), standalone = FALSE)),
        p(class = "fw-caption", fw_t("plan", "html_map_note")),
        fw_map_note(),
        if (n_no_coords > 0) {
          p(class = "fw-caption",
            fw_fill(fw_t("plan", "r_map_missing"), n = fw_fmt_num(n_no_coords)))
        },
        fw_html_block(fw_t("plan", "report_where"),
                      fw_t("plan", "report_where_note"),
                      fw_html_table(fw_report_country_table(sel),
                                    num = fw_t("export", "col_attempts")))
      )
    },

    # ---- Outcomes ------------------------------------------------------------
    fw_html_block(fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
                  fw_outcome_bars_ui(sel)),

    fw_html_figure(fw_t("plan", "r_waterbody"), fw_fill(fw_t("plan", "r_waterbody_note"), n_word = fw_num_word(FW_TOP_N)),
                   fw_chart_waterbody(sel, mode = waterbody_mode)),

    fw_html_figure(fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
                   fw_chart_method(data, sel, mode = method_mode),
                   extra = if (n_no_method > 0) {
                     p(class = "fw-caption",
                       fw_fill(fw_t("plan", "r_method_missing"),
                               n = fw_fmt_num(n_no_method)))
                   }),

    fw_html_figure(fw_t("plan", "r_method_wb"), fw_t("plan", "r_method_wb_note"),
                   fw_chart_method_waterbody(data, sel, mode = method_wb_mode)),

    # The species tiles are HTML rather than plotly, so they go through
    # fw_html_block() like the outcome bars do. Built ONCE above and tested for
    # NULL here: a block with NULL content still prints its heading, and
    # building them twice to ask whether they exist would repeat ten image
    # lookups for nothing.
    # THE SAME TWO TITLES AS THE PAGE - "Top three invasive species targeted
    # (of 41 total)" and its protected pair - from fw_species_top_title(), so
    # the document and the screen name the same count. No note under either.
    if (!is.null(tiles_invasive)) {
      fw_html_block(fw_species_top_title(data, sel, "invasive"), NULL, tiles_invasive)
    },
    if (!is.null(tiles_beneficiary)) {
      fw_html_block(fw_species_top_title(data, sel, "beneficiary"), NULL, tiles_beneficiary)
    },

    fw_html_figure(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      fw_chart_duration(data, sel),
      extra = p(class = "fw-caption",
                fw_fill(fw_t("plan", "r_duration_missing"), n = fw_fmt_num(n_duration)))
    ),
    # The cumulative chart is NOT here, and its absence is deliberate. It moved
    # to the dashboard, because it answers how the database has grown rather
    # than anything about the selection this report is about. A report is read
    # as being about its selection, which is exactly what made it misleading.

    # ---- The attempts, which are no longer rendered here ---------------------
    #
    # EVERY MATCHING ROW USED TO BE PRINTED HERE, drawn by the page's own table
    # function so the columns matched the screen exactly. Both are gone: the
    # client removed the table from the page, and this followed it so the
    # document and the page stay the same document.
    #
    # THE RECORDS THEMSELVES ARE NOT LOST. The CSV and the workbook in this same
    # bundle carry every row with its full, unshortened values - more than this
    # table ever showed, since it truncated multi-value cells - and the map above
    # still holds the full record behind every marker.
    #
    # WHAT IS LOST IS THE NARRATIVE READING, and the client asked on the same
    # call for a record-by-record appendix in prose. That is the opposite
    # direction from a table and was deferred to the next round; when it
    # arrives it belongs here, and it should not be built out of cells.

    # ---- The people ----------------------------------------------------------
    # The page's own contacts block, drawn by the same function, so the document
    # names the same people in the same order. Every row on one page: there is
    # no pager in a file that is read as a document and printed.
    #
    # fw_plan_contacts() reads fw_contacts_summary(), which has already removed
    # the address of anyone who asked not to be listed - and this file leaves the
    # building, so that matters here more than anywhere else in the app.
    local({
      people <- fw_plan_contacts(data, sel)
      if (!nrow(people)) return(NULL)
      tagList(
        fw_html_block(fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note")),
        div(class = "fw-table-scroll",
            fw_plan_contacts_ui(people, page = 1L, per_page = nrow(people)))
      )
    }),

    # ---- Caveats -------------------------------------------------------------
    # Last, and never optional. The page's own panel, built from the same
    # fw_caveats() vector as the workbook's Caveats sheet.
    fw_caveats_ui(data),

    p(class = "fw-report__footer", fw_t("plan", "report_footer")),

    # The payloads the toolbar hands back, and the script that hands them.
    fw_html_payload("fw-file-csv", csv_path, "fwise-attempts.csv",
                    "text/csv;charset=utf-8"),
    fw_html_payload("fw-file-xlsx", xlsx_path, fw_export_filename(),
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    fw_html_payload("fw-file-txt", txt_path, fw_methods_filename(),
                    "text/plain;charset=utf-8"),
    fw_html_report_script()
  )

  rendered <- htmltools::renderTags(div(class = "fw-report", body))

  doc <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<meta name=\"generator\" content=\"", htmlEscape(fw_t("app", "title")), "\">\n",
    "<title>", htmlEscape(paste0(title, " - ", fw_t("app", "title"))), "</title>\n",
    as.character(tagList(
      tags$style(type = "text/css", HTML(fw_html_app_css())),
      tags$style(type = "text/css", fw_html_report_css()),
      fw_html_dependency_tags(rendered$dependencies),
      if (nzchar(rendered$head %||% "")) HTML(rendered$head)
    )),
    "\n</head>\n<body>\n",
    rendered$html,
    "\n</body>\n</html>\n"
  )

  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(doc)), con)
  invisible(path)
}

#' Filename for the report download
fw_html_filename <- function() {
  paste0("fwise-report_", format(Sys.Date(), "%Y%m%d"), ".html")
}
