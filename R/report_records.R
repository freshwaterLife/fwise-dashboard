# report_records.R
# The DETAILED REPORT (.html): the summary report's figures, live, followed by
# every attempt in the reader's selection written out in full, one card after
# another.
#
# THE FIGURES FIRST (client, 29 Sept 2026). A keen reader who opens only this
# file must still get the comparisons, not only the case-by-case detail. So it
# opens on the same sequence as the PDF - fw_pdf_body() in R/report_pdf.R - as
# interactive plotly charts and a leaflet map, with both versions of each
# toggled chart (number of attempts, then success rate) one above the other,
# and then goes on to the find box and the cards. See fw_records_summary_ui().
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
# SELF-CONTAINED. The stylesheet, the fonts, the logos and every widget
# library (plotly.js, leaflet) are inlined, so it opens from a saved file with
# no server behind it - most of its size is plotly.js. Two things still come
# from the network when there is one: the map's background tiles and the
# species photographs. Without a connection the markers, the charts and the
# cards all still work.

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

#' Read a text asset as UTF-8, whatever the machine's locale says
fw_html_read_text <- function(path) {
  out <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(out) <- "UTF-8"
  out
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
#' plotly.js is three and a half megabytes of minified JavaScript. If a string
#' literal in there contains the characters "</script", the browser's HTML
#' parser ends the element in the middle of the library. Escaping the slash is
#' valid inside a JS string and impossible outside one, so this is safe in both
#' directions.
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
#'
#' Revived from the interactive report dropped on 21 Sept 2026 (git d74d068).
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
      path <- file.path(dir, if (is.list(js)) js$src else js)
      if (!file.exists(path)) next
      out <- c(out, list(tags$script(
        HTML(fw_html_guard(fw_html_read_text(path), "script"))
      )))
    }
    if (!is.null(dep$head)) out <- c(out, list(HTML(dep$head)))
  }
  tagList(out)
}

#' The detailed report's stylesheet, compiled and inlined
#'
#' records.scss imports the app's own _components.scss, so the summary strip,
#' outcome bars, species tiles and contacts table are drawn by the same rules
#' as on the page.
fw_records_css <- function(dir = "www/scss") {
  fw_html_inline_css_urls(fw_compile_css(file.path(dir, "records.scss")), dir)
}

# ---- A card --------------------------------------------------------------------

fw_record_copy <- function(species = NULL) {
  list(labels = fw_t("export", "record_labels"),
       groups = fw_t("export", "record_groups"),
       none = fw_t("species", "p_not_noted"),
       top = fw_t("export", "records_top"),
       # Each coded species' label to the label with its Red List code, for
       # the protected species line. NULL without `species`: no codes.
       iucn = if (!is.null(species)) fw_iucn_lookup(species))
}

#' Plain species label -> the same with its Red List code, coded species only
#'
#' @param species data$species
fw_iucn_lookup <- function(species) {
  sp <- fw_species_shown(fw_species_label(species))
  coded <- !is.na(sp$label) & sp$shown != sp$label
  stats::setNames(sp$shown[coded], sp$label[coded])
}

#' The record's protected species, each with its Red List code
#'
#' THE CARD ONLY. The export frame the card reads is the download's, and the
#' download carries no code (Alex, 6 Oct 2026) - fw_export_frame() keeps to
#' FW_EXPORT_COLUMNS - so the codes are put on the collapsed "a; b" string
#' here, one name at a time.
fw_iucn_relabel <- function(x, lookup) {
  if (!length(lookup) || fw_record_blank(x)) return(x)
  parts <- strsplit(x, FW_MULTI_SEP, fixed = TRUE)[[1]]
  hit <- parts %in% names(lookup)
  parts[hit] <- unname(lookup[parts[hit]])
  paste(parts, collapse = FW_MULTI_SEP)
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
  # Stored as Lentic / Lotic; read as Still water / Flowing water, the same
  # words the filters and the form use. See FW_REGIME_LABELS in R/data_load.R.
  if (field == "water_regime") return(htmlEscape(fw_regime_label(as.character(value))))
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
#' @param top whether to end on the "back to the contents" link. The Plan
#'   page's record search opens a card under its own table row, where there is
#'   no contents list to go back to.
#' @param id whether the card carries the attempt id as its element id, which
#'   the report's contents list links to. Off on the Plan page, where a map
#'   card or another output could carry the same id.
fw_record_card <- function(row, copy = fw_record_copy(), top = TRUE, id = TRUE) {
  esc <- function(x) htmlEscape(as.character(x))
  # The find box matches the plain names; the card shows the coded ones.
  search <- fw_record_search(row)
  row$beneficiary_species <- fw_iucn_relabel(row$beneficiary_species, copy$iucn)
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
    '<article class="fw-rec-card"',
    if (id) paste0(' id="', esc(row$attempt_id), '"') else "",
    ' data-search="', htmlEscape(search, attribute = TRUE), '">',
    '<div class="fw-rec-card__head"><div><h2>', esc(title), "</h2>",
    if (nzchar(place)) paste0('<p class="fw-rec-card__place">', esc(place), "</p>") else "",
    '</div><div class="fw-rec-card__meta">', fw_record_outcome(row$outcome, copy$none),
    '<span class="fw-rec-id">', esc(row$attempt_id), "</span></div></div>",
    paste(groups, collapse = ""),
    if (top) paste0('<p class="fw-rec-card__top"><a href="#fw-rec-contents">',
                    esc(copy$top), "</a></p>") else "",
    "</article>"
  )
}

# ---- The figures ---------------------------------------------------------------------

#' A table in the app's own table style
#'
#' @param num columns set in the mono face and right-aligned
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

#' One titled section of the figures, or nothing when it has no content
fw_records_block <- function(title, note, ...) {
  content <- Filter(Negate(is.null), list(...))
  if (!length(content)) return(NULL)
  tags$section(
    class = "fw-rec-block",
    h2(title),
    if (!is.null(note)) p(class = "fw-rec-block__note", note),
    content
  )
}

#' A live plotly chart, or NULL when it has nothing to draw
fw_records_chart <- function(p) {
  if (is.null(p)) return(NULL)
  div(class = "fw-rec-figure", as.tags(p, standalone = FALSE))
}

#' Both versions of a toggled chart, the count above the rate, each labelled
#' with the toggle's own words. See both_modes() in fw_pdf_body().
fw_records_pair <- function(build, labels) {
  figs <- lapply(c("count", "share"), function(mode) {
    fig <- fw_records_chart(build(mode))
    if (is.null(fig)) return(NULL)
    tagList(h3(class = "fw-rec-figure__label", labels[[mode]]), fig)
  })
  figs <- Filter(Negate(is.null), figs)
  if (length(figs)) do.call(tagList, figs)
}

#' The summary report's figures, live, in the PDF's order
#'
#' The selection, the counts, the outcomes, the top species, the map and the
#' countries, the kind of water and the methods (each twice: number, then
#' success rate), how long they took, and who to ask. The same builders the
#' page and the PDF use, so the three cannot disagree about a figure.
fw_records_summary_ui <- function(data, sel, filters = NULL) {
  n_no_coords <- sum(is.na(sel$latitude) | is.na(sel$longitude))
  method_caption <- fw_method_caption_text(data, sel)
  n_duration <- nrow(fw_duration_sel(data, sel))
  caption <- function(key, n) {
    if (n > 0) p(class = "fw-caption", fw_fill(fw_t("plan", key), n = fw_fmt_num(n)))
  }
  species <- function(role_name) {
    title <- fw_species_top_title(data, sel, role_name, f = filters)
    if (is.null(title)) return(NULL)
    fw_records_block(title, NULL,
                     fw_species_tiles_ui(data, sel, role_name, limit = FW_PLAN_SPECIES_N,
                                         f = filters))
  }
  map <- if (nrow(sel) > n_no_coords) {
    # A click goes to the attempt's card below. See detail = "anchor" in
    # fw_add_attempt_markers().
    m <- fw_plan_map(data, sel, detail = "anchor")
    m$width <- "100%"
    m$height <- "100%"
    div(class = "fw-map", style = fw_map_shape_style(), as.tags(m, standalone = FALSE))
  }
  people <- fw_plan_contacts(data, sel)

  div(
    class = "fw-rec-report",
    fw_records_block(fw_t("plan", "r_heading"), NULL,
                     fw_plan_summary_ui(fw_plan_summary(data, sel))),
    fw_records_block(fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
                     fw_outcome_bars_ui(sel)),
    species("invasive"),
    species("beneficiary"),
    fw_records_block(fw_t("maps", "title"), fw_t("maps", "note"),
                     map, if (!is.null(map)) fw_map_note(),
                     caption("r_map_missing", n_no_coords)),
    fw_records_block(fw_t("plan", "report_where"), NULL,
                     fw_html_table(fw_report_country_table(sel),
                                   num = fw_t("export", "col_attempts"))),
    if (fw_show_waterbody(filters)) fw_records_block(
      fw_t("plan", "r_waterbody"), fw_t("plan", "r_waterbody_note"),
      fw_records_pair(function(mode) fw_chart_waterbody(sel, mode = mode),
                      list(count = fw_t("plan", "r_waterbody_count"),
                           share = fw_t("plan", "r_waterbody_share")))),
    fw_records_block(
      fw_t("plan", "r_method"), fw_t("plan", "r_method_note"),
      fw_records_pair(function(mode) fw_chart_method(data, sel, mode = mode),
                      list(count = fw_t("plan", "r_method_count"),
                           share = fw_t("plan", "r_method_share"))),
      if (!is.null(method_caption)) p(class = "fw-caption", method_caption)),
    fw_records_block(
      fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"),
      fw_records_chart(fw_chart_duration(data, sel)),
      p(class = "fw-caption",
        fw_fill(fw_t("plan", "r_duration_missing"), n = fw_fmt_num(n_duration)))),
    # EVERY CONTACT, not the PDF's six busiest: this file is scrolled, not
    # printed. fw_plan_contacts() has already removed the address of anyone
    # who asked not to be listed.
    if (nrow(people)) {
      fw_records_block(fw_t("plan", "r_contacts"), fw_t("plan", "report_contacts_note"),
                       div(class = "fw-table-scroll",
                           fw_plan_contacts_ui(people, page = 1L, per_page = nrow(people),
                                               reveal = FALSE)))
    }
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

#' Write the detailed report
#'
#' @param path    where to write. The bundle's working directory.
#' @param data    the loaded tables, for the caveats and the filter record
#' @param sel     the attempt rows the figures are about
#' @param export  fw_export_frame() for the selection: the rows, in order
#' @param filters the filter snapshot taken when Build was pressed
fw_write_records_html <- function(path, data, sel, export, filters, meta = NULL) {
  generated <- format(Sys.time(), "%d %B %Y", tz = "UTC")
  n <- nrow(export)
  # As plain lists: a one-row data frame per card is the slow way to read a
  # field, and there are fifty-five fields a card.
  rows <- lapply(seq_len(n), function(i) as.list(export[i, , drop = FALSE]))
  copy <- fw_record_copy(data$species)

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
      tags$summary(fw_t("plan", "report_selection")),
      tags$table(
        class = "fw-rec-table",
        tags$thead(tags$tr(lapply(names(selection), function(h) tags$th(scope = "col", h)))),
        tags$tbody(lapply(seq_len(nrow(selection)), function(i) {
          tags$tr(tags$td(selection[i, 1]), tags$td(selection[i, 2]))
        }))
      )
    ),

    # THE FIGURES, before a single card.
    fw_records_summary_ui(data, sel, filters),

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
      lapply(fw_closing_blocks(data, meta), function(b) {
        title <- fw_caveat_title(b$heading)
        # A caveat sits one level under the "Caveats" part.
        tagList(if (nzchar(title)) (if (isTRUE(b$sub)) h4 else h3)(title),
                lapply(b$body, p))
      })
    ),

    tags$footer(
      class = "fw-rec-foot",
      # FWISE, WEIRD FISHES, THEN THE COLLABORATORS in the app footer's order
      # (client, 5 Oct 2026, when UNIL and the Norwegian Veterinary Institute
      # joined). Eight logos, so the four-column grid gives two even rows.
      div(class = "fw-rec-foot__logos",
          logo(FW_LOGO$mark_file, fw_t("footer", "logo_alt_fwise")),
          logo(FW_LOGO$wfa_file, fw_t("footer", "logo_alt_wfa")),
          lapply(names(FW_LOGO$collab_files), function(k)
            logo(FW_LOGO$collab_files[[k]], fw_t("footer", paste0("logo_alt_", k))))),
      p(fw_t("plan", "report_footer"))
    ),

    fw_records_script(fw_t("export", "records_showing"))
  )

  # The widgets' libraries come out of the tags here, and go into <head>.
  rendered <- htmltools::renderTags(body)
  doc <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmlEscape(paste0(fw_t("export", "records_title"), " - ",
                                 fw_t("app", "title"))), "</title>\n",
    as.character(fw_html_dependency_tags(rendered$dependencies)),
    if (nzchar(rendered$head %||% "")) as.character(rendered$head) else "",
    # AFTER the widgets' own stylesheets, so the app's rules win a tie as they
    # do in the app: leaflet.css sets display:block on every marker, and read
    # last it undid .fw-cluster's centring of the group count.
    "<style>", fw_records_css(), "</style>\n",
    "\n</head>\n<body>\n",
    rendered$html, "\n</body>\n</html>\n"
  )
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(doc)), con)
  invisible(path)
}

#' Filename for the attempts file
fw_records_filename <- function(stamp = fw_file_stamp()) {
  fw_fill(fw_t("export", "records_filename"), stamp = stamp)
}
