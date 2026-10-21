# report_records.R
# The Detailed report (.html): the summary figures, live (fw_pdf_body() order,
# toggled charts in both versions), then every attempt in the selection as a
# card with a find box.
#
# Cards are rows of fw_export_frame(), so they carry exactly what the
# spreadsheet does, with the same redaction. Every field is drawn ("Not
# noted" when empty), except the chemical section on non-chemical attempts
# (fw_record_group_drop()) and an empty region (FW_RECORD_OMIT_BLANK).
#
# Self-contained: styles, fonts, logos and widget libraries are inlined. Only
# map tiles and species photos need the network.

library(htmltools)

# ---- Inlining ------------------------------------------------------------------

#' The MIME type for an inlined asset, by extension
#'
#' A closed list; anything else isn't embedded.
FW_HTML_MIME = c(
  png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
  svg = "image/svg+xml", woff2 = "font/woff2", woff = "font/woff", ttf = "font/ttf"
)

#' Base64 for embedding, on ONE line
#
fw_html_base64 = function(path) {
  gsub("[\r\n]", "", jsonlite::base64_enc(readBin(path, "raw", file.size(path))))
}

#' A file as a base64 data URI, or NULL if it cannot be embedded
fw_html_data_uri = function(path) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(NULL)
  mime = unname(FW_HTML_MIME[tolower(tools::file_ext(path))])
  if (!length(mime) || is.na(mime)) return(NULL)
  paste0("data:", mime, ";base64,", fw_html_base64(path))
}

#' Rewrite every relative url() in a stylesheet to a data URI
#'
#' Relative urls (the fonts) would break once the file is saved elsewhere.
fw_html_inline_css_urls = function(css, dir) {
  refs = regmatches(css, gregexpr("url\\(\\s*['\"]?([^'\")]+)['\"]?\\s*\\)", css))[[1]]
  for (ref in unique(refs)) {
    target = sub("['\"]?\\s*\\)$", "", sub("^url\\(\\s*['\"]?", "", ref))
    if (grepl("^(data:|https?:|//)", target)) next
    uri = fw_html_data_uri(file.path(dir, sub("[?#].*$", "", target)))
    if (!is.null(uri)) css = gsub(ref, paste0("url(\"", uri, "\")"), css, fixed = TRUE)
  }
  css
}

#' Read a text asset as UTF-8, whatever the machine's locale says
fw_html_read_text = function(path) {
  out = rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(out) = "UTF-8"
  out
}

#' Where a resolved html dependency's files actually live
fw_html_dep_dir = function(dep) {
  path = dep$src$file
  if (is.null(path)) return(NULL)
  if (!is.null(dep$package)) path = system.file(path, package = dep$package)
  if (!nzchar(path) || !dir.exists(path)) NULL else path
}

#' A script or style tag whose content cannot close its own element
#'
#' Escapes "</" so a "</script" inside plotly.js can't end the element.
fw_html_guard = function(text, tag) {
  gsub(paste0("</", tag), paste0("<\\/", tag), text, fixed = TRUE)
}

#' Every dependency of a set of widgets, inlined into head tags
#'
#' In htmltools' resolved order (don't sort). Must be in <head>, so
#' htmlwidgets.js renders the widgets on DOMContentLoaded without Shiny.
fw_html_dependency_tags = function(deps) {
  out = list()
  for (dep in htmltools::resolveDependencies(deps)) {
    dir = fw_html_dep_dir(dep)
    if (is.null(dir)) next
    for (css in unlist(dep$stylesheet)) {
      path = file.path(dir, css)
      if (!file.exists(path)) next
      out = c(out, list(tags$style(
        type = "text/css",
        HTML(fw_html_guard(fw_html_inline_css_urls(
          fw_html_read_text(path), dirname(path)), "style"))
      )))
    }
    for (js in unlist(dep$script)) {
      path = file.path(dir, if (is.list(js)) js$src else js)
      if (!file.exists(path)) next
      out = c(out, list(tags$script(
        HTML(fw_html_guard(fw_html_read_text(path), "script"))
      )))
    }
    if (!is.null(dep$head)) out = c(out, list(HTML(dep$head)))
  }
  tagList(out)
}

#' The detailed report's stylesheet, compiled and inlined
#'
#' records.scss imports _components.scss, so pieces match the page.
fw_records_css = function(dir = "www/scss") {
  fw_html_inline_css_urls(fw_compile_css(file.path(dir, "records.scss")), dir)
}

# ---- A card --------------------------------------------------------------------

fw_record_copy = function(species = NULL) {
  list(labels = fw_t("export", "record_labels"),
       groups = fw_t("export", "record_groups"),
       none = fw_t("species", "p_not_noted"),
       top = fw_t("export", "records_top"),
       # Label -> label with Red List code. NULL without `species`.
       iucn = if (!is.null(species)) fw_iucn_lookup(species))
}

#' Plain species label -> the same with its Red List code, coded species only
#'
#' @param species data$species
fw_iucn_lookup = function(species) {
  sp = fw_species_shown(fw_species_label(species))
  coded = !is.na(sp$label) & sp$shown != sp$label
  stats::setNames(sp$shown[coded], sp$label[coded])
}

#' The record's protected species, each with its Red List code
#'
#' Card only: the download carries no codes.
fw_iucn_relabel = function(x, lookup) {
  if (!length(lookup) || fw_record_blank(x)) return(x)
  parts = strsplit(x, FW_MULTI_SEP, fixed = TRUE)[[1]]
  hit = parts %in% names(lookup)
  parts[hit] = unname(lookup[parts[hit]])
  paste(parts, collapse = FW_MULTI_SEP)
}

#' The outcome in words, with its data colour as a swatch beside it
fw_record_outcome = function(outcome, none) {
  o = if (is.na(outcome) || !outcome %in% FW_OUTCOME_LEVELS) "Unknown" else outcome
  paste0('<span class="fw-rec-outcome"><span class="fw-rec-outcome__dot" ',
         'aria-hidden="true" style="background:', FW_OUTCOME_COLOURS[[o]], ';"></span>',
         htmlEscape(if (is.na(outcome)) none else outcome), "</span>")
}

#' Whether a field of a record is empty, and so prints as "Not noted"
#'
#' Shared by the renderer and fw_record_group_drop().
fw_record_blank = function(value) {
  length(value) != 1 || is.na(value) || !nzchar(trimws(as.character(value)))
}

#' Whether this card should leave a group out altogether
#'
#' Only the chemical section, only when no method is chemical (NA counts as
#' not chemical), and only when the section is empty, so no value is lost.
fw_record_group_drop = function(g, row) {
  if (!identical(g$id, "chemical")) return(FALSE)
  classes = strsplit(as.character(row$method_classes), FW_MULTI_SEP, fixed = TRUE)[[1]]
  if ("chemical" %in% classes) return(FALSE)
  all(vapply(g$fields, function(f) fw_record_blank(row[[f]]), logical(1)))
}

# Fields hidden when empty rather than shown as "Not noted" (client). Region is
# rarely recorded. Keep this list short.
FW_RECORD_OMIT_BLANK = c("region")

fw_record_value = function(field, value, none) {
  if (fw_record_blank(value)) {
    return(paste0('<span class="fw-rec-none">', htmlEscape(none), "</span>"))
  }
  v = htmlEscape(as.character(value), attribute = TRUE)
  if (field == "reference_link" && grepl("^https?://", value)) {
    return(paste0('<a href="', v, '" target="_blank" rel="noopener noreferrer">', v, "</a>"))
  }
  if (grepl("_email$", field)) return(paste0('<a href="mailto:', v, '">', v, "</a>"))
  if (field == "outcome") return(fw_record_outcome(value, none))
  # Lentic/Lotic shown as Still/Flowing water (FW_REGIME_LABELS).
  if (field == "water_regime") return(htmlEscape(fw_regime_label(as.character(value))))
  v
}

#' What the find box matches a card on
fw_record_search = function(row) {
  x = c(row$attempt_id, row$site_name, row$country, row$region,
         row$invasive_species, row$beneficiary_species, row$methods)
  tolower(paste(x[!is.na(x)], collapse = " "))
}

#' The years a card covers, "1994-2002", for its contents line
fw_record_years = function(row) {
  s = row$start_year; e = row$end_year
  if (is.na(s) && is.na(e)) return(NULL)
  paste0(if (is.na(s)) "?" else s, "-", if (is.na(e)) "" else e)
}

#' One attempt as a card, as an HTML string
#'
#' @param row one row of fw_export_frame(), as a list
#' @param copy fw_record_copy()
#' @param top end on a "back to the contents" link (off on the Plan page)
#' @param id use the attempt id as the element id (off on the Plan page)
fw_record_card = function(row, copy = fw_record_copy(), top = TRUE, id = TRUE) {
  esc = function(x) htmlEscape(as.character(x))
  # The find box matches the plain names; the card shows the coded ones.
  search = fw_record_search(row)
  row$beneficiary_species = fw_iucn_relabel(row$beneficiary_species, copy$iucn)
  place = c(row$region, row$country)
  place = paste(place[!is.na(place) & nzchar(place)], collapse = ", ")
  title = if (is.na(row$site_name) || !nzchar(row$site_name)) copy$none else row$site_name
  # Drop sections before building headings.
  shown = Filter(function(g) !fw_record_group_drop(g, row), copy$groups)
  groups = vapply(shown, function(g) {
    # Drop omitted fields before building pairs.
    keep = Filter(function(f) {
      !(f %in% FW_RECORD_OMIT_BLANK) || !fw_record_blank(row[[f]])
    }, g$fields)
    fields = vapply(keep, function(f) {
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
fw_html_table = function(df, num = character(0)) {
  if (is.null(df) || !nrow(df)) return(NULL)
  cls = function(nm) if (nm %in% num) "fw-col-num" else NULL
  tags$table(
    class = "fw-table",
    tags$thead(tags$tr(lapply(names(df), function(nm)
      tags$th(scope = "col", class = cls(nm), nm)))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(names(df), function(nm) {
        v = df[[nm]][i]
        tags$td(class = cls(nm),
                if (is.na(v) || !nzchar(as.character(v))) fw_t("common", "empty_value")
                else as.character(v))
      }))
    }))
  )
}

#' One titled section of the figures, or nothing when it has no content
fw_records_block = function(title, note, ...) {
  content = Filter(Negate(is.null), list(...))
  if (!length(content)) return(NULL)
  tags$section(
    class = "fw-rec-block",
    h2(title),
    if (!is.null(note)) p(class = "fw-rec-block__note", note),
    content
  )
}

#' A live plotly chart, or NULL when it has nothing to draw
fw_records_chart = function(p) {
  if (is.null(p)) return(NULL)
  div(class = "fw-rec-figure", as.tags(p, standalone = FALSE))
}

#' Both versions of a toggled chart, the count above the rate, each labelled
#' with the toggle's own words. See both_modes() in fw_pdf_body().
fw_records_pair = function(build, labels) {
  figs = lapply(c("count", "share"), function(mode) {
    fig = fw_records_chart(build(mode))
    if (is.null(fig)) return(NULL)
    tagList(h3(class = "fw-rec-figure__label", labels[[mode]]), fig)
  })
  figs = Filter(Negate(is.null), figs)
  if (length(figs)) do.call(tagList, figs)
}

#' The summary report's figures, live, in the PDF's order
#'
#' Same builders as the page and the PDF.
fw_records_summary_ui = function(data, sel, filters = NULL) {
  n_no_coords = sum(is.na(sel$latitude) | is.na(sel$longitude))
  method_caption = fw_method_caption_text(data, sel)
  n_duration = nrow(fw_duration_sel(data, sel))
  caption = function(key, n) {
    if (n > 0) p(class = "fw-caption", fw_fill(fw_t("plan", key), n = fw_fmt_num(n)))
  }
  species = function(role_name) {
    title = fw_species_top_title(data, sel, role_name, f = filters)
    if (is.null(title)) return(NULL)
    fw_records_block(title, NULL,
                     fw_species_tiles_ui(data, sel, role_name, limit = FW_PLAN_SPECIES_N,
                                         f = filters))
  }
  map = if (nrow(sel) > n_no_coords) {
    # A click scrolls to the card (detail = "anchor").
    m = fw_plan_map(data, sel, detail = "anchor")
    m$width = "100%"
    m$height = "100%"
    div(class = "fw-map", style = fw_map_shape_style(), as.tags(m, standalone = FALSE))
  }
  people = fw_plan_contacts(data, sel)

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
    # Every contact, unlike the PDF. Already redacted.
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
#' Filters the contents and cards by id, site, place, species or methods.
#' Plain DOM; works offline.
fw_records_script = function(total_label) {
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
fw_write_records_html = function(path, data, sel, export, filters, meta = NULL) {
  generated = format(Sys.time(), "%d %B %Y", tz = "UTC")
  n = nrow(export)
  # Plain lists: faster than one-row data frames.
  rows = lapply(seq_len(n), function(i) as.list(export[i, , drop = FALSE]))
  copy = fw_record_copy(data$species)

  logo = function(file, alt) {
    uri = fw_html_data_uri(file)
    if (!is.null(uri)) tags$img(src = uri, alt = alt)
  }
  selection = fw_filters_sheet(filters, n, nrow(data$attempt), meta)

  body = div(
    class = "fw-rec",
    tags$header(
      class = "fw-rec-head",
      # The explicit lockup (full name), since this file gets passed on.
      if (!is.null(fw_html_data_uri(FW_LOGO$explicit_file))) {
        tags$img(class = "fw-rec-head__mark", src = fw_html_data_uri(FW_LOGO$explicit_file),
                 alt = fw_t("app", "full_title"))
      },
      h1(fw_t("export", "records_title")),
      p(class = "fw-rec-head__subtitle",
        fw_fill(fw_t("export", "records_subtitle"), n = fw_fmt_num(n), date = generated)),
      p(class = "fw-rec-head__lead", fw_t("export", "records_lead"))
    ),

    # The filters, folded.
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

    # The figures, before the cards.
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
        yrs = fw_record_years(r)
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

    # Methods and caveats, always last.
    tags$section(
      class = "fw-rec-caveats",
      h2(fw_t("export", "closing_heading")),
      lapply(fw_closing_blocks(data, meta), function(b) {
        title = fw_caveat_title(b$heading)
        # A caveat sits one level under the "Caveats" part.
        tagList(if (nzchar(title)) (if (isTRUE(b$sub)) h4 else h3)(title),
                lapply(b$body, p))
      })
    ),

    tags$footer(
      class = "fw-rec-foot",
      # Footer order: FWISE, Weird Fishes, then the collaborators (2 rows of 4).
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
  rendered = htmltools::renderTags(body)
  doc = paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmlEscape(paste0(fw_t("export", "records_title"), " - ",
                                 fw_t("app", "title"))), "</title>\n",
    as.character(fw_html_dependency_tags(rendered$dependencies)),
    if (nzchar(rendered$head %||% "")) as.character(rendered$head) else "",
    # After the widget stylesheets, so ours win ties (e.g. .fw-cluster).
    "<style>", fw_records_css(), "</style>\n",
    "\n</head>\n<body>\n",
    rendered$html, "\n</body>\n</html>\n"
  )
  con = file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(doc)), con)
  invisible(path)
}

#' Filename for the attempts file
fw_records_filename = function(stamp = fw_file_stamp()) {
  fw_fill(fw_t("export", "records_filename"), stamp = stamp)
}
