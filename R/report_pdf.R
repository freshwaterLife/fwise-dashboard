# report_pdf.R
# The report builder's PDF: the report the reader built, on FWISE letterhead,
# with every logo the site carries, static charts and a static map, laid out
# for A4. Rendered on the server by Quarto, whose Typst engine writes the PDF.
#
# WHY QUARTO AND TYPST. The client asked for a real PDF, not a web page to
# print. Typst ships inside Quarto (1.4 and later), so no LaTeX is installed
# and nothing is added to the deployment beyond the Quarto CLI that Posit
# Connect Cloud already provides. The PDF this replaced was the old HTML
# report's print stylesheet driven through the reader's own browser.
#
# HOW IT IS BUILT. Everything is decided in R, in this process:
#
#   1. The figures are drawn to PNG by the ggplot twins in R/charts_static.R,
#      which count through the same functions as the page's plotly charts.
#   2. The body is written as calls to the components in
#      resources/report/typst-template.typ, with every piece of text passed as
#      an escaped string (fw_typ_str()), never spliced into markup.
#   3. fwise-tokens.typ is written from R/brand.R, R/config.R and the copy
#      deck, so the template carries no values of its own.
#   4. Quarto renders report.qmd, which holds that body as one raw Typst block.
#
# THE .qmd HAS NO R CHUNKS, and that is deliberate. A chunk would make Quarto
# start a second R process in a temp directory, which would have to find this
# app's renv library and reload the data to draw what this process already has
# in memory. Here Quarto's only job is markdown to Typst to PDF.
#
# SAME ORDER AS THE SCREEN. A reader who looked at the report builder and then
# downloaded this has to find the same argument in the same sequence: what was
# asked, the summary, the species, where, what happened, how long, in what
# water, who, and the caveats last.

# ---- Quarto ------------------------------------------------------------------

#' Where the Quarto CLI is, or "" if it is not installed
#'
#' QUARTO_PATH first, which is Quarto's own convention for pointing at a copy
#' that is not on the PATH, then the PATH.
fw_quarto_path <- function() {
  p <- Sys.getenv("QUARTO_PATH", unset = "")
  if (nzchar(p) && file.exists(p)) return(p)
  unname(Sys.which("quarto"))
}

#' Can this process write a PDF at all?
fw_pdf_available <- function() nzchar(fw_quarto_path())

#' The Quarto CLI's version, for the startup log, or NA
fw_quarto_version <- function() {
  q <- fw_quarto_path()
  if (!nzchar(q)) return(NA_character_)
  v <- tryCatch(system2(q, "--version", stdout = TRUE, stderr = TRUE, timeout = 20),
                error = function(e) NA_character_, warning = function(w) NA_character_)
  if (!length(v)) NA_character_ else v[1]
}

# ---- Typst text ----------------------------------------------------------------

#' A value as a Typst string literal
#'
#' THE ONE ESCAPING RULE. Inside a Typst string only the backslash and the
#' double quote are special, and newlines are written as \n. Everything the
#' report prints goes through here, so no species name, site name or free-text
#' note can become markup. NA prints as the empty-value placeholder, the same as
#' the page's tables.
fw_typ_str <- function(x, na = fw_t("common", "empty_value")) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- na
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\r?\n", "\\n", x)
  paste0("\"", x, "\"")
}

#' A Typst array from R values already written as Typst
fw_typ_array <- function(items) {
  if (!length(items)) return("()")
  # A one-element array needs its trailing comma, or it is a parenthesised value.
  paste0("(", paste(items, collapse = ", "), if (length(items) == 1) "," else "", ")")
}

#' A Typst dictionary from named values already written as Typst
fw_typ_dict <- function(...) {
  x <- list(...)
  paste0("(", paste(names(x), unlist(x), sep = ": ", collapse = ", "), ")")
}

#' A length in mm, as Typst writes it
fw_typ_mm <- function(x) sprintf("%.1fmm", x)

#' A URL as a Typst string, or none when there is no usable one
fw_typ_url <- function(x) {
  if (!fw_has_str(x) || !grepl("^https?://", x)) return("none")
  fw_typ_str(x)
}

#' The tokens the template reads: colours, sizes, logos and fixed wording
#'
#' Written fresh for every render from the same values the app uses. The
#' template imports this and holds no values of its own.
#'
#' @param logos named file paths, relative to the render directory
fw_pdf_tokens <- function(logos) {
  col <- function(name, hex) sprintf('#let %s = rgb("%s")', name, hex)
  pt <- function(name, v) sprintf("#let %s = %spt", name, format(v))
  c(
    "// Written by fw_pdf_tokens() in R/report_pdf.R. Do not edit.",
    col("fw-ink", FW_COLOURS$ink),
    col("fw-ink-muted", FW_COLOURS$ink_muted),
    col("fw-teal", FW_COLOURS$brand_teal),
    col("fw-teal-text", FW_COLOURS$teal_text),
    col("fw-teal-tint", FW_COLOURS$teal_tint),
    col("fw-teal-wash", FW_COLOURS$teal_wash),
    col("fw-border", FW_COLOURS$border),
    col("fw-sunken", FW_COLOURS$sunken),
    # THE PAGE IS THE SURFACE TONE, not the page tone. On screen the page is a
    # step darker than the cards on it; on paper a full-bleed tint that dark
    # prints as grey. The surface tone is the lightest value the palette has,
    # and it is still not white.
    col("fw-page-fill", FW_COLOURS$surface),
    pt("fw-floor", FW_PRINT$floor),
    pt("fw-title", FW_PRINT$title),
    pt("fw-h2", FW_PRINT$h2),
    pt("fw-h3", FW_PRINT$h3),
    pt("fw-stat", FW_PRINT$stat),
    # The credit line under a photograph is the type scale's own exemption from
    # the floor (FW_TYPE$size_credit), and so is the page count beside the logos.
    pt("fw-credit", fw_rem_pt(FW_TYPE$size_credit)),
    sprintf("#let fw-leading = %sem", format(FW_PRINT$leading)),
    sprintf("#let fw-margin = %s", fw_typ_mm(FW_PDF$page_margin_mm)),
    "#let fw-logo-h = 9mm",
    paste0("#let fw-logo-mark = ", fw_typ_str(logos[["mark"]])),
    paste0("#let fw-logos = ", fw_typ_array(fw_typ_str(logos[names(logos) != "mark"]))),
    paste0("#let fw-doc-title = ", fw_typ_str(fw_t("plan", "report_title"))),
    paste0("#let fw-page-label = ", fw_typ_str(fw_t("plan", "pdf_page"))),
    paste0("#let fw-of-label = ", fw_typ_str(fw_t("common", "of"))),
    paste0("#let fw-no-image = ", fw_typ_str(fw_t("species", "no_image"))),
    paste0("#let fw-credit-sep = ", fw_typ_str(fw_t("species", "credit_sep")))
  )
}

# ---- Pieces of the report --------------------------------------------------------

#' Attempts by country
#'
#' A reader with the report on paper asking "where has this been tried" wants
#' a list as well as a picture. Sorted by count and capped, with the tail
#' gathered rather than dropped, so the total still adds up.
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

#' A data frame as an fw-table() call
#'
#' @param num names of the columns set right-aligned in the mono face
#' @param widths Typst column widths, or NULL to let the template choose
fw_typ_table <- function(df, num = character(0), widths = NULL) {
  if (is.null(df) || !nrow(df)) return(NULL)
  rows <- vapply(seq_len(nrow(df)), function(i) {
    fw_typ_array(fw_typ_str(vapply(df[i, , drop = FALSE], as.character, "")))
  }, character(1))
  num_idx <- which(names(df) %in% num) - 1L
  paste0(
    "#fw-table(", fw_typ_array(fw_typ_str(names(df))), ", ",
    fw_typ_array(rows),
    ", num: ", fw_typ_array(as.character(num_idx)),
    if (!is.null(widths)) paste0(", widths: ", fw_typ_array(widths)) else "",
    ")"
  )
}

#' A titled block
fw_typ_block <- function(title, note = NULL, body = "[]") {
  paste0("#fw-block(title: ", fw_typ_str(title),
         if (!is.null(note)) paste0(", note: ", fw_typ_str(note)) else "",
         ")[\n", paste(body, collapse = "\n"), "\n]")
}

#' A caption line under a figure
fw_typ_caption <- function(txt) paste0("#fw-caption(", fw_typ_str(txt), ")")

#' A figure drawn to PNG in the render directory, as an fw-figure() call
#'
#' @return the Typst call, or NULL when the chart had nothing to draw - and the
#'   caller drops its block, heading and all, rather than stand it over nothing
fw_typ_figure <- function(plot, dir, name, height_mm) {
  if (is.null(plot)) return(NULL)
  fw_gg_png(plot, file.path(dir, name), height_mm = height_mm)
  sprintf('#fw-figure("%s", %s)', name, fw_typ_mm(height_mm))
}

#' The summary strip
fw_typ_stats <- function(s) {
  item <- function(value, label) fw_typ_array(fw_typ_str(c(value, label)))
  paste0("#fw-stats(", fw_typ_array(c(
    item(fw_fmt_num(s$attempts),  fw_t("plan", "r_attempts")),
    item(fw_fmt_num(s$countries), fw_t("plan", "r_countries")),
    item(fw_fmt_num(s$species),   fw_t("plan", "r_species")),
    item(paste0(">", fw_fmt_num(s$beneficiaries)), fw_t("plan", "r_beneficiaries")),
    item(s$year_span,             fw_t("plan", "r_years"))
  )), ")")
}

#' The four outcome bars, with the page's numbers
fw_typ_outcome_bars <- function(sel) {
  o <- fw_outcome_counts(sel)
  total <- sum(o$n)
  rows <- vapply(seq_len(nrow(o)), function(i) {
    pc <- if (total > 0) 100 * o$n[i] / total else 0
    fw_typ_array(c(
      fw_typ_str(o$outcome[i]),
      fw_typ_str(paste0(fw_fmt_num(o$n[i]), " ", sprintf("(%.0f%%)", pc))),
      sprintf("%.2f", pc),
      fw_typ_str(FW_OUTCOME_COLOURS[[o$outcome[i]]])
    ))
  }, character(1))
  paste0("#fw-outcome-bars(", fw_typ_array(rows), ")")
}

#' Fetch the species photographs a report needs, in parallel
#'
#' The cache in species.csv holds URLs, not files: the pictures are
#' Wikimedia's. The web page lets the browser fetch them; a PDF has to carry
#' them, so they are fetched here - all at once, each under
#' FW_PDF$image_timeout_s, with Wikimedia's required User-Agent. One that fails
#' or times out prints as the page's placeholder. It never fails the report.
#'
#' @param prefix names this batch's files, so the two roles' photographs do
#'   not overwrite each other in the one render directory
#' @return a named character vector, species_id -> local file, NA where no
#'   picture arrived
fw_pdf_fetch_images <- function(urls, dir, prefix = "sp") {
  urls <- urls[!is.na(urls) & grepl("^https?://", urls)]
  if (!length(urls)) return(character(0))
  dest <- file.path(dir, paste0(prefix, "-", seq_along(urls)))
  res <- tryCatch(
    curl::multi_download(urls, dest, timeout = FW_PDF$image_timeout_s,
                         progress = FALSE, useragent = FW_SPECIES_UA),
    error = function(e) NULL
  )
  out <- stats::setNames(rep(NA_character_, length(urls)), names(urls))
  if (is.null(res)) return(out)
  for (i in seq_along(urls)) {
    if (!isTRUE(res$success[i]) || !identical(as.integer(res$status_code[i]), 200L)) next
    ext <- fw_image_ext(dest[i])
    if (is.na(ext)) next
    final <- paste0(dest[i], ".", ext)
    file.rename(dest[i], final)
    out[i] <- basename(final)
  }
  out
}

#' An image file's format from its first bytes, or NA if Typst cannot read it
#'
#' From the bytes and not the URL, because a Wikimedia thumbnail's name keeps
#' the original's extension even where the thumbnail is a different format.
fw_image_ext <- function(path) {
  if (!file.exists(path) || file.size(path) < 16) return(NA_character_)
  b <- readBin(path, "raw", 12)
  if (identical(b[1:3], as.raw(c(0xff, 0xd8, 0xff)))) return("jpg")
  if (identical(b[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))) return("png")
  if (identical(rawToChar(b[1:3]), "GIF")) return("gif")
  if (identical(rawToChar(b[1:4]), "RIFF") && identical(rawToChar(b[9:12]), "WEBP")) return("webp")
  NA_character_
}

#' One role's species tiles, with their photographs
#'
#' @return the Typst call, or NULL when the selection has none of that role -
#'   and the caller drops the block with it, as the page does
fw_typ_species <- function(data, sel, role_name, dir) {
  top <- fw_species_top_n(data, sel, role_name, FW_PLAN_SPECIES_N)
  if (!nrow(top)) return(NULL)
  imgs <- lapply(top$species_id, function(id) fw_species_image_cached(data$species, id))
  urls <- vapply(imgs, function(x) if (is.null(x)) NA_character_ else x$url, "")
  names(urls) <- top$species_id
  files <- fw_pdf_fetch_images(urls, dir, prefix = role_name)

  tiles <- vapply(seq_len(nrow(top)), function(i) {
    row <- top[i, ]
    img <- imgs[[i]]
    file <- files[row$species_id]
    counts <- vapply(FW_OUTCOME_LEVELS, function(o) as.integer(row[[o]]), 1L)
    segs <- vapply(FW_OUTCOME_LEVELS[counts > 0], function(o) {
      fw_typ_array(c(sprintf("%.2f", 100 * counts[[o]] / sum(counts)),
                     fw_typ_str(FW_OUTCOME_COLOURS[[o]])))
    }, "")
    has_img <- length(file) == 1 && !is.na(file)
    fw_typ_dict(
      image = if (has_img) fw_typ_str(file) else "none",
      # The credit and licence travel only with a photograph that is printed.
      credit = if (has_img) fw_typ_str(img$credit) else "none",
      credit_url = if (has_img) fw_typ_url(img$page_url) else "none",
      licence = if (has_img) fw_typ_str(img$licence) else "none",
      licence_url = if (has_img) fw_typ_url(fw_licence_url(img$licence_url)) else "none",
      name = fw_typ_str(row$label),
      count = fw_typ_str(paste(fw_fmt_num(row$n),
        fw_t("plan", if (row$n == 1) "r_tile_attempt" else "r_tile_attempts"))),
      segments = fw_typ_array(unname(segs))
    )
  }, character(1))
  paste0("#fw-species-tiles(", fw_typ_array(tiles), ")")
}

#' The contacts, every row, as the page's contacts table
#'
#' fw_plan_contacts() reads fw_contacts_summary(), which has already removed the
#' address of anyone who asked not to be listed. The address is printed plainly
#' here - there is no click to assemble it at - which is the same thing the
#' spreadsheet in the same download does.
fw_pdf_contacts_table <- function(people) {
  data.frame(
    a = people$contact_name,
    b = ifelse(is.na(people$organisation), fw_t("networking", "no_organisation"),
               people$organisation),
    c = people$country_label,
    d = vapply(people$attempt_count, fw_fmt_num, ""),
    # A break opportunity after every @ and dot. An address has no spaces, so
    # without these a long one runs straight off the right-hand edge.
    e = gsub("([@.])", "\\1\u200b", people$contact_email),
    stringsAsFactors = FALSE
  ) |>
    stats::setNames(c(fw_t("plan", "col_contact_name"), fw_t("plan", "col_contact_org"),
                      fw_t("networking", "col_country"), fw_t("plan", "col_contact_n"),
                      fw_t("plan", "col_contact_email")))
}

#' The report body, as Typst, with its figures written into `dir`
#'
#' Separate from the render so the tests can read what the document says
#' without needing Quarto: dev/plan_test.R and dev/value_test.R assert against
#' this text and these files.
#'
#' @return the Typst body as one string
fw_pdf_body <- function(dir, data, sel, filters, meta = NULL,
                        method_mode = "count", method_wb_mode = "count",
                        waterbody_mode = "count") {
  s <- fw_plan_summary(data, sel)
  n_no_coords <- sum(is.na(sel$latitude) | is.na(sel$longitude))
  n_no_method <- fw_n_no_method(data, sel)
  n_duration <- nrow(fw_duration_sel(data, sel))
  generated <- format(Sys.time(), "%d %B %Y", tz = "UTC")

  species_block <- function(role_name) {
    tiles <- fw_typ_species(data, sel, role_name, dir)
    if (is.null(tiles)) return(NULL)
    fw_typ_block(fw_species_top_title(data, sel, role_name), NULL, tiles)
  }

  map <- fw_gg_map(data, sel)
  map_fig <- if (!is.null(map)) fw_typ_figure(map$plot, dir, "map.png", map$height_mm)

  n_rows <- function(order_lv) length(order_lv)
  method_fig <- local({
    md <- fw_method_data(data, sel, method_mode)
    if (!is.null(md)) fw_typ_figure(fw_gg_method(data, sel, method_mode), dir,
                                    "methods.png", fw_gg_height("method", n_rows(md$order_lv)))
  })
  duration_fig <- local({
    dd <- fw_duration_data(data, sel)
    if (!is.null(dd)) fw_typ_figure(fw_gg_duration(data, sel), dir, "duration.png",
                                    fw_gg_height("duration", n_rows(dd$order_lv)))
  })
  waterbody_fig <- local({
    cd <- fw_category_data(fw_waterbody_rows(sel), FW_TOP_N, waterbody_mode)
    if (!is.null(cd)) fw_typ_figure(fw_gg_waterbody(sel, waterbody_mode), dir,
                                    "waterbody.png", fw_gg_height("category", n_rows(cd$order_lv)))
  })
  method_wb_fig <- local({
    md <- fw_method_waterbody_data(data, sel, method_wb_mode)
    if (!is.null(md)) fw_typ_figure(fw_gg_method_waterbody(data, sel, method_wb_mode), dir,
                                    "method-waterbody.png",
                                    fw_gg_height("method_waterbody", n_rows(md$order_lv)) + 8)
  })

  people <- fw_plan_contacts(data, sel)
  caveats <- fw_caveat_blocks(data)

  parts <- list(
    paste0("#fw-letterhead(title: ", fw_typ_str(fw_t("plan", "report_title")),
           ", subtitle: ", fw_typ_str(fw_fill(fw_t("plan", "report_subtitle"), date = generated)),
           ", tagline: ", fw_typ_str(fw_t("app", "tagline")), ")"),

    # What was asked, before anything that came back.
    fw_typ_block(fw_t("plan", "report_selection"), NULL,
                 fw_typ_table(fw_filters_sheet(filters, nrow(sel), nrow(data$attempt), meta),
                              widths = c("auto", "1fr"))),

    fw_typ_block(fw_t("plan", "r_heading"), NULL, fw_typ_stats(s)),

    species_block("invasive"),
    species_block("beneficiary"),

    if (!is.null(map_fig)) {
      fw_typ_block(fw_t("plan", "r_map"), fw_t("plan", "pdf_map_note"), c(
        map_fig,
        if (n_no_coords > 0) fw_typ_caption(fw_fill(fw_t("plan", "r_map_missing"),
                                                   n = fw_fmt_num(n_no_coords)))
      ))
    },
    fw_typ_block(fw_t("plan", "report_where"), NULL,
                 fw_typ_table(fw_report_country_table(sel),
                              num = fw_t("export", "col_attempts"),
                              widths = c("1fr", "auto"))),

    fw_typ_block(fw_t("plan", "r_outcomes"), fw_t("plan", "r_outcome_note"),
                 fw_typ_outcome_bars(sel)),

    if (!is.null(method_fig)) {
      fw_typ_block(fw_t("plan", "r_method"), fw_t("plan", "r_method_note"), c(
        method_fig,
        if (n_no_method > 0) fw_typ_caption(fw_fill(fw_t("plan", "r_method_missing"),
                                                   n = fw_fmt_num(n_no_method)))
      ))
    },

    if (!is.null(duration_fig)) {
      fw_typ_block(fw_t("plan", "r_duration"), fw_t("plan", "r_duration_note"), c(
        duration_fig,
        fw_typ_caption(fw_fill(fw_t("plan", "r_duration_missing"), n = fw_fmt_num(n_duration)))
      ))
    },

    if (!is.null(waterbody_fig)) {
      fw_typ_block(fw_t("plan", "r_waterbody"),
                   fw_fill(fw_t("plan", "r_waterbody_note"), n_word = fw_num_word(FW_TOP_N)),
                   waterbody_fig)
    },

    if (!is.null(method_wb_fig)) {
      fw_typ_block(fw_t("plan", "r_method_wb"), fw_t("plan", "r_method_wb_note"),
                   method_wb_fig)
    },

    # Every contact, no pager: a document is read, not clicked through.
    if (nrow(people)) {
      fw_typ_block(fw_t("plan", "r_contacts"), fw_t("plan", "r_contacts_note"),
                   fw_typ_table(fw_pdf_contacts_table(people),
                                num = fw_t("plan", "col_contact_n"),
                                widths = c("1fr", "1.4fr", "0.9fr", "auto", "1.5fr")))
    },

    # Last, and never optional.
    paste0("#fw-caveats(", fw_typ_str(fw_t("about", "caveats_heading")), ", ",
           fw_typ_array(vapply(caveats, function(b) fw_typ_array(c(
             fw_typ_str(fw_caveat_title(b$heading)),
             fw_typ_str(paste(b$body, collapse = "\n\n")))), "")), ")"),

    paste0("#v(6mm)\n#text(fill: fw-ink-muted)[#", fw_typ_str(fw_t("plan", "report_footer")), "]")
  )
  paste(unlist(Filter(Negate(is.null), parts)), collapse = "\n\n")
}

# ---- The document ------------------------------------------------------------

#' Write the PDF report
#'
#' @param path where to write. The download handler's temp file.
#' @param method_mode,method_wb_mode,waterbody_mode whichever mode each chart's
#'   toggle is showing, so the document matches the screen
#' @param keep a directory to copy the render directory into, for debugging
#'   and the tests. NULL removes it.
fw_write_pdf_report <- function(path, data, sel, export = NULL, filters, meta = NULL,
                                method_mode = "count", method_wb_mode = "count",
                                waterbody_mode = "count", keep = NULL) {
  quarto <- fw_quarto_path()
  if (!nzchar(quarto)) {
    stop("The PDF report needs the Quarto CLI, which is not installed here.",
         call. = FALSE)
  }

  dir <- tempfile("fw-pdf-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # The template, the fonts and the logos, beside the document that uses them.
  res <- file.path("resources", "report")
  file.copy(file.path(res, c("typst-template.typ", "typst-show.typ", "page.typ")), dir)
  dir.create(file.path(dir, "fonts"))
  file.copy(list.files(file.path(res, "fonts"), pattern = "[.]ttf$", full.names = TRUE),
            file.path(dir, "fonts"))
  logo_src <- c(mark = FW_LOGO$mark_file, wfa = FW_LOGO$wfa_file, FW_LOGO$collab_files)
  # A logo that is missing is skipped, as the old report did, rather than
  # failing the download - but only if it is missing; dev/plan_test.R checks
  # every one is there.
  logo_src <- logo_src[file.exists(logo_src)]
  logos <- stats::setNames(paste0("logo-", names(logo_src), ".png"), names(logo_src))
  file.copy(logo_src, file.path(dir, logos))
  writeLines(fw_pdf_tokens(logos), file.path(dir, "fwise-tokens.typ"), useBytes = TRUE)

  body <- fw_pdf_body(dir, data, sel, filters, meta, method_mode = method_mode,
                      method_wb_mode = method_wb_mode, waterbody_mode = waterbody_mode)

  qmd <- c(
    "---",
    "format:",
    "  typst:",
    "    papersize: a4",
    "    font-paths: [fonts]",
    "    template-partials: [typst-template.typ, typst-show.typ, page.typ]",
    # The intermediate .typ is kept beside the PDF: it is deleted with the
    # render directory anyway, and the tests and fw_write_pdf_report(keep =)
    # read it.
    "    keep-typ: true",
    "engine: markdown",
    "---",
    "",
    # A five-backtick fence, so nothing a record says can close it.
    "`````{=typst}",
    body,
    "`````"
  )
  writeLines(enc2utf8(qmd), file.path(dir, "report.qmd"), useBytes = TRUE)

  log <- suppressWarnings(system2(
    quarto, c("render", shQuote(file.path(dir, "report.qmd")), "--quiet"),
    stdout = TRUE, stderr = TRUE, timeout = FW_PDF$timeout_s
  ))
  status <- attr(log, "status") %||% 0L
  out <- file.path(dir, "report.pdf")

  if (!is.null(keep)) {
    dir.create(keep, showWarnings = FALSE, recursive = TRUE)
    file.copy(list.files(dir, full.names = TRUE), keep, recursive = TRUE, overwrite = TRUE)
  }

  # A PDF that silently arrives empty is worse than an error: checked, the same
  # rule as the zip in fw_write_bundle().
  if (!identical(as.integer(status), 0L) || !file.exists(out) || file.size(out) < 1000) {
    stop("Could not write the PDF report (quarto exit status ", status, "): ",
         paste(utils::tail(log, 8), collapse = "\n"), call. = FALSE)
  }
  file.copy(out, path, overwrite = TRUE)

  # A large one is logged beside its estimate, so the calibration in FW_PDF can
  # be checked against what readers actually download.
  mb <- file.size(path) / 1e6
  pages <- fw_pdf_page_count(path)
  if (mb >= FW_PDF$warn_mb || isTRUE(pages >= FW_PDF$warn_pages)) {
    est <- fw_pdf_size_estimate(data, sel)
    message(sprintf("FWISE PDF report: %.1f MB, %d pages for %d attempts (estimated %.1f MB, %d pages)",
                    mb, pages, nrow(sel), est$mb, est$pages))
  }
  invisible(path)
}

#' How big a PDF of this selection will be, before it is made
#'
#' Read by the download picker, which warns at FW_PDF$warn_pages or
#' FW_PDF$warn_mb. The charts cost about the same whatever the selection; what
#' grows is the contacts table (a row per person), the map (a dot per attempt)
#' and the species photographs. Coefficients in FW_PDF, fitted to real renders.
#'
#' @return list(mb, pages)
fw_pdf_size_estimate <- function(data, sel) {
  n_contacts <- nrow(fw_plan_contacts(data, sel))
  n_images <- sum(vapply(c("invasive", "beneficiary"), function(r) {
    top <- fw_species_top_n(data, sel, r, FW_PLAN_SPECIES_N)
    sum(vapply(top$species_id, function(id)
      !is.null(fw_species_image_cached(data$species, id)), logical(1)))
  }, numeric(1)))
  n_points <- sum(!is.na(sel$latitude) & !is.na(sel$longitude))
  bytes <- FW_PDF$est_base + n_images * FW_PDF$est_per_image +
    min(n_points, FW_PDF$est_point_cap) * FW_PDF$est_per_point +
    n_contacts * FW_PDF$est_per_contact
  pages <- FW_PDF$est_pages_base + ceiling(n_contacts / FW_PDF$est_contacts_per_page)
  list(mb = bytes / 1e6, pages = as.integer(pages))
}

#' How many pages a PDF has, read from its page tree
#'
#' The page tree's /Count is the one page figure Typst writes uncompressed, so
#' this needs no PDF library. The largest /Count is the root's - an outline
#' carries a /Count of its own. For the tests and the size calibration.
fw_pdf_page_count <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("/Count [0-9]+", bytes, value = TRUE, all = TRUE)
  if (!length(hits)) return(NA_integer_)
  max(as.integer(sub("/Count ", "", vapply(hits, rawToChar, ""))))
}

#' Filename for the PDF report
fw_pdf_filename <- function() {
  paste0("fwise-report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
}
