# questions_docx.R
# The offline question list as Word, from the same walker as the text version
# (fw_step_items()), so new form fields appear in both automatically.
#
# Deliberately plain: a logo, headings, questions. No page breaks or ruled
# lines, since people type into it.

library(officer)

# Ink and teal_text from brand.R, read as defaults to avoid a source-order
# dependency.
fw_docx_fp = function(size = 11, colour = FW_COLOURS$ink, bold = FALSE,
                       italic = FALSE) {
  fp_text(font.family = FW_TYPE$font_docx, font.size = size, color = colour,
          bold = bold, italic = italic)
}

#' The question list as an officer document
#'
#' @param choices the startup choice lists, so dropdown options can be listed
#' @param logo path to the FWISE mark, sized by its own aspect ratio
fw_questions_docx = function(choices, logo = FW_LOGO$mark_file) {
  ns = shiny::NS(NULL)
  doc = read_docx()

  # One paragraph, with a little space after (officer's default is none).
  para = function(doc, text, size = 11, colour = FW_COLOURS$ink, bold = FALSE,
                   after = 4) {
    body_add_fpar(doc, fpar(
      ftext(fw_squish(text), fw_docx_fp(size, colour, bold = bold)),
      fp_p = fp_par(padding.bottom = after)
    ))
  }

  # ---- Masthead --------------------------------------------------------------
  if (file.exists(logo)) {
    dims = tryCatch(dim(png::readPNG(logo)), error = function(e) NULL)
    w = 1.9
    h = if (is.null(dims)) 0.75 else round(w * dims[1] / dims[2], 2)
    doc = body_add_img(doc, logo, width = w, height = h)
    # A blank line under the logo.
    doc = body_add_par(doc, "")
  }

  q = fw_t("questions")
  doc = doc |>
    para(q$title_docx, size = 16, colour = FW_COLOURS$teal_text,
         bold = TRUE, after = 8) |>
    para(paste(fw_fill(q$intro, required = q$required_docx), q$no_save)) |>
    para(paste(q$generated, format(Sys.Date(), "%d %B %Y")))

  # ---- The sections ----------------------------------------------------------
  step_no = 0L
  for (step in FW_STEPS) {
    builder = FW_STEP_UI[[step$id]]
    if (is.null(builder)) next
    step_no = step_no + 1L

    title = fw_t("contribute", "steps", step$title_key)
    items = fw_step_items(builder(ns, choices), ns, choices,
                           conditional = isTRUE(step$conditional))
    questions = Filter(function(i) identical(i$kind, "question"), items)
    # Skip the review step.
    if (!length(questions)) next

    doc = body_add_par(doc, "")
    doc = para(doc, paste0(step_no, ". ", title), size = 14,
                colour = FW_COLOURS$teal_text, bold = TRUE, after = 6)
    if (isTRUE(step$conditional)) {
      doc = para(doc, q$conditional_note)
    }

    q_no = 0L
    for (item in items) {
      if (identical(item$kind, "heading")) {
        doc = para(doc, item$text, size = 12, colour = FW_COLOURS$teal_text,
                    bold = TRUE, after = 6)

      } else if (identical(item$kind, "blurb") || identical(item$kind, "help")) {
        doc = para(doc, item$text)

      } else if (identical(item$kind, "note")) {
        doc = para(doc, paste0("(", item$text, ")"))

      } else if (identical(item$kind, "question")) {
        q_no = q_no + 1L
        # "Required" as a word, so it survives print and screen readers.
        label = paste0(step_no, ".", q_no, "  ", fw_squish(item$text))
        if (isTRUE(item$required))    label = paste0(label, "  ", q$required_docx)
        if (isTRUE(item$conditional)) label = paste0(label, "  ", q$conditional_tag)
        doc = para(doc, label, bold = TRUE)

        if (!is.null(item$guidance)) doc = para(doc, item$guidance)
        if (length(item$options)) {
          doc = para(doc, paste(q$options, fw_options_line(item$options)))
        }
        # An empty paragraph for the answer.
        doc = body_add_par(doc, "")
      }
    }
  }

  doc |>
    body_add_par("") |>
    para(fw_t("footer", "licence"))
}

#' Write the question list to a .docx file
fw_write_questions_docx = function(path, choices) {
  print(fw_questions_docx(choices), target = path)
  invisible(path)
}
