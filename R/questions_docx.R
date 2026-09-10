# questions_docx.R
# The offline question list as a Word document.
#
# SAME WALKER AS THE TEXT VERSION. This reads fw_step_items() - the function
# questions_text.R uses - rather than building its own idea of what the form
# asks. That is the whole reason the download is generated at all: the old Word
# file was maintained by hand and went stale the first time a label changed,
# with nobody the wiser. Two generated files from one walker cannot disagree;
# two hand-maintained ones always eventually do.
#
# Add a field to a step and it appears in BOTH downloads with no edit here.
#
# DELIBERATELY PLAIN. This is a working document that people type into and mail
# around, not a designed handout: a logo, headings, questions, nothing else. It
# has no page breaks and no ruled answer lines, because both fight the reader
# the moment they start typing - a break pushes a section onto its own page and
# leaves the previous one half empty, and a ruled line does not grow with the
# answer written on it. Sections simply follow one another and answers go under
# the question. If you are tempted to add layout here, add it to the PDF or the
# web form instead.

library(officer)

# The two colours the document uses. Headings carry the FWISE green so the file
# is recognisably ours; everything else is default black body text.
FW_DOCX_INK <- "#0a2e29"
FW_DOCX_DEEP <- "#0d574c"

fw_docx_fp <- function(size = 11, colour = FW_DOCX_INK, bold = FALSE,
                       italic = FALSE) {
  fp_text(font.family = "Calibri", font.size = size, color = colour,
          bold = bold, italic = italic)
}

#' The question list as an officer document
#'
#' @param choices the startup choice lists, so dropdown options can be listed
#' @param logo path to the FWISE lockup, placed at the top
fw_questions_docx <- function(choices, logo = "www/img/FWISE-LOGO-ALL-6.png") {
  ns <- shiny::NS(NULL)
  doc <- read_docx()

  # One paragraph. The only styling is the font, the colour and a little space
  # underneath, because officer's default is no space at all and the questions
  # run into their own guidance without it.
  para <- function(doc, text, size = 11, colour = FW_DOCX_INK, bold = FALSE,
                   after = 4) {
    body_add_fpar(doc, fpar(
      ftext(fw_squish(text), fw_docx_fp(size, colour, bold = bold)),
      fp_p = fp_par(padding.bottom = after)
    ))
  }

  # ---- Masthead --------------------------------------------------------------
  if (file.exists(logo)) {
    # Sized by width with height derived from the file's own aspect ratio, so a
    # replacement lockup of different proportions is not squashed.
    dims <- tryCatch(dim(png::readPNG(logo)), error = function(e) NULL)
    w <- 1.9
    h <- if (is.null(dims)) 0.75 else round(w * dims[1] / dims[2], 2)
    doc <- body_add_img(doc, logo, width = w, height = h)
  }

  doc <- doc |>
    para("FWISE submission question list", size = 16, colour = FW_DOCX_DEEP,
         bold = TRUE, after = 8) |>
    para(paste(
      "Every question on the online submission form, in the order you will meet",
      "it. Use this to gather your answers offline, then copy them across when",
      "you are ready. Only the questions marked REQUIRED have to be answered; a",
      "partial record is far better than none. The form cannot save your",
      "progress, so please complete it in one sitting."
    )) |>
    para(paste("Generated", format(Sys.Date(), "%d %B %Y")))

  # ---- The sections ----------------------------------------------------------
  step_no <- 0L
  for (step in FW_STEPS) {
    builder <- FW_STEP_UI[[step$id]]
    if (is.null(builder)) next
    step_no <- step_no + 1L

    title <- fw_t("contribute", "steps", step$title_key)
    items <- fw_step_items(builder(ns, choices), ns, choices,
                           conditional = isTRUE(step$conditional))
    questions <- Filter(function(i) identical(i$kind, "question"), items)
    # The review step only plays back what has already been entered, so it has
    # nothing to prepare for and is left out.
    if (!length(questions)) next

    doc <- body_add_par(doc, "")
    doc <- para(doc, paste0(step_no, ". ", title), size = 14,
                colour = FW_DOCX_DEEP, bold = TRUE, after = 6)
    if (isTRUE(step$conditional)) {
      doc <- para(doc, "This section is only shown if your earlier answers call for it.")
    }

    q_no <- 0L
    for (item in items) {
      if (identical(item$kind, "heading")) {
        doc <- para(doc, item$text, size = 12, colour = FW_DOCX_DEEP,
                    bold = TRUE, after = 6)

      } else if (identical(item$kind, "blurb") || identical(item$kind, "help")) {
        doc <- para(doc, item$text)

      } else if (identical(item$kind, "note")) {
        doc <- para(doc, paste0("(", item$text, ")"))

      } else if (identical(item$kind, "question")) {
        q_no <- q_no + 1L
        # REQUIRED is a word, not a colour or a symbol. It has to survive being
        # printed in black and white and read aloud.
        label <- paste0(step_no, ".", q_no, "  ", fw_squish(item$text))
        if (isTRUE(item$required))    label <- paste0(label, "  REQUIRED")
        if (isTRUE(item$conditional)) label <- paste0(label, "  [only if it applies]")
        doc <- para(doc, label, bold = TRUE)

        if (!is.null(item$guidance)) doc <- para(doc, item$guidance)
        if (length(item$options)) {
          doc <- para(doc, paste("Options:", fw_options_line(item$options)))
        }
        # Where the answer goes. An empty paragraph, not a ruled line: it grows
        # with whatever is typed into it instead of being pushed out of shape.
        doc <- body_add_par(doc, "")
      }
    }
  }

  doc |>
    body_add_par("") |>
    para(fw_t("footer", "licence"))
}

#' Write the question list to a .docx file
fw_write_questions_docx <- function(path, choices) {
  print(fw_questions_docx(choices), target = path)
  invisible(path)
}
