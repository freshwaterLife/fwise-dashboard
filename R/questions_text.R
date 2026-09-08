# questions_text.R
# The offline question list, generated from the form itself.
#
# WHY IT IS GENERATED RATHER THAN WRITTEN. The old download was a Word file kept
# by hand, which meant it went stale the first time a label changed and nobody
# would know. This walks the same step builders the wizard renders, so the file a
# contributor downloads is by construction the form they are about to fill in.
# Add a field to a step and it appears here with no second edit.
#
# It reads the rendered tags rather than a separate registry for the same reason:
# a registry is a second copy of the truth.

library(htmltools)

# Repeatable blocks render as an empty container that the server fills in. The
# container id is the key; the value produces row 1 so its questions can be shown
# in the text file. Keep in step with the insertUI targets in mod_contribute.R.
FW_QUESTION_ROWS <- list(
  target_rows  = function(ns, choices) fw_target_row(ns, 1L, choices),
  benefit_rows = function(ns, choices) fw_species_row(ns, 1L, choices, "benefit"),
  method_rows  = function(ns, choices) fw_method_row(ns, 1L, choices)
)

# ---- Tag helpers -------------------------------------------------------------

#' The class attribute of a tag, as a character vector
fw_tag_classes <- function(x) {
  cls <- x$attribs$class
  if (is.null(cls)) return(character(0))
  strsplit(paste(unlist(cls), collapse = " "), "\\s+")[[1]]
}

fw_tag_has_class <- function(x, cls) cls %in% fw_tag_classes(x)

#' Every text node under a tag, joined
#'
#' Anything marked visually hidden is dropped: it exists to be announced by a
#' screen reader and would read as noise here. So is the decorative asterisk,
#' which is replaced by the word "required" further down.
fw_tag_text <- function(x, drop_hidden = TRUE) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = " "))
  if (inherits(x, "html")) return(as.character(x))
  if (inherits(x, "shiny.tag")) {
    if (drop_hidden &&
        (fw_tag_has_class(x, "fw-visually-hidden") ||
         fw_tag_has_class(x, "fw-required-mark"))) {
      return("")
    }
    return(fw_tag_text(x$children, drop_hidden))
  }
  if (is.list(x)) {
    parts <- vapply(x, fw_tag_text, character(1), drop_hidden = drop_hidden)
    return(paste(parts[nzchar(parts)], collapse = " "))
  }
  ""
}

#' Collapse runs of whitespace, so wrapped source strings read as one line
fw_squish <- function(x) {
  x <- gsub("[\r\n\t ]+", " ", paste(x, collapse = " "))
  trimws(x)
}

#' fw_squish() over a vector, element by element
fw_squish_each <- function(x) {
  if (!length(x)) return(character(0))
  vapply(x, fw_squish, character(1), USE.NAMES = FALSE)
}

#' Find every descendant tag matching a predicate, in document order
fw_tag_find <- function(x, pred) {
  out <- list()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (isTRUE(pred(node))) out[[length(out) + 1L]] <<- node
      walk(node$children)
    } else if (is.list(node)) {
      lapply(node, walk)
    }
    invisible(NULL)
  }
  walk(x)
  out
}

# Above this many options the list is summarised rather than printed. The
# country dropdown alone runs to about 200 entries and would bury the questions.
FW_QUESTION_OPTION_CAP <- 18L

#' The answer options offered by a field, where it offers a fixed set
#'
#' Covers the two shapes the form uses: a select (every dropdown) and a group of
#' radios or checkboxes. Server-side selectizes carry no options in the markup -
#' the species pickers are the only ones - and correctly come back empty.
#'
#' Radios are read through .shiny-options-group and the labels inside it rather
#' than through a .radio class, because the wrapper markup differs between
#' Bootstrap versions and the labels do not.
fw_field_options <- function(node) {
  selects <- fw_tag_find(node, function(n) identical(n$name, "select"))
  if (length(selects)) {
    # A select's options are NOT tags. shiny's selectOptions() hands back one
    # pre-rendered HTML string, so walking the children finds nothing and the
    # markup has to be read back out.
    html <- as.character(selects[[1]])
    m <- regmatches(html, gregexpr("<option[^>]*>[^<]*</option>", html))[[1]]
    labels <- fw_squish_each(sub("^<option[^>]*>", "", sub("</option>$", "", m)))
    return(labels[nzchar(labels) & labels != "Select..."])
  }
  groups <- fw_tag_find(node, function(n) {
    fw_tag_has_class(n, "shiny-options-group")
  })
  if (!length(groups)) return(character(0))
  labels <- unlist(lapply(groups, function(g) {
    vapply(fw_tag_find(g, function(n) identical(n$name, "label")),
           function(l) fw_squish(fw_tag_text(l)), character(1))
  }))
  labels[nzchar(labels)]
}

#' Options as one line, summarised once the list gets long
fw_options_line <- function(opts) {
  if (length(opts) > FW_QUESTION_OPTION_CAP) {
    shown <- opts[seq_len(FW_QUESTION_OPTION_CAP)]
    return(paste0(paste(shown, collapse = "; "),
                  sprintf("; ... and %d more, listed on the form",
                          length(opts) - FW_QUESTION_OPTION_CAP)))
  }
  paste(opts, collapse = "; ")
}

# ---- Walking one step --------------------------------------------------------

#' Turn one step's rendered UI into a list of text items
#'
#' Items come out in document order. A label row and its help text are handled
#' as units and everything else is recursed into, which is what lets a field
#' nested inside another field - latitude and longitude inside the location
#' block - come through in the right place.
fw_step_items <- function(ui, ns, choices, conditional = FALSE) {
  items <- list()
  add <- function(...) items[[length(items) + 1L]] <<- list(...)

  walk <- function(node, cond) {
    if (is.null(node) || is.character(node)) return(invisible(NULL))

    if (is.list(node) && !inherits(node, "shiny.tag")) {
      lapply(node, walk, cond = cond)
      return(invisible(NULL))
    }
    if (!inherits(node, "shiny.tag")) return(invisible(NULL))

    nm <- node$name

    # A repeatable block: an empty container the server fills in. Render row 1
    # so its questions are not silently missing from the list.
    id <- node$attribs$id
    if (!is.null(id)) {
      key <- sub("^.*-", "", as.character(id))
      builder <- FW_QUESTION_ROWS[[key]]
      if (!is.null(builder)) {
        add(kind = "note",
            text = paste("You can add as many of these as you need;",
                         "the form starts with one."))
        # A repeatable row carries its own "Target 1" heading in the app; in
        # print the numbered question below already says which one it is.
        walk(builder(ns, choices), cond)
        return(invisible(NULL))
      }
    }

    # Everything inside a conditional panel is only asked sometimes.
    if (fw_tag_has_class(node, "shiny-panel-conditional")) {
      walk(node$children, cond = TRUE)
      return(invisible(NULL))
    }

    if (nm %in% c("h2", "h3", "h4")) {
      add(kind = "heading", text = fw_squish(fw_tag_text(node)))
      return(invisible(NULL))
    }

    if (fw_tag_has_class(node, "fw-lead")) {
      add(kind = "blurb", text = fw_squish(fw_tag_text(node)))
      return(invisible(NULL))
    }

    if (fw_tag_has_class(node, "fw-field__help")) {
      add(kind = "help", text = fw_squish(fw_tag_text(node)))
      return(invisible(NULL))
    }

    if (fw_tag_has_class(node, "fw-field__label-row")) {
      label <- fw_tag_find(node, function(n) identical(n$name, "label"))
      info  <- fw_tag_find(node, function(n) fw_tag_has_class(n, "fw-info-btn"))
      required <- length(fw_tag_find(node, function(n) {
        fw_tag_has_class(n, "fw-required-mark")
      })) > 0
      add(
        kind = "question",
        text = if (length(label)) fw_squish(fw_tag_text(label[[1]])) else "",
        required = required,
        conditional = cond,
        guidance = if (length(info)) {
          fw_squish(info[[1]]$attribs$`data-bs-content`)
        } else NULL
      )
      return(invisible(NULL))
    }

    # A field's control sits next to its label row, so options are collected
    # from the .fw-field wrapper once the label has been recorded.
    if (fw_tag_has_class(node, "fw-field")) {
      lapply(node$children, walk, cond = cond)
      opts <- fw_field_options(node)
      if (length(opts)) {
        n_before <- length(items)
        # Attach to the most recent question, which is this field's own.
        for (i in rev(seq_len(n_before))) {
          if (identical(items[[i]]$kind, "question")) {
            items[[i]]$options <<- opts
            break
          }
        }
      }
      return(invisible(NULL))
    }

    walk(node$children, cond = cond)
  }

  walk(ui, cond = conditional)
  items
}

# ---- Rendering ---------------------------------------------------------------

#' Wrap a block of text under a hanging indent
fw_wrap_block <- function(text, first, hang, width = 78) {
  lines <- strwrap(text, width = width - nchar(hang))
  if (!length(lines)) return(character(0))
  c(paste0(first, lines[1]),
    if (length(lines) > 1) paste0(hang, lines[-1]))
}

#' The whole question list as plain text
#'
#' @param choices the startup choice lists, so dropdown options can be listed
#' @param width   wrap column
fw_questions_text <- function(choices, width = 78) {
  ns <- shiny::NS(NULL)
  rule <- strrep("=", width)
  thin <- strrep("-", width)

  out <- c(
    rule,
    "FWISE - SUBMISSION QUESTION LIST",
    fw_t("app", "full_title"),
    rule,
    "",
    strwrap(paste(
      "Every question on the online submission form, in the order you will",
      "meet it. Use this to gather your answers offline, then copy them across",
      "when you are ready. Only the questions marked *REQUIRED* have to be",
      "answered; a partial record is far better than none."
    ), width = width),
    "",
    strwrap(paste(
      "There are no accounts and no logins, so the form cannot save your",
      "progress. Please complete it in one sitting."
    ), width = width),
    "",
    paste("Generated", format(Sys.Date(), "%d %B %Y")),
    ""
  )

  step_no <- 0L
  for (step in FW_STEPS) {
    builder <- FW_STEP_UI[[step$id]]
    if (is.null(builder)) next
    step_no <- step_no + 1L

    title <- fw_t("contribute", "steps", step$title_key)
    items <- fw_step_items(builder(ns, choices), ns, choices,
                           conditional = isTRUE(step$conditional))

    questions <- Filter(function(i) identical(i$kind, "question"), items)
    # The review step only plays back what you have already entered, so it has
    # nothing to prepare for and is left out.
    if (!length(questions)) next

    out <- c(out, "", rule,
             paste0("SECTION ", step_no, ". ", toupper(title)),
             rule)
    if (isTRUE(step$conditional)) {
      out <- c(out, strwrap(
        "This section is only shown if your earlier answers call for it.",
        width = width))
    }

    q_no <- 0L
    hang <- strrep(" ", 5)
    # The answer rule is held back until everything belonging to the question
    # has been printed. fw_field() renders its help text AFTER the control, so
    # emitting the rule as soon as the label appears put the ruled line above
    # the sentence explaining what to write on it.
    pending <- FALSE
    flush <- function() {
      if (pending) {
        out <<- c(out, paste0(hang, "Answer: ", strrep("_", 50)))
        pending <<- FALSE
      }
    }

    for (item in items) {
      if (identical(item$kind, "blurb")) {
        flush()
        out <- c(out, "", strwrap(item$text, width = width))
      } else if (identical(item$kind, "heading")) {
        flush()
        out <- c(out, "", item$text, strrep("-", nchar(item$text)))
      } else if (identical(item$kind, "note")) {
        flush()
        out <- c(out, "", strwrap(paste0("(", item$text, ")"), width = width,
                                  prefix = "    "))
      } else if (identical(item$kind, "question")) {
        flush()
        q_no <- q_no + 1L
        tag <- sprintf("%-5s", sprintf("%d.%d", step_no, q_no))
        head_line <- item$text
        if (isTRUE(item$required)) head_line <- paste(head_line, "*REQUIRED*")
        if (isTRUE(item$conditional)) {
          head_line <- paste(head_line, "[only if it applies]")
        }
        out <- c(out, "", fw_wrap_block(head_line, tag, hang, width))
        if (!is.null(item$guidance)) {
          out <- c(out, fw_wrap_block(item$guidance, paste0(hang, "Guidance: "),
                                      paste0(hang, "          "), width))
        }
        if (length(item$options)) {
          out <- c(out, fw_wrap_block(fw_options_line(item$options),
                                      paste0(hang, "Options:  "),
                                      paste0(hang, "          "), width))
        }
        pending <- TRUE
      } else if (identical(item$kind, "help")) {
        out <- c(out, fw_wrap_block(item$text, hang, hang, width))
      }
    }
    flush()
  }

  out <- c(out, "", thin,
           strwrap(fw_t("footer", "licence"), width = width),
           thin, "")

  paste(out, collapse = "\n")
}
