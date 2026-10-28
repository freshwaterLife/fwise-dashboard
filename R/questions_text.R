# questions_text.R
# The offline question list, generated from the form itself.


library(htmltools)

# Repeatable blocks render as an empty container that the server fills in. The
# container id is the key; the value produces row 1 so its questions can be shown
# in the text file. Keep in step with the insertUI targets in mod_contribute.R.
FW_QUESTION_ROWS = list(
  target_rows  = function(ns, choices) fw_target_row(ns, 1L, choices),
  benefit_rows = function(ns, choices) fw_beneficiary_row(ns, 1L, choices),
  method_rows  = function(ns, choices) fw_method_row(ns, 1L, choices)
)

# ---- Tag helpers -------------------------------------------------------------

#' The class attribute of a tag, as a character vector
fw_tag_classes = function(x) {
  cls = x$attribs$class
  if (is.null(cls)) return(character(0))
  strsplit(paste(unlist(cls), collapse = " "), "\\s+")[[1]]
}

fw_tag_has_class = function(x, cls) cls %in% fw_tag_classes(x)

#' Every text node under a tag, joined
#'
#' Drops visually hidden text and the decorative asterisk.
fw_tag_text = function(x, drop_hidden = TRUE) {
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
    parts = vapply(x, fw_tag_text, character(1), drop_hidden = drop_hidden)
    return(paste(parts[nzchar(parts)], collapse = " "))
  }
  ""
}

#' Collapse runs of whitespace, so wrapped source strings read as one line
fw_squish = function(x) {
  x = gsub("[\r\n\t ]+", " ", paste(x, collapse = " "))
  trimws(x)
}

#' fw_squish() over a vector, element by element
fw_squish_each = function(x) {
  if (!length(x)) return(character(0))
  vapply(x, fw_squish, character(1), USE.NAMES = FALSE)
}

#' Find every descendant tag matching a predicate, in document order
fw_tag_find = function(x, pred) {
  out = list()
  walk = function(node) {
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

# Above FW_QUESTION_OPTION_CAP options (R/config.R) the list is summarised
# rather than printed.

#' The answer options offered by a field, where it offers a fixed set
#'
#' Selects and radio/checkbox groups. Radios are read via
#' .shiny-options-group labels, which are stable across Bootstrap versions.
fw_field_options = function(node) {
  selects = fw_tag_find(node, function(n) identical(n$name, "select"))
  if (length(selects)) {
    # A select's options are one pre-rendered HTML string, so parse it.
    html = as.character(selects[[1]])
    m = regmatches(html, gregexpr("<option[^>]*>[^<]*</option>", html))[[1]]
    labels = fw_squish_each(sub("^<option[^>]*>", "", sub("</option>$", "", m)))
    return(labels[nzchar(labels) & labels != fw_t("contribute", "placeholder", "select")])
  }
  groups = fw_tag_find(node, function(n) {
    fw_tag_has_class(n, "shiny-options-group")
  })
  if (!length(groups)) return(character(0))
  labels = unlist(lapply(groups, function(g) {
    vapply(fw_tag_find(g, function(n) identical(n$name, "label")),
           function(l) fw_squish(fw_tag_text(l)), character(1))
  }))
  labels[nzchar(labels)]
}

#' Options as one line, summarised once the list gets long
fw_options_line = function(opts) {
  if (length(opts) > FW_QUESTION_OPTION_CAP) {
    shown = opts[seq_len(FW_QUESTION_OPTION_CAP)]
    return(paste0(paste(shown, collapse = "; "),
                  fw_fill(fw_t("questions", "more_options"),
                          n = length(opts) - FW_QUESTION_OPTION_CAP)))
  }
  paste(opts, collapse = "; ")
}

# ---- Walking one step --------------------------------------------------------

#' Turn one step's rendered UI into a list of text items
#'
#' In document order; label rows and help text are units, everything else
#' recursed (so nested fields come out in place).
fw_step_items = function(ui, ns, choices, conditional = FALSE) {
  items = list()
  add = function(...) items[[length(items) + 1L]] <<- list(...)

  walk = function(node, cond) {
    if (is.null(node) || is.character(node)) return(invisible(NULL))

    if (is.list(node) && !inherits(node, "shiny.tag")) {
      lapply(node, walk, cond = cond)
      return(invisible(NULL))
    }
    if (!inherits(node, "shiny.tag")) return(invisible(NULL))

    nm = node$name

    # A repeatable block: render row 1 so its questions are listed.
    id = node$attribs$id
    if (!is.null(id)) {
      key = sub("^.*-", "", as.character(id))
      builder = FW_QUESTION_ROWS[[key]]
      if (!is.null(builder)) {
        add(kind = "note", text = fw_t("questions", "repeat_note"))
        # Drop the row heading; the numbered question says it.
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
      label = fw_tag_find(node, function(n) identical(n$name, "label"))
      info  = fw_tag_find(node, function(n) fw_tag_has_class(n, "fw-info-btn"))
      required = length(fw_tag_find(node, function(n) {
        fw_tag_has_class(n, "fw-required-mark")
      })) > 0
      add(
        kind = "question",
        # Join every label: fw_with_info() splits the last word into its own.
        text = if (length(label)) {
          fw_squish(paste(vapply(label, fw_tag_text, character(1)), collapse = " "))
        } else "",
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
      opts = fw_field_options(node)
      if (length(opts)) {
        n_before = length(items)
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
fw_wrap_block = function(text, first, hang, width = 78) {
  lines = strwrap(text, width = width - nchar(hang))
  if (!length(lines)) return(character(0))
  c(paste0(first, lines[1]),
    if (length(lines) > 1) paste0(hang, lines[-1]))
}

#' The whole question list as plain text
#'
#' @param choices the startup choice lists, so dropdown options can be listed
#' @param width   wrap column
fw_questions_text = function(choices, width = 78) {
  ns = shiny::NS(NULL)
  rule = strrep("=", width)
  thin = strrep("-", width)

  q = fw_t("questions")
  out = c(
    rule,
    q$title_text,
    fw_t("app", "full_title"),
    rule,
    "",
    strwrap(fw_fill(q$intro, required = q$required_text), width = width),
    "",
    strwrap(q$no_save, width = width),
    "",
    paste(q$generated, format(Sys.Date(), "%d %B %Y")),
    ""
  )

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

    out = c(out, "", rule,
             fw_fill(q$section, n = step_no, title = toupper(title)),
             rule)
    if (isTRUE(step$conditional)) {
      out = c(out, strwrap(q$conditional_note, width = width))
    }

    q_no = 0L
    hang = strrep(" ", 5)
    # Print the answer line after the help text (which follows the control).
    pending = FALSE
    flush = function() {
      if (pending) {
        # A blank line either side of the answer line.
        out <<- c(out, "", paste0(hang, q$answer, strrep("_", 50)), "")
        pending <<- FALSE
      }
    }

    for (item in items) {
      if (identical(item$kind, "blurb")) {
        flush()
        out = c(out, "", strwrap(item$text, width = width))
      } else if (identical(item$kind, "heading")) {
        flush()
        out = c(out, "", item$text, strrep("-", nchar(item$text)))
      } else if (identical(item$kind, "note")) {
        flush()
        out = c(out, "", strwrap(paste0("(", item$text, ")"), width = width,
                                  prefix = "    "))
      } else if (identical(item$kind, "question")) {
        flush()
        q_no = q_no + 1L
        tag = sprintf("%-5s", sprintf("%d.%d", step_no, q_no))
        head_line = item$text
        if (isTRUE(item$required)) head_line = paste(head_line, q$required_text)
        if (isTRUE(item$conditional)) {
          head_line = paste(head_line, q$conditional_tag)
        }
        out = c(out, "", fw_wrap_block(head_line, tag, hang, width))
        if (!is.null(item$guidance)) {
          out = c(out, fw_wrap_block(item$guidance, paste0(hang, q$guidance),
                                      paste0(hang, strrep(" ", nchar(q$guidance))),
                                      width))
        }
        if (length(item$options)) {
          out = c(out, fw_wrap_block(fw_options_line(item$options),
                                      paste0(hang, q$options, "  "),
                                      paste0(hang, strrep(" ", nchar(q$options) + 2)),
                                      width))
        }
        pending = TRUE
      } else if (identical(item$kind, "help")) {
        out = c(out, fw_wrap_block(item$text, hang, hang, width))
      }
    }
    flush()
  }

  out = c(out, "", thin,
           strwrap(fw_t("footer", "licence"), width = width),
           thin, "")

  paste(out, collapse = "\n")
}
