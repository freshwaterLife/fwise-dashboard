# analytics.R
# The one place the app reports that something happened. A NO-OP FOR NOW: no
# analytics tool has been chosen (Oct 2026), so track_event() checks what it
# was given and returns. Wiring a tool in later means filling in the marked
# line below and nothing else - every caller already reports through here.
#
# NO PERSONAL DATA, BY CONSTRUCTION. The signature takes an event name from a
# fixed list and, optionally, the page it happened on, and nothing more. There
# is no `...` and no free-form properties argument, so a name, an email or a
# message cannot be passed in by accident: doing it would mean changing this
# function, where the reason not to is written down. The privacy notice's
# "Usage analytics" section promises exactly this.

# Every event the app may report. Add a name here before calling it.
FW_EVENTS <- c("newsletter_signup", "feedback_submitted")

#' Report that something happened
#'
#' @param name one of FW_EVENTS
#' @param page optional: the navbar value of the page it happened on, e.g.
#'   "plan". A page name, never anything a visitor typed.
#' @return NULL, invisibly
track_event <- function(name, page = NULL) {
  stopifnot(length(name) == 1, name %in% FW_EVENTS)
  if (!is.null(page)) stopifnot(length(page) == 1, is.character(page))

  # WIRE THE ANALYTICS TOOL IN HERE, with `name` and `page` and nothing else.

  invisible(NULL)
}
