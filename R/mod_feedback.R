# mod_feedback.R
# "Send feedback", in a dialog.
#
# ONE INSTANCE FOR THE WHOLE APP, like the newsletter (see mod_newsletter.R),
# opened by any fw_form_open("feedback", ...).
#
# MORE THAN ONE PER VISIT IS FINE, unlike the newsletter: someone working
# through the app may find three things. Each opening of the dialog is a fresh
# form; the thank-you has no "send more" button (Alex, 2 Oct 2026). A short cooldown
# (FW_FEEDBACK_COOLDOWN_S) stops a double press or an impatient resend writing
# the same message twice.
#
# NO CONSENT BOX. Feedback is not marketing, and the email is optional and used
# only to reply; the lawful basis is legitimate interests, which the privacy
# page says in its feedback section.

#' @param open a reactive holding input$fw_open_form: list(form, source)
#' @param nav  a reactive holding input$fw_nav, the page the reader is on
mod_feedback_server <- function(id, open, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sent      <- reactiveVal(FALSE)
    opened    <- reactiveVal(0L)
    status    <- reactiveVal(NULL)
    last_sent <- reactiveVal(-Inf)

    vals <- reactive(fw_feedback_values(input$page, input$message, input$email))
    problems <- reactive(fw_feedback_problems(vals()))

    fields <- c("page", "message", "email")
    validators <- lapply(stats::setNames(fields, fields), function(f) {
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule(f, function(value) problems()[[f]])
      iv
    })
    reset <- function() {
      status(NULL)
      sent(FALSE)
      for (iv in validators) iv$disable()
      opened(opened() + 1L)
    }

    observeEvent(open(), {
      req(identical(open()$form, "feedback"))
      reset()
      showModal(fw_form_modal(fw_t("feedback", "title"), uiOutput(ns("body"))))
    })

    observeEvent(input$touched, {
      f <- sub(ns(""), "", input$touched, fixed = TRUE)
      if (f %in% fields) validators[[f]]$enable()
    })

    output$body <- renderUI({
      opened()
      if (sent()) return(fw_form_success(fw_t("feedback", "success")))
      isolate(fw_feedback_form(ns, fw_feedback_default_page(nav())))
    })
    output$status <- renderUI(fw_form_status(status()))

    observe({
      opened()
      updateActionButton(session, "submit", disabled = length(problems()) > 0)
    })

    observeEvent(input$submit, {
      if (sent()) return()

      if (nzchar(fw_form_value(input$website))) {
        sent(TRUE)
        return()
      }

      if (length(problems()) > 0) {
        for (iv in validators) iv$enable()
        updateActionButton(session, "submit", disabled = TRUE)
        return()
      }

      now <- as.numeric(Sys.time())
      if (now - last_sent() < FW_FEEDBACK_COOLDOWN_S) {
        status(fw_t("feedback", "cooldown"))
        updateActionButton(session, "submit", disabled = FALSE)
        return()
      }

      v <- vals()
      res <- store_feedback(fw_feedback_record(v))
      if (isTRUE(res$success)) {
        last_sent(now)
        status(NULL)
        sent(TRUE)
        fw_track(session, "form_submit", list(form = "feedback"))
      } else {
        status(fw_t("feedback", "failure"))
        updateActionButton(session, "submit", disabled = FALSE)
      }
    })
  })
}

#' The feedback form
#'
#' @param selected the page to pre-select, from fw_feedback_default_page()
fw_feedback_form <- function(ns, selected) {
  t <- function(key) fw_t("feedback", key)
  max <- FW_FORM_LIMITS$message
  counter_id <- ns("message_counter")

  # THE LIMIT IS ENFORCED BY THE BROWSER AS WELL AS CHECKED HERE: maxlength
  # stops typing at the limit, and the counter under the box says how close
  # the reader is. The server checks again, because maxlength is a request.
  area <- htmltools::tagQuery(textAreaInput(ns("message"), label = NULL,
                                            width = "100%", rows = 6))
  area$find("textarea")$addAttrs(maxlength = max, `aria-required` = "true",
                                 `data-fw-counter` = counter_id,
                                 `aria-describedby` = counter_id)

  # A NATIVE SELECT, NOT SELECTIZE: eight fixed choices need no search box,
  # and the browser's own control is the most dependable one for a keyboard
  # and a screen reader. Shiny gives it .form-control, which in Bootstrap 5
  # draws no chevron, so it gets .form-select instead.
  page_select <- htmltools::tagQuery(
    selectInput(ns("page"), label = NULL, choices = fw_feedback_pages(),
                selected = selected, selectize = FALSE, width = "100%"))
  page_select$find("select")$removeClass("form-control")$addClass("form-select")$
    addAttrs(`aria-required` = "true")

  div(
    class = "fw-modal-form",
    `data-fw-touch` = ns("touched"),
    fw_field(page_select$allTags(), t("page"), required = TRUE, input_id = ns("page")),
    fw_field(
      tagList(
        area$allTags(),
        p(id = counter_id, class = "fw-modal-form__counter",
          `data-template` = t("counter"), `data-max` = max,
          fw_fill(t("counter"), n = 0, max = fw_fmt_num(max)))
      ),
      t("issue"), required = TRUE, input_id = ns("message")
    ),
    fw_field(fw_form_text(ns("email"), type = "email", autocomplete = "email"),
             t("email"), input_id = ns("email")),
    fw_form_data_line(t("data_line"), "feedback"),
    fw_honeypot(ns("website")),
    p(class = "fw-modal-form__required-note", fw_t("forms", "required_note")),
    div(
      class = "fw-modal-form__actions",
      actionButton(ns("submit"), t("submit"), class = "btn btn-primary",
                   disabled = TRUE, `data-fw-busy` = "true")
    ),
    uiOutput(ns("status"))
  )
}
