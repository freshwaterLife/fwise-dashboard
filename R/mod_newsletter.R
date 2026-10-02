# mod_newsletter.R
# The "Get FWISE updates" sign-up, in a dialog.
#
# ONE INSTANCE FOR THE WHOLE APP, started in app.R's server rather than inside
# a page, because its buttons are on more than one page and in the footer. Any
# fw_form_open("newsletter", ...) opens it; see that function in R/forms.R.
#
# ONE SIGN-UP PER VISIT. Once it has gone, every button that opens the form
# opens the thank-you instead, for the rest of the session.
#
# Rules and records are in R/forms.R, the write in R/forms_store.R, the words
# in R/copy_forms.R.

#' @param open a reactive holding input$fw_open_form: list(form, source)
mod_newsletter_server <- function(id, open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    done   <- reactiveVal(FALSE)
    opened <- reactiveVal(0L)
    entry  <- reactiveVal("unknown")
    status <- reactiveVal(NULL)

    vals <- reactive(fw_newsletter_values(input$name, input$email,
                                          input$organisation, input$consent))
    problems <- reactive(fw_newsletter_problems(vals()))

    # ONE VALIDATOR PER FIELD, so each can start showing its message on its
    # own, when the reader leaves that field. A single validator would light up
    # every empty field the moment it was switched on.
    fields <- c("name", "email", "organisation", "consent")
    validators <- lapply(stats::setNames(fields, fields), function(f) {
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule(f, function(value) problems()[[f]])
      iv
    })

    observeEvent(open(), {
      req(identical(open()$form, "newsletter"))
      entry(open()$source)
      status(NULL)
      for (iv in validators) iv$disable()
      opened(opened() + 1L)
      showModal(fw_form_modal(fw_t("newsletter", "title"), uiOutput(ns("body"))))
    })

    # A field has been left (see the focusout listener in fw_client_script()).
    observeEvent(input$touched, {
      f <- sub(ns(""), "", input$touched, fixed = TRUE)
      if (f %in% fields) validators[[f]]$enable()
    })

    # A FRESH FORM ON EVERY OPEN, and NOT redrawn on a failed send: the failure
    # message goes into its own slot, so what the reader typed stays put.
    output$body <- renderUI({
      opened()
      if (done()) return(fw_form_success(fw_t("newsletter", "success")))
      isolate(fw_newsletter_form(ns))
    })
    output$status <- renderUI(fw_form_status(status()))

    observe({
      opened()
      updateActionButton(session, "submit", disabled = length(problems()) > 0)
    })

    observeEvent(input$submit, {
      if (done()) return()

      # A filled honeypot is a bot: thank it, write nothing. See fw_honeypot().
      if (nzchar(fw_form_value(input$website))) {
        done(TRUE)
        return()
      }

      # The button is disabled while anything is wrong, so this is the browser
      # having been talked round. Show every message and stop.
      if (length(problems()) > 0) {
        for (iv in validators) iv$enable()
        updateActionButton(session, "submit", disabled = TRUE)
        return()
      }

      res <- store_newsletter_signup(fw_newsletter_record(vals(), entry()))
      if (isTRUE(res$success)) {
        status(NULL)
        done(TRUE)
        track_event("newsletter_signup")
      } else {
        status(fw_t("newsletter", "failure"))
        # The click disabled the button in the browser; give it back.
        updateActionButton(session, "submit", disabled = FALSE)
      }
    })
  })
}

#' The sign-up form
fw_newsletter_form <- function(ns) {
  t <- function(key) fw_t("newsletter", key)
  div(
    class = "fw-modal-form",
    # Where the focusout listener reports a field the reader has left.
    `data-fw-touch` = ns("touched"),
    fw_field(fw_form_text(ns("name"), autocomplete = "name", required = TRUE), t("name"),
             required = TRUE, input_id = ns("name")),
    fw_field(fw_form_text(ns("email"), type = "email", autocomplete = "email",
                          required = TRUE),
             t("email"), required = TRUE, input_id = ns("email")),
    fw_field(fw_form_text(ns("organisation"), autocomplete = "organization"),
             t("organisation"), input_id = ns("organisation")),
    div(
      class = "fw-field fw-modal-form__consent",
      checkboxInput(ns("consent"), tagList(
        t("consent"),
        tags$span(class = "fw-required-mark", `aria-hidden` = "true", "*"),
        tags$span(class = "fw-visually-hidden", fw_t("a11y", "required"))
      ), value = FALSE),
      fw_form_data_line(t("data_line"), "newsletter")
    ),
    fw_honeypot(ns("website")),
    p(class = "fw-modal-form__required-note", fw_t("forms", "required_note")),
    div(
      class = "fw-modal-form__actions",
      # Disabled until the form is valid. data-fw-busy disables it again in
      # the browser the instant it is pressed, so a double press cannot send
      # twice while the row is being written.
      actionButton(ns("submit"), t("submit"), class = "btn btn-primary",
                   disabled = TRUE, `data-fw-busy` = "true")
    ),
    uiOutput(ns("status"))
  )
}
