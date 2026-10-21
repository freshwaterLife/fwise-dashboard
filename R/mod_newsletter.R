# mod_newsletter.R
# The "Get FWISE updates" dialog. One instance for the app (started in app.R),
# opened by fw_form_open("newsletter", ...). One sign-up per visit; after that
# the buttons open the thank-you. Rules in forms.R, storage in forms_store.R.

#' @param open a reactive holding input$fw_open_form: list(form, source)
mod_newsletter_server = function(id, open) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    done   = reactiveVal(FALSE)
    opened = reactiveVal(0L)
    entry  = reactiveVal("unknown")
    status = reactiveVal(NULL)

    vals = reactive(fw_newsletter_values(input$name, input$email,
                                          input$organisation, input$consent))
    problems = reactive(fw_newsletter_problems(vals()))

    # One validator per field, so each shows its message only once left.
    fields = c("name", "email", "organisation", "consent")
    validators = lapply(stats::setNames(fields, fields), function(f) {
      iv = shinyvalidate::InputValidator$new()
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
      f = sub(ns(""), "", input$touched, fixed = TRUE)
      if (f %in% fields) validators[[f]]$enable()
    })

    # A fresh form on every open; a failed send keeps what was typed.
    output$body = renderUI({
      opened()
      if (done()) return(fw_form_success(fw_t("newsletter", "success")))
      isolate(fw_newsletter_form(ns))
    })
    output$status = renderUI(fw_form_status(status()))

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

      # The button is disabled while invalid, so show every message and stop.
      if (length(problems()) > 0) {
        for (iv in validators) iv$enable()
        updateActionButton(session, "submit", disabled = TRUE)
        return()
      }

      res = store_newsletter_signup(fw_newsletter_record(vals(), entry()))
      if (isTRUE(res$success)) {
        status(NULL)
        done(TRUE)
        fw_track(session, "form_submit", list(form = "newsletter"))
      } else {
        status(fw_t("newsletter", "failure"))
        # The click disabled the button in the browser; give it back.
        updateActionButton(session, "submit", disabled = FALSE)
      }
    })
  })
}

#' The sign-up form
fw_newsletter_form = function(ns) {
  t = function(key) fw_t("newsletter", key)
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
      # Disabled until valid; data-fw-busy blocks a double press.
      actionButton(ns("submit"), t("submit"), class = "btn btn-primary",
                   disabled = TRUE, `data-fw-busy` = "true")
    ),
    uiOutput(ns("status"))
  )
}
