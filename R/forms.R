# forms.R
# Shared by the newsletter and feedback forms (mod_newsletter.R,
# mod_feedback.R): rules, records, honeypot and open buttons. Storage is in
# forms_store.R.
#
# fw_*_problems() return problems per field. Both the field messages and the
# submit gate read them, so they can't disagree.

# ---- Small shared pieces -----------------------------------------------------

#' Is this an email address?
#'
#' Deliberately loose (x@y.z). Gotcha: use [:space:], not \\s, inside a
#' bracket: R's default engine reads [^@\\s] as excluding the letter s.
fw_is_email = function(x) {
  grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", x %||% "")
}

#' The version of the privacy and data terms in force, for a stored row
fw_terms_version = function() fw_t("privacy", "version")

#' One value from a form, as a trimmed string ("" for nothing)
fw_form_value = function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  trimws(as.character(x[1]))
}

#' The address of one section of the privacy page
#'
#' A ?page=privacy&section= URL, so it works from anywhere. See app.R.
fw_privacy_href = function(section = NULL) {
  if (is.null(section)) return("?page=privacy")
  stopifnot(section %in% FW_PRIVACY_SECTIONS)
  paste0("?page=privacy&section=", section)
}

#' A link that opens in a new tab and says so
#'
#' So a visitor can read the terms without losing their form.
fw_new_tab_link = function(href, label) {
  tags$a(href = href, target = "_blank", rel = "noopener", label,
         tags$span(class = "fw-visually-hidden", fw_t("forms", "new_tab")))
}

#' The line under a form's personal fields: what it is for, and the terms link
fw_form_data_line = function(text, section) {
  p(class = "fw-modal-form__data-line", text, " ",
    fw_new_tab_link(fw_privacy_href(section), fw_t("forms", "data_link")))
}

#' A text input whose own label fw_field() supplies, with browser hints
#'
#' @param type "text" or "email". An email input brings up the @ keyboard on a
#'   phone; Shiny binds both the same way.
#' @param autocomplete the browser autofill token: "name", "email",
#'   "organization", or "off"
#' @param required sets aria-required, not HTML `required` (shinyvalidate owns
#'   the messages)
fw_form_text = function(id, type = "text", autocomplete = NULL, required = FALSE) {
  tq = htmltools::tagQuery(textInput(id, label = NULL, width = "100%"))
  # Remove then add: addAttrs() appends, and type="text email" breaks Shiny's
  # binding.
  tq$find("input")$removeAttrs("type")$addAttrs(
    type = type, autocomplete = autocomplete,
    `aria-required` = if (required) "true")
  tq$allTags()
}

#' The honeypot: a field no person sees and a form-filling bot does
#'
#' Off-screen rather than display:none (bots skip those), out of the tab
#' order, aria-hidden and autocomplete off. If filled, the bot sees success and
#' nothing is written.
fw_honeypot = function(id) {
  tq = htmltools::tagQuery(textInput(id, label = fw_t("forms", "honeypot")))
  tq$find("input")$addAttrs(tabindex = "-1", autocomplete = "off")
  div(class = "fw-hp", `aria-hidden` = "true", tq$allTags())
}

#' A button that opens one of the forms, from anywhere on the page
#'
#' Not an actionButton: a delegated listener sets input$fw_open_form =
#' {form, source} from anywhere, including the footer. No server code needed.
#'
#' @param form   "newsletter" or "feedback"
#' @param source where the button is, e.g. "footer" or "about". Stored with a
#'   sign-up as entry_point, so the client can see which spot works.
#' @param class  the button's look: "btn btn-primary", or "fw-link-button" for
#'   one that reads as a link
fw_form_open = function(form, label, source, class = "btn btn-primary") {
  stopifnot(form %in% c("newsletter", "feedback"))
  tags$button(type = "button", class = class, `data-fw-open` = form,
              `data-fw-source` = source, `aria-haspopup` = "dialog", label)
}

#' Where a sign-up came from, as stored: letters, digits, - and _ only
#'
#' From the browser, so cleaned before it reaches the sheet.
fw_entry_point = function(x) {
  x = gsub("[^a-z0-9_-]", "", tolower(fw_form_value(x)))
  if (nzchar(x)) substr(x, 1, 30) else "unknown"
}

# ---- Newsletter --------------------------------------------------------------

#' The newsletter form's values, cleaned: trimmed, the email lowercased
fw_newsletter_values = function(name, email, organisation, consent) {
  list(name = fw_form_value(name),
       email = tolower(fw_form_value(email)),
       organisation = fw_form_value(organisation),
       consent = isTRUE(consent))
}

#' What is wrong with a newsletter sign-up, field by field
#'
#' @param v from fw_newsletter_values()
#' @return a named list of messages, one per field that has a problem; empty
#'   when the sign-up can be sent
fw_newsletter_problems = function(v) {
  lim = FW_FORM_LIMITS
  msg = function(key, n = NULL) {
    out = fw_t("newsletter", "validate", key)
    if (is.null(n)) out else fw_fill(out, n = n)
  }
  out = list()
  if (!nzchar(v$name)) out$name = msg("name_required")
  else if (nchar(v$name) > lim$name) out$name = msg("name_long", lim$name)

  if (nchar(v$email) > lim$email) out$email = msg("email_long", lim$email)
  else if (!fw_is_email(v$email)) out$email = msg("email")

  if (nchar(v$organisation) > lim$organisation) {
    out$organisation = msg("org_long", lim$organisation)
  }
  if (!isTRUE(v$consent)) out$consent = msg("consent")
  out
}

#' One sign-up, as stored
#'
#' consent_wording stores the exact sentence ticked.
fw_newsletter_record = function(v, entry_point, now = Sys.time()) {
  list(
    submitted_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    name = v$name,
    email = v$email,
    organisation = v$organisation,
    consent = "yes",
    consent_wording = fw_t("newsletter", "consent"),
    terms_version = fw_terms_version(),
    entry_point = fw_entry_point(entry_point)
  )
}

# ---- Feedback ----------------------------------------------------------------

#' The feedback form's page choices: label = value
#'
#' Navbar pages (navbar values, so input$fw_nav can pre-select), then General
#' and Other.
fw_feedback_pages = function() {
  pages = c("home", "explore", "plan", "contribute", "networking", "about")
  labels = vapply(pages, function(p) fw_t("nav", p), character(1))
  c(stats::setNames(pages, labels),
    stats::setNames(c("general", "other"),
                    c(fw_t("feedback", "page_general"), fw_t("feedback", "page_other"))))
}

#' Which page to pre-select: the one the reader is on, or General
fw_feedback_default_page = function(nav) {
  nav = fw_form_value(nav)
  if (nav %in% fw_feedback_pages()) nav else "general"
}

fw_feedback_values = function(page, message, email) {
  list(page = fw_form_value(page),
       message = fw_form_value(message),
       email = tolower(fw_form_value(email)))
}

#' What is wrong with a piece of feedback, field by field. Email is optional.
#'
#' The page is checked against the list: browser values can be anything.
fw_feedback_problems = function(v) {
  lim = FW_FORM_LIMITS
  msg = function(key, n = NULL) {
    out = fw_t("feedback", "validate", key)
    if (is.null(n)) out else fw_fill(out, n = n)
  }
  out = list()
  if (!v$page %in% fw_feedback_pages()) out$page = msg("page")
  if (!nzchar(v$message)) out$message = msg("issue_required")
  else if (nchar(v$message) > lim$message) out$message = msg("issue_long", lim$message)
  if (nzchar(v$email)) {
    if (nchar(v$email) > lim$email) out$email = msg("email_long", lim$email)
    else if (!fw_is_email(v$email)) out$email = msg("email")
  }
  out
}

#' One piece of feedback, as stored
fw_feedback_record = function(v, now = Sys.time()) {
  list(
    submitted_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    page = v$page,
    message = v$message,
    email = v$email,
    terms_version = fw_terms_version()
  )
}

# ---- The modal frame ---------------------------------------------------------

#' The dialog both forms open in
#'
#' One uiOutput body: the form, or the thank-you in its place.
fw_form_modal = function(title, body) {
  modalDialog(
    body,
    title = title,
    footer = modalButton(fw_t("forms", "close")),
    easyClose = TRUE,
    size = "m",
    class = "fw-form-modal"
  )
}

#' The success state: a message announced on arrival, and optional actions
fw_form_success = function(text, ...) {
  div(class = "fw-modal-form__done", role = "status",
      p(class = "fw-modal-form__done-text", text), ...)
}

#' The status slot under a form's button: failure and cooldown messages
fw_form_status = function(text) {
  if (is.null(text)) return(NULL)
  div(class = "fw-modal-form__status", role = "alert", text)
}

# ---- The browser side --------------------------------------------------------

#' Delegated listeners for both forms, and for every button that opens one
#'
#' Delegated from the document, so they work for forms drawn later.
fw_forms_script = function() {
  tags$script(HTML("
    $(function () {
      // fw_form_open() buttons: send the form and source (the time makes each
      // press a new value).
      document.addEventListener('click', function (e) {
        if (!e.target.closest) return;
        var btn = e.target.closest('[data-fw-open]');
        if (btn) {
          Shiny.setInputValue('fw_open_form', {
            form: btn.getAttribute('data-fw-open'),
            source: btn.getAttribute('data-fw-source') || '',
            at: Date.now()
          }, { priority: 'event' });
          return;
        }
        // Disable submit on press (Shiny has already sent it); the server
        // re-enables it if the write fails.
        var busy = e.target.closest('button[data-fw-busy]');
        if (busy && !busy.disabled) busy.disabled = true;
      });

      // A field shows its message once left.
      document.addEventListener('focusout', function (e) {
        var t = e.target;
        if (!t.closest || !t.id || t.closest('.fw-hp')) return;
        var form = t.closest('.fw-modal-form[data-fw-touch]');
        if (!form) return;
        Shiny.setInputValue(form.getAttribute('data-fw-touch'), t.id, { priority: 'event' });
      });

      // The feedback box's character count.
      document.addEventListener('input', function (e) {
        var t = e.target;
        if (!t.matches || !t.matches('textarea[data-fw-counter]')) return;
        var out = document.getElementById(t.getAttribute('data-fw-counter'));
        if (!out) return;
        var fmt = function (n) { return Number(n).toLocaleString('en-GB'); };
        out.textContent = out.getAttribute('data-template')
          .replace('{n}', fmt(t.value.length))
          .replace('{max}', fmt(out.getAttribute('data-max')));
      });

      // Tie shinyvalidate messages to their controls (aria-invalid,
      // aria-describedby) so screen readers hear them.
      var sync = function () {
        document.querySelectorAll('.fw-modal-form .shiny-input-container').forEach(function (box) {
          var ctl = box.querySelector('input:not([type=hidden]), textarea, select');
          if (!ctl || !ctl.id) return;
          var msg = box.querySelector(':scope > .shiny-validation-message');
          var errId = ctl.id + '-error';
          var ids = (ctl.getAttribute('aria-describedby') || '').split(' ')
            .filter(function (x) { return x && x !== errId; });
          if (msg) {
            msg.id = errId;
            ids.push(errId);
            ctl.setAttribute('aria-invalid', 'true');
          } else {
            ctl.removeAttribute('aria-invalid');
          }
          if (ids.length) ctl.setAttribute('aria-describedby', ids.join(' '));
          else ctl.removeAttribute('aria-describedby');
        });
      };
      new MutationObserver(function (records) {
        for (var i = 0; i < records.length; i++) {
          var nodes = [].concat([].slice.call(records[i].addedNodes),
                                [].slice.call(records[i].removedNodes));
          for (var j = 0; j < nodes.length; j++) {
            if (nodes[j].classList && nodes[j].classList.contains('shiny-validation-message')) {
              sync();
              return;
            }
          }
        }
      }).observe(document.body, { childList: true, subtree: true });
    });
  "))
}
