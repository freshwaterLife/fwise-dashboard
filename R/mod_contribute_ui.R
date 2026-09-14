# mod_contribute_ui.R
# The three states the contribute page can be in: the opening panel, the wizard
# shell, and the confirmation. Pure UI builders with no reactive context, kept
# out of mod_contribute.R so that file holds only server logic.

# ---- Stage UI ----------------------------------------------------------------

fw_intro_panel <- function(ns) {
  block <- function(heading, body) {
    tagList(tags$h3(heading), p(body))
  }
  tagList(
  # PINNED ABOVE EVERYTHING ELSE. What counts as an eradication, and that FWISE
  # is about animals rather than plants, decide whether someone should be
  # filling this form in at all - so they are the first thing on the page,
  # not the fourth panel down where the old layout had them.
  fw_preamble(),

  div(
    class = "fw-panel fw-prose",
    tags$h2(fw_t("contribute", "intro", "heading")),
    block(fw_t("contribute", "intro", "what_heading"), fw_t("contribute", "intro", "what")),
    block(fw_t("contribute", "intro", "review_heading"), fw_t("contribute", "intro", "review")),
    block(fw_t("contribute", "intro", "no_save_heading"), fw_t("contribute", "intro", "no_save")),
    block(fw_t("contribute", "intro", "time_heading"), fw_t("contribute", "intro", "time")),

    div(
      style = "margin-block: 1.5rem;",
      downloadButton(ns("download_questions"),
                     fw_t("contribute", "intro", "download_label"),
                     class = "btn btn-outline-primary"),
      downloadButton(ns("download_questions_txt"),
                     fw_t("contribute", "intro", "download_label_txt"),
                     class = "btn btn-outline-primary btn-sm"),
      div(class = "fw-caption", style = "margin-block-start:.4rem;",
          fw_t("contribute", "intro", "download_hint"))
    ),

    tags$hr(),

    tags$h3(fw_t("contribute", "consent", "heading")),
    p(fw_t("contribute", "consent", "statement")),
    checkboxInput(ns("consent_data_use"),
                  fw_t("contribute", "consent", "agree_label"), value = FALSE),
    p(tags$a(href = fw_t("contribute", "consent", "terms_url"),
             fw_t("contribute", "consent", "terms_link_label"))),
    checkboxInput(ns("email_private"),
                  fw_t("contribute", "consent", "email_private_label"), value = FALSE),
    div(class = "fw-field__help", fw_t("contribute", "consent", "email_private_help")),

    div(
      style = "margin-block-start:1.5rem;",
      actionButton(ns("start"), fw_t("contribute", "intro", "start_action"),
                   class = "btn btn-primary")
    )
  )
  )
}

#' What counts as an eradication, and what belongs in FWISE
#'
#' One definition, one place, at the top of the contribute page.
#'
#' IT WAS ON THE ABOUT PAGE TOO, on the reasoning that a contributor and a
#' reader have to be working from the same definition for the database to mean
#' anything. The client removed it from there: on About it opened the page with
#' two headings of scope rules before a reader had been told what they were
#' reading about, and the audience that has to APPLY the definition is the one
#' filling in this form. The helper is still shared rather than inlined, so if
#' the definition is ever wanted in a second place there is one copy of it.
fw_preamble <- function() {
  div(
    class = "fw-preamble",
    tags$h2(fw_t("contribute", "preamble", "heading")),
    p(class = "fw-lead",
      tags$strong(fw_t("contribute", "preamble", "definition")),
      " - ",
      tags$em(fw_t("contribute", "preamble", "citation"))),
    tags$h2(fw_t("contribute", "preamble", "scope_heading")),
    tags$ul(lapply(fw_t("contribute", "preamble", "scope"),
                   function(point) tags$li(fw_emphasis(point))))
  )
}

#' The whole form, on one page
#'
#' WHY IT IS NOT A WIZARD ANY MORE. The stepped version rendered the current step
#' through renderUI, which meant every Back and every Next destroyed and rebuilt
#' the inputs. Values survived only because Shiny remembered them, the repeatable
#' rows did not survive at all, and the server-side species selectize was being
#' populated against elements that were not in the DOM yet. Rendering every
#' section ONCE and never again removes that whole class of bug, and it is the
#' faster option too: after this function runs there is no further UI rendering
#' on the form at all.
#'
#' The progress rail is driven entirely in the browser - see fw_form_script() -
#' so scrolling and typing never touch the server.
fw_form_shell <- function(ns, choices) {
  sections <- Filter(function(s) !is.null(FW_STEP_UI[[s$id]]), FW_STEPS)

  body <- lapply(seq_along(sections), function(i) {
    sec <- sections[[i]]
    title <- fw_t("contribute", "steps", sec$title_key)
    inner <- tags$section(
      class = "fw-form-section",
      id = ns(paste0("section_", sec$id)),
      `data-fw-section` = title,
      tags$h2(
        class = "fw-form-section__title",
        tags$span(class = "fw-form-section__num fw-num", i),
        title
      ),
      FW_STEP_UI[[sec$id]](ns, choices)
    )
    # The chemical section is only relevant once a chemical method has been
    # named. It stays in the page and is shown or hidden, rather than being
    # rendered in and out, so its answers are never thrown away by a change of
    # mind further up.
    if (isTRUE(sec$conditional) && identical(sec$id, "chemical")) {
      conditionalPanel(
        condition = "output.chemical_needed === true",
        ns = ns,
        inner
      )
    } else {
      inner
    }
  })

  tagList(
    div(
      class = "fw-progress-rail",
      div(
        class = "fw-progress-rail__inner",
        tags$span(class = "fw-progress-rail__section", id = ns("rail_section"),
                  `aria-live` = "off", fw_t("contribute", "steps",
                                            sections[[1]]$title_key)),
        div(
          class = "fw-progress-rail__bar",
          role = "progressbar",
          `aria-valuemin` = 0, `aria-valuemax` = 100, `aria-valuenow` = 0,
          `aria-label` = fw_t("contribute", "announce", "form_progress"),
          id = ns("rail_bar"),
          div(class = "fw-progress-rail__fill", id = ns("rail_fill"))
        ),
        tags$span(class = "fw-progress-rail__pct", id = ns("rail_pct"),
                  role = "status")
      )
    ),
    div(class = "fw-form", id = ns("form"), body),
    div(
      class = "fw-form-footer",
      div(
        class = "fw-form-footer__inner",
        tags$span(class = "fw-form-footer__note",
                  fw_t("contribute", "send_note")),
        # ALWAYS ENABLED. A disabled Send on a form this long tells a
        # contributor they have got something wrong without telling them what or
        # where; pressing it and being taken to the field that needs attention
        # is the more useful answer. It also keeps the button out of the
        # reactive path, so nothing re-renders as they type.
        actionButton(ns("send"), fw_t("contribute", "send_action"),
                     class = "btn btn-primary")
      )
    )
  )
}

#' Progress and scroll tracking, in the browser
#'
#' Both jobs are done client-side on purpose. Recomputing a percentage on the
#' server on every keystroke is the difference between a form that feels instant
#' and one that stutters on a long page, and neither number is worth a round
#' trip: the bar is a hint, not a value anything depends on.
fw_form_script <- function(ns) {
  # The element ids travel as a JSON object in their own tag. The behaviour
  # below is then a plain string: it used to be built with sprintf(), where a
  # single unescaped per-cent sign anywhere in the JavaScript was read as a
  # format conversion and took the whole page down with "too few arguments".
  tagList(
    tags$script(HTML(paste0(
      "window.fwFormIds = ",
      jsonlite::toJSON(list(
        form    = ns("form"),
        fill    = ns("rail_fill"),
        pct     = ns("rail_pct"),
        bar     = ns("rail_bar"),
        section = ns("rail_section")
      ), auto_unbox = TRUE),
      ";"
    ))),
    tags$script(HTML("
    (function () {
      var ids = window.fwFormIds || {};
      var FORM = ids.form, FILL = ids.fill, PCT = ids.pct,
          BAR = ids.bar, SECT = ids.section;

      function isVisible(el) {
        return !!(el.offsetParent || el.offsetHeight);
      }

      // One control counts as answered when it holds something. Radios in a
      // group share a name and count once, answered if any is checked.
      function isFilled(el) {
        if (el.type === 'checkbox' || el.type === 'radio') return el.checked;
        if (el.multiple) return el.selectedOptions && el.selectedOptions.length > 0;
        return el.value != null && String(el.value).trim() !== '';
      }

      function controls(scope) {
        var out = [];
        scope.querySelectorAll('input, select, textarea').forEach(function (el) {
          if (el.disabled || el.type === 'hidden' || el.type === 'button' ||
              el.type === 'submit') return;
          // Selectize builds its own inputs beside the original <select>. The
          // original keeps the value, so only it is counted.
          if (el.closest('.selectize-control')) return;
          out.push(el);
        });
        return out;
      }

      // THE COUNT IS OF REQUIRED FIELDS, NOT OF ALL OF THEM. Most of this form
      // is optional by design, so a complete and perfectly valid submission
      // touches well under a fifth of the inputs. A bar reading 17% at the
      // point someone is ready to send tells them they have failed at
      // something, which is the opposite of what it is for.
      function requiredState(form) {
        var total = 0, done = 0;
        form.querySelectorAll('.fw-field').forEach(function (field) {
          var label = field.querySelector(':scope > .fw-field__label-row');
          if (!label || !label.querySelector('.fw-required-mark')) return;
          if (!isVisible(field)) return;
          var els = controls(field);
          if (!els.length) return;
          total += 1;
          // Every control in the field, so the map block only counts as
          // answered once both a latitude and a longitude are present.
          var seen = {}, ok = true;
          els.forEach(function (el) {
            var key = el.name || el.id;
            if (seen[key] === undefined) seen[key] = false;
            if (isFilled(el)) seen[key] = true;
          });
          Object.keys(seen).forEach(function (k) { if (!seen[k]) ok = false; });
          if (ok) done += 1;
        });
        return { total: total, done: done };
      }

      // The bar tracks how far through the form you have scrolled. That is the
      // question a bar on a long page actually answers.
      function scrolled(form) {
        var rect = form.getBoundingClientRect();
        var top = rect.top + window.scrollY;
        var travel = form.offsetHeight - window.innerHeight;
        if (travel <= 0) return 100;
        var pct = 100 * (window.scrollY - top + 160) / travel;
        return Math.max(0, Math.min(100, Math.round(pct)));
      }

      function refresh() {
        var form = document.getElementById(FORM);
        if (!form) return;

        var pct = scrolled(form);
        var fill = document.getElementById(FILL);
        var bar = document.getElementById(BAR);
        if (fill) fill.style.width = pct + '%';
        if (bar) bar.setAttribute('aria-valuenow', pct);

        var st = requiredState(form);
        var label = document.getElementById(PCT);
        if (!label) return;
        var text = st.total === 0 ? ''
          : (st.done >= st.total ? 'All required answered'
                                 : st.done + ' of ' + st.total + ' required');
        if (label.textContent !== text) label.textContent = text;
      }

      function spy() {
        var sects = document.querySelectorAll('.fw-form-section');
        if (!sects.length) return;
        var label = document.getElementById(SECT);
        if (!label) return;
        var best = null;
        sects.forEach(function (s) {
          // A hidden conditional section reports a zero rect, which would
          // otherwise read as 'top of the page' and win every comparison.
          if (!s.offsetParent && s.offsetHeight === 0) return;
          if (best === null) best = s;
          if (s.getBoundingClientRect().top <= 160) best = s;
        });
        if (best === null) return;
        var name = best.getAttribute('data-fw-section');
        if (name && label.textContent !== name) label.textContent = name;
      }

      var queued = false;
      function schedule() {
        if (queued) return;
        queued = true;
        window.requestAnimationFrame(function () { queued = false; refresh(); spy(); });
      }

      document.addEventListener('input', schedule, true);
      document.addEventListener('change', schedule, true);
      // Scroll moves the bar as well as the section name, so it runs the
      // same refresh - still one rAF per frame at most.
      window.addEventListener('scroll', schedule, { passive: true });
      window.addEventListener('resize', schedule);
      $(document).on('shiny:value shiny:inputchanged shiny:visualchange', schedule);
      $(document).on('shiny:idle', schedule);

      // Sending an incomplete form leaves the first problem somewhere off
      // screen. shinyvalidate marks the CONTROL with .is-invalid - the
      // Bootstrap 5 name, not the .has-error on the container that older
      // versions used - so the first of those is where the contributor needs to
      // be. The timeout lets the messages render before we look for them.
      // Jump to one named field, from the 'Check my answers' list.
      Shiny.addCustomMessageHandler('fw-scroll-to-field', function (id) {
        var el = document.getElementById(id);
        if (!el) return;
        var field = el.closest('.fw-field') || el;
        field.scrollIntoView({ behavior: 'smooth', block: 'center' });
        var focusable = field.querySelector('input:not([type=hidden]), select, textarea');
        if (focusable) focusable.focus({ preventScroll: true });
      });

      Shiny.addCustomMessageHandler('fw-scroll-to-error', function (formId) {
        var form = document.getElementById(formId);
        if (!form) return;
        window.setTimeout(function () {
          var bad = form.querySelector('.is-invalid');
          if (!bad) return;
          var field = bad.closest('.fw-field') || bad;
          field.scrollIntoView({ behavior: 'smooth', block: 'center' });
          var focusable = field.querySelector('input:not([type=hidden]), select, textarea');
          if (focusable) focusable.focus({ preventScroll: true });
        }, 150);
      });
    })();
"))
  )
}

fw_confirmation <- function(ns, res) {
  if (is.null(res)) return(NULL)

  body <- fw_fill(fw_t("contribute", "confirm", "body_template"),
                  nth = fw_ordinal(res$total_attempts),
                  nth_country = fw_ordinal(res$country_attempts),
                  country = res$country)

  div(
    class = "fw-confirm",
    fw_confirm_mark(),
    tags$h2(class = "fw-confirm__heading", fw_t("contribute", "confirm", "heading")),
    p(class = "fw-confirm__body", body),
    p(class = "fw-confirm__body", fw_t("contribute", "confirm", "followup")),
    p(class = "fw-confirm__body", fw_t("contribute", "confirm", "thanks")),
    p(class = "fw-caption",
      fw_t("contribute", "announce", "reference_prefix"),
      tags$span(class = "fw-num", res$submission_id)),
    div(
      class = "fw-confirm__actions",
      actionButton(ns("another"), fw_t("contribute", "confirm", "another_action"),
                   class = "btn btn-outline-primary"),
      actionButton(ns("to_explore"), fw_t("contribute", "confirm", "explore_action"),
                   class = "btn btn-primary")
    )
  )
}

#' The mark above the confirmation
#'
#' [PLACEHOLDER] A frog, standing in until the client picks the real thing -
#' likely an animated GIF. To swap it, replace the span below with an
#' tags$img(src = "img/whatever.gif") and keep the class and the aria-label:
#' the sizing, the centring and the reduced-motion rule all hang off
#' .fw-confirm__mark, and the label is what a screen reader announces in place
#' of an image that says nothing on its own.
fw_confirm_mark <- function() {
  tags$span(
    class = "fw-confirm__mark",
    role = "img", `aria-label` = fw_t("contribute", "announce", "submission_received"),
    "\U0001F438"
  )
}
