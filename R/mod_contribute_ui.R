# mod_contribute_ui.R
# The Add a record page's three states: opening panel, form, confirmation.
# UI only.

# ---- Stage UI ----------------------------------------------------------------

fw_intro_panel = function(ns) {
  block = function(heading, body) {
    tagList(tags$h3(heading), p(body))
  }
  tagList(
  # The definition and scope come first: they decide whether to fill this in.
  fw_preamble(),

  div(
    class = "fw-panel fw-prose",
    tags$h2(class = "fw-visually-hidden", fw_t("contribute", "intro", "heading")),
    block(fw_t("contribute", "intro", "what_heading"), fw_t("contribute", "intro", "what")),
    block(fw_t("contribute", "intro", "review_heading"), fw_t("contribute", "intro", "review")),
    block(fw_t("contribute", "intro", "no_save_heading"), fw_t("contribute", "intro", "no_save")),
    block(fw_t("contribute", "intro", "time_heading"), fw_t("contribute", "intro", "time")),

    # Two equal buttons (client). See .fw-contribute__downloads.
    div(
      class = "fw-contribute__downloads",
      div(
        class = "fw-contribute__download-buttons",
        downloadButton(ns("download_questions"),
                       fw_t("contribute", "intro", "download_label"),
                       class = "btn btn-outline-primary"),
        downloadButton(ns("download_questions_txt"),
                       fw_t("contribute", "intro", "download_label_txt"),
                       class = "btn btn-outline-primary")
      ),
      div(class = "fw-caption", fw_t("contribute", "intro", "download_hint"))
    ),

    # Consent is in the form's first section (fw_step_contributor_ui()).
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
#' At the top of the Add a record page only.
fw_preamble = function() {
  div(
    class = "fw-preamble",
    tags$h2(class = "fw-visually-hidden", fw_t("contribute", "preamble", "heading")),
    # Bold comes from ** in the copy. .noWS on the brackets, or the tagList's
    # newlines render "( Genovesi 2000 )".
    p(class = "fw-lead",
      fw_emphasis(fw_t("contribute", "preamble", "definition")),
      span(.noWS = "outside", " ("),
      tags$a(
        href = fw_t("contribute", "preamble", "cite_url"),
        target = "_blank", rel = "noopener noreferrer", .noWS = "outside",
        fw_t("contribute", "preamble", "cite_short")
      ),
      span(.noWS = "outside", ").")),
    # The full reference for the citation above.
    p(class = "fw-preamble__ref",
      tags$em(fw_t("contribute", "preamble", "citation"))),
    tags$h2(class = "fw-visually-hidden", fw_t("contribute", "preamble", "scope_heading")),
    tags$ul(lapply(fw_t("contribute", "preamble", "scope"),
                   function(point) tags$li(fw_emphasis(point))))
  )
}

#' The whole form, on one page
#'
#' Every section rendered once; no further UI rendering. The progress rail
#' runs in the browser (fw_form_script()).
fw_form_shell = function(ns, choices) {
  sections = Filter(function(s) !is.null(FW_STEP_UI[[s$id]]), FW_STEPS)

  body = lapply(seq_along(sections), function(i) {
    sec = sections[[i]]
    title = fw_t("contribute", "steps", sec$title_key)
    inner = tags$section(
      class = "fw-form-section",
      id = ns(paste0("section_", sec$id)),
      `data-fw-section` = title,
      tags$h2(
        class = "fw-form-section__title fw-visually-hidden",
        tags$span(class = "fw-form-section__num fw-num", i),
        title
      ),
      FW_STEP_UI[[sec$id]](ns, choices)
    )
    # Chemical section: hidden, not removed, so answers survive.
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
        # Always enabled: pressing it takes the contributor to the problem.
        actionButton(ns("send"), fw_t("contribute", "send_action"),
                     class = "btn btn-primary")
      )
    )
  )
}

#' Progress and scroll tracking, in the browser
#'
#' Client-side, so typing never round-trips.
fw_form_script = function(ns) {
  # Ids go in a JSON tag so the script is a plain string (no sprintf(): a
  # stray % broke it).
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

      // A control is answered when it holds something; a radio group counts
      // once.
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
          // Count the original <select>, not selectize's own inputs.
          if (el.closest('.selectize-control')) return;
          out.push(el);
        });
        return out;
      }

      // Count required fields only: most of the form is optional.
      function requiredState(form) {
        var total = 0, done = 0;
        form.querySelectorAll('.fw-field').forEach(function (field) {
          var label = field.querySelector(':scope > .fw-field__label-row');
          if (!label || !label.querySelector('.fw-required-mark')) return;
          if (!isVisible(field)) return;
          var els = controls(field);
          if (!els.length) return;
          total += 1;
          // Every control, so location needs both coordinates.
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

      // The bar tracks scroll position.
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
          // Skip hidden sections (zero rect).
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
      // One rAF per frame at most.
      window.addEventListener('scroll', schedule, { passive: true });
      window.addEventListener('resize', schedule);
      $(document).on('shiny:value shiny:inputchanged shiny:visualchange', schedule);
      $(document).on('shiny:idle', schedule);

      // Scroll to the first .is-invalid control after a refused send (the
      // timeout lets messages render).
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

fw_confirmation = function(ns, res) {
  if (is.null(res)) return(NULL)

  body = fw_fill(fw_t("contribute", "confirm", "body_template"),
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

#' The frog above the confirmation (styled by .fw-confirm__mark)
fw_confirm_mark = function() {
  tags$span(
    class = "fw-confirm__mark",
    role = "img", `aria-label` = fw_t("contribute", "announce", "submission_received"),
    "\U0001F438"
  )
}
