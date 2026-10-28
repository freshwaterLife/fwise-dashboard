# mod_contribute.R
# The Add a record form: server logic. One scrolling page, every section
# rendered once (re-rendering broke repeatable rows and selectize). Validation
# switches on at the first refused send, then shows live.
#
# Section builders are in mod_contribute_steps.R, page states in
# mod_contribute_ui.R.

library(shiny)
library(bslib)
library(shinyvalidate)
library(leaflet)

mod_contribute_ui = function(id) {
  ns = NS(id)
  tagList(
    fw_page_header(fw_t("contribute", "title"), fw_t("contribute", "description"),
                   show_title = FALSE),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          # Three states: opening panel, form, confirmation.
          uiOutput(ns("stage"))
        )
      )
    )
  )
}

#' @param on_submit called with no arguments after a submission is safely
#'   written; app.R uses it to refresh the in-review count at once.
mod_contribute_server = function(id, data, choices, on_submit = NULL) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    stage = reactiveVal("intro")     # intro | form | done
    result = reactiveVal(NULL)

    # Row indices on screen per repeatable block, so removing one doesn't
    # renumber the rest.
    rows = reactiveValues(
      target  = 1L,
      benefit = 1L,
      method  = 1L
    )
    next_index = reactiveValues(target = 2L, benefit = 2L, method = 2L)

    # ---- The conditional section ----------------------------------------------

    # Shown only when a chemical method is chosen. Hidden, not removed, so its
    # answers survive.
    chemical_selected = reactive({
      picked = vapply(rows$method, function(i) {
        input[[paste0("method_", i)]] %||% ""
      }, character(1))
      any(picked %in% choices$chemical_methods)
    })

    output$chemical_needed = reactive(chemical_selected())
    # Otherwise the hidden output is suspended and never unhides itself.
    outputOptions(output, "chemical_needed", suspendWhenHidden = FALSE)

    # ---- Validation -----------------------------------------------------------

    # One validator per section.
    iv = InputValidator$new()

    iv_site = InputValidator$new()
    iv_site$add_rule("site_name", sv_required(message = fw_t("contribute", "validate", "site_name")))
    iv_site$add_rule("country", sv_required(message = fw_t("contribute", "validate", "country")))
    iv_site$add_rule("latitude", sv_required(message = fw_t("contribute", "validate", "latitude")))
    iv_site$add_rule("latitude", sv_between(-90, 90, message = fw_t("contribute", "validate", "latitude_range")))
    iv_site$add_rule("longitude", sv_required(message = fw_t("contribute", "validate", "longitude")))
    iv_site$add_rule("longitude", sv_between(-180, 180, message = fw_t("contribute", "validate", "longitude_range")))

    iv_waterbody = InputValidator$new()
    iv_waterbody$add_rule("waterbody_type", sv_required(message = fw_t("contribute", "validate", "waterbody_type")))
    iv_waterbody$add_rule("water_regime", sv_required(message = fw_t("contribute", "validate", "water_regime")))

    iv_invasive = InputValidator$new()
    iv_invasive$add_rule("target_taxa_1", sv_required(message = fw_t("contribute", "validate", "target_taxa")))
    iv_invasive$add_rule("target_species_1", sv_required(message = fw_t("contribute", "validate", "target_species")))

    this_year = as.integer(format(Sys.Date(), "%Y"))
    iv_timeline = InputValidator$new()
    iv_timeline$add_rule("start_year", sv_required(message = fw_t("contribute", "validate", "start_year")))
    year_range_msg = fw_fill(fw_t("contribute", "validate", "year_range"),
                              min = FW_YEAR_MIN, year = this_year)
    iv_timeline$add_rule("start_year", sv_between(FW_YEAR_MIN, this_year,
      message = year_range_msg))
    iv_timeline$add_rule("invasion_year", sv_optional())
    iv_timeline$add_rule("invasion_year", sv_between(FW_YEAR_MIN, this_year,
      message = year_range_msg))
    iv_timeline$add_rule("end_year", sv_optional())
    iv_timeline$add_rule("end_year", function(value) {
      if (is.null(value) || is.na(value)) return(NULL)
      if (value < FW_YEAR_MIN || value > this_year + FW_YEAR_FUTURE) {
        return(fw_fill(fw_t("contribute", "validate", "end_year_range"),
                       min = FW_YEAR_MIN))
      }
      start = input$start_year
      if (!is.null(start) && !is.na(start) && value < start) {
        fw_t("contribute", "validate", "end_before_start")
      }
    })
    iv_timeline$add_rule("driver", sv_required(message = fw_t("contribute", "validate", "driver")))

    iv_methods = InputValidator$new()
    iv_methods$add_rule("method_1", sv_required(message = fw_t("contribute", "validate", "method")))
    # The currency becomes required only once a cost has been entered.
    iv_methods$add_rule("cost_currency", function(value) {
      cost = input$cost_estimate
      if (!is.null(cost) && !is.na(cost) && (is.null(value) || !nzchar(value))) {
        fw_t("contribute", "validate", "cost_currency")
      }
    })

    iv_outcome = InputValidator$new()
    iv_outcome$add_rule("outcome", sv_required(message = fw_t("contribute", "validate", "outcome")))

    iv_contributor = InputValidator$new()
    iv_contributor$add_rule("primary_contact_name", sv_required(message = fw_t("contribute", "validate", "contact_name")))
    iv_contributor$add_rule("primary_contact_email", sv_required(message = fw_t("contribute", "validate", "contact_email")))
    iv_contributor$add_rule("primary_contact_email", sv_email(message = fw_t("contribute", "validate", "email_format")))

    step_validators = list(
      site = iv_site, waterbody = iv_waterbody, invasive = iv_invasive,
      timeline = iv_timeline, methods = iv_methods, outcome = iv_outcome,
      contributor = iv_contributor
    )
    # Not enabled yet, so an untouched form isn't painted red.
    touched = reactiveVal(character(0))

    fw_touch_step = function(id) {
      v = step_validators[[id]]
      if (is.null(v)) return(invisible(NULL))
      if (!id %in% touched()) {
        touched(c(touched(), id))
        v$enable()
      }
      invisible(NULL)
    }

    # The consent controls gate the whole submission, not one step.
    iv_consent = InputValidator$new()
    iv_consent$add_rule("consent_data_use", function(value) {
      if (!isTRUE(value)) fw_t("contribute", "validate", "consent")
    })
    # Enabled on "Check my answers" or Send, not at startup.
    

    # Send gating, separate from the validators: is_valid() is TRUE for a
    # disabled validator. A required rule added above needs a line here too.

    filled = function(nm) {
      v = input[[nm]]
      !is.null(v) && length(v) > 0 && !all(is.na(v)) &&
        any(nzchar(as.character(v[!is.na(v)])))
    }
    in_range = function(nm, lo, hi) {
      v = suppressWarnings(as.numeric(input[[nm]]))
      length(v) == 1 && !is.na(v) && v >= lo && v <= hi
    }

    all_valid = reactive({
      isTRUE(input$consent_data_use) &&
        filled("site_name") && filled("country") &&
        in_range("latitude", -90, 90) && in_range("longitude", -180, 180) &&
        filled("waterbody_type") && filled("water_regime") &&
        filled("target_taxa_1") && filled("target_species_1") &&
        in_range("start_year", 1500, this_year) &&
        # An end year is optional, but must not precede the start year.
        (!filled("end_year") ||
           (in_range("end_year", 1500, this_year + 20) &&
              as.numeric(input$end_year) >= as.numeric(input$start_year))) &&
        filled("driver") && filled("method_1") && filled("outcome") &&
        # The currency is required only once a cost has been entered.
        (!filled("cost_estimate") || filled("cost_currency")) &&
        filled("primary_contact_name") &&
        fw_is_email(input$primary_contact_email)
    })

    # ---- "Check my answers" ---------------------------------------------------

    # What is wrong and where. Each hard error matches a clause of all_valid().
    # Hard blocks sending; soft never does (refusing incomplete records would
    # bias the database).
    fw_check = function() {
      msg = function(key, ...) {
        out = fw_t("contribute", "check", key)
        subs = list(...)
        for (nm in names(subs)) out = sub(paste0("{", nm, "}"), subs[[nm]], out, fixed = TRUE)
        out
      }
      errors = list(); notes = list()
      err  = function(id, m) errors <<- c(errors, list(list(id = id, msg = m)))
      note = function(id, m) notes  <<- c(notes,  list(list(id = id, msg = m)))

      # ---- Hard: the record is unusable without these ----
      if (!filled("site_name"))      err("site_name", msg("e_site_name"))
      if (!filled("country"))        err("country",   msg("e_country"))
      if (!in_range("latitude",  -90,  90))  err("latitude",  msg("e_latitude"))
      if (!in_range("longitude", -180, 180)) err("longitude", msg("e_longitude"))
      if (!filled("water_regime"))   err("water_regime",   msg("e_regime"))
      if (!filled("waterbody_type")) err("waterbody_type", msg("e_waterbody"))
      if (!filled("target_taxa_1"))    err("target_taxa_1",    msg("e_target_taxa"))
      if (!filled("target_species_1")) err("target_species_1", msg("e_target_sp"))

      if (!in_range("start_year", 1500, this_year))
        err("start_year", msg("e_start_year", year = this_year))
      if (filled("end_year")) {
        if (!in_range("end_year", 1500, this_year + 20)) {
          err("end_year", msg("e_end_year", max_year = this_year + 20))
        } else if (filled("start_year") &&
                   suppressWarnings(as.numeric(input$end_year)) <
                   suppressWarnings(as.numeric(input$start_year))) {
          err("end_year", msg("e_end_before"))
        }
      }
      if (!filled("driver"))   err("driver",   msg("e_driver"))
      if (!filled("method_1")) err("method_1", msg("e_method"))
      if (!filled("outcome"))  err("outcome",  msg("e_outcome"))
      if (filled("cost_estimate") && !filled("cost_currency"))
        err("cost_currency", msg("e_cost_currency"))

      if (!filled("primary_contact_name"))
        err("primary_contact_name", msg("e_contact_name"))
      if (!filled("primary_contact_email")) {
        err("primary_contact_email", msg("e_contact_mail"))
      } else if (!fw_is_email(input$primary_contact_email)) {
        err("primary_contact_email", msg("e_contact_bad"))
      }

      if (!isTRUE(input$consent_data_use)) err("consent_data_use", msg("e_consent"))

      # ---- Soft: worth having, never required ----
      if (!filled("end_year"))       note("end_year",      msg("w_end_year"))
      if (!filled("area_treated"))   note("area_treated",  msg("w_area"))
      if (!filled("invasion_year"))  note("invasion_year", msg("w_invasion"))
      if (!any(vapply(rows$benefit, function(i) filled(paste0("benefit_species_", i)),
                      logical(1))))
        note("benefit_species_1", msg("w_beneficiary"))
      if (!filled("reference"))      note("reference",     msg("w_reference"))
      if (!filled("reference_link")) note("reference_link", msg("w_reference_link"))
      if (!filled("verification"))   note("verification",  msg("w_verification"))
      if (!filled("duration_days"))  note("duration_days", msg("w_duration"))
      if (!filled("method_description"))
        note("method_description", msg("w_method_desc"))
      # Flagged, not refused: older attempts are possible.
      if (in_range("start_year", 1500, 1900))
        note("start_year", msg("w_old_year", year = as.integer(input$start_year)))

      list(errors = errors, notes = notes)
    }

    # ---- Stages ---------------------------------------------------------------

    output$stage = renderUI({
      switch(stage(),
        intro = fw_intro_panel(ns),
        form  = tagList(fw_form_shell(ns, choices), fw_form_script(ns)),
        done  = fw_confirmation(ns, result())
      )
    })

    # Not gated: consent is in the first section and gates Send.
    observeEvent(input$start, {
      stage("form")
      session$sendCustomMessage("fw-scroll-top", TRUE)
    })

    # ---- Repeatable blocks ----------------------------------------------------

    # Row 1 is rendered by the section builder; insertUI only adds rows to a
    # container already on the page.

    #' Narrow a row's species picker to the group that row names
    #'
    #' Both roles. Client-side list swap; a typed-in species is kept.
    bind_pair_row = function(i, kind) {
      taxa_id    = paste0(kind, "_taxa_", i)
      species_id = paste0(kind, "_species_", i)

      observeEvent(input[[taxa_id]], ignoreInit = TRUE, {
        taxa = input[[taxa_id]] %||% ""
        lst = choices$species_by_taxa[[taxa]]
        if (is.null(lst) || !length(lst)) lst = choices$species_by_taxa[[FW_ALL]]

        current = input[[species_id]] %||% ""
        if (nzchar(current) && !current %in% lst) lst = c(current, lst)

        # Keep the leading blank, or selectize auto-picks the first species.
        updateSelectizeInput(session, species_id, choices = c("", lst),
                             selected = current, server = FALSE)
      })
      invisible(NULL)
    }

    bind_pair_row(1L, "target")
    bind_pair_row(1L, "benefit")

    # The area unit follows the regime; a dash until one is chosen.
    output$area_unit_suffix = renderText({
      unit = fw_regime_unit(input$water_regime)
      if (nzchar(unit)) unit else "\u2014"
    })

    # Narrow waterbody types to the regime.
    observeEvent(input$water_regime, ignoreInit = TRUE, {
      rg = input$water_regime %||% ""
      lst = choices$waterbody_by_regime[[rg]]
      if (is.null(lst) || !length(lst)) lst = choices$waterbody_by_regime[[FW_ALL]]

      current = input$waterbody_type %||% ""
      keep = if (nzchar(current) && current %in% lst) current else ""
      if (nzchar(current) && !nzchar(keep)) {
        fw_announce(session, fw_fill(
          fw_t("contribute", "announce", "waterbody_changed"),
          regime = tolower(fw_regime_label(rg))))
      }
      updateSelectizeInput(
        session, "waterbody_type",
        choices = c(stats::setNames("", fw_t("contribute", "placeholder", "select")), lst),
        selected = keep, server = FALSE
      )
    })

    # Region narrows to the country's ISO subdivisions. create = TRUE, since
    # ISO doesn't name every catchment or district.
    observeEvent(input$country, ignoreInit = TRUE, {
      subs = fw_subdivisions_for(choices, input$country %||% "")

      current = input$region %||% ""
      # Keep the answer if it still applies.
      keep = if (nzchar(current) && (!length(subs) || current %in% subs)) {
        current
      } else {
        ""
      }
      if (nzchar(current) && !nzchar(keep)) {
        fw_announce(session, fw_fill(
          fw_t("contribute", "announce", "regions_changed"),
          country = input$country))
      }
      updateSelectizeInput(
        session, "region",
        choices = c(stats::setNames("", fw_t("contribute", "placeholder", "select")), subs),
        selected = keep, server = FALSE,
        options = list(create = TRUE,
                       placeholder = fw_t("contribute", "placeholder", "select_or_type"))
      )
    })

    observeEvent(input$add_target, {
      i = next_index$target
      insertUI(paste0("#", ns("target_rows")), where = "beforeEnd",
               ui = fw_target_row(ns, i, choices))
      rows$target = c(rows$target, i)
      next_index$target = i + 1L
      bind_pair_row(i, "target")
      fw_bind_remove(session, ns, input, i, "target", rows)
      fw_announce(session, fw_fill(fw_t("contribute", "announce", "target_added"), n = i))
    })

    observeEvent(input$add_beneficiary, {
      i = next_index$benefit
      insertUI(paste0("#", ns("benefit_rows")), where = "beforeEnd",
               ui = fw_beneficiary_row(ns, i, choices))
      rows$benefit = c(rows$benefit, i)
      next_index$benefit = i + 1L
      bind_pair_row(i, "benefit")
      fw_bind_remove(session, ns, input, i, "benefit", rows)
      fw_announce(session, fw_fill(fw_t("contribute", "announce", "beneficiary_added"), n = i))
    })

    observeEvent(input$add_method, {
      i = next_index$method
      insertUI(paste0("#", ns("method_rows")), where = "beforeEnd",
               ui = fw_method_row(ns, i, choices))
      rows$method = c(rows$method, i)
      next_index$method = i + 1L
      fw_bind_remove(session, ns, input, i, "method", rows)
    })

    # ---- Location picker ------------------------------------------------------

    # Map and coordinate boxes sync both ways; a guard flag stops the loop.
    syncing = reactiveVal(FALSE)

    output$picker = renderLeaflet({
      # The shared basemaps, so Satellite and Terrain help find the water.
      fw_leaflet() |>
        fw_add_basemaps() |>
        setView(lng = FW_MAP$empty_view$lng, lat = FW_MAP$empty_view$lat,
                zoom = FW_MAP$empty_view$zoom)
    })

    observeEvent(input$picker_click, {
      click = input$picker_click
      req(click)
      syncing(TRUE)
      updateNumericInput(session, "latitude",  value = round(click$lat, FW_COORD_DP))
      updateNumericInput(session, "longitude", value = round(click$lng, FW_COORD_DP))
      syncing(FALSE)
      fw_place_pin(session, click$lat, click$lng)
      fmt = paste0("%.", FW_COORD_DP, "f")
      fw_announce(session, fw_fill(fw_t("contribute", "announce", "location_set"),
                                   lat = sprintf(fmt, click$lat),
                                   lng = sprintf(fmt, click$lng)))
    })

    observeEvent(list(input$latitude, input$longitude), {
      if (isTRUE(syncing())) return()
      lat = input$latitude; lng = input$longitude
      if (is.null(lat) || is.null(lng) || is.na(lat) || is.na(lng)) return()
      if (lat < -90 || lat > 90 || lng < -180 || lng > 180) return()
      fw_place_pin(session, lat, lng)
    }, ignoreInit = TRUE)

    # ---- Review ---------------------------------------------------------------

    # Enabling validators is a side effect of asking, so its own observer.
    observeEvent(input$check_answers, {
      for (nm in names(step_validators)) fw_touch_step(nm)
      iv_consent$enable()
      res = fw_check()
      fw_announce(session, if (length(res$errors) == 0) {
        fw_t("contribute", "check", "heading_clear")
      } else {
        fw_fill(fw_t("contribute", "check", "heading_errors_many"), n = length(res$errors))
      })
    })

    # Jump to a field from the check list (the browser scrolls).
    observeEvent(input$goto_field, {
      session$sendCustomMessage("fw-scroll-to-field", ns(input$goto_field))
    })

    # bindEvent, so this renders when asked for and never while typing.
    output$review_summary = bindEvent(renderUI({
      res = fw_check()
      n_err = length(res$errors)

      item = function(x, kind) {
        tags$li(
          class = paste0("fw-check__item fw-check__item--", kind),
          # One input for all rows, so observers don't pile up.
          tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;",
              ns("goto_field"), x$id
            ),
            x$msg
          )
        )
      }

      errors_block = if (n_err > 0) {
        div(
          class = "fw-check fw-check--errors", role = "alert",
          h3(if (n_err == 1) fw_t("contribute", "check", "heading_errors_one")
             else fw_fill(fw_t("contribute", "check", "heading_errors_many"), n = n_err)),
          p(fw_t("contribute", "check", "body_errors")),
          tags$ul(class = "fw-check__list",
                  lapply(res$errors, item, kind = "error"))
        )
      } else {
        div(
          class = "fw-check fw-check--clear", role = "status",
          h3(fw_t("contribute", "check", "heading_clear")),
          p(fw_t("contribute", "check", "body_clear"))
        )
      }

      notes_block = if (length(res$notes) > 0) {
        div(
          class = "fw-check fw-check--notes",
          h3(fw_t("contribute", "check", "heading_notes")),
          p(fw_t("contribute", "check", "body_notes")),
          tags$ul(class = "fw-check__list",
                  lapply(res$notes, item, kind = "note"))
        )
      }

      rec = assemble_record()
      shown = Filter(function(x) nzchar(x), lapply(rec, function(v) {
        if (is.null(v) || length(v) == 0) return("")
        paste(as.character(v), collapse = "; ")
      }))

      answers = if (length(shown) == 0) {
        p(class = "fw-caption", fw_t("contribute", "announce", "nothing_entered"))
      } else {
        tagList(
          h3(fw_t("contribute", "check", "review_heading")),
          div(
            class = "fw-table-scroll",
            tags$table(
              class = "fw-table",
              tags$tbody(
                lapply(names(shown), function(k) {
                  tags$tr(tags$th(scope = "row", fw_prettify_key(k)),
                          tags$td(shown[[k]]))
                })
              )
            )
          )
        )
      }

      div(style = "margin-block-start: 1rem;", errors_block, notes_block, answers)
    }), input$check_answers)

    # ---- Assemble and send ----------------------------------------------------

    # The record is built by fw_collect_submission() in submit.R.
    assemble_record = reactive({
      fw_collect_submission(input, rows, choices)
    })

    observeEvent(input$send, {
      if (!all_valid()) {
        # Show every error and scroll to the first.
        for (nm in names(step_validators)) fw_touch_step(nm)
        iv_consent$enable()
        fw_announce(session, fw_t("contribute", "error", "validation"))
        session$sendCustomMessage("fw-scroll-to-error", ns("form"))
        return()
      }
      # fw_safely(), so an assembly error doesn't end the session.
      out = fw_safely(session, fw_submit_attempt(assemble_record(), data))
      if (is.null(out)) return()
      if (!isTRUE(out$success)) {
        showNotification(out$message, type = "error", duration = NULL)
        return()
      }
      result(out)
      if (is.function(on_submit)) on_submit()
      fw_track(session, "form_submit", list(form = "submission"))
      stage("done")
      fw_announce(session, fw_t("contribute", "confirm", "heading"))
    })

    observeEvent(input$another, {
      # Reload for a clean form, back on this page via ?page=contribute
      # (handled and cleared in app.R).
      updateQueryString("?page=contribute", mode = "replace", session = session)
      session$reload()
    })

    observeEvent(input$to_explore, {
      session$sendCustomMessage("fw-nav", "explore")
    })

    # ---- Downloads ------------------------------------------------------------

    # Generated from the section builders (R/questions_text.R).
    output$download_questions = downloadHandler(
      filename = function() paste0("fwise-submission-questions_", fw_file_stamp(), ".docx"),
      contentType = paste0("application/vnd.openxmlformats-officedocument.",
                           "wordprocessingml.document"),
      content = function(file) {
        fw_write_questions_docx(file, choices)
        fw_track(session, "download", list(type = "docx"))
      }
    )

    # Plain text too: opens anywhere, no Word needed.
    output$download_questions_txt = downloadHandler(
      filename = function() paste0("fwise-submission-questions_", fw_file_stamp(), ".txt"),
      contentType = "text/plain",
      content = function(file) {
        writeLines(fw_questions_text(choices), file, useBytes = TRUE)
        fw_track(session, "download", list(type = "txt"))
      }
    )
  })
}

# ---- Helpers -----------------------------------------------------------------

#' Write a message into the page's live region so it is announced
fw_announce = function(session, text) {
  session$sendCustomMessage("fw-announce", text)
}

#' Move the single pin on the location picker
fw_place_pin = function(session, lat, lng) {
  leafletProxy("picker", session = session) |>
    clearMarkers() |>
    addMarkers(lng = lng, lat = lat)
}

#' Wire up a repeatable row's removal control
fw_bind_remove = function(session, ns, input, index, kind, rows) {
  observeEvent(input[[paste0("remove_", kind, "_", index)]], once = TRUE, {
    removeUI(paste0("#", ns(paste0(kind, "_row_", index))))
    rows[[kind]] = setdiff(rows[[kind]], index)
  })
}

#' Turn a record key into a readable label for the review table
fw_prettify_key = function(k) {
  k = gsub("_", " ", k)
  paste0(toupper(substr(k, 1, 1)), substr(k, 2, nchar(k)))
}

