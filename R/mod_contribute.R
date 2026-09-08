# mod_contribute.R
# BUILT. The contribute form.
#
# ONE SCROLLING PAGE, not a wizard. The opening panel sets expectations, then
# every section is rendered once and scrolled through, with a sticky progress
# rail at the top. There is no Back and no Next.
#
# WHY IT CHANGED. The stepped version rendered the current step through
# renderUI, so moving between steps destroyed and rebuilt every input on the
# step being left. Repeatable rows did not survive that, the server-side species
# selectize was being populated against elements not yet in the DOM, and going
# back left the form in a state it could not recover from. Rendering everything
# once removes the whole class of problem, and there is no UI rendering on the
# form after the first paint.
#
# Validation therefore runs at the point of sending rather than per step: the
# validators are enabled on the first refused send, the page scrolls to the
# first field that needs attention, and corrections show live from then on.
#
# The section UI builders live in mod_contribute_steps.R.

library(shiny)
library(bslib)
library(shinyvalidate)
library(leaflet)

mod_contribute_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fw_page_header(fw_t("contribute", "title"), fw_t("contribute", "description")),
    tags$main(
      id = "fw-main",
      fw_section(
        fw_container(
          # Three states: the opening panel, the wizard, the confirmation.
          uiOutput(ns("stage"))
        )
      )
    )
  )
}

mod_contribute_server <- function(id, data, choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stage <- reactiveVal("intro")     # intro | form | done
    result <- reactiveVal(NULL)

    # Counters driving the repeatable blocks. Each holds the indices currently on
    # screen, so removing a row in the middle does not renumber the others.
    rows <- reactiveValues(
      target  = 1L,
      benefit = 1L,
      method  = 1L
    )
    next_index <- reactiveValues(target = 2L, benefit = 2L, method = 2L)

    # ---- The conditional section ----------------------------------------------

    # The chemical section appears only when a chemical method has been chosen.
    # It is shown and hidden in the browser rather than rendered in and out, so
    # anything already answered there survives a change of mind about the method.
    chemical_selected <- reactive({
      picked <- vapply(rows$method, function(i) {
        input[[paste0("method_", i)]] %||% ""
      }, character(1))
      any(picked %in% choices$chemical_methods)
    })

    output$chemical_needed <- reactive(chemical_selected())
    # Without this the output is suspended while its panel is hidden, and a
    # suspended output never reports the value that would unhide it.
    outputOptions(output, "chemical_needed", suspendWhenHidden = FALSE)

    # ---- Validation -----------------------------------------------------------

    # One validator per step. Keeping them separate is what lets Next block on
    # just this step's required fields rather than on the whole form.
    iv <- InputValidator$new()

    iv_site <- InputValidator$new()
    iv_site$add_rule("site_name", sv_required(message = "Please give the site a name."))
    iv_site$add_rule("country", sv_required(message = "Please choose a country."))
    iv_site$add_rule("latitude", sv_required(message = "Place a pin on the map, or type a latitude."))
    iv_site$add_rule("latitude", sv_between(-90, 90, message = "Latitude must be between -90 and 90."))
    iv_site$add_rule("longitude", sv_required(message = "Place a pin on the map, or type a longitude."))
    iv_site$add_rule("longitude", sv_between(-180, 180, message = "Longitude must be between -180 and 180."))

    iv_waterbody <- InputValidator$new()
    iv_waterbody$add_rule("waterbody_type", sv_required(message = "Please choose a waterbody type."))
    iv_waterbody$add_rule("water_regime", sv_required(message = "Please choose still or flowing water."))
    # The unit becomes required only once a size has been entered.
    iv_waterbody$add_rule("area_unit", function(value) {
      size <- input$area_treated
      if (!is.null(size) && !is.na(size) && (is.null(value) || !nzchar(value))) {
        "Please give the unit for the area you entered."
      }
    })

    iv_invasive <- InputValidator$new()
    iv_invasive$add_rule("target_taxa_1", sv_required(message = "Please choose the kind of animal targeted."))
    iv_invasive$add_rule("target_species_1", sv_required(message = "Please name the species targeted."))

    this_year <- as.integer(format(Sys.Date(), "%Y"))
    iv_timeline <- InputValidator$new()
    iv_timeline$add_rule("start_year", sv_required(message = "Please give the year the eradication began."))
    iv_timeline$add_rule("start_year", sv_between(1500, this_year,
      message = paste0("Enter a year between 1500 and ", this_year, ".")))
    iv_timeline$add_rule("invasion_year", sv_optional())
    iv_timeline$add_rule("invasion_year", sv_between(1500, this_year,
      message = paste0("Enter a year between 1500 and ", this_year, ".")))
    iv_timeline$add_rule("end_year", sv_optional())
    iv_timeline$add_rule("end_year", function(value) {
      if (is.null(value) || is.na(value)) return(NULL)
      if (value < 1500 || value > this_year + 20) {
        return("Enter a year between 1500 and twenty years from now.")
      }
      start <- input$start_year
      if (!is.null(start) && !is.na(start) && value < start) {
        "The end year cannot be before the start year."
      }
    })
    iv_timeline$add_rule("driver", sv_required(message = "Please choose a driver."))

    iv_methods <- InputValidator$new()
    iv_methods$add_rule("method_1", sv_required(message = "Please choose the main method."))

    iv_outcome <- InputValidator$new()
    iv_outcome$add_rule("outcome", sv_required(message = "Please choose an outcome."))

    iv_contributor <- InputValidator$new()
    iv_contributor$add_rule("primary_contact_name", sv_required(message = "Please give a contact name."))
    iv_contributor$add_rule("primary_contact_email", sv_required(message = "Please give a contact email."))
    iv_contributor$add_rule("primary_contact_email", sv_email(message = "That does not look like an email address."))
    iv_contributor$add_rule("secondary_contact_email", sv_optional())
    iv_contributor$add_rule("secondary_contact_email", sv_email(message = "That does not look like an email address."))

    step_validators <- list(
      site = iv_site, waterbody = iv_waterbody, invasive = iv_invasive,
      timeline = iv_timeline, methods = iv_methods, outcome = iv_outcome,
      contributor = iv_contributor
    )
    # Deliberately NOT enabled here. A validator enabled up front paints the step
    # red before the contributor has typed anything, which reads as being told
    # off for arriving. Each step's validator is enabled the first time they try
    # to move on from it, and stays enabled after that so corrections show live.
    touched <- reactiveVal(character(0))

    fw_touch_step <- function(id) {
      v <- step_validators[[id]]
      if (is.null(v)) return(invisible(NULL))
      if (!id %in% touched()) {
        touched(c(touched(), id))
        v$enable()
      }
      invisible(NULL)
    }

    # The consent controls gate the whole submission, not one step.
    iv_consent <- InputValidator$new()
    iv_consent$add_rule("consent_data_use", function(value) {
      if (!isTRUE(value)) "Please confirm you agree before sending."
    })
    # Deliberately NOT enabled here. Enabling at startup shows the contributor an
    # error on the opening panel before they have touched anything, which reads
    # as being told off for arriving. It is enabled the first time they try to
    # start the form without ticking the box.
    

    # Gating is computed independently of the validators, on purpose.
    #
    # shinyvalidate's is_valid() returns TRUE for a validator that is DISABLED,
    # and the step validators start disabled so a contributor is not shown errors
    # for fields they have not reached. Relying on is_valid() for gating would
    # therefore leave "Send submission" enabled on an empty form. The validators
    # own the DISPLAY of errors; this function owns whether the form may be sent.
    #
    # Keep the two in step: a required rule added above needs a line here.
    # POSIX [:space:] rather than \\s. R's default regex engine does NOT read \\s
    # as a whitespace shorthand inside a character class, so [^@\\s] excludes the
    # LETTER s and quietly rejects any address containing one.
    fw_is_email <- function(x) {
      grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", x %||% "")
    }

    filled <- function(nm) {
      v <- input[[nm]]
      !is.null(v) && length(v) > 0 && !all(is.na(v)) &&
        any(nzchar(as.character(v[!is.na(v)])))
    }
    in_range <- function(nm, lo, hi) {
      v <- suppressWarnings(as.numeric(input[[nm]]))
      length(v) == 1 && !is.na(v) && v >= lo && v <= hi
    }

    all_valid <- reactive({
      isTRUE(input$consent_data_use) &&
        filled("site_name") && filled("country") &&
        in_range("latitude", -90, 90) && in_range("longitude", -180, 180) &&
        filled("waterbody_type") && filled("water_regime") &&
        # The unit is required only once an area has been entered.
        (!filled("area_treated") || filled("area_unit")) &&
        filled("target_taxa_1") && filled("target_species_1") &&
        in_range("start_year", 1500, this_year) &&
        # An end year is optional, but must not precede the start year.
        (!filled("end_year") ||
           (in_range("end_year", 1500, this_year + 20) &&
              as.numeric(input$end_year) >= as.numeric(input$start_year))) &&
        filled("driver") && filled("method_1") && filled("outcome") &&
        filled("primary_contact_name") &&
        fw_is_email(input$primary_contact_email) &&
        (!filled("secondary_contact_email") ||
           fw_is_email(input$secondary_contact_email))
    })

    # ---- "Check my answers" ---------------------------------------------------

    # ONE source of truth for what is wrong with the form. all_valid() answers
    # "may this be sent"; this answers "what, specifically, and where". Both read
    # the same conditions, and the check below is what keeps them honest: every
    # hard error here corresponds to a clause of all_valid(), so the button
    # cannot report a clean form that the send gate then refuses.
    #
    # HARD blocks sending. SOFT never does. A great many real attempts are
    # ongoing, unmeasured or unpublished, and refusing those records would bias
    # the database towards the tidy ones - which is precisely the reporting bias
    # the caveats warn readers about.
    fw_check <- function() {
      msg <- function(key, ...) {
        out <- fw_t("contribute", "check", key)
        subs <- list(...)
        for (nm in names(subs)) out <- sub(paste0("{", nm, "}"), subs[[nm]], out, fixed = TRUE)
        out
      }
      errors <- list(); notes <- list()
      err  <- function(id, m) errors <<- c(errors, list(list(id = id, msg = m)))
      note <- function(id, m) notes  <<- c(notes,  list(list(id = id, msg = m)))

      # ---- Hard: the record is unusable without these ----
      if (!filled("site_name"))      err("site_name", msg("e_site_name"))
      if (!filled("country"))        err("country",   msg("e_country"))
      if (!in_range("latitude",  -90,  90))  err("latitude",  msg("e_latitude"))
      if (!in_range("longitude", -180, 180)) err("longitude", msg("e_longitude"))
      if (!filled("water_regime"))   err("water_regime",   msg("e_regime"))
      if (!filled("waterbody_type")) err("waterbody_type", msg("e_waterbody"))
      if (filled("area_treated") && !filled("area_unit"))
        err("area_unit", msg("e_area_unit"))
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

      if (!filled("primary_contact_name"))
        err("primary_contact_name", msg("e_contact_name"))
      if (!filled("primary_contact_email")) {
        err("primary_contact_email", msg("e_contact_mail"))
      } else if (!fw_is_email(input$primary_contact_email)) {
        err("primary_contact_email", msg("e_contact_bad"))
      }
      if (filled("secondary_contact_email") &&
          !fw_is_email(input$secondary_contact_email))
        err("secondary_contact_email", msg("e_second_mail"))

      if (!isTRUE(input$consent_data_use)) err("consent_data_use", msg("e_consent"))

      # ---- Soft: worth having, never required ----
      if (!filled("end_year"))       note("end_year",      msg("w_end_year"))
      if (!filled("area_treated"))   note("area_treated",  msg("w_area"))
      if (!filled("invasion_year"))  note("invasion_year", msg("w_invasion"))
      if (!any(vapply(rows$benefit, function(i) filled(paste0("benefit_species_", i)),
                      logical(1))))
        note("benefit_species_1", msg("w_beneficiary"))
      if (!filled("reference"))      note("reference",     msg("w_reference"))
      if (!filled("verification"))   note("verification",  msg("w_verification"))
      if (!filled("duration_days"))  note("duration_days", msg("w_duration"))
      if (!filled("method_description"))
        note("method_description", msg("w_method_desc"))
      # Not an error: the earliest attempt on record is 1934, but an older one is
      # possible and we would rather have it flagged than refused.
      if (in_range("start_year", 1500, 1900))
        note("start_year", msg("w_old_year", year = as.integer(input$start_year)))

      list(errors = errors, notes = notes)
    }

    # ---- Stages ---------------------------------------------------------------

    output$stage <- renderUI({
      switch(stage(),
        intro = fw_intro_panel(ns),
        form  = tagList(fw_form_shell(ns, choices), fw_form_script(ns)),
        done  = fw_confirmation(ns, result())
      )
    })

    observeEvent(input$start, {
      if (!isTRUE(input$consent_data_use)) {
        iv_consent$enable()
        fw_announce(session, "Please confirm you agree before starting.")
        return()
      }
      iv_consent$enable()
      stage("form")
    })

    # ---- Repeatable blocks ----------------------------------------------------

    # Row 1 of each block is rendered by the section builder itself, so there is
    # no insertUI racing the page into existence - the old cause of the invasive
    # section arriving empty or doubled. insertUI now only ever ADDS to a
    # container that is already on the page.

    #' Narrow a row's species picker to the group that row names
    #'
    #' ONE binder for both roles, because they are now the same widget pair.
    #'
    #' Client-side: the whole list is already in the browser, so this is a list
    #' swap rather than a search round trip. A species the contributor typed in
    #' themselves is carried across, otherwise changing the group after naming a
    #' species would silently discard it.
    bind_pair_row <- function(i, kind) {
      taxa_id    <- paste0(kind, "_taxa_", i)
      species_id <- paste0(kind, "_species_", i)

      observeEvent(input[[taxa_id]], ignoreInit = TRUE, {
        taxa <- input[[taxa_id]] %||% ""
        lst <- choices$species_by_taxa[[taxa]]
        if (is.null(lst) || !length(lst)) lst <- choices$species_by_taxa[[FW_ALL]]

        current <- input[[species_id]] %||% ""
        if (nzchar(current) && !current %in% lst) lst <- c(current, lst)

        # The blank stays at the front for the same reason it is there in
        # fw_species_picker(): without it selectize picks the first species by
        # itself the moment the list is replaced.
        updateSelectizeInput(session, species_id, choices = c("", lst),
                             selected = current, server = FALSE)
      })
      invisible(NULL)
    }

    bind_pair_row(1L, "target")
    bind_pair_row(1L, "benefit")

    # Narrow the waterbody types to the regime. Still water should not be
    # offered "River", and flowing water should not be offered "Lake".
    observeEvent(input$water_regime, ignoreInit = TRUE, {
      rg <- input$water_regime %||% ""
      lst <- choices$waterbody_by_regime[[rg]]
      if (is.null(lst) || !length(lst)) lst <- choices$waterbody_by_regime[[FW_ALL]]

      current <- input$waterbody_type %||% ""
      keep <- if (nzchar(current) && current %in% lst) current else ""
      if (nzchar(current) && !nzchar(keep)) {
        fw_announce(session, paste("The waterbody types have changed to match",
                                   rg, "water. Please choose again."))
      }
      updateSelectizeInput(
        session, "waterbody_type",
        choices = c(stats::setNames("", "Select..."), lst),
        selected = keep, server = FALSE
      )
    })

    observeEvent(input$add_target, {
      i <- next_index$target
      insertUI(paste0("#", ns("target_rows")), where = "beforeEnd",
               ui = fw_target_row(ns, i, choices))
      rows$target <- c(rows$target, i)
      next_index$target <- i + 1L
      bind_pair_row(i, "target")
      fw_bind_remove(session, ns, input, i, "target", rows)
      fw_announce(session, paste("Target", i, "added."))
    })

    observeEvent(input$add_beneficiary, {
      i <- next_index$benefit
      insertUI(paste0("#", ns("benefit_rows")), where = "beforeEnd",
               ui = fw_beneficiary_row(ns, i, choices))
      rows$benefit <- c(rows$benefit, i)
      next_index$benefit <- i + 1L
      bind_pair_row(i, "benefit")
      fw_bind_remove(session, ns, input, i, "benefit", rows)
      fw_announce(session, paste("Beneficiary", i, "added."))
    })

    observeEvent(input$add_method, {
      i <- next_index$method
      insertUI(paste0("#", ns("method_rows")), where = "beforeEnd",
               ui = fw_method_row(ns, i, choices))
      rows$method <- c(rows$method, i)
      next_index$method <- i + 1L
      fw_bind_remove(session, ns, input, i, "method", rows)
    })

    # ---- Location picker ------------------------------------------------------

    # The map and the two numeric boxes stay in step in both directions. The
    # guard flag stops a map click updating the boxes, which updates the map,
    # which fires another click handler.
    syncing <- reactiveVal(FALSE)

    output$picker <- renderLeaflet({
      # A muted, low-chroma base map so the pin carries all the colour.
      #
      # NOTE: CartoDB.Positron is the usual choice here and was the first pick,
      # but Carto now watermarks keyless requests with "API KEY REQUIRED" across
      # every tile. Esri.WorldGrayCanvas is equally muted and still keyless. If
      # the client obtains a Carto key, switch back and pass it through.
      leaflet(options = leafletOptions(worldCopyJump = TRUE)) |>
        addProviderTiles("Esri.WorldGrayCanvas",
                         options = providerTileOptions(noWrap = FALSE)) |>
        setView(lng = 0, lat = 20, zoom = 2)
    })

    observeEvent(input$picker_click, {
      click <- input$picker_click
      req(click)
      syncing(TRUE)
      updateNumericInput(session, "latitude",  value = round(click$lat, FW_COORD_DP))
      updateNumericInput(session, "longitude", value = round(click$lng, FW_COORD_DP))
      syncing(FALSE)
      fw_place_pin(session, click$lat, click$lng)
      fw_announce(session, sprintf("Location set to latitude %.6f, longitude %.6f",
                                   click$lat, click$lng))
    })

    observeEvent(list(input$latitude, input$longitude), {
      if (isTRUE(syncing())) return()
      lat <- input$latitude; lng <- input$longitude
      if (is.null(lat) || is.null(lng) || is.na(lat) || is.na(lng)) return()
      if (lat < -90 || lat > 90 || lng < -180 || lng > 180) return()
      fw_place_pin(session, lat, lng)
    }, ignoreInit = TRUE)

    # ---- Review ---------------------------------------------------------------

    # Turning on the in-place highlighting is a SIDE EFFECT of asking, not of
    # rendering, so it happens in its own observer. A renderUI that also enabled
    # validators would fire them again on every invalidation.
    observeEvent(input$check_answers, {
      for (nm in names(step_validators)) fw_touch_step(nm)
      iv_consent$enable()
      res <- fw_check()
      fw_announce(session, if (length(res$errors) == 0) {
        fw_t("contribute", "check", "heading_clear")
      } else {
        sub("{n}", length(res$errors),
            fw_t("contribute", "check", "heading_errors_many"), fixed = TRUE)
      })
    })

    # Jump to a named field from the check list. The browser does the scrolling;
    # the server only says which field.
    observeEvent(input$goto_field, {
      session$sendCustomMessage("fw-scroll-to-field", ns(input$goto_field))
    })

    # bindEvent, so this renders when asked for and never while typing.
    output$review_summary <- bindEvent(renderUI({
      res <- fw_check()
      n_err <- length(res$errors)

      item <- function(x, kind) {
        tags$li(
          class = paste0("fw-check__item fw-check__item--", kind),
          # An actionLink per row would leave a dead observer behind every time
          # this panel re-rendered. One input, carrying which field was asked
          # for, is the same pattern the networking pager uses.
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

      errors_block <- if (n_err > 0) {
        div(
          class = "fw-check fw-check--errors", role = "alert",
          h3(if (n_err == 1) fw_t("contribute", "check", "heading_errors_one")
             else sub("{n}", n_err, fw_t("contribute", "check", "heading_errors_many"),
                      fixed = TRUE)),
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

      notes_block <- if (length(res$notes) > 0) {
        div(
          class = "fw-check fw-check--notes",
          h3(fw_t("contribute", "check", "heading_notes")),
          p(fw_t("contribute", "check", "body_notes")),
          tags$ul(class = "fw-check__list",
                  lapply(res$notes, item, kind = "note"))
        )
      }

      rec <- assemble_record()
      shown <- Filter(function(x) nzchar(x), lapply(rec, function(v) {
        if (is.null(v) || length(v) == 0) return("")
        paste(as.character(v), collapse = "; ")
      }))

      answers <- if (length(shown) == 0) {
        p(class = "fw-caption", "Nothing entered yet.")
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

    # The record is shaped by fw_collect_submission() in submit.R, next to the
    # code that writes it, so the column set and the thing that fills it stay
    # side by side.
    assemble_record <- reactive({
      fw_collect_submission(input, rows, choices$species_family)
    })

    observeEvent(input$send, {
      if (!all_valid()) {
        # Enable everything, so the contributor can see which field still needs
        # attention rather than being refused with no explanation. On one long
        # page an error can easily be several screens away, so the browser is
        # asked to take them to the first one.
        for (nm in names(step_validators)) fw_touch_step(nm)
        iv_consent$enable()
        fw_announce(session, fw_t("contribute", "error", "validation"))
        session$sendCustomMessage("fw-scroll-to-error", ns("form"))
        return()
      }
      out <- fw_submit_attempt(assemble_record(), data)
      if (!isTRUE(out$success)) {
        showNotification(out$message, type = "error", duration = NULL)
        return()
      }
      result(out)
      stage("done")
      fw_announce(session, fw_t("contribute", "confirm", "heading"))
    })

    observeEvent(input$another, {
      # A fresh session is the honest way to reset a form this size. Reloading
      # gives the contributor an empty form with no stale values hiding in
      # inputs that were never re-rendered.
      session$reload()
    })

    observeEvent(input$to_explore, {
      session$sendCustomMessage("fw-nav", "explore")
    })

    # ---- Downloads ------------------------------------------------------------

    # Generated from the step builders themselves rather than kept as a separate
    # document, so it cannot fall out of step with the form. See
    # R/questions_text.R.
    output$download_questions <- downloadHandler(
      filename = function() "fwise-submission-questions.txt",
      contentType = "text/plain",
      content = function(file) {
        writeLines(fw_questions_text(choices), file, useBytes = TRUE)
      }
    )
  })
}

# ---- Helpers -----------------------------------------------------------------

#' Write a message into the page's live region so it is announced
fw_announce <- function(session, text) {
  session$sendCustomMessage("fw-announce", text)
}

#' Move the single pin on the location picker
fw_place_pin <- function(session, lat, lng) {
  leafletProxy("picker", session = session) |>
    clearMarkers() |>
    addMarkers(lng = lng, lat = lat)
}

#' Wire up a repeatable row's removal control
fw_bind_remove <- function(session, ns, input, index, kind, rows) {
  observeEvent(input[[paste0("remove_", kind, "_", index)]], once = TRUE, {
    removeUI(paste0("#", ns(paste0(kind, "_row_", index))))
    rows[[kind]] <- setdiff(rows[[kind]], index)
  })
}

#' Turn a record key into a readable label for the review table
fw_prettify_key <- function(k) {
  k <- gsub("_", " ", k)
  paste0(toupper(substr(k, 1, 1)), substr(k, 2, nchar(k)))
}

