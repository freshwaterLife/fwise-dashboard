# app.R
# Entry point; Connect Cloud expects it at the repo root. Data loads once and is
# shared by every session.

library(shiny)
library(bslib)
library(htmltools)

# Shiny sources every file in R/ itself, so there is no sourcing loop here.
# Anything in R/ runs on boot: data scripts belong in dev/.

# ---- Startup -----------------------------------------------------------------

# Loaded once per process; read-only for every visitor.
FW_DATA = fw_load_data()

# Release metadata from the data repo.
FW_META = fw_load_metadata()

# Dropdown choices come from the data.
FW_CHOICES = fw_startup_choices(FW_DATA)

# Explore's filter bar is static UI, so its choices are built before any
# session.
FW_FILTER_CHOICES = fw_filter_choices(FW_DATA)

FW_LAST_UPDATED = fw_last_updated(FW_DATA, FW_META)

# Records waiting on review, for the first paint. Sessions keep it current
# through fw_review_count_live(); this call also warms its cache.
FW_IN_REVIEW = fw_review_count_live(FW_DATA)

# The data mode says whether a deployment picked up its token.
message("FWISE startup: ", nrow(FW_DATA$attempt), " attempts, ",
        nrow(FW_DATA$contact), " contacts, released ", format(FW_LAST_UPDATED),
        ", mode ", fw_data_mode(),
        if (fw_data_mode() == "api") paste0(" (", FW_DATA_REPO, "@", FW_DATA_REF, ")") else "")

# Newsletter and feedback: "sheets" or "local" (dev/forms/).
message("FWISE startup: forms ", fw_forms_mode())

# Submissions: "sheets" or "local" (dev/forms/).
message("FWISE startup: submissions inbox ", fw_inbox_mode())

# Usage tracking: "sheet", "console" or "off". See R/tracking.R.
message("FWISE startup: event log ", fw_track_mode_why())

# The PDF needs Quarto 1.4+. Without it the picker drops the PDF and says why.
message("FWISE startup: quarto ",
        if (fw_pdf_available()) paste0(fw_quarto_version(), " at ", fw_quarto_path())
        else "NOT FOUND - the PDF report is unavailable")

# ---- UI ----------------------------------------------------------------------

ui = page_navbar(
  id = "fw_nav",
  title = fw_brand(),
  window_title = fw_t("app", "full_title"),
  theme = fw_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(class = "fw-navbar", underline = FALSE),

  header = tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = FW_LOGO$badge_web),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      # Description, canonical, Open Graph, structured data, and the no-JS
      # loader rule. See fw_head_meta().
      fw_head_meta(),
      fw_noscript_head(),
      # Arrivals only. See R/tracking.R.
      fw_goatcounter_tag(),
      # www/scss/ compiled with the R/brand.R tokens.
      tags$style(HTML(fw_compile_css("www/scss/main.scss")))
    ),
    fw_noscript(),
    fw_loader(),
    # Spinning badge on slow outputs, styled in _components.scss. Leaflet proxy
    # updates never mark .recalculating, so fw_client_script() covers maps with
    # the same delay.
    useBusyIndicators(spinners = TRUE, pulse = FALSE),
    busyIndicatorOptions(spinner_delay = paste0(FW_SPINNER_DELAY_MS, "ms"),
                         spinner_size = "64px"),
    fw_skip_link(),
    fw_popover_script(),
    fw_client_script(),
    fw_forms_script(),
    fw_privacy_script(),
    fw_live_region("fw_announce")
  ),

  nav_panel(fw_t("nav", "home"),       value = "home",       mod_home_ui("home", fw_headline_stats(FW_DATA))),
  nav_panel(fw_t("nav", "explore"),    value = "explore",    mod_explore_ui("explore", FW_FILTER_CHOICES)),
  nav_panel(fw_t("nav", "plan"),       value = "plan",       mod_plan_ui("plan")),
  nav_panel(fw_t("nav", "contribute"), value = "contribute", mod_contribute_ui("contribute")),
  nav_panel(fw_t("nav", "networking"), value = "networking", mod_networking_ui("networking")),
  nav_panel(fw_t("nav", "about"),      value = "about",      mod_about_ui("about")),
  # Navbar link hidden by CSS; reached from the footer and ?page=privacy.
  nav_panel(fw_t("nav", "privacy"),    value = "privacy",    mod_privacy_ui("privacy")),

  footer = fw_footer(FW_LAST_UPDATED, FW_IN_REVIEW)
)

# ---- Server ------------------------------------------------------------------

server = function(input, output, session) {

  # Reconnect to the same session after a dropped websocket instead of the
  # grey screen. The keepalive in fw_client_script() prevents idle drops.
  session$allowReconnect(TRUE)

  # Lets a page from an earlier deploy reload itself. See FW_BUILD_ID.
  session$sendCustomMessage("fw-build", FW_BUILD_ID)

  # Usage log: session_start now, the rest written when the session ends.
  fw_track_session(session)

  # Contact address revealed. The id must be a real contact_id.
  observeEvent(input$fw_contact_click, {
    id = input$fw_contact_click
    if (is.character(id) && length(id) == 1 && id %in% FW_DATA$contact$contact_id) {
      fw_track(session, "contact_click", list(contact_id = id))
    }
  })

  # In-review count: refreshed every FW_REVIEW_REFRESH_S and straight after a
  # submission. The footer updates by message; Explore re-renders.
  review_tick = reactiveVal(0L)
  in_review = reactive({
    review_tick()
    invalidateLater(FW_REVIEW_REFRESH_S * 1000)
    fw_review_count_live(FW_DATA)
  })
  observeEvent(in_review(), fw_send_review_count(session, in_review()), ignoreInit = FALSE)

  mod_home_server("home", FW_DATA)
  mod_explore_server("explore", FW_DATA, in_review)
  mod_plan_server("plan", FW_DATA, FW_META)
  mod_contribute_server("contribute", FW_DATA, FW_CHOICES,
                        on_submit = function() review_tick(review_tick() + 1L))
  mod_networking_server("networking", FW_DATA)
  mod_about_server("about", FW_DATA, FW_META)

  # Cross-page links set this instead of each module touching the navbar.
  observeEvent(input$fw_nav_to, {
    nav_select("fw_nav", input$fw_nav_to, session = session)
  })

  # ---- The two forms ----------------------------------------------------------
  #
  # One of each for the app. fw_form_open() buttons set fw_open_form; each
  # module opens for its own name.
  open_form = reactive(input$fw_open_form)
  mod_newsletter_server("newsletter", open = open_form)
  mod_feedback_server("feedback", open = open_form, nav = reactive(input$fw_nav))

  # ---- The privacy page and the address bar -----------------------------------
  #
  # Only the privacy page keeps a URL (?page=privacy). ?page=contribute is the
  # form's "Add another attempt" reload target and is cleared on arrival.
  url_is_privacy = reactiveVal(FALSE)
  # last_page: the last ordinary page. before_privacy: where Back goes from
  # the privacy page, copied only on an in-app visit so Back/Forward don't
  # overwrite it.
  last_page      = reactiveVal("home")
  before_privacy = reactiveVal("home")

  show_privacy = function() {
    nav_select("fw_nav", "privacy", session = session)
    session$sendCustomMessage("fw-scroll-top", TRUE)
  }

  observeEvent(input$fw_privacy_link, show_privacy())

  # Deep link, read once: ?page=privacy opens the page; &section= (one of
  # FW_PRIVACY_SECTIONS) scrolls to it.
  observe({
    q = parseQueryString(session$clientData$url_search %||% "")
    # Replaced, not pushed, so Back doesn't reopen the form.
    if (identical(q$page, "contribute")) {
      nav_select("fw_nav", "contribute", session = session)
      updateQueryString(session$clientData$url_pathname %||% "/", mode = "replace",
                        session = session)
      return()
    }
    if (!identical(q$page, "privacy")) return()
    url_is_privacy(TRUE)
    nav_select("fw_nav", "privacy", session = session)
    if (isTRUE(q$section %in% FW_PRIVACY_SECTIONS)) {
      session$sendCustomMessage("fw-scroll-to-section", q$section)
    }
  }) |> bindEvent(session$clientData$url_search, once = TRUE)

  # Keep the address in step with the page. ignoreInit so a deep link's
  # address isn't cleared on arrival.
  observeEvent(input$fw_nav, {
    if (identical(input$fw_nav, "privacy")) {
      if (!url_is_privacy()) {
        before_privacy(last_page())
        updateQueryString("?page=privacy", mode = "push", session = session)
        url_is_privacy(TRUE)
      }
    } else {
      if (url_is_privacy()) {
        session$sendCustomMessage("fw-url-clear", TRUE)
        url_is_privacy(FALSE)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fw_nav, {
    if (!identical(input$fw_nav, "privacy")) last_page(input$fw_nav)
  })

  # Back/Forward changed the address: follow it with the page.
  observeEvent(input$fw_popstate, {
    q = parseQueryString(input$fw_popstate %||% "")
    if (identical(q$page, "privacy")) {
      url_is_privacy(TRUE)
      nav_select("fw_nav", "privacy", session = session)
    } else {
      url_is_privacy(FALSE)
      if (identical(input$fw_nav, "privacy")) {
        nav_select("fw_nav", before_privacy(), session = session)
      }
    }
  })
}

shinyApp(ui, server)
