# app.R
# Entry point. Posit Connect Cloud expects to find this at the repository root.
#
# Everything in R/ is sourced at startup, then the data is loaded ONCE and shared
# by every session. Nothing here should do per-session work.

library(shiny)
library(bslib)
library(htmltools)

# NOTE FOR MAINTAINERS: Shiny automatically sources every .R file in the R/
# directory at startup. That is a documented Shiny feature, not something this
# file does, so there is deliberately no sourcing loop here. Adding one would
# source every module twice.
#
# It also means ANY file placed in R/ runs on boot. Scripts that build or change
# data therefore live in dev/, never here. If one ever has to sit in R/, guard
# its body with `if (sys.nframe() == 0L)` so it runs only under Rscript.

# ---- Startup -----------------------------------------------------------------

# Loaded once for the life of the process, not once per session. The tables are
# read-only and identical for every visitor.
FW_DATA <- fw_load_data()

# The release metadata that travels with the data. Read once, alongside it.
FW_META <- fw_load_metadata()

# Dropdown choices come from the data, so the client's ongoing cleaning flows
# through without a code change.
FW_CHOICES <- fw_startup_choices(FW_DATA)

# The Explore page's filter bar is drawn in its static UI, so its choices are
# needed before any session exists. See mod_explore_ui().
FW_FILTER_CHOICES <- fw_filter_choices(FW_DATA)

FW_LAST_UPDATED <- fw_last_updated(FW_DATA, FW_META)

# Records waiting on review: pending rows carried into the schema, plus whatever
# is still sitting in the submissions inbox. Read once at startup like everything
# else, so it is accurate as of the last republish rather than live.
FW_IN_REVIEW <- fw_review_count(FW_DATA)

# Naming the mode is the first thing to check when a deployment misbehaves: it
# says in one word whether the token was picked up, and submissions can only be
# written at all in api mode.
message("FWISE startup: ", nrow(FW_DATA$attempt), " attempts, ",
        nrow(FW_DATA$contact), " contacts, released ", format(FW_LAST_UPDATED),
        ", mode ", fw_data_mode(),
        if (fw_data_mode() == "api") paste0(" (", FW_DATA_REPO, "@", FW_DATA_REF, ")") else "")

# Where the newsletter and feedback forms write: "sheets" once FWISE_FORMS_SHEET_ID
# and GS4_SA_KEY_B64 are both set, "local" (dev/forms/) otherwise.
message("FWISE startup: forms ", fw_forms_mode())

# THE PDF REPORT NEEDS QUARTO (1.4 or later, for Typst), and this line is where
# a deployment says whether it has it. Without it the Plan page's download
# picker drops the PDF and says why; every other download is unaffected.
message("FWISE startup: quarto ",
        if (fw_pdf_available()) paste0(fw_quarto_version(), " at ", fw_quarto_path())
        else "NOT FOUND - the PDF report is unavailable")

# ---- UI ----------------------------------------------------------------------

ui <- page_navbar(
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
      # The description, canonical, Open Graph and structured data, and the
      # rule that hides the loader with JavaScript off. See "What a crawler
      # reads" in R/ui_helpers.R.
      fw_head_meta(),
      fw_noscript_head(),
      # Compiled from www/scss/ with the tokens from R/brand.R injected. See
      # fw_compile_css() for why the cache key has to include the partials.
      tags$style(HTML(fw_compile_css("www/scss/main.scss")))
    ),
    fw_noscript(),
    fw_loader(),
    # A SPINNING BADGE ON ANY CHART OR MAP THAT IS TAKING A WHILE. Shiny's own
    # busy indicators decide when - an output marked .recalculating, after the
    # delay, so a quick redraw never flashes one - and _components.scss decides
    # what: its default is a colour-filled mask, which would draw the badge as
    # a flat silhouette. No page-top pulse bar; the loader is the one bar.
    #
    # THE DELAY IS SHARED with the map overlay in fw_client_script(), which
    # covers the one thing this cannot: markers swapped through a leaflet proxy
    # never mark their output .recalculating. See FW_SPINNER_DELAY_MS.
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
  # No navbar link: hidden by CSS, reached from the footer and ?page=privacy.
  # See R/mod_privacy.R.
  nav_panel(fw_t("nav", "privacy"),    value = "privacy",    mod_privacy_ui("privacy")),

  footer = fw_footer(FW_LAST_UPDATED, FW_IN_REVIEW)
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # "DISCONNECTED FROM THE SERVER" OFTEN (client, 29 Sept 2026). A dropped
  # websocket - a network blip, a proxy recycling the connection, a laptop
  # waking - used to end the session there and then. With this the browser
  # reconnects to the same session when it can, and Shiny shows its own
  # "reconnecting" notice rather than the grey screen. The keepalive in
  # fw_client_script() stops an idle connection being closed in the first
  # place.
  session$allowReconnect(TRUE)

  mod_home_server("home", FW_DATA)
  mod_explore_server("explore", FW_DATA, FW_IN_REVIEW)
  mod_plan_server("plan", FW_DATA, FW_META)
  mod_contribute_server("contribute", FW_DATA, FW_CHOICES)
  mod_networking_server("networking", FW_DATA)
  mod_about_server("about", FW_DATA, FW_META)

  # Cross-page links (the stub actions, the hero buttons) set this rather than
  # each module reaching into the navbar itself.
  observeEvent(input$fw_nav_to, {
    nav_select("fw_nav", input$fw_nav_to, session = session)
  })

  # ---- The two forms ----------------------------------------------------------
  #
  # One of each for the whole app. Every fw_form_open() button sets
  # fw_open_form; each module opens only for its own name. See R/forms.R.
  open_form <- reactive(input$fw_open_form)
  mod_newsletter_server("newsletter", open = open_form)
  mod_feedback_server("feedback", open = open_form, nav = reactive(input$fw_nav))

  # ---- The privacy page and the address bar -----------------------------------
  #
  # NOTHING ELSE IN THE APP READS OR WRITES THE URL (checked 2 Oct 2026), so
  # the query string is this page's alone: ?page=privacy while it is open,
  # nothing otherwise. If another page ever wants the URL, this is the code to
  # make room in.
  url_is_privacy <- reactiveVal(FALSE)
  # The last ordinary page shown, and - copied from it when the reader goes
  # to the privacy page from inside the app - where Back takes them from
  # there. Copied only then: arriving at the privacy page by Back or Forward
  # must not overwrite it, or Back twice lands on the page they left FOR the
  # privacy page rather than the one they came from.
  last_page      <- reactiveVal("home")
  before_privacy <- reactiveVal("home")

  show_privacy <- function() {
    nav_select("fw_nav", "privacy", session = session)
    session$sendCustomMessage("fw-scroll-top", TRUE)
  }

  observeEvent(input$fw_privacy_link, show_privacy())

  # A DEEP LINK, read once, when the session starts: ?page=privacy opens the
  # page, and &section= (one of FW_PRIVACY_SECTIONS, nothing else) scrolls to
  # that heading once the page is showing.
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (!identical(q$page, "privacy")) return()
    url_is_privacy(TRUE)
    nav_select("fw_nav", "privacy", session = session)
    if (isTRUE(q$section %in% FW_PRIVACY_SECTIONS)) {
      session$sendCustomMessage("fw-scroll-to-section", q$section)
    }
  }) |> bindEvent(session$clientData$url_search, once = TRUE)

  # Keep the address in step with the page. ignoreInit: the first value is the
  # page the app opened on, which a deep link is about to change; acting on it
  # would clear the address the reader arrived with.
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

  # Back or Forward moved the address: move the page to match. The address is
  # already right, so url_is_privacy is set to agree rather than written again.
  observeEvent(input$fw_popstate, {
    q <- parseQueryString(input$fw_popstate %||% "")
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
