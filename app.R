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
      tags$meta(name = "description", content = fw_t("app", "tagline")),
      # Compiled from www/scss/ with the tokens from R/brand.R injected. See
      # fw_compile_css() for why the cache key has to include the partials.
      tags$style(HTML(fw_compile_css("www/scss/main.scss")))
    ),
    fw_loader(),
    # A SPINNING BADGE ON ANY CHART OR MAP THAT IS TAKING A WHILE. Shiny's own
    # busy indicators decide when - an output marked .recalculating, after the
    # delay, so a quick redraw never flashes one - and _components.scss decides
    # what: its default is a colour-filled mask, which would draw the badge as
    # a flat silhouette. No page-top pulse bar; the loader is the one bar.
    useBusyIndicators(spinners = TRUE, pulse = FALSE),
    busyIndicatorOptions(spinner_delay = "400ms", spinner_size = "64px"),
    fw_skip_link(),
    fw_popover_script(),
    fw_client_script(),
    fw_live_region("fw_announce")
  ),

  nav_panel(fw_t("nav", "home"),       value = "home",       mod_home_ui("home", fw_headline_stats(FW_DATA))),
  nav_panel(fw_t("nav", "explore"),    value = "explore",    mod_explore_ui("explore", FW_FILTER_CHOICES)),
  nav_panel(fw_t("nav", "plan"),       value = "plan",       mod_plan_ui("plan")),
  nav_panel(fw_t("nav", "contribute"), value = "contribute", mod_contribute_ui("contribute")),
  nav_panel(fw_t("nav", "networking"), value = "networking", mod_networking_ui("networking")),
  nav_panel(fw_t("nav", "about"),      value = "about",      mod_about_ui("about")),

  footer = fw_footer(FW_LAST_UPDATED, FW_IN_REVIEW)
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

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
}

shinyApp(ui, server)
