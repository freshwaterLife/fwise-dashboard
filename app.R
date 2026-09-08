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
# It also means ANY file placed in R/ runs on boot. R/data_prep.R is a build
# script, so its body is wrapped in a function and only executes when the file is
# run directly with Rscript. Keep that guard if you add another script here.

# ---- Startup -----------------------------------------------------------------

# Loaded once for the life of the process, not once per session. The tables are
# read-only and identical for every visitor.
FW_DATA <- fw_load_data()

# The release metadata that travels with the data. Read once, alongside it.
FW_META <- fw_load_metadata()

# Dropdown choices come from the data, so the client's ongoing cleaning flows
# through without a code change.
FW_CHOICES <- fw_startup_choices(FW_DATA)

FW_LAST_UPDATED <- fw_last_updated(FW_DATA, FW_META)

# Records waiting on review: pending rows carried into the schema, plus whatever
# is still sitting in the submissions inbox. Read once at startup like everything
# else, so it is accurate as of the last republish rather than live.
FW_IN_REVIEW <- fw_review_count(FW_DATA)

message("FWISE startup: ", nrow(FW_DATA$attempt), " attempts, ",
        nrow(FW_DATA$contact), " contacts, released ", format(FW_LAST_UPDATED),
        ", source ", if (fw_source_is_remote()) "remote" else "local")

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
      tags$link(rel = "icon", type = "image/svg+xml", href = "img/favicon.svg"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = fw_t("app", "tagline")),
      # Compiled from www/scss/. sass caches the result, so this is a one-off
      # cost at startup rather than per request.
      #
      # cache_key_extra IS NOT OPTIONAL. sass keys its cache on the input it is
      # handed, which here is main.scss alone - it does not look at what that
      # file @imports. Without the digest below, an edit to _tokens.scss or
      # _components.scss compiles to the previously cached CSS and the change
      # appears to have done nothing, even across a full restart. Hashing every
      # file in the directory is what makes the cache notice.
      tags$style(sass::sass(
        sass::sass_file("www/scss/main.scss"),
        options = sass::sass_options(output_style = "compressed"),
        cache_key_extra = fw_scss_digest()
      ))
    ),
    fw_skip_link(),
    fw_popover_script(),
    fw_client_script(),
    fw_live_region("fw_announce")
  ),

  nav_panel(fw_t("nav", "home"),       value = "home",       mod_home_ui("home")),
  nav_panel(fw_t("nav", "explore"),    value = "explore",    mod_explore_ui("explore")),
  nav_panel(fw_t("nav", "plan"),       value = "plan",       mod_plan_ui("plan")),
  nav_panel(fw_t("nav", "contribute"), value = "contribute", mod_contribute_ui("contribute")),
  nav_panel(fw_t("nav", "networking"), value = "networking", mod_networking_ui("networking")),
  nav_panel(fw_t("nav", "about"),      value = "about",      mod_about_ui("about")),

  footer = fw_footer(FW_LAST_UPDATED, FW_IN_REVIEW)
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  mod_home_server("home", FW_DATA)
  mod_explore_server("explore", FW_DATA)
  mod_plan_server("plan", FW_DATA, FW_META)
  mod_contribute_server("contribute", FW_DATA, FW_CHOICES)
  mod_networking_server("networking", FW_DATA)
  mod_about_server("about", FW_DATA)

  # Cross-page links (the stub actions, the hero buttons) set this rather than
  # each module reaching into the navbar itself.
  observeEvent(input$fw_nav_to, {
    nav_select("fw_nav", input$fw_nav_to, session = session)
  })
}

shinyApp(ui, server)
