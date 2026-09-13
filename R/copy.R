# copy.R
# EVERY user-facing string in the app lives in the copy deck. No module file
# contains hardcoded English. Renaming a navigation item or rewording a prompt
# is a one-line edit here.
#
# THE DECK IS THREE FILES, ONE LIST. This file holds the chrome and the pages;
# copy_contribute.R holds the contribute form; copy_export.R holds the
# spreadsheet, the report's tables and the question-list downloads. fw_t()
# reads all three through fw_copy_all(), so a module never needs to know which
# file a string is in. A section name may appear in only ONE of the three.
#
# PLACEHOLDERS. A string may carry {name} slots that the caller fills with
# fw_fill(): fw_fill(fw_t("plan", "r_table_note"), n = 42). Never paste a
# number into a sentence by hand; the slot is what lets the wording change
# without the code changing.
#
# House style: UK spelling, sentence case, active voice. A button says what
# happens ("Send submission", not "Submit") and keeps the same name through the
# whole flow.
#
# Strings marked [PLACEHOLDER] are stand-ins awaiting client copy. They are
# listed in HANDOVER.md. Search this file for "[PLACEHOLDER]" to find them all.

FW_COPY <- list(

  # ---- Application chrome ----------------------------------------------------

  app = list(
    title      = "FWISE",
    full_title = "FWISE: Freshwater Invasive Species Eradication database",
    tagline    = "A world evidence base for freshwater invasive species eradication",
    org        = "Freshwater Life",
    built_by   = "Built by Weird Fishes Advisory"
  ),

  # Navigation labels. Order here is the order in the navbar.
  nav = list(
    home       = "Home",
    explore    = "Explore the data",
    plan       = "Plan an eradication",
    contribute = "Contribute data",
    networking = "Networking",
    about      = "About"
  ),

  footer = list(
    last_updated  = "Data last updated",
    # Deliberately understated. Submissions being reviewed is a sign the database
    # is alive; it is not a metric and it is not a call to action.
    in_review_one  = "record in review",
    in_review_many = "records in review",
    doi_label     = "Zenodo DOI",
    doi_url       = "#",          # [PLACEHOLDER] awaiting the minted DOI
    github_label  = "Source code on GitHub",
    github_url    = "https://github.com/", # [PLACEHOLDER] awaiting the public repo URL
    licence       = "Data released under CC BY-NC 4.0 - non-commercial data. Code released under the MIT license.",
    logo_alt_fwise = "FWISE, the Freshwater Invasive Species Eradication database",
    logo_alt_wfa   = "Weird Fishes Advisory",
    # Each footer logo links out to the organisation it belongs to.
    fwise_url     = "https://www.freshwaterlife.org",
    wfa_url       = "https://weirdfishesadvisory.com" # [PLACEHOLDER] confirm the Weird Fishes Advisory URL
  ),

  # ---- Shared UI -------------------------------------------------------------

  stub = list(
    badge   = "In development",
    heading = "This page is not built yet",
    body    = paste(
      "We are building this section now. It will be ready for the public launch",
      "in October 2026. In the meantime you can contribute a record or browse",
      "the contacts list."
    ),
    action_contribute = "Contribute data",
    action_networking = "Find people to talk to"
  ),

  common = list(
    loading      = "Loading",
    no_results   = "No records match those filters.",
    clear_filters = "Clear filters",
    search       = "Search",
    of           = "of",
    required_note = "Fields marked with an asterisk are required.",
    info_icon_label = "More information about this field",
    # What an empty cell or a missing figure shows.
    empty_value  = "-"
  ),

  # ---- Accessibility -----------------------------------------------------------
  # Text that is announced rather than seen: aria labels, visually hidden
  # spans, the skip link.
  a11y = list(
    skip_link   = "Skip to main content",
    required    = " (required)",
    more_about  = "More information about {label}",
    page_n      = "Page {n}",
    pagination  = "Pagination",
    email_name  = "Email {name}"
  ),

  # ---- Charts --------------------------------------------------------------------
  # Axis titles, tick labels and hover fragments. The %{x}-style placeholders
  # in hover templates belong to plotly and are assembled in charts.R; only the
  # words are here.
  charts = list(
    x_year        = "Year the attempt began",
    y_cumulative  = "Attempts to date",
    x_attempts    = "Attempts",
    x_share       = "Share of attempts (%)",
    x_share_uses  = "Share of uses (%)",
    x_times_used  = "Times used",
    x_duration    = "Days from start to finish  \u2190 days   \u00b7   years \u2192",
    # Named ticks on the log axis, in step with FW_CHART$duration_ticks.
    duration_ticks = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"),
    other         = "Other",
    hover_by      = "By ",
    hover_days    = " days",
    hover_of      = " of ",
    # Shown in a chart's own slot when the selection gives it nothing to draw.
    empty         = "Nothing to draw for this selection."
  ),

  # ---- Maps ----------------------------------------------------------------------
  maps = list(
    # The basemap switcher. Named so it reads as a question the reader might
    # ask rather than as a list of vendors.
    basemap_plain     = "Plain",
    basemap_water     = "Water",
    basemap_terrain   = "Terrain",
    basemap_satellite = "Satellite",
    year_one  = "year",
    year_many = "years",
    # The title of a marker-sized group at coarse zoom, where the count is not
    # drawn. {n} is the number of attempts stacked at that point.
    stack_title = "{n} attempts at this point - zoom in or click to open them"
  ),

  # ---- Home ------------------------------------------------------------------

  home = list(
    title = "Freshwater eradication works. It has just not happened where it is needed most.", # [PLACEHOLDER] final headline belongs to the client
    lead  = paste(
      "[PLACEHOLDER] FWISE gathers eradication attempts against freshwater",
      "invasive animals from around the world, so practitioners can see what has",
      "been tried, where, and with what result, and importantly, what species has been saved."
    ),
    action_primary   = "Plan an eradication",
    action_secondary = "Explore the data",
    description = "What FWISE is, the shape of the evidence, and where to start."
  ),

  # ---- Explore ---------------------------------------------------------------

  explore = list(
    title = "Explore the data",
    description = paste(
      "The whole record of eradication attempts against freshwater invasive",
      "animals shown by place, species, method and outcome.",
      "Filter the data for your desired view."
    ),

    f_heading = "Filter the data",
    f_note = paste(
      "Filters are off by default, and apply to",
      "all charts and tables on this page."
    ),

    in_review = "in review",
    in_review_tip = paste(
      "Submissions waiting for review by our team. ",
      "They are not included here."
    ),

    map = "Where eradications have been attempted",
    map_note = paste(
      "Each marker is one attempt, coloured and labelled by outcome. Select one",
      "for the species targeted, who recorded it, and what happened. Switch the",
      "base map to Terrain to judge whether a waterbody is isolated."
    ),

    waterbody = "What kind of water",
    waterbody_note = paste(
      "Attempts by the kind of waterbody treated, with the outcome mix in each.",
      "The {n_word} most common are named and the rest gathered into Other."
    ),

    driver = "Why they were carried out",
    driver_note = "The main reason recorded for each attempt.",

    invasive = "What gets targeted",
    invasive_note = paste(
      "The {n_word} species named most often, counted once per attempt. This is a",
      "record of what has been REPORTED, so it reflects where the literature is",
      "as much as where the problem is."
    ),

    beneficiary = "What was meant to benefit",
    beneficiary_note = paste(
      "Read this one carefully. Beneficiary species are recorded far less",
      "consistently than targets - many attempts name none at all, and those",
      "that do tend to be the ones written up for a named endangered species.",
      "It shows what has been claimed, not what recovered."
    ),

    incoming = "Showing only attempts recorded by {name}.",
    incoming_clear = "Show all attempts"
  ),

  # ---- Species and map popups ------------------------------------------------

  species = list(
    no_image   = "No photograph available",
    alt_prefix = "Photograph of",
    unnamed_site = "Unnamed site",
    p_country  = "Country",
    p_species  = "Invasive species",
    p_beneficiary = "Species that benefited",
    p_outcome  = "Outcome",
    p_began    = "Ran",
    p_recorded_by = "Recorded by",
    p_also     = "Also recorded by",

    # ---- The detail panel, opened by clicking a marker ------------------------
    p_method       = "Method",
    p_method_desc  = "What was done",
    p_verified     = "Verified by",
    p_verified_notes = "Verification",
    p_waterbody    = "Kind of water",
    p_area         = "Area treated",
    p_driver       = "Reason",
    p_reference    = "Reference",
    p_read_source  = "Read the source",
    more_hint      = "Select for the full record",
    fig_prev       = "Previous species",
    fig_next       = "Next species",

    credit_fallback = "Wikimedia Commons",
    credit_sep      = " / ",
    card_label = "Eradication attempt",
    card_close = "Close",
    detail_label = "Eradication attempt, full record",
    image_note = paste(
      "Species photographs come from Wikimedia Commons and are credited to",
      "their authors. Where no photograph could be matched to a species, the",
      "record shows a placeholder."
    )
  ),

  # ---- Filters ---------------------------------------------------------------
  #
  # ONE set of labels for the one filter engine in R/filters.R, shared by the
  # report builder and the dashboard. Keys match the `copy` field of each entry
  # in FW_FILTERS. A page-specific heading lives with that page.

  filters = list(
    continent   = "Continent",
    country     = "Country",
    taxa        = "Kind of animal",
    species     = "Invasive species",
    beneficiary = "Species that benefited",
    method      = "Method used",
    regime      = "Still or flowing water",
    waterbody   = "Kind of waterbody",
    outcome     = "Outcome",
    years       = "Attempt began between",
    no_year     = "Include attempts with no recorded start year",

    # ---- The tips -------------------------------------------------------------
    #
    # ONE TIP PER FILTER, shown through fw_info()'s popover rather than printed
    # under the control. Nine controls each carrying a line of prose turned the
    # panel into a form to be worked through; the guidance is the same, it is
    # just asked for rather than issued. Keys are named in FW_FILTERS$<id>$tip.
    #
    # {n} and {min} are substituted at the call site in mod_plan_filters.R.
    tip_continent = paste(
      "Choose a continent to filter for."
    ),
    tip_country = paste(
      "Country of the eradication attempt(s)."
    ),
    tip_regime = paste(
      "Still water (Lotic) is lakes, ponds and reservoirs, etc; flowing water (lentic)",
      "is rivers and streams, etc."
    ),
    tip_waterbody = paste(
      "The specific kind of water body rather than the still/flowing split."
    ),
    tip_taxa = paste(
      "The broad group the invasive species belongs to - fish, crayfish, plant etc."
    ),
    tip_species = paste(
      "The species the attempt was trying to remove. Choosing more than one."
    ),
    tip_beneficiary = paste(
      "The species the attempt was meant to help. Note - this is recorded far less",
      "consistently than the invasive species, plus is likely not representative."
    ),
    tip_method = paste(
      "The eradication method used. Many attempts used more than one, so picking",
      "multiple matches an attempt that used any of them."
    ),
    tip_outcome = paste(
      "What the attempt achieved. Successful, Failed, Ongoing and Unknown."
    ),
    tip_years = paste(
      "Filters on the year the attempt began. The record starts at {min} but",
      "stays sparse until around 1950."
    ),
    tip_no_year = paste(
      "{n} attempts have no start year recorded. Leaving this ticked keeps",
      "them in whatever range you choose, so they are not silently dropped."
    ),

    range_of    = "to",
    all         = "All"
  ),

  # ---- Plan ------------------------------------------------------------------

  plan = list(
    title = "Plan an eradication",

    # ---- The introduction ----------------------------------------------------
    # ONE introduction, in the page header, in one treatment. It used to be two:
    # a line here and a separate "Build a report" block below the filter panel,
    # which meant the reader met the explanation of the page after the controls
    # it was explaining. A character vector; fw_page_header() draws one
    # paragraph per element at a single size and colour.
    description = c(
      paste(
        "Build a report of the eradication attempts that match your situation or interest by adjusting the filters below, then select",
        "**Build report**."
      ),
      paste(
        "You will get a map of where those attempts happened, the",
        "outcome, the methods used, with the matching",
        "records in full underneath. All of it is downloadable as a report or a",
        "spreadsheet."
      ),
      paste(
        paste(
          "Filters are set to \"All\" by default."
        )
      )
    ),

    # ---- Zero results --------------------------------------------------------
    zero_heading = "No attempts match those filters",
    zero_body = paste(
      "That combination has nothing in it. This is common and usually says more",
      "about what has been reported than about what is possible."
    ),
    zero_hint_lead = "Try relaxing one of these first:",
    zero_hint_none = "Try clearing a filter and building again.",

    build   = "Build report",
    rebuild = "Rebuild with these filters",
    stale   = "Filters have changed since this report was built.",
    clear   = "Clear all filters",
    download_heading = "Take this away",
    download = "Download as spreadsheet",
    download_note = paste(
      "A spreadsheet of this selection, with the field definitions,",
      "the filters you applied and the caveats on their own sheets."
    ),

    # ---- The HTML report -----------------------------------------------------
    # The spreadsheet is the data; this is the document. Two different jobs, so
    # both buttons are offered rather than one being the "real" one.
    download_html = "Download as report",
    download_html_note = paste(
      "Self-contained report on FWISE letterhead, with the charts, the map",
      "and table. All plots are interactive, and data is available as a spreadsheet",
      "inside the report. Open it in any browser and use 'Save as PDF' to print it."
    ),

    # ---- Inside the report ---------------------------------------------------
    # The toolbar the reader sees at the top of the downloaded file. It is the
    # only interactive chrome in the document and it does not print.
    html_print = "Save as PDF",
    html_csv   = "Download the data (CSV)",
    html_xlsx  = "Download the data (Excel)",
    html_print_hint = paste(
      "Save as PDF opens the browser's print dialogue - choose Save as PDF as",
      "the destination. These buttons do not appear in the printed copy."
    ),
    # Said once, at the top, because a file that travels by email has to explain
    # itself to whoever it reaches.
    html_about = c(
      paste("This file is self-contained. The charts, the tables and the data",
            "inside work with no internet connection."),
      paste("The map is the exception: the tiles require connection to Carto's",
            "servers. The other elements work offline."),
      paste("The data is inside this file. The two download buttons above give",
            "you every field of every matching attempt, including the values",
            "this page shortens."),
      "Read the caveats at the end before quoting any figure from this report."
    ),
    html_map_note = paste(
      "The map is interactive - drag, zoom, and select a marker for the record.",
      "Its background needs an internet connection; the markers do not."
    ),

    report_title    = "Eradication attempt planning report",
    report_subtitle = "Generated from the FWISE database on {date}",
    report_selection = "What this report covers",
    # NOT "Where these attempts happened" - that is the map's heading, three
    # inches above, and two identical headings in one document read as a
    # duplication rather than as two views of the same question.
    report_where     = "Attempts by country",
    report_where_note = paste(
      "The map above plots each attempt at its own coordinates. This is what",
      "the map is read for on paper, and it is what is left when the tiles",
      "cannot load."
    ),
    report_table_note = paste(
      "All {n} matching attempts. Species and method lists are shortened here",
      "to keep the columns readable - the downloads contain full values."
    ),
    report_footer = paste(
      "FWISE, the Freshwater Invasive Species Eradication database. Data",
      "released under CC BY-NC 4.0. Read the caveats above before quoting any",
      "figure in this report."
    ),

    # ---- Filters -------------------------------------------------------------
    # The field labels are shared with the dashboard and live in FW_COPY$filters.
    # Only the panel heading is specific to this page.
    f_heading   = "Describe your situation",
    f_lead = paste(
      "All fields are optional. Fields default to 'All'."
    ),
    built_announce = "Report built. {n} attempts match your description.",

    # ---- Results -------------------------------------------------------------
    r_heading    = "What the matching attempts show",
    r_attempts   = "attempts",
    r_countries  = "countries",
    r_species    = "invasive species",
    r_methods    = "methods used",
    r_years      = "year range",
    r_outcomes   = "Outcomes",
    r_outcome_note = paste(
      "All four states are shown. Failure is as important to know about as success."
    ),
    r_map        = "Where these attempts happened",
    r_map_note   = paste(
      "Each marker is labelled with its outcome as well as coloured by it."
    ),
    r_map_missing = "{n} of these attempts have no coordinates and are not on the map.",

    r_waterbody  = "What kind of water",
    r_waterbody_note = paste(
      "Attempts by the kind of waterbody treated, with the outcome mix in each.",
      "The {n_word} most common are named and the rest gathered into Other."
    ),

    r_method     = "Outcomes of methods within selection",
    r_method_note = paste(
      "Outcomes within each method, with the number of attempts beside it."
    ),
    r_method_mode  = "Show",
    r_method_share = "Share of attempts",
    r_method_count = "Number of attempts",
    r_method_missing = "{n} of these attempts have no method recorded and are not in the two method charts.",

    r_method_wb  = "Methods used within each kind of waterbody",
    r_method_wb_note = paste(
      "The methods used in each kind of waterbody, counted once per attempt."
    ),
    # Uses, not attempts: an attempt with two methods is two uses here.
    r_method_wb_share = "Share of uses",
    r_method_wb_count = "Number of uses",

    # ---- The species tiles ----------------------------------------------------
    r_tile_attempt  = "attempt",
    r_tile_attempts = "attempts",
    r_invasive   = "What these attempts targeted",
    r_invasive_note = paste(
      "The {n_word} invasive species named most often in this selection, counted",
      "once per attempt, out of {n} species in total. The bar under each is its outcome",
      "mix."
    ),
    r_beneficiary = "What benefited",
    r_beneficiary_note = paste(
      "Beneficiary species are recorded far less consistently than targets, sometimes not at all.",
      "Where they are stated, this is not a complete list of what benefited, but a record of what was claimed.",
      "These are the {n_word} named most often of {n} species."
    ),

    r_duration   = "How long these attempts took",
    r_duration_note = paste(
      "Start to finish, on a log scale. Each point is one attempt."
    ),
    r_duration_missing = paste(
      "Based on the {n} of these attempts with both a start and an end recorded."
    ),
    r_cumulative = "How the record has built up",
    r_cumulative_note = paste(
      "Cumulative attempts by the year they began. This is a record of",
      "reporting, so a rise can mean more work or better reporting of it.",
      "Not complete: many attempts have no start year recorded, and those are not shown."
    ),
    r_table      = "The matching attempts",
    r_table_note = paste(
      "All {n} of them, a page at a time. Long species and method lists are",
      "shortened here; the spreadsheet contains all in full."
    ),
    r_table_size    = "Rows per page",
    # The results table. Column headings, and how a long list is shortened.
    col_site     = "Site",
    col_country  = "Country",
    col_began    = "Began",
    col_species  = "Invasive species",
    col_methods  = "Methods",
    col_outcome  = "Outcome",
    col_contact  = "Contact",
    more_suffix  = " +{n} more",
    r_table_showing = "Showing",

    caveats_heading = "Important to note whilst reviewing the visuals and data"
  ),

  # ---- About -----------------------------------------------------------------

  about = list(
    # [PLACEHOLDER] THE BODY COPY ON THIS PAGE IS LOREM IPSUM, on the client's
    # instruction, while they write the real wording. The headings are the real
    # ones and the structure is settled, so replacing this is a copy edit rather
    # than a rebuild. Two things here are NOT placeholder and must survive that
    # edit: `scale`, whose {placeholders} are filled from the loaded data, and
    # `citation`, which is the citation format itself.
    title = "About FWISE",
    description = paste(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
      "tempor incididunt ut labore et dolore magna aliqua."
    ),

    database_heading = "What is in FWISE",
    database = paste(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
      "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim",
      "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea",
      "commodo consequat."
    ),
    database2 = paste(
      "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum",
      "dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non",
      "proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    ),

    # {placeholders} are filled from the loaded data by fw_about_scale().
    scale = paste(
      "{attempts} eradication attempts across {countries} countries, against",
      "{species} invasive species, going back to {year} - curated from the",
      "published literature and contributed directly by {contributors} people."
    ),
    scale_action = "Become the {n}.",

    method_heading = "How it was built",
    method = paste(
      "Sed ut perspiciatis unde omnis iste natus error sit voluptatem",
      "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab",
      "illo inventore veritatis et quasi architecto beatae vitae dicta sunt."
    ),
    method2 = paste(
      "Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut",
      "fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem",
      "sequi nesciunt."
    ),
    # [PLACEHOLDER] THE GAPS MATTER AS MUCH AS THE COVERAGE and the client is
    # explicit about wanting them stated rather than glossed. The caveats panel
    # carries the detail; the real wording for this paragraph has to say plainly
    # that there is one.
    method3 = paste(
      "Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet,",
      "consectetur, adipisci velit, sed quia non numquam eius modi tempora",
      "incidunt ut labore et dolore magnam aliquam quaerat voluptatem."
    ),

    images_heading = "Species photographs",

    cite_heading = "How to cite FWISE",
    cite = paste(
      "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse",
      "quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo",
      "voluptas nulla pariatur."
    ),
    citation = paste(
      "Freshwater Life ({year}). FWISE: Freshwater Invasive Species",
      "Eradication database, release {release} ({n} attempts).",
      "https://doi.org/[PLACEHOLDER]"
    ),

    licence_heading = "Licence",
    links_heading = "Links",
    link_fwise = "Freshwater Life",
    link_zenodo = "The archived dataset on Zenodo",

    # ---- Feedback ------------------------------------------------------------
    # NO BACKEND. See the note at the top of mod_about.R.
    fb_heading = "Tell us what is wrong",
    fb_body = paste(
      "If a record is wrong, a species is misnamed, or something on this site",
      "does not work, we would rather hear it than not. There are no accounts",
      "here, so this opens a message in your own email program - you send it."
    ),
    fb_where = "What is this about",
    fb_label = "What would you like to tell us",
    fb_placeholder = "",
    fb_action = "Open this in your email",
    fb_note = paste(
      "Nothing is sent from this page. Your message opens in your own email",
      "program so you can see it and send it yourself."
    ),
    fb_empty = "Write your message first, then select Open this in your email.",
    fb_sent = "Your email program should now be open with the message ready.",
    fb_subject = "FWISE feedback",
    feedback_email = "hello@example.org" # [PLACEHOLDER] awaiting the real address
  ),

  # ---- Contacts --------------------------------------------------------------

  networking = list(
    title = "Networking",
    description = "The people behind the records in FWISE, and how to reach them.",
    # [PLACEHOLDER] framing line, to be replaced with the client's wording
    intro = paste(
      "Every record in FWISE has been submitted by a practitioner - the",
      "conservationists and researchers who ran these eradications or wrote them up.",
      "If you are planning something similar, these are the people worth talking",
      "to. Find someone working in your region, or on the species you are dealing",
      "with, and get in touch."
    ),
    # [PLACEHOLDER] closing note, to be replaced with the client's wording
    outro_heading = "Not sure who to ask?",
    outro = paste(
      "If the right person is not obvious from this list, write to",
      "the FWISE team and we will try to point you to someone who can help."
    ),
    outro_action = "Email the FWISE team",
    outro_email  = "hello@example.org", 

    filter_continent = "Continent",
    filter_country   = "Country",
    filter_search    = "Search by name or organization",
    filter_all       = "All",

    # THE COVERAGE LINE. The page must not imply reach it does not have. Most
    # records have a named contact; a good many of those have no published
    # address, and a substantial minority have no contact at all. Saying so is
    # what stops a visitor concluding that an absent person is a dead end rather
    # than simply someone we hold no address for. The numbers are computed from
    # the data, never written down here.
    coverage = paste(
      "This directory covers {reachable} of the {total} attempts in FWISE.",
      "{no_email} of the {contacts} people listed have no published email",
      "address, and {no_contact} attempts have no contact recorded at all.",
      "If the person you want is not here, the reference on the attempt itself",
      "is usually the next best route."
    ),

    summary_contacts     = "contacts",
    summary_countries    = "countries",
    summary_continents   = "continents",
    summary_showing      = "Showing",

    col_name         = "Name",
    col_organisation = "Organization",
    col_continent    = "Continent",
    col_country      = "Country",
    col_attempts     = "Attempts",
    col_contact      = "Contact",
    col_attempts_link = "Attempts link",

    email_action     = "Email",
    email_none       = "",
    email_none_label = "No public email address",
    view_attempts    = "See their attempts",
    no_organisation  = "Not recorded",

    attempts_one  = "attempt",
    attempts_many = "attempts",

    page_of       = "Page",
    page_size     = "Contacts per page",
    page_showing  = "Showing"
  )
)

# ---- Reading the deck --------------------------------------------------------

#' The whole copy deck as one list
#'
#' Assembled on every call rather than once at startup, because the three files
#' are sourced in an order that depends on who is sourcing them (Shiny uses C
#' byte order; a test script using list.files() gets the locale's order), and a
#' merge cached on the first call could be missing a file. It is three list
#' concatenations, which costs nothing.
fw_copy_all <- function() {
  c(FW_COPY, FW_COPY_CONTRIBUTE, FW_COPY_EXPORT)
}

#' A string from the deck: fw_t("networking", "title")
#'
#' Stops with the path if it is not defined, so a typo fails at render rather
#' than printing NULL. NEVER call this at the top level of a file in R/: the
#' deck is not complete until every file has been sourced.
fw_t <- function(...) {
  path <- c(...)
  out <- fw_copy_all()
  for (p in path) {
    out <- out[[p]]
    if (is.null(out)) stop("No copy defined at: ", paste(path, collapse = " > "),
                           call. = FALSE)
  }
  out
}

#' Fill the {name} slots in a string
#'
#' fw_fill(fw_t("plan", "r_table_note"), n = 42). Every occurrence of a slot is
#' filled, values are coerced to character, and a slot with no value supplied
#' is left in place so it is visible rather than silently blank.
fw_fill <- function(text, ...) {
  values <- list(...)
  for (nm in names(values)) {
    text <- gsub(paste0("{", nm, "}"), as.character(values[[nm]]), text,
                 fixed = TRUE)
  }
  text
}

#' A small number in words, for prose that names a limit
#'
#' "the ten most common" reads better than "the 10 most common", and the copy
#' is filled from FW_TOP_N rather than carrying the number itself. Beyond
#' twenty the digits are used, which is also house style.
fw_num_word <- function(n) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  n <- as.integer(n)
  if (!is.na(n) && n >= 1L && n <= length(words)) words[n] else as.character(n)
}
