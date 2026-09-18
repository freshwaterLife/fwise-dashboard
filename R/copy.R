# copy.R
# EVERY user-facing string in the app lives in the copy deck. No module file
# contains hardcoded English.

FW_COPY <- list(

  # ---- Application chrome ----------------------------------------------------

  app = list(
    title      = "FWISE",
    full_title = "FWISE: Freshwater Invasive Species Eradication database",
    tagline    = "A world evidence base for freshwater invasive species eradication",
    # Read out by a screen reader while the app is starting, and on nothing
    # else - the loader is the badge and a bar. See fw_loader().
    loading    = "Loading FWISE",
    built_by   = "This tool was built by Weird Fishes Advisory.",
    data_by    = "FWISE database is built and maintained by Freshwater Life and friends.",
    illustrated_by = "Logo and illustrations by Georgie Bull."
  ),

  # Navigation labels. Order here is the order in the navbar.
  nav = list(
    home       = "The solution",
    explore    = "Explore the database",
    plan       = "Plan an eradication",
    contribute = "Add a record",
    networking = "Connect with community",
    about      = "About"
  ),

  footer = list(
    last_updated  = "Data last updated",
    in_review_one  = "record in review",
    in_review_many = "records in review",
    doi_label     = "Zenodo DOI",
    doi_url       = "#",          
    github_label  = "Source code on GitHub",
    github_url    = "https://github.com/freshwaterLife/fwise-dashboard", 
    licence       = "Data released under CC BY-NC 4.0 - non-commercial data. Code released under the MIT license.",
    logo_alt_fwise = "FWISE, the Freshwater Invasive Species Eradication database",
    logo_alt_wfa   = "Weird Fishes Advisory",
    # The collaborators names.
    logo_alt_ucsc    = "University of California, Santa Cruz",
    logo_alt_scripps = "Scripps Institution of Oceanography, UC San Diego",
    logo_alt_fwl     = "Freshwater Life",
    logo_alt_issg    = "IUCN SSC Invasive Species Specialist Group",
    # Collaborator logo links out to the organisation it belongs to. The FWISE
    # logo does not: it goes to this app's own Welcome page, as the navbar's
    # does.
    wfa_url       = "https://www.weirdfishes.fish",
    ucsc_url      = "https://www.ucsc.edu/",
    scripps_url   = "https://scripps.ucsd.edu/",
    fwl_url       = "https://fwlife.org/",
    issg_url      = "https://issg.org/"
  ),

  # ---- Shared UI -------------------------------------------------------------

  common = list(
    loading      = "Loading",
    no_results   = "No records match those filters.",
    clear_filters = "Clear filters",
    search       = "Search",
    of           = "of",
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
  # in hover templates belong to plotly and are assembled in charts.R;
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
    # The accessible name of a group of markers. The count is drawn inside the
    # circle, so this is what says what the number means - to a screen reader,
    # and on hover. {n} is the number of attempts grouped at that point.
    stack_title = "{n} attempts at this point - zoom in or click to open them"
  ),

  # ---- Home (the Welcome page) -----------------------------------------------

  home = list(
    title = paste(
      "Freshwaters cover <1% of earth yet are home to 45% of all threatened animal species.",
      "Eradicating freshwater invasives is the one of the best way to save them from extinction."
    ),
    lead = c(
      "The **Freshwater Invasive Species Eradication Database**. FWISE shows the world, for the first time, **what works**, **where**, and **how**.",
      paste(
        "Use it now to [[explore|understand this solution]],",
        "[[plan|plan a new eradication]], [[contribute|add your own data]],",
        "and [[networking|connect with others]]."
      )
    ),

    # THE ONE SENTENCE across the top of the page, in the teal box. Both
    # numbers are filled from the data and never typed: {attempts} is attempts
    # recorded as successful, {protected} the distinct beneficiaries of those.
    # The ** pairs set them in bold.
    kpi_sentence = paste(
      "Over **{attempts}** successful eradication attempts have led to **{protected}** species protected.",
      "Click the species images to read case studies from around the globe."
    ),

    # ---- Success stories ----
    # ONE ENTRY PER STORY, A-Z by continent, keyed as FW_HOME_IMG$stories is.
    # The order on the page is FW_HOME_ORDER's, not this. Every string is a
    # [PLACEHOLDER] until the client supplies the stories.
    stories_open = "Read the {continent} success story: {name}",
    card_close = "Close story",
    image_placeholder = "Image to come",
    stories = list(
      africa = list(
        continent = "Africa",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Fiery redfin",    alt = "Illustration of a fiery redfin")
      ),
      asia = list(
        continent = "Asia",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Little grebe",        alt = "Illustration of a little grebe")
      ),
      europe = list(
        continent = "Europe",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Freshwater pearl mussel", alt = "Illustration of a freshwater pearl mussel")
      ),
      latin_america = list(
        continent = "Latin America",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Valcheta frog", alt = "Illustration of a Valcheta frog")
      ),
      north_america = list(
        continent = "North America",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Apache trout",     alt = "Illustration of an Apache trout")
      ),
      oceania = list(
        continent = "Oceania",
        title   = "[PLACEHOLDER] Story title",
        summary = "[PLACEHOLDER] A sentence or two on what was removed, where, and what came back.",
        body    = "[PLACEHOLDER] The longer account of the eradication: the site, the method, how long it took, and how the beneficiary species has responded since.",
        beneficiary = list(name = "Golden galaxias", alt = "Illustration of a golden galaxias")
      )
    ),

    # ---- Current work and gaps ----
    # The caption IS the legend: the two named pieces are set in the map's
    # blue and amber (map_now_text, map_next_text). No swatches.
    map_slider_label = "Reveal the priority countries map over the successful eradications map",
    map_caption = c(
      "Use the slider to move from ",
      now  = "past successes",
      " to ",
      later = "future opportunities"
    )
  ),

  # ---- Explore ---------------------------------------------------------------

  explore = list(
    title = "Explore the data",
    description = paste(
      "See where freshwater eradications have been tried, then open any",
      "attempt on the map for its full record."
    ),

    # ---- The database panel --------------------------------------------------
    # The whole database, never filtered. Counts, all the same size, no rate.
    db_heading = "The database",
    db_span = "Attempts recorded from {from} to {to}.",
    db_attempts    = "attempts recorded",
    db_countries   = "countries",
    db_invasive    = "invasive species targeted",
    db_beneficiary = "species recorded as protected",
    db_beneficiary_tip = paste(
      "Species protected by the attempt, as recorded by the person",
      "reporting it. It is recorded less consistently than the species targeted and is likely not a complete record."
    ),
    in_review = "in review",
    in_review_tip = paste(
      "Submissions waiting for review by our team. ",
      "They are not included here."
    ),

    # ---- The filters ---------------------------------------------------------
    f_heading = "Narrow the list",
    f_note = "Filters are off by default. Everything below follows them as you change them.",

    # ---- The summary graphics ------------------------------------------------

    method = "Methods used, and how they turned out",
    method_note = paste(
      "One bar per method, split by outcome. An attempt that used more than one",
      "method is counted once under each of them, so the bars add up to more",
      "than the number of attempts. Hover a segment for its count."
    ),
    cumulative = "Eradication attempts over time are increasing",
    cumulative_note = paste(
      "Attempts counted from the year each one began, adding up over time and",
      "split by the outcome of the attempt. Attempts with no start year are not on this chart."
    ),

    # ---- The map -------------------------------------------------------------
    map = "Where these attempts happened",
    map_note = paste(
      "Each marker is one attempt, coloured and labelled by outcome. Hover for",
      "a summary, select for the full record."
    )
  ),

  # ---- Species and map popups ------------------------------------------------

  species = list(
    no_image   = "No photograph available",
    fig_none   = "None noted",
    alt_prefix = "Photograph of",
    unnamed_site = "Unnamed site",
    p_country  = "Country",
    p_species  = "Invasive species",
    p_beneficiary = "Species protected",
    p_none     = "Not recorded",
    fig_invasive = "Targeted",
    fig_beneficiary = "Protected",
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
    beneficiary = "Species protected",
    taxa_beneficiary = "Kind of animal protected",
    family      = "Fish family",
    family_beneficiary = "Fish family protected",
    method      = "Method used",
    regime      = "Still or flowing water",
    waterbody   = "Kind of waterbody",
    outcome     = "Outcome",
    size        = "Size of the area treated",
    years       = "Attempt began between",
    no_year     = "Include attempts with no recorded start year",
    no_size     = "Include attempts with no recorded size",
    unit_ha     = "hectares, still water",
    unit_km     = "kilometres, flowing water",
    size_in_ha  = "in hectares",
    size_in_km  = "in kilometres",
    size_in_both = "in hectares and kilometres",
    # The unit printed after each figure on a size slider's handles and ends.
    unit_short_ha = "ha",
    unit_short_km = "km",

    # ---- The tips -------------------------------------------------------------
    #

    tip_continent = paste(
      "Choose a continent to filter for."
    ),
    tip_country = paste(
      "Country of the eradication attempt(s). Only countries with attempts",
      "recorded in FWISE are listed. If yours is not here, filter by continent",
      "instead - that will show you the closest evidence there is."
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
    tip_taxa_beneficiary = paste(
      "The broad group the protected species belongs to - fish,",
      "amphibian, bird etc. Recorded less consistently than the invasive side."
    ),
    tip_family = paste(
      "The family the invasive fish belongs to - trout and salmon are",
      "Salmonidae, carp and minnows Cyprinidae. Shown while Fish is picked above."
    ),
    tip_family_beneficiary = paste(
      "The family the protected fish belongs to. Shown while Fish is picked",
      "under the kind of animal protected."
    ),
    tip_method = paste(
      "The eradication method used. Many attempts used more than one, so picking",
      "multiple matches an attempt that used any of them."
    ),
    tip_outcome = paste(
      "What the attempt achieved. Successful, Failed, Ongoing and Unknown."
    ),
    tip_size = paste(
      "The size of the water treated. Still water is measured in hectares and",
      "flowing water in kilometres, so choosing one or the other above leaves",
      "only that unit's slider here. An attempt is compared against the slider",
      "for its own unit and against no other. The scale is logarithmic,",
      "because recorded sizes run from a fraction of a hectare to tens of",
      "thousands of them."
    ),
    tip_no_size = paste(
      "{n} attempts have no size recorded. Leaving this ticked keeps them in",
      "whatever range you choose, so they are not silently dropped."
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
    # ONE LINE, at the client's request. The filters and the Build report
    # button explain the rest of the page by being there.
    description = "Set the filters to match your situation, then select **Build report**.",

    # ---- Zero results --------------------------------------------------------
    zero_heading = "No attempts match those filters",
    zero_body = paste(
      "That combination has nothing in it. This is common and usually says more",
      "about what has been reported than about what is possible."
    ),
    zero_hint_lead = "Try relaxing one of these first:",
    zero_hint_none = "Try clearing a filter and building again.",

    build   = "Build report",
    clear   = "Clear all filters",
    download_heading = "Take this away",
    download_open = "Download this report",
    download_close = "Close",
    download_lead = paste(
      "Choose what to include. More than one and they arrive together in a zip;",
      "on its own, a file arrives as itself."
    ),
    download_parts = "Include in your download",
    download = "Download",
    download_xlsx = "Attempt data, spreadsheet (.xlsx)",
    download_xlsx_note = paste(
      "Every field of every matching attempt, with the field definitions, the",
      "contacts, the filters you applied and the caveats on their own sheets."
    ),
    download_csv = "Attempt data, plain text (.csv)",
    download_csv_note = "The same rows as the spreadsheet, for a data tool rather than Excel.",
    download_html = "Interactive report (.html)",
    download_html_note = paste(
      "Self-contained report on FWISE letterhead, with the charts, the map and",
      "the contacts. Every plot stays interactive. Opens in any browser."
    ),
    download_pdf = "Formatted PDF",
    download_pdf_note = paste(
      "The report above, ready to print: open it and choose Save as PDF. It",
      "arrives as the same .html file, so ticking both adds nothing."
    ),
    download_txt = "Methods and caveats (.txt)",
    download_txt_note = "is always included, whatever else you choose.",

    # ---- Inside the report ---------------------------------------------------
    html_print = "Save as PDF",
    html_csv   = "Download the data (CSV)",
    html_xlsx  = "Download the data (Excel)",
    html_txt   = "Methods and caveats (text)",
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
      paste("The data is inside this file. The download buttons above give you",
            "every field of every matching attempt, including the values this",
            "page shortens, and the methods and caveats as plain text."),
      "Read the caveats at the end before quoting any figure from this report."
    ),
    html_map_note = paste(
      "The map is interactive - drag, zoom, and select a marker for the record.",
      "Its background needs an internet connection; the markers do not."
    ),

    report_title    = "Eradication attempt planning report",
    report_subtitle = "Generated from the FWISE database on {date}",
    report_selection = "What this report covers",
    report_where     = "Attempts by country",
    report_where_note = paste(
      "The map above plots each attempt at its own coordinates. This is what",
      "the map is read for on paper, and it is what is left when the tiles",
      "cannot load."
    ),
    report_footer = paste(
      "FWISE, the Freshwater Invasive Species Eradication database. Data",
      "released under CC BY-NC 4.0. Read the caveats above before quoting any",
      "figure in this report."
    ),

    # ---- Filters -------------------------------------------------------------
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
    r_method_wb_share = "Share of uses",
    r_method_wb_count = "Number of uses",

    # ---- The species tiles ----------------------------------------------------

    r_species_pair = "Most targeted species and beneficiaries",
    r_species_pair_note = paste(
      "What these attempts targetting, and what was protected. Each tile is",
      "one species, and includes number of attempts and a visual representation of the outcome mix"
    ),
    r_tile_attempt  = "attempt",
    r_tile_attempts = "attempts",
    r_invasive   = "What these attempts targeted",
    r_beneficiary = "What was protected",
    r_duration   = "How long these attempts took",
    r_duration_note = paste(
      "Start to finish, on a log scale, with a dotted line at a day, a week, a",
      "month, a year, five years and ten. Each point is one attempt that used a",
      "single method, so the dates on it describe that one treatment."
    ),
    r_duration_missing = paste(
      "Based on the {n} of these attempts that have a start, an end and a",
      "single recorded method. Attempts using more than one method are left",
      "out: their start and end dates span every method, not any one of them."
    ),
    r_table_showing = "Showing",

    # ---- Potential relevant contacts -----------------------------------------
    r_contacts = "Potential relevant contacts",
    r_contacts_note = paste(
      "The people recorded against the attempts above, most involved first.",
      "They have not been asked about your work - an address here means they",
      "agreed to be listed in FWISE, not that they are expecting to hear from",
      "you. Contacts who asked not to be listed appear without one."
    ),
    r_contacts_none = paste(
      "None of the attempts in this selection has a contact recorded against it."
    ),
    r_contacts_size = "Contacts per page",
    r_contacts_all  = "Browse every contact in FWISE",
    col_contact_name = "Name",
    col_contact_org  = "Organisation",
    col_contact_n    = "Attempts here",
    col_contact_email = "Get in touch"
  ),

  # ---- About -----------------------------------------------------------------

  about = list(
    title = "About FWISE",
    description = paste(
      "What FWISE is, important caveats about the data, and how to cite it, methods, and how to keep informed on developemnt at FWISE."
    ),

    database_heading = "About FWISE",
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

    scale = paste(
      "{attempts} eradication attempts across {countries} countries, against",
      "{species} invasive species, going back to {year} - curated from the",
      "published literature and contributed directly by {contributors} people."
    ),
    scale_action = "Become the {n}.",

    # ---- Sign-up -------------------------------------------------------------
    # NO ADDRESS IS COLLECTED HERE. The button is a link out to the list, so the
    # app never holds an email address, never has a form to secure and never has
    # a delivery failure to hide. Same reasoning as the feedback box below.
    signup_heading = "Keep up with FWISE",
    signup_body = paste(
      "We send an occasional update when a new release of the",
      "database goes out, or when something is published from it. No more than",
      "a few times a year."
    ),
    signup_action = "Sign up for updates",
    signup_url = "#", # [PLACEHOLDER] awaiting the mailing list URL

    # ---- Citation ------------------------------------------------------------
    cite_heading = "How to cite FWISE",
    cite_summary = "The citation to use in any publication that draws on FWISE",
    cite = paste(
      "FWISE data is open source and freely available for use. Please cite it in any publication that uses it with the below citation."
    ),
    citation_db = paste(
      "Freshwater Life ({year}). FWISE: Freshwater Invasive Species",
      "Eradication database, release {release} ({n} attempts).",
      "https://doi.org/[PLACEHOLDER]"
    ),

    # ---- The panels ----------------------------------------------------------
    # Each is a <details>. `*_heading` is the summary, `*_summary` the one line
    # under it that says what is inside - a reader decides whether to open a
    # panel from those two strings alone, so neither may be decorative.

    # MOVED HERE FROM THE REPORT BUILDER, and now behind a disclosure. They are
    # properties of the whole database rather than of any one selection, and on
    # Plan they sat under a result the reader had just built and read as
    # qualifications of that selection alone. The blocks themselves are computed
    # - see fw_caveat_blocks() in R/export.R - and the same text still travels
    # inside every download.
    caveats_heading = "Data caveats",
    caveats_summary = "How success is defined, what is missing, and why there is no success rate",
    caveats_lead = paste(
      "Caveats apply to all data within FWISE, and should be bared in mind when analyzing data or viewing the dashboard."
    ),

    method_heading = "How it was built",
    method_summary = "Where the records come from and how they were compiled",
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

    method3 = paste(
      "Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet,",
      "consectetur, adipisci velit, sed quia non numquam eius modi tempora",
      "incidunt ut labore et dolore magnam aliquam quaerat voluptatem."
    ),
    method_paper_heading = "Methods, in full",
    method_paper = c(
      paste(
        "[PLACEHOLDER] The methods from the FWISE paper go here, in full, once",
        "it is written. Until then this panel carries the summary above."
      )
    ),

    # [PLACEHOLDER] AWAITING THE CLIENT'S LIST. Each entry is a name, a URL and
    # one line saying what it holds that FWISE does not - the last of those is
    # the point of the panel, because a bare list of links does not tell anyone
    # which one to follow.
    related_heading = "Related databases",
    related_summary = "Where to look for what FWISE does not hold",
    related = paste(
      "[PLACEHOLDER] Other databases worth knowing about if FWISE does not have",
      "what you need."
    ),
    related_items = list(
      list(name = "[PLACEHOLDER] Database one", url = "#",
           note = "What it holds that FWISE does not."),
      list(name = "[PLACEHOLDER] Database two", url = "#",
           note = "What it holds that FWISE does not."),
      list(name = "[PLACEHOLDER] Database three", url = "#",
           note = "What it holds that FWISE does not.")
    ),

    # ---- Other information ---------------------------------------------------

    other_heading = "Other information",
    other_summary = "Species photographs, licence and links",
    images_heading = "Species photographs",
    licence_heading = "Licence",
    links_heading = "Links",
    link_fwise = "Freshwater Life",
    link_zenodo = "The archived dataset on Zenodo",

    # ---- Feedback ------------------------------------------------------------
    # NO BACKEND. See the note at the top of mod_about.R. It sits in a band of
    # its own at the foot of the page and is deliberately NOT one of the panels
    # above: a reader who has found something wrong should not have to open
    # anything to say so.
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
    filter_species   = "Species",
    filter_organisation = "Organization",
    filter_category  = "Species was",
    category_either  = "Either",
    category_invasive = "Invasive",
    category_beneficiary = "Protected",
    tip_category = paste(
      "Whether the species you pick was the invasive one being removed, the",
      "protected one the attempt was meant to help, or either."
    ),
    filter_search    = "Search",
    search_placeholder = "Name or organization",

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

    email_action     = "Email",
    email_none_label = "No public email address",
    no_organisation  = "Not recorded",

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
