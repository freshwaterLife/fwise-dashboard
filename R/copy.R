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
    # What the number in the middle of the donut counts. USES, not attempts:
    # one attempt can use several methods, so the ring's total is larger than
    # the number of attempts behind it. See fw_chart_method_donut().
    donut_uses     = "uses",
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
    description = c(
      paste(
        "The FWISE database aggregates attempts of freshwater invasive species eradications from all over the globe.",
        "Narrow by place and by the animals involved, see the details of the database in",
        "the summaries below, then find an attempt on the map or in the table",
        "and open it for the full record: the species, the methods and how they",
        "were applied, what happened, who recorded it and where it was published."
      ),
      paste(
        "\nTo narrow to a situation like your own and download the evidence for",
        "it, use **Plan an eradication.**"
      )
    ),

    # ---- The database panel --------------------------------------------------
    # The whole database, never filtered. Counts, all the same size, no rate.
    db_heading = "The database today",
    db_span = "Attempts recorded from {from} to {to}.",
    db_attempts    = "attempts recorded",
    db_countries   = "countries",
    db_invasive    = "invasive species targeted",
    db_beneficiary = "species recorded as benefitted",
    db_beneficiary_tip = paste(
      "Species that benefitted from the attempt, as recorded by the person",
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
    #
    # THE DONUT COUNTS USES, NOT ATTEMPTS, and the note says so. One attempt can
    # use several methods, so the slices are uses. Leaving that to a footnote is
    # how a reader ends up quoting a share of the wrong denominator.
    #
    # THE OUTCOME DONUT WAS REMOVED at the client's request - the outcome split
    # is already the segmentation of every stacked bar and the colour of every
    # marker on the map, so a ring of it was the fourth telling and the thinnest.
    donut_method = "Breakdown of methods used",
    donut_method_note = paste(
      "Each slice is one use of a method, There may have been multiple methods used for one attempt. Hover a slice for",
      "its count and share."
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
    ),

    # ---- The list ------------------------------------------------------------
    list_heading = "The attempts",
    list_count   = "{n} attempts",
    list_note = paste(
      "One row per attempt, with the species it targeted and the species it was",
      "meant to help. Select a row for the full record: the methods as applied,",
      "the verification, the contacts and the reference. Attempts with no",
      "coordinates are here even though they are not on the map."
    ),
    skip_map     = "Skip the map, go to the table of attempts",

    # ---- The table -----------------------------------------------------------
    col_invasive    = "Targeted",
    col_beneficiary = "Meant to benefit",
    col_site        = "Site",
    col_country     = "Country",
    col_began       = "Began",
    col_outcome     = "Outcome",
    col_open        = "Open the full record",
    open_record     = "Open record",
    no_beneficiary  = "None recorded",

    sort_label   = "Sort by",
    sort_newest  = "Most recent first",
    sort_oldest  = "Oldest first",
    sort_site    = "Site name",
    sort_country = "Country",
    page_size    = "Show",
    page_showing = "Showing",
    show_on_map  = "Show on map",

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
    rec_prev       = "Previous attempt",
    rec_next       = "Next attempt",

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
    taxa_beneficiary = "Kind of animal that benefited",
    method      = "Method used",
    regime      = "Still or flowing water",
    waterbody   = "Kind of waterbody",
    outcome     = "Outcome",
    size        = "Size of the area treated",
    years       = "Attempt began between",
    no_year     = "Include attempts with no recorded start year",
    no_size     = "Include attempts with no recorded size",

    # The two units size is recorded in. Still water is measured as an area and
    # flowing water as a length, so these are not convertible into one another
    # and the control never tries - see the header of R/filters.R.
    unit_ha     = "hectares, still water",
    unit_km     = "kilometres, flowing water",

    # The unit the reader is actually looking at, in the size filter's own
    # heading. It changes with the water-body selection, so the heading says
    # which quantity the slider under it is measuring rather than leaving the
    # reader to infer it from the regime they picked further up the panel.
    size_in_ha  = "in hectares",
    size_in_km  = "in kilometres",
    size_in_both = "in hectares and kilometres",

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
    tip_taxa_beneficiary = paste(
      "The broad group the species that was meant to benefit belongs to - fish,",
      "amphibian, bird etc. Recorded less consistently than the invasive side."
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
    # ONE BUTTON AND A PICKER, not a row of buttons. Two buttons made the reader
    # choose between the data and the document when most of them wanted both,
    # and neither carried the methods and caveats out of the building with it.
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
      "the table. Every plot stays interactive. Opens in any browser."
    ),
    download_pdf = "Formatted PDF",
    download_pdf_note = paste(
      "The report above, ready to print: open it and choose Save as PDF. It",
      "arrives as the same .html file, so ticking both adds nothing."
    ),
    # NOT a checkbox, and not a line in the list either. It is the one thing in
    # the bundle a reader cannot choose to leave behind - same reason the
    # workbook's caveats sheet is not optional, see the header of R/export.R -
    # so it is stated in the lead above the options, where the two halves read
    # as one sentence.
    download_txt = "Methods and caveats (.txt)",
    download_txt_note = "is always included, whatever else you choose.",
    download_none = "Nothing selected, so this downloads the methods and caveats on their own.",

    # ---- Inside the report ---------------------------------------------------
    # The toolbar the reader sees at the top of the downloaded file. It is the
    # only interactive chrome in the document and it does not print.
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
    # The cumulative chart LIVES ON EXPLORE NOW (FW_COPY$explore$cumulative). It
    # answers how the database has grown, which is a question about the record
    # rather than about the reader's own situation, and it was the one block
    # here that a narrow selection made actively misleading.
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

    # ---- Potential relevant contacts -----------------------------------------
    # The networking side of FWISE, brought to the point where a reader has just
    # seen what was tried near them and the obvious next question is who did it.
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
    # [PLACEHOLDER] MOST OF THE BODY COPY ON THIS PAGE IS LOREM IPSUM, on the
    # client's instruction, while they write the real wording. The headings are
    # the real ones and the structure is settled, so replacing this is a copy
    # edit rather than a rebuild. Three things here are NOT placeholder and must
    # survive that edit: `scale`, whose {placeholders} are filled from the loaded
    # data, and `citation`/`citation_db`, which are the citation formats.
    #
    # THE PAGE IS A SHORT SUMMARY WITH DEPTH BEHIND IT, at the client's request.
    # What is in FWISE, the sign-up and how to cite it are always visible; the
    # caveats, the methods, the success stories and the related databases are
    # click-to-open panels (fw_disclosure()). "What counts as an eradication"
    # used to open this page and now lives only on Contribute, where the person
    # who has to apply the definition is.
    title = "About FWISE",
    description = paste(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
      "tempor incididunt ut labore et dolore magna aliqua."
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

    # {placeholders} are filled from the loaded data by fw_about_scale().
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
      "[PLACEHOLDER] We send an occasional update when a new release of the",
      "database goes out, or when something is published from it. No more than",
      "a few times a year."
    ),
    signup_action = "Sign up for updates",
    signup_url = "#", # [PLACEHOLDER] awaiting the mailing list URL

    # ---- Citation ------------------------------------------------------------
    # TWO CITATIONS, because they are two different things to cite and a reader
    # quoting a figure needs the one that pins the release they read it in.
    cite_heading = "How to cite FWISE",
    cite = paste(
      "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse",
      "quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo",
      "voluptas nulla pariatur."
    ),
    cite_dashboard_heading = "The dashboard",
    cite_database_heading  = "The database",
    citation = paste(
      "Freshwater Life ({year}). FWISE: Freshwater Invasive Species",
      "Eradication dashboard, release {release}.",
      "https://doi.org/[PLACEHOLDER]"
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
    caveats_heading = "Important to note whilst reading the data",
    caveats_summary = "How success is defined, what is missing, and why there is no success rate",
    caveats_lead = paste(
      "These apply to every figure in FWISE, on every page and in every",
      "download. The numbers in them are counted from the data as it stands",
      "today, not written down and left to go stale."
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
    # [PLACEHOLDER] THE GAPS MATTER AS MUCH AS THE COVERAGE and the client is
    # explicit about wanting them stated rather than glossed. The caveats panel
    # carries the detail; the real wording for this paragraph has to say plainly
    # that there is one.
    method3 = paste(
      "Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet,",
      "consectetur, adipisci velit, sed quia non numquam eius modi tempora",
      "incidunt ut labore et dolore magnam aliquam quaerat voluptatem."
    ),
    # [PLACEHOLDER] RESERVED FOR THE PAPER'S METHODS. The client is writing up
    # the compilation as a paper and wants its methods section to land here
    # rather than be summarised. Expect several paragraphs; method_paper is a
    # character VECTOR for that reason and the page renders one <p> per element.
    method_paper_heading = "Methods, in full",
    method_paper = c(
      paste(
        "[PLACEHOLDER] The methods from the FWISE paper go here, in full, once",
        "it is written. Until then this panel carries the summary above."
      )
    ),

    # [PLACEHOLDER] THE STORIES THEMSELVES LIVE ON THE LANDING PAGE, which is
    # not built yet - see the case studies section of the specification in
    # R/mod_home.R. This panel is the hook for them: when they exist, the link
    # below points at them rather than at the top of the home page.
    stories_heading = "Success stories",
    stories_summary = "Eradications that worked, and what they took",
    stories = paste(
      "[PLACEHOLDER] Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
      "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
    ),
    stories_action = "See the success stories",

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
