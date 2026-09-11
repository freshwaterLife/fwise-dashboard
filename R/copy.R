# copy.R
# EVERY user-facing string in the app lives here. No module file should contain
# hardcoded English. Renaming a navigation item or rewording a prompt is a
# one-line edit in this file.
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
    info_icon_label = "More information about this field"
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
      "The ten most common are named and the rest gathered into Other."
    ),

    driver = "Why they were carried out",
    driver_note = "The main reason recorded for each attempt.",

    invasive = "What gets targeted",
    invasive_note = paste(
      "The ten species named most often, counted once per attempt. This is a",
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
        "Build a report of the eradication attempts that match your situation.",
        "Set the filters below to describe where you are, the water you are",
        "working in and the species you are dealing with, then select",
        "Build report."
      ),
      paste(
        "You will get a map of where those attempts happened, what they",
        "achieved, the methods used and how long they took, with the matching",
        "records in full underneath. All of it downloads as a report or a",
        "spreadsheet."
      ),
      paste(
        "Nothing is filtered out to begin with, so leaving everything set to",
        "All and building gives you the whole database."
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
      "The ten most common are named and the rest gathered into Other."
    ),

    r_method     = "How the methods compare",
    r_method_note = paste(
      "Outcomes within each method, with the number of attempts beside it."
    ),
    r_method_mode  = "Show",
    r_method_share = "Share of attempts",
    r_method_count = "Number of attempts",

    r_method_wb  = "What gets used in what kind of water",
    r_method_wb_note = paste(
      "The methods used in each kind of waterbody, counted once per attempt.",
      "Colour here is the method, not the outcome. Draining a pond and draining",
      "a river are one method and two different propositions, which is what",
      "this separates and the chart above cannot."
    ),

    # ---- The species tiles ----------------------------------------------------
    r_tile_attempt  = "attempt",
    r_tile_attempts = "attempts",
    r_invasive   = "What these attempts targeted",
    r_invasive_note = paste(
      "The ten invasive species named most often in this selection, counted",
      "once per attempt, out of {n} in total. The bar under each is its outcome",
      "mix."
    ),
    r_beneficiary = "What was meant to benefit",
    r_beneficiary_note = paste(
      "Read this one carefully. Beneficiary species are recorded far less",
      "consistently than targets - many attempts name none at all, and those",
      "that do tend to be the ones written up for a named endangered species.",
      "These are the ten named most often of {n}, and they show what has been",
      "claimed rather than what recovered."
    ),

    r_duration   = "How long these attempts took",
    r_duration_note = paste(
      "Start to finish, on a log scale. Fortnight and a decade both fit on",
      "one axis. Each point is one attempt."
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

    team_heading = "Who is behind it",
    team = paste(
      "Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis",
      "suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur."
    ),

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
  ),

  # ---- Contribute ------------------------------------------------------------
  #
  # Field labels, prompts and tooltips follow the FWISE Upload Form field
  # specification. Where the specification was ambiguous a TODO(alex) comment
  # sits next to the field in mod_contribute_steps.R.

  contribute = list(
    title = "Contribute data",
    description = paste(
      "Add an eradication attempt to FWISE. It takes about fifteen minutes and",
      "only a few fields are required."
    ),

    # THE DEFINITION AND THE SCOPE, pinned to the top of the page. This is the
    # client's wording and the citation is Piero Genovesi's review; do not
    # reword either without asking them. Shown by fw_preamble() on the
    # contribute page and again on About, from this one source.
    preamble = list(
      heading = "What counts as an eradication",
      definition = paste(
        "Eradication is the complete and permanent removal of all wild",
        "populations of an alien plant or animal species from a defined area,",
        "by means of a time-limited campaign."
      ),
      citation = paste(
        "Genovesi, Piero. Limits and Potentialities of Eradication as a Tool",
        "for Addressing Biological Invasions."
      ),
      scope_heading = "What is the scope of contributions?",
      # A list rather than prose because the two points answer different
      # questions: which attempts we want, and which organisms.
      scope = c(
        paste(
          "FWISE is interested in all stages of an eradication - failed,",
          "in-progress, successful, or even unverified. Past, present. The more",
          "data we are able to collate at different cycles of eradication the",
          "more practitioners have to leverage, the more gaps we can expose to",
          "funders to help invasive species eradication."
        ),
        paste(
          "FWISE covers freshwater invasive **animals** - fish, crayfish,",
          "molluscs, amphibians etc. It does not cover plants."
        )
      )
    ),

    # Opening panel, before step one
    intro = list(
      heading = "Add an eradication attempt",
      what_heading = "What FWISE is",
      what = paste(
        "FWISE is a global evidence base of eradication attempts against",
        "freshwater invasive animals. Every record here came from someone in",
        "the field conducting these conservation efforts. Adding yours makes",
        "the picture more complete for everyone planning one."
      ),
      review_heading = "Every submission is reviewed",
      review = paste(
        "Your submission goes to the FWISE team. We quality assure it,",
        "consolidate it with the rest of the database, and get in touch if",
        "anything needs clarifying. It appears in the dashboard once that is",
        "done."
      ),
      no_save_heading = "You cannot save and come back",
      no_save = paste(
        "There are no accounts or logins, therefore no way to save data. Please",
        "complete the form in one sitting. We recommend you download the",
        "question list below, prepare responses offline, and copy your answers",
        "across when you are ready. This saves any loss of data and time."
      ),
      time_heading = "How long it takes",
      time = paste(
        "About fifteen minutes to fill in the information once collated. Only a",
        "few fields are required, so a partial record is far better than none."
      ),
      download_label     = "Download the question list (Word)",
      download_label_txt = "Plain text version",
      download_hint  = paste(
        "Every question on the form, in order, with room to write your answers.",
        "Both files are generated from the form itself, so neither can go out",
        "of date."
      ),
      start_action   = "Start the form"
    ),

    # Consent controls, section 0.1 of the specification
    consent = list(
      heading = "Before you begin",
      # This is the client's wording from the specification and should not be
      # reworded without asking them.
      statement = paste(
        "By submitting this information you confirm that FWISE staff may contact",
        "you regarding your submission, and allow FWISE to display contact",
        "information within the dashboard."
      ),
      agree_label = "I agree to the above and to FWISE using this information as described",
      # [PLACEHOLDER] the full terms of data use are still being drafted
      terms_link_label = "Read the full terms of data use",
      terms_url = "#",
      email_private_label = paste(
        "Keep my email address private. FWISE staff can still contact me, but my",
        "address will not be shown to public users."
      ),
      email_private_help = paste(
        "Leave this unticked and your address appears on the contacts page, so",
        "other practitioners can reach you directly."
      )
    ),

    # Step titles, shown in the progress indicator
    steps = list(
      site       = "Site and location",
      waterbody  = "Waterbody",
      invasive   = "Invasive species targeted",
      timeline   = "Timeline",
      benefit    = "Beneficiaries",
      methods    = "Methods",
      chemical   = "Chemical detail",
      outcome    = "Outcome and evidence",
      contributor = "Contributor",
      other      = "Other details",
      review     = "Review and send"
    ),


    # Section blurbs
    blurb = list(
      site      = "Where the eradication took place. These fields put the record on the map, so all three are required.",
      waterbody = "What the waterbody is like. Answer still or flowing first and the rest of the section follows from it. Everything after the type is optional context.",
      invasive  = "The animals the eradication targeted. Give the kind of animal and the species for each one, and add another target if more than one was targeted.",
      timeline  = "When the invasion and the intervention happened, and why it was carried out.",
      benefit   = "What the eradication was meant to help. All optional.",
      methods   = "How the eradication was carried out.",
      chemical  = "Detail on the chemical treatment. All optional.",
      outcome   = "What happened, and where the evidence for it sits.",
      contributor = "Who to credit and contact. We need a primary contact so the review team can follow up.",
      other     = "Anything else you would like the FWISE team to know.",
      review    = "A last read-through before sending. Everything here is editable above; scroll back up to change anything."
    ),

    # Repeatable block controls
    add_target      = "Add another target",
    check_action    = "Check my answers",

    # ---- "Check my answers" ---------------------------------------------------
    # HELPFUL, NOT PUNITIVE. A contributor is doing us a favour by filling this
    # in. The wording says what is missing and why it is worth having, and never
    # implies they have done something wrong.
    #
    # HARD vs SOFT is the important distinction. Hard errors block sending
    # because the record would be unusable without them. Soft warnings NEVER
    # block: a great many real attempts are ongoing, unmeasured or unpublished,
    # and refusing those records would bias the database towards tidy ones.
    check = list(
      heading_clear  = "This all looks good",
      body_clear     = paste(
        "Everything needed is filled in. Have a last read through below, then",
        "send it."
      ),
      heading_errors_one  = "One thing needs your attention",
      heading_errors_many = "{n} things need your attention",
      body_errors    = paste(
        "These are marked in the form as well. Select one to jump straight to",
        "it."
      ),
      heading_notes  = "Worth adding if you have it",
      body_notes     = paste(
        "None of these stop you sending. They are the fields that make a record",
        "more useful to someone planning their own attempt, so add them if you",
        "can and leave them if you cannot."
      ),
      review_heading = "Your answers",

      # Hard errors
      e_site_name    = "The site needs a name.",
      e_country      = "Choose the country the site is in.",
      e_latitude     = "Place a pin on the map, or type a latitude between -90 and 90.",
      e_longitude    = "Place a pin on the map, or type a longitude between -180 and 180.",
      e_regime       = "Say whether the water is still or flowing.",
      e_waterbody    = "Choose the kind of waterbody treated.",
      e_area_unit    = "You gave a size for the area treated, so we need its unit.",
      e_target_taxa  = "Choose the kind of animal the eradication targeted.",
      e_target_sp    = "Name the species targeted. If it is not in the list, type it in.",
      e_start_year   = "Give the year the eradication attempt began, between 1500 and {year}.",
      e_end_year     = "The end year must be between 1500 and {max_year}.",
      e_end_before   = "The attempt cannot have ended before it began. Check the two years.",
      e_driver       = "Choose the main reason the eradication was carried out.",
      e_method       = "Choose the main method used.",
      e_outcome      = "Choose the outcome.",
      e_contact_name = "We need a contact name so the review team can follow up.",
      e_contact_mail = "We need a contact email so the review team can follow up.",
      e_contact_bad  = "That does not look like an email address.",
      e_second_mail  = "The second contact's email does not look like an email address.",
      e_consent      = "Confirm you are happy for us to use this before sending.",

      # Soft warnings
      w_end_year     = "No end year. Leave it blank if the attempt is still going - many are.",
      w_area         = "No size for the area treated. Even a rough figure helps people judge scale.",
      w_invasion     = "No year of invasion. Useful for showing how long a problem ran before anyone acted.",
      w_beneficiary  = "No beneficiary species. What the eradication was meant to help is one of the most useful things you can record.",
      w_reference    = "No reference or link. A DOI, URL or citation is what lets someone check the record.",
      w_verification = "No note on how the outcome was verified. Useful even when the answer is that it was not.",
      w_duration     = "No duration. Helps others plan the effort involved.",
      w_method_desc  = "No description of the approach. This is the field practitioners say they read first.",
      w_old_year     = "A start year of {year} is unusually early. Worth a second look."
    ),

    send_note       = "Required fields are marked with an asterisk.",
    add_species     = "Add another species",
    add_method      = "Add another method",
    add_beneficiary = "Add another beneficiary",
    remove_row      = "Remove this row",

    # The action keeps this name from the button through to the confirmation.
    send_action = "Send submission",
    sending     = "Sending your submission",

    # Confirmation
    confirm = list(
      heading = "Congratulations!",
      # Assembled at runtime with the live counts.
      body_template = paste(
        "You have added the {nth} eradication attempt, and the {nth_country}",
        "in {country}."
      ),
      followup = paste(
        "We are excited by and grateful for this new data, and will review it",
        "and email you soon to let you know FWISE has been updated! We'll also",
        "send you a data contributor badge and certificate."
      ),
      thanks = paste(
        "Thank you for making the time to contribute to this global movement.",
        "If you have any more eradication attempts, please enter them!"
      ),
      another_action = "Add another attempt",
      explore_action = "Explore the data"
    ),

    error = list(
      write_failed = paste(
        "We could not save your submission. Nothing has been lost from this page,",
        "so please try again. If it keeps failing, copy your answers somewhere",
        "safe and email the FWISE team."
      ),
      validation = "Some answers still need attention. They are marked below."
    )
  )
)

# Small convenience so modules read as fw_t("networking", "title") rather than a
# chain of dollar signs.
fw_t <- function(...) {
  path <- c(...)
  out <- FW_COPY
  for (p in path) {
    out <- out[[p]]
    if (is.null(out)) stop("No copy defined at: ", paste(path, collapse = " > "),
                           call. = FALSE)
  }
  out
}
