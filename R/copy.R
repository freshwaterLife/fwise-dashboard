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
    data_by    = "FWISE Database is built and maintained by Freshwater Life and friends.",
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
    # THE ADDRESS IS NOT IN THE MARKUP, in two halves that JavaScript joins at
    # click time - the same speed bump the Networking directory uses, and for
    # the same reason. See fw_contact_action() in mod_networking.R and
    # fw_footer_contact() in ui_helpers.R.
    contact_label  = "Contact FWISE",
    contact_aria   = "Show the FWISE contact address",
    contact_user   = "fwise",
    contact_domain = "fwlife.org",
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
    # SUCCESS RATE, NOT "SHARE OF ATTEMPTS" (client, 23 Sept 2026). The
    # toggles beside these charts read "Success rate", so the axis has to say
    # the same thing; two names for one view is how a reader ends up thinking
    # they are looking at two different quantities. The bar is still the full
    # outcome mix - see the note at the head of charts.R.
    x_share       = "Success rate (%)",
    # NO X AXIS TITLE ON THE DURATION CHART (client, 23 Sept 2026). The named
    # ticks below say what the axis is; a title under them said it twice.
    # Named ticks on the log axis, in step with FW_CHART$duration_ticks.
    duration_ticks = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"),
    # THE TERMINAL TICK, at the longest attempt in the selection, so the axis
    # labels reach the last dot rather than stopping at the fixed tick below it
    # (client, 23 Sept 2026). {n} is a whole number of the unit named. Which
    # unit is chosen, and when a fixed tick is dropped to make room, is
    # fw_duration_ticks() in charts.R.
    duration_max_days   = "{n} days",
    duration_max_months = "{n} months",
    duration_max_years  = "{n} years",
    # Under the scale bar on the PDF report's map. It NAMES THE LATITUDE the
    # bar is correct at, because the map is unprojected longitude/latitude and
    # a bar drawn on one cannot be right everywhere - see fw_gg_scale_bar().
    scale_bar     = "{km} km at {lat}",
    other         = "Other",
    hover_days    = " days",
    # The stacked bars' hover is no longer assembled from fragments here: it
    # reuses plan$r_tile_seg, the species tile's popover template, so the two
    # cannot drift apart (client, 24 Sept 2026). hover_of and hover_share went
    # with the assembly. See fw_hover_counts() in R/charts.R.
    # Shown in a chart's own slot when the selection gives it nothing to draw.
    empty         = "Nothing to draw for this selection."
  ),

  # ---- Maps ----------------------------------------------------------------------
  maps = list(
    # ONE TITLE AND ONE (i) FOR EVERY MAP IN THE APP (client, 23 Sept 2026).
    # The dashboard's map and the report builder's both read these, so the two
    # cannot drift apart. The PDF's static map is a different picture with a
    # different caption - see plan$pdf_map_note.
    title = "Where attempts happened",
    note = paste(
      "Each marker is one eradication attempt, coloured and labelled by",
      "outcome. Hover for a summary, select for the full record. Toggle",
      "between layers for hydrological, topographic, and satellite imagery."
    ),
    # The basemap switcher. Named so it reads as a question the reader might
    # ask rather than as a list of vendors.
    basemap_plain     = "Plain",
    basemap_water     = "Water",
    basemap_terrain   = "Terrain",
    basemap_satellite = "Satellite",
    year_one  = "year",
    year_many = "years",
    # The Duration field on the hover card and the record: days under a year,
    # years to one decimal place from there. See fw_popup_duration().
    day_one   = "day",
    day_many  = "days",
    # Under both interactive maps. Worded by the client. Set below the type
    # floor - see $fw-size-fine in _tokens.scss.
    mercator_note = paste(
      "Leaflet currently requires the use of Mercator. This map will be",
      "updated to Equal Earth when it becomes available."
    ),
    # The accessible name of a group of markers. The count is drawn inside the
    # circle, so this is what says what the number means - to a screen reader,
    # and on hover. {n} is the number of attempts grouped at that point.
    stack_title = "{n} attempts at this point - zoom in or click to open them"
  ),

  # ---- Home (the Welcome page) -----------------------------------------------

  home = list(
    title = paste(
      "Freshwaters cover **<1%** of earth yet are home to **45% of all threatened animal species**.",
      "Eradicating freshwater invasives is the **best** way to **save them from extinction**."
    ),
    lead = c(
      "But almost nobody knows this. Enter the **Freshwater Invasive Species Eradication Database**. It shows the world **what works**, **where**, and **how**.",
      paste(
        "Use it now to [[explore|understand this solution]],",
        "[[plan|plan a new eradication]], [[contribute|add your own data]],",
        "and [[networking|connect with others]]."
      )
    ),
    # THE ">" IS INSIDE THE BOLD (client, 24 Sept 2026), so it is teal and in the
    # numeric face with the figure it qualifies rather than sitting outside in
    # plain ink. The report builder and the PDF already build theirs this way -
    # paste0(">", fw_fmt_num(...)) in mod_plan_results.R and report_pdf.R.
    kpi_sentence = paste(
      "**{attempts}** successful eradications recorded so far have protected **>{protected}** species.",
      "Click the species to read worldwide success stories."
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
      "Move the map from ",
      now  = "successes (blue)",
      " to ",
      later = "opportunities (yellow)",
      "."
    ),

    # ---- The footnote ----
    # The last thing on the page, above the footer, spanning its full width and
    # set below the type floor (client, 23 Sept 2026 - see $fw-size-fine in
    # _tokens.scss). HTML, not markdown: the two citations are live links and
    # fw_emphasis() does not make links. Rendered by fw_home_footnote().
    #
    # THE LINK IS THE PHRASE, not the DOI (client, 24 Sept 2026). The bare
    # numbers were the anchor text and the sentence had to carry them in
    # brackets to make sense; naming the paper reads as a sentence and still
    # goes to the same place. .fw-home-footnote a underlines them in teal, so a
    # reader can still see the two citations are links.
    footnote = paste0(
      "<a href=\"https://doi.org/10.1038/s41586-024-08375-z\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">An article in Nature</a> ",
      "found that invasive species have contributed to 55% of freshwater ",
      "extinctions, second only to dams. Dam removal is scaling fast; ",
      "freshwater eradications are next. ",
      "<a href=\"https://doi.org/10.1126/science.adj6598\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">A meta-analysis in ",
      "Science</a> found that managing invasive species has the “largest ",
      "impact of [all possible] conservation action”. Eradication is the most ",
      "effective form of invasive species management: cheaper and more ",
      "enduring than long-term control."
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
      "Species protected by a SUCCESSFUL eradication, as recorded by the",
      "person reporting it. It is recorded less consistently than the species",
      "targeted and is likely not a complete record."
    ),
    in_review = "in review",
    in_review_tip = paste(
      "Submissions waiting for review by our team. ",
      "They are not included here."
    ),

    # ---- The filters ---------------------------------------------------------
    f_heading = "Narrow the list",

    # ---- The summary graphics ------------------------------------------------

    method = "Methods used, and how they turned out",
    method_note = paste(
      "One bar per method, split by outcome. An attempt that used more than one",
      "method is counted once under each of them, so the bars add up to more",
      "than the number of attempts. Hover a segment for its count."
    ),
    cumulative = "Eradication attempts over time"

    # NO map / map_note HERE EITHER - see maps$title and maps$note.
  ),

  # ---- Species and map popups ------------------------------------------------

  species = list(
    no_image   = "No photograph available",
    fig_none   = "None noted",
    alt_prefix = "Photograph of",
    # EVERY FIELD IS ALWAYS DRAWN, on the hover card and in the record, and a
    # field with nothing in it says this (client, 21 Sept 2026). The photograph
    # tile for a role with no species says the same, so tile and row agree.
    p_not_noted = "Not noted",
    p_none     = "Not noted",
    p_species  = "Invasive species targeted",
    p_beneficiary = "Species protected",
    # The hover card's shorter pair, the same words as the photograph captions.
    p_targeted  = "Targeted",
    p_protected = "Protected",
    fig_invasive = "Targeted",
    fig_beneficiary = "Protected",
    p_outcome  = "Outcome",
    p_began    = "Years",
    p_duration = "Duration",

    # ---- The detail panel, opened by clicking a marker ------------------------
    p_methods      = "Method(s)",
    p_method_desc  = "What was done",
    p_verified     = "Verification via",
    p_contacts     = "Contact(s)",
    p_waterbody    = "Kind of water",
    p_area         = "Area/length treated",
    p_driver       = "Reason",
    p_reference    = "Reference",
    p_read_source  = "Read the source",
    p_download_hint = paste(
      "Download the data for more details (e.g., water volume and flow rates",
      "or chemical concentrations)."
    ),
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
    taxa        = "Kind of invasive animal",
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

    # NO tip_continent AND NO tip_waterbody (client, 23 Sept 2026). Both
    # tooltips only restated their label, so the (i) has gone from those two
    # filters; FW_FILTERS carries tip = NULL for them and fw_field() draws no
    # button when there is nothing to say.
    tip_country = paste(
      "Country of the eradication attempt(s). Only countries with attempts",
      "recorded in FWISE are listed. If yours is not here, filter by continent",
      "instead - that will show you the closest evidence there is."
    ),
    tip_regime = paste(
      "Still water is lakes, ponds and reservoirs, etc; flowing water is",
      "rivers and streams, etc."
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

    # ---- The callout ---------------------------------------------------------
    # THE CLIENT'S OWN WORDING (24 Sept 2026), in the green card above the
    # filters: what the report contains, what it deliberately does not, and how
    # the filters behave. One string per paragraph - fw_emphasis() takes the
    # ** pairs and mod_plan_ui() draws one <p> each.
    #
    # It repeats plan$description above it and plan$f_lead inside the filter
    # card. The client asked for all three to stay (24 Sept 2026): this card is
    # the one a reader arriving cold actually reads.
    callout = c(
      paste(
        "Recreate your situation or interest by adjusting the filters below and",
        "clicking **Build Report**. You will get: a map of where matching",
        "eradication attempts happened, the species involved, what methods were",
        "used, how long they took, and the outcomes. Plus the full records with",
        "much more detail. Of course, planning an eradication from start to",
        "finish requires much more than the technical and ecological evidence",
        "alone. So the report also gives you the contact information of who you",
        "should reach out to learn more and move forward."
      ),
      "All downloadable as a pdf, interactive html, and spreadsheet.",
      paste(
        "All fields are optional. Fields default to 'All'. You can make",
        "multiple selections within a field, and you can search for selections."
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
      "contacts, the filters you applied and the methods and caveats on their",
      "own sheets."
    ),
    # NO .csv (client, 23 Sept 2026): it was the spreadsheet's rows a second
    # time, and the picker now offers the three documents that differ from one
    # another. The order here is the order of FW_BUNDLE_PARTS in export.R.
    download_pdf = "Report (.pdf)",
    download_pdf_note = paste(
      "This report on FWISE letterhead, ready to print or send: the summary,",
      "species, map, charts, contacts, and the methods and caveats."
    ),
    download_records = "Every attempt in full (.html)",
    download_records_note = paste(
      "One scrollable page with each matching attempt written out in full,",
      "one after another. Opens in any browser, with no connection needed."
    ),
    # Shown under the PDF's checkbox when the estimate reaches FW_PDF$warn_pages
    # or FW_PDF$warn_mb.
    pdf_warn = paste(
      "This selection makes a long PDF - about {pages} pages and {mb} MB,",
      "most of it the contacts table. It can take up to a minute to build.",
      "Narrowing the filters makes it shorter."
    ),
    # Shown in place of the PDF's checkbox where the server cannot make one.
    pdf_unavailable = paste(
      "The PDF report is not available on this server at the moment. The",
      "other downloads are unaffected."
    ),
    # Under the boxes, and the Download button is disabled until one is ticked.
    # A methods-and-caveats .txt used to travel with every download, so ticking
    # nothing still produced a file; the client removed it (24 Sept 2026) and
    # with it the only thing an empty selection could have been.
    download_none = "Pick at least one to download.",

    # ---- Inside the PDF ------------------------------------------------------
    pdf_page = "Page",
    pdf_map_note = paste(
      "Each dot is one attempt, at its recorded coordinates, coloured by its",
      "outcome. Stacked dots are attempts at the same site."
    ),

    report_title    = "Eradication attempt planning report",
    report_subtitle = "Generated from the FWISE Database on {date}",
    report_selection = "What this report covers",
    # Under the filters table (client, 23 Sept 2026): what the report is for,
    # and what it deliberately is not.
    report_selection_note = paste(
      "This report provides you with technical and ecological evidence to plan",
      "your own eradication, based on the specific situation you filtered for.",
      "Of course, planning an eradication from start to finish requires much",
      "more information and strategy (such as: regulation; funding; Free,",
      "prior and informed consent - FPIC; etc.). The table below contains",
      "contact information of who you could reach out to learn more and move",
      "forward."
    ),
    report_where     = "Attempts by country",
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
    # EACH LABEL IS A PAIR, singular and plural, and fw_plural() picks between
    # them against the figure above it (client, 23 Sept 2026: one country was
    # labelled "countries"). "year range" has no singular - it is a range
    # whatever it spans - so it stays a single string.
    r_attempts_one  = "attempt",
    r_attempts      = "attempts",
    r_countries_one = "country",
    r_countries     = "countries",
    r_species_one   = "invasive species",
    r_species       = "invasive species",
    # Shown after a ">" on the figure: beneficiaries are under-recorded, so the
    # count is a floor rather than a total. See db_beneficiary_tip. The figure
    # counts only species protected by SUCCESSFUL attempts (client, 23 Sept
    # 2026) - see fw_plan_summary().
    r_beneficiaries_one = "species protected",
    r_beneficiaries = "species protected",
    r_years      = "year range",
    r_outcomes   = "Outcomes",
    r_outcome_note = paste(
      "All four states are shown. Failure is as important to know about as success."
    ),
    # NO r_map / r_map_note HERE. Every map in the app carries the same
    # heading and the same (i) (client, 23 Sept 2026), so both live in the
    # `maps` block as maps$title and maps$note. Reword them there and both
    # maps follow.
    r_map_missing = "{n} of these attempts have no coordinates and are not on the map.",

    r_waterbody  = "What kind of water",
    r_waterbody_note = paste(
      "Attempts by the kind of waterbody treated, with the outcome mix in each.",
      "The {n_word} most common are named and the rest gathered into Other."
    ),
    # Its own pair, though the words match r_method_count/share today. The two
    # charts agree on their denominator by coincidence - both count attempts -
    # and sharing one key would tie the wording of two blocks together for a
    # reason that is not about either of them.
    r_waterbody_share = "Success rate",
    r_waterbody_count = "Number of attempts",

    r_method     = "Outcomes of methods within selection",
    r_method_note = paste(
      "Outcomes within each method, with the number of attempts beside it."
    ),
    r_method_mode  = "Show",
    r_method_share = "Success rate",
    r_method_count = "Number of attempts",
    r_method_missing = "{n} of these attempts have no method recorded and are not on the method chart.",

    # ---- The species tiles ----------------------------------------------------

    # Two plain block titles (client, 21 Sept 2026). {n_word} is how many tiles
    # are shown, {total} how many species of that role the selection holds.
    # The protected total carries a ">" because beneficiaries are
    # under-recorded - see db_beneficiary_tip.
    r_species_top_inv = "Top {n_word} invasive species targeted (of {total} total)",
    r_species_top_ben = "Top {n_word} species protected (of >{total} total)",
    r_tile_attempt  = "attempt",
    r_tile_attempts = "attempts",
    # A species tile's outcome bar, one segment on hover - AND THE HOVER ON
    # EVERY STACKED BAR CHART (client, 24 Sept 2026). The tiles said
    # "Successful: 25% (3 of 12)" while the bars said "Successful: 3 of 12" and
    # only added a percentage in share mode. One template, so the reader meets
    # one sentence wherever they hover. See fw_hover_counts() in R/charts.R.
    r_tile_seg = "{outcome}: {pc}% ({n} of {total})",
    r_duration   = "How long attempts took",
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
    col_contact_n    = "# Attempts",
    col_contact_email = "Get in touch",
    # The download's progress bar (fw_write_bundle() names each step as it
    # starts it; the PDF report adds its own two).
    progress_title   = "Preparing your download",
    progress_xlsx    = "Building the spreadsheet",
    progress_charts  = "Drawing the charts and map",
    progress_pdf     = "Typesetting the PDF report",
    progress_records = "Writing the attempts file",
    progress_zip     = "Packing the zip",
    progress_done    = "Starting the download"
  ),

  # ---- About -----------------------------------------------------------------

  about = list(
    title = "About FWISE",
    description = paste(
      "What FWISE is, important caveats about the data, and how to cite it, methods, and how to keep informed on development at FWISE."
    ),

    # NO OPENING PROSE SECTION. It held Lorem Ipsum waiting on client copy and
    # was removed on 24 Sept 2026 rather than shipped; the page now opens on the
    # sign-up card. When the client supplies the text, add the paragraphs and
    # their `database_heading` back here, restore section() in mod_about.R, and
    # add "database" to the heading list in dev/value_test.R.

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
    # THE FORM IS THE CLIENT'S (24 Sept 2026). {year}, {release} and {n} are
    # filled by fw_about_citations() from the release actually loaded, so the
    # version and the attempt count cannot go stale in a citation somebody
    # copies. "[other authors]" and the DOI stay as placeholders until the
    # client supplies them.
    citation_db = paste(
      "Espinosa et al. [other authors]. ({year}). FWISE: Freshwater Invasive",
      "Species Eradication Database. (Version {release}; {n} attempts).",
      "Zenodo. https://doi.org/[PLACEHOLDER]"
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
    caveats_summary = "What to keep in mind when reading this data",
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

    # ---- Methods glossary ----------------------------------------------------
    # ONE ENTRY PER METHOD THE DATA ACTUALLY RECORDS, and the `term` strings
    # match the values in attempts.csv$methods exactly - Rotenone, Netting /
    # Trapping, Electrofishing, Antimycin-A, Draining, Other chemical, Other
    # mechanical - so a reader who meets a method on a chart finds it here
    # under the same name. If a new method is added to the vocabulary, add it
    # here too; dev/value_test.R checks the two lists against each other.
    glossary_heading = "Methods glossary",
    glossary_summary = "What each eradication method on the charts means",
    glossary_items = list(
      list(term = "Rotenone", body = paste(
        "A naturally occurring chemical compound found in the roots of",
        "bean-family plants that has been used by Indigenous people for",
        "millennia as a fish toxicant. It works by blocking respiration in",
        "their gills, and has no effect on air-breathing animals like mammals",
        "(including humans).")),
      list(term = "Netting / Trapping", body = paste(
        "Any type of net or trap used to capture invasive fish.")),
      list(term = "Electrofishing", body = paste(
        "Using a controlled current to shock and stun invasive fish so they",
        "can be captured with a dip net.")),
      list(term = "Antimycin-A", body = paste(
        "A naturally occurring bacterium that is used as a fish toxicant. It",
        "is grown through fermentation.")),
      list(term = "Draining", body = paste(
        "Removing water from the entire lake or stream so that no invasive",
        "fish survive.")),
      # [PLACEHOLDER] AWAITING AN EXAMPLE OR TWO FROM THE CLIENT (23 Sept
      # 2026), marked the same way about$method_paper and export$methods are.
      list(term = "Other chemical methods", body = paste(
        "[PLACEHOLDER] An example or two of the other chemical methods",
        "recorded under this heading.")),
      list(term = "Other mechanical methods", body = paste(
        "[PLACEHOLDER] An example or two of the other mechanical methods",
        "recorded under this heading."))
    ),

    # ---- Related databases ---------------------------------------------------
    # The client's list (23 Sept 2026). Names and URLs only: these are
    # well-known resources and the client did not want a line of our own
    # characterising each one, so fw_about_related() draws the note only when
    # an entry carries one.
    related_heading = "Related databases",
    related_summary = "Where to look for what FWISE does not hold",
    related = paste(
      "Other databases worth knowing about if FWISE does not have what you",
      "need."
    ),
    related_items = list(
      list(name = "Global Invasive Species Database (GISD)",
           url = "https://www.iucngisd.org/gisd/"),
      list(name = "Global Register of Introduced and Invasive Species (GRIIS)",
           url = "https://griis.org"),
      list(name = "Global Fish Invasions Database (GFID)",
           url = "https://zenodo.org/records/22694143"),
      list(name = "SHOAL 1000 fishes",
           url = "https://shoalconservation.org/1000-fishes/"),
      list(name = paste("One-quarter of freshwater fauna threatened with",
                        "extinction"),
           url = "https://www.iucnredlist.org/resources/data-repository"),
      list(name = "Global Lakes and Wetlands Database (GLWD)",
           url = "https://www.hydrosheds.org/products/glwd"),
      list(name = "Database of Island Invasive Species Eradications (DIISE)",
           url = "https://diise.islandconservation.org"),
      list(name = paste("95 freshwater-relevant online data systems for",
                        "biodiversity"),
           url = paste0("https://docs.google.com/spreadsheets/d/",
                        "1CAo6m2HnFET4IdoSORcy4OIYrtG2SAS3Ijj75j7FUMQ/edit",
                        "?gid=1303723149#gid=1303723149"))
    ),

    # ---- More on the solution ------------------------------------------------

    other_heading = "More on the solution",
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
    feedback_email = "fwise@fwlife.org"
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
    # IN TWO HALVES, joined in the browser - see the footer's contact_user /
    # contact_domain and the note at fw_footer_contact(). The single-string
    # outro_email that was here served the whole address in the markup.
    outro_user   = "fwise",
    outro_domain = "fwlife.org",

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
