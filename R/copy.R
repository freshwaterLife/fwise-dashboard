# copy.R
# The copy deck, part 1: chrome and pages. Every user-facing string lives in
# the deck (this file plus copy_contribute.R, copy_export.R, copy_forms.R).

FW_COPY = list(

  # ---- Application chrome ----------------------------------------------------

  app = list(
    title      = "FWISE",
    full_title = "FWISE: Freshwater Invasive Species Eradication Database",
    # Link preview text (meta and og:description). Plain words, no markup.
    meta_description = "Freshwater Invasive Species Eradication Database",
    # schema.org description for crawlers. 50+ characters for Google Dataset
    # Search, and enough text that firewall crawlers don't block the domain.
    dataset_description = paste(
      "FWISE is a free, open world database of freshwater invasive species",
      "eradication attempts, their methods and outcomes, built by Freshwater",
      "Life and partners."
    ),
    # The <noscript> block, for JS-off browsers and crawlers. Plain text, no
    # ** or [[ ]]. Drafted by us, not the client.
    noscript_heading = "FWISE: the Freshwater Invasive Species Eradication Database",
    noscript_body = c(
      paste(
        "Freshwaters cover less than 1% of the earth yet are home to 45% of all",
        "threatened animal species, and eradicating freshwater invasive species is",
        "the most effective way to save them from extinction."
      ),
      paste(
        "FWISE brings together eradication attempts from around the world, with",
        "the species, the waterbody, the methods used and the outcome, so that",
        "managers, researchers and funders can see what has worked and plan new",
        "eradications."
      ),
      paste(
        "FWISE is built and maintained by Freshwater Life with the University of",
        "California, Santa Cruz, Scripps Institution of Oceanography and the",
        "IUCN SSC Invasive Species Specialist Group. The data are released under",
        "CC BY-NC 4.0 and the code under the MIT license."
      )
    ),
    noscript_needs_js = "The interactive database needs JavaScript. Please turn it on to explore, plan and add records.",
    noscript_link_fwl     = "Freshwater Life",
    noscript_link_github  = "Source code on GitHub",
    noscript_link_privacy = "Privacy and data terms",
    # Screen-reader text for the loader. See fw_loader().
    loading    = "Loading FWISE",
    built_by   = "This app was built by **Weird Fishes Advisory**.",
    data_by    = "FWISE database is built and maintained by **Freshwater Life and friends**.",
    illustrated_by = "Logo and illustrations by **Georgie Bull**."
  ),

  # Navigation labels. Order here is the order in the navbar.
  nav = list(
    home       = "The solution",
    explore    = "Explore the database",
    plan       = "Plan an eradication",
    contribute = "Add a record",
    networking = "Connect with community",
    about      = "About",
    # Hidden from the navbar; reached from the footer and ?page=privacy.
    privacy    = "Privacy and data terms"
  ),

  footer = list(
    last_updated  = "Data last updated",
    in_review_one  = "record in review",
    in_review_many = "records in review",
    doi_label     = "DOI on Zenodo",
    # "#" until the dataset is on Zenodo; DOI links open the "coming soon"
    # card until then. Put the real address here. See fw_doi_link().
    doi_url       = "#",
    doi_soon_title = "Coming soon",
    doi_soon      = paste(
      "The archived FWISE dataset will be published on Zenodo, with a DOI you",
      "can cite. Check back soon."
    ),
    github_label  = "Source code on GitHub",
    github_url    = "https://github.com/freshwaterLife/fwise-dashboard", 
    licence       = "Data released under CC BY-NC 4.0 (non-commercial data). Code released under the MIT license.",
    # The one copy of the FWISE address. Never served as written:
    # fw_email_reveal() encodes it. Pasted together so repo scrapers miss it.
    contact_email  = paste0("fwise", "@", "fwlife.org"),
    contact_label  = "Contact FWISE",
    contact_aria   = "Show the FWISE contact email address",
    logo_alt_fwise = "FWISE, the Freshwater Invasive Species Eradication database",
    logo_alt_wfa   = "Weird Fishes Advisory",
    # The collaborators names.
    logo_alt_ucsc    = "University of California, Santa Cruz",
    logo_alt_scripps = "Scripps Institution of Oceanography, UC San Diego",
    logo_alt_fwl     = "Freshwater Life",
    logo_alt_issg    = "IUCN SSC Invasive Species Specialist Group",
    logo_alt_unil    = "University of Lausanne",
    logo_alt_vetinst = "Norwegian Veterinary Institute",
    # Collaborator logo links. The FWISE logo links to the Welcome page.
    wfa_url       = "https://www.weirdfishes.fish",
    ucsc_url      = "https://www.ucsc.edu/",
    scripps_url   = "https://scripps.ucsd.edu/",
    fwl_url       = "https://fwlife.org/",
    issg_url      = "https://issg.org/",
    unil_url      = "https://www.unil.ch/unil/fr/home.html",
    vetinst_url   = "https://www.vetinst.no/en"
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
    empty_value  = "-",
    # A server error the session survived. See fw_safely().
    went_wrong   = "Something went wrong there. Please try again.",
    # The revealed email address's Copy button. See fw_email_reveal().
    email_copy       = "Copy",
    email_copy_aria  = "Copy {address}",
    email_copied     = "Copied",
    email_copied_say = "Email address copied"
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
    email_name  = "Show the email address for {name}"
  ),

  # ---- Charts --------------------------------------------------------------------
  # Axis titles, tick labels and hover text. %{x}-style placeholders are
  # plotly's.
  charts = list(
    x_year        = "Year the attempt began",
    y_cumulative  = "Attempts to date",
    x_attempts    = "Attempts",
    # Matches the "Success rate" toggle. Successful against Failed only.
    x_share       = "Success rate (%)",
    # Duration axis ticks, in step with FW_CHART$duration_ticks. The axis has
    # no title.
    duration_ticks = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"),
    # The duration box's hover, in the map card's units. See
    # fw_duration_box_stats().
    duration_hover = paste0(
      "<b>{method}</b> ({n} successful)<br>",
      "Typical (median): {median}<br>",
      "Box: {q1} to {q3}<br>",
      "Range: {shortest} to {longest}"
    ),
    # PDF map scale bar, naming the latitude it holds at. See fw_gg_scale_bar().
    scale_bar     = "{km} km at {lat}",
    other         = "Other",
    # Shown in a chart's slot when there's nothing to draw.
    empty         = "Nothing to draw for this selection.",
    # The camera button's tooltip. See fw_png_button().
    save_png      = "Download chart as a PNG"
  ),

  # ---- Maps ----------------------------------------------------------------------
  maps = list(
    # Title and (i) for every interactive map. The PDF map uses
    # plan$pdf_map_note.
    title = "Where attempts happened",
    note = paste(
      "Each marker is one eradication attempt, coloured and labelled by",
      # The \n is a line break (the note is white-space: pre-line).
      "outcome. Hover for a summary, select for the full record.\nToggle",
      "between layers for hydrological, topographic, and satellite imagery."
    ),
    # The basemap switcher.
    basemap_plain     = "Plain",
    basemap_water     = "Water",
    basemap_terrain   = "Terrain",
    basemap_satellite = "Satellite",
    year_one  = "year",
    year_many = "years",
    # Duration units for the hover card and record. See fw_popup_duration().
    day_one   = "day",
    day_many  = "days",
    # Under both interactive maps (client wording), at $fw-size-fine.
    mercator_note = paste(
      "Leaflet currently requires the use of Mercator. This map will be",
      "updated to Equal Earth when it becomes available."
    ),
    # Accessible name and hover title of a marker group.
    stack_title = "{n} attempts at this point - zoom in or click to open them"
  ),

  # ---- Home (the Welcome page) -----------------------------------------------

  home = list(
    # One string per line: fw_page_header() puts each on its own line.
    title = c(
      "Freshwaters cover **<1%** of earth yet are home to **45% of all Threatened animal species**.",
      "Eradicating freshwater invasives is **the most effective way to save them from extinction**."
    ),
    lead = c(
      "But almost nobody knows this. Enter the **Freshwater Invasive Species Eradication Database**.",
      paste(
        "Use FWISE now to [[explore|understand this solution]],",
        "[[plan|plan a new eradication]], [[contribute|add your own data]],",
        "and [[networking|connect with community]]."
      )
    ),
    # The ">" sits inside the bold so it's styled with its figure.
    kpi_sentence = paste(
      "**{attempts}** successful eradications recorded so far have protected **>{protected}** species.",
      "Click the species to read worldwide success stories."
    ),

    # ---- Success stories ----
    # Client copy, one entry per story, A-Z by continent label (so South
    # America is last), keyed like FW_HOME_IMG$stories. Page order is
    # FW_HOME_ORDER. `body` is one string per paragraph; [words](https://...)
    # is an inline link (fw_home_story_text()). `links`: `name` is the link
    # text, `note` the parenthetical. `title` may differ from
    # `beneficiary$name`, which is what the tile announces.
    stories_open = "Read the {continent} success story: {name}",
    card_close = "Close story",
    image_placeholder = "Image to come",
    story_links_heading = "Links",
    stories = list(
      africa = list(
        continent = "Africa",
        title   = "Fiery redfin",
        summary = "A historic restoration that, in just a few months, brought back this Endangered fish over a decade after it had disappeared.",
        body    = "In South Africa's Western Cape, world famous for its biodiversity, runs the Rondegat River: home to Threatened fish species including the fiery redfin. But smallmouth bass, introduced to South Africa for angling in 1937, invaded the lower river up to a waterfall and ate almost all the redfins and other native fish. So in 2012 and 2013, CapeNature and partners treated 4 km of river with the plant-based traditional fishing toxin rotenone. They upgraded a weir to stop the bass from coming back, and cleared invasive trees along the banks so the whole river could heal. What happened next was utterly amazing: native fish began returning within months, their densities rising from fewer than 1 to nearly 12 fish per 100 m², and the river's insects and other invertebrates bounced back quickly. All for about US$250,000 to remove the bass (US$64,000/km) and US$550,000 for tree clearing: a low cost to bring a river back to life.",
        links   = list(
          list(name = "Threatened Endemic Fishes in South Africa’s Cape Floristic Region: A New Beginning for the Rondegat River",
               note = "(article in Fisheries with great details and graphs)",
               url  = "https://units.fisheries.org/rotenone-stewardship/wp-content/uploads/sites/45/2019/02/Rotenone-Use-South-Africa.pdf")
        ),
        beneficiary = list(name = "Fiery redfin", alt = "Illustration of a fiery redfin")
      ),
      asia = list(
        continent = "Asia",
        title   = "Little grebe カイツブリ",
        summary = "A beloved bird on the brink in Tokyo, brought back by the restoration of a historic pond.",
        body    = "Grebes are diving waterbirds that look a bit like ducks, yet their closest living relatives are, surprisingly, flamingos. And they are in trouble worldwide. Three species have gone Extinct since 1977 (in Colombia, Guatemala, and Madagascar), and invasive fish were involved in each extinction. Four more species of grebe are Critically Endangered or Endangered, with invasive fish again big threats. So when largemouth bass and bluegill were introduced to Tokyo's Inokashira Pond, a pond once clean enough to drink from, Tokyo's little grebes were in trouble. The invasives ate the small fish and shrimp the grebes depend on, and the grebes stopped breeding and eventually vanished from the pond. Local residents noticed, and from 2007 worked with the Tokyo Metropolitan Government to fight back, finally reviving a traditional Japanese practice called kaibori: draining the pond, letting its bed dry in the sun, and sorting every animal caught, returning the natives and removing the invaders. After three kaibori the bass and bluegill were gone for good. Native fish and shrimp rebounded, rare water plants returned after some 60 years, and the grebes came home. In 2025, eight pairs bred and 44 chicks hatched!",
        links   = list(
          list(name = "The premier breeding ground for little grebes in Tokyo",
               note = "(blog post in the Inokashira Pond Newsletter with great photos and graphs)",
               url  = "https://www.kensetsu.metro.tokyo.lg.jp/jimusho/seibuk/seibukouen0041/ikedayorir7.11")
        ),
        beneficiary = list(name = "Little grebe", alt = "Illustration of a little grebe")
      ),
      europe = list(
        continent = "Europe",
        title   = "Freshwater pearl mussel",
        summary = "One of the world’s longest-living animals (up to nearly 200 years!), this Endangered species is recovering after decades of decline thanks to a national programme in Norway to save the Atlantic salmon.",
        body    = "Freshwater bivalves are among the world's most threatened group of animals, with dozens of Extinct species. So this story is a source of much-needed hope. The Endangered freshwater pearl mussels can live for nearly 200 years, but their larvae can’t survive alone — they need to harmlessly hitch a ride on the gills of a juvenile Atlantic salmon for a few months before they drop off and settle on the river bottom. And, the mussel becomes particularly vulnerable in rivers infected by the invasive parasite known as the salmon killer (Gyrodactylus salaris). The parasite causes high mortality of juvenile salmon and leads indirectly to reduced recruitment of freshwater pearl mussels. Thankfully, the Norwegian Veterinary Institute has been using rotenone, a piscicide derived from traditional fishing methods, to eradicate infected fish and then restock healthy salmon. And today, in the Steinkjer rivers, high density of young salmon has led to exceptionally strong recruitment of freshwater pearl mussels. Restoring relationships, saving species.",
        links   = list(
          list(name = "Beautifully thorough yet elegant report from the Norwegian Research Information Repository",
               note = "(though you’ll likely need to translate it!)",
               url  = "https://hdl.handle.net/11250/2642750")
        ),
        beneficiary = list(name = "Freshwater pearl mussel", alt = "Illustration of a freshwater pearl mussel")
      ),
      north_america = list(
        continent = "North America",
        title   = "Apache trout",
        summary = "This sacred trout was removed from the US Endangered Species List thanks to a historic Indigenous-led decades long recovery effort.",
        body    = "“Colonization wasn’t kind to the Apache trout. The newcomers caught and ate them by the bushel, and logging, overgrazing and mining degraded their mountain creeks. Worst of all were the legions of non-native trout — brook, brown and rainbow — that the state stocked for mining camps and anglers, which swiftly overwhelmed their native rivals. By the 1940s, Apache trout endured in just 12 streams — all of them on the Fort Apache Indian Reservation.” So the tribe, to whom the Apache trout is sacred, banned all fishing and started working with the state of Arizona (USA) to build barriers that keep invasive fish from moving upstream. Then they started eradicating invasive fish above the barrier and reintroducing Apache trout. Today the Apache trout lives in 30 streams, and in 2024 was removed from the list of threatened species under the US Endangered Species Act. That makes it the first ever sport fish and trout species removed from the US federal list due to recovery!",
        links   = list(
          list(name = "After half a century, the Apache trout swims off the threatened species list",
               note = "(article in the High Country News with stunning photographs)",
               url  = "https://www.hcn.org/issues/56-11/after-half-a-century-the-apache-trout-swims-off-the-threatened-species-list/")
        ),
        beneficiary = list(name = "Apache trout", alt = "Illustration of an Apache trout")
      ),
      oceania = list(
        continent = "Oceania",
        title   = "Golden galaxias",
        summary = "The world’s largest and longest ever successful carp eradication that restored both this Endangered endemic and the trout fishery!",
        body    = "The Tasmanian Inland Fisheries Service (IFS) Carp Management Program achieved the world's largest and longest ever successful carp eradication from Lakes Sorell (≈54 km²) and Crescent (≈23 km²). They restored these Ramsar-listed wetlands both to save endemic and Endangered species like the Endangered golden galaxias and bring back what had once been a thriving recreational trout fishery. This one-of-a-kind project took 28 years (1995-2023) and pioneered innovative techniques like “Judas carp” radio-telemetry. A historic success showing that almost anything is possible with enough resources and will. Even restoring lakes thought to be “too big”.",
        links   = list(
          list(name = "The largest lake system globally from which common carp have been successfully eradicated",
               note = "(informative learning resource with great photos and videos from Tasmania in Action)",
               url  = "https://sites.google.com/ourcommonplace.org.au/tasmaniainaction/the-case-studies/eradication-of-common-carp-from-lakes-crescent-and-sorell-tasmania?pli=1&authuser=0")
        ),
        beneficiary = list(name = "Golden galaxias", alt = "Illustration of a golden galaxias")
      ),
      latin_america = list(
        continent = "South America",
        title   = "Valcheta frog",
        summary = "Critically Endangered microendemics brought back from the edge of extinction by Argentina’s first-ever freshwater eradication.",
        body    = c(
          "On the cold and windy Patagonian steppe of Argentina rises a vast basaltic tableland: the Somuncura Plateau. And in a place where it barely rains, water is everything. Every stream is a lifeline. Take the Arroyo Valcheta: here live four species found nowhere else on earth: the Valcheta Frog, a fully aquatic and evolutionarily distinct frog. The Naked characin, a small fish that loses its scales as it grows, the only one of its kind; and also the only native fish species of the stream. And two tiny freshwater snail species so rare they only have scientific names. But all four species are Critically Endangered, being eaten to extinction by invasive trout. Until May 2022 when the Fundación Somuncura and partner Freshwater Life eradicated the invasive fish in a pilot stretch of stream in a single day. And then the stream did the rest: the endemic species came back. This was the first-ever freshwater eradication in Argentina and won the Whitley award. Now Fundación Somuncura is planning to build an invasive fish barrier and restore 30 km of stream. This will save all four species, securing a future for some of the least visible inhabitants of the Patagonian desert.",
          "And all four species will be downlisted from Critically Endangered to not just Endangered, but Vulnerable! Such a feat is exceedingly rare: from 2007 to 2024, of all the 164k species assessed on the Red List, [only 4 species a year across the entire world were downlisted by two or more categories](https://doi.org/10.1111/cobi.70103)."
        ),
        links   = list(
          list(name = "On a Patagonian plateau, a microendemic frog makes a hopeful comeback",
               note = "(article in Mongabay that’s a quick and inspiring read)",
               url  = "https://news.mongabay.com/2025/06/on-a-patagonian-plateau-a-microendemic-frog-makes-a-hopeful-comeback/")
        ),
        beneficiary = list(name = "Valcheta frog", alt = "Illustration of a Valcheta frog")
      )
    ),

    # ---- Current work and gaps ----
    # The caption is the legend: `now` and `later` are set in the map's colours.
    map_slider_label = "Reveal the priority countries map over the successful eradications map",
    map_caption = c(
      "Move the slider from ",
      now  = "countries with eradications",
      " to ",
      later = "top opportunity countries",
      "."
    ),


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
    # Whole database, never filtered. Counts only, no rate.
    db_heading = "The database",
    db_attempts    = "recorded attempts",
    db_countries   = "countries",
    db_invasive    = "invasive species targeted",
    db_beneficiary = "species protected",
    db_beneficiary_tip = paste(
      "Species recorded as being protected. This often goes unreported. And",
      "even when it is, relationships among species often mean that many more",
      "species are protected than recorded (think of a frog-focused project",
      "where birds benefit from more mayflies. That’s tens of species",
      "protected, with only one noted)."
    ),
    in_review = "in review",
    in_review_tip = paste(
      "Submissions waiting for review by the FWISE team.",
      "They are not included here."
    ),

    # ---- The filters ---------------------------------------------------------
    f_heading = "Narrow the list",

    # ---- The summary graphics ------------------------------------------------

    # One block, three views, picked by the "Show" switch.
    views_title = paste(
      "How many eradications have been recorded? How well do they work?",
      "How long do they take?"
    ),
    views_show     = "Show",
    view_count     = "Number of attempts",
    view_rate      = "Success rate",
    view_duration  = "Duration",

    method = "Methods used",
    method_note = paste(
      "One bar per method, split by outcome. Only attempts that used a single",
      "method are shown, so each outcome belongs to one method. Hover a",
      "segment for its count or success rate."
    ),
    method_rate_note = paste(
      "Successful against failed attempts for each method, from attempts that",
      "used a single method. Ongoing and unknown attempts are left out. The",
      "number beside each method is how many finished attempts the rate rests on."
    ),
    cumulative = "Eradication attempts over time",
    duration = "How long successful eradications took",

    # Under the charts. [[plan|...]] links to that tab (fw_home_links()).
    to_plan = "[[plan|Dig deeper into the data to **plan your own eradication**]].",

    # Explore only: its own paragraph under maps$note.
    map_contribute = "Don't see your own eradication attempts on here? [[contribute|Please add them now!]]"
  ),

  # ---- Species and map popups ------------------------------------------------

  species = list(
    no_image   = "No photograph available",
    fig_none   = "None noted",
    alt_prefix = "Photograph of",
    # Every field is always drawn; empty ones say this (cards, records, tiles).
    p_not_noted = "Not noted",
    p_species  = "Invasive species targeted",
    p_beneficiary = "Species protected",
    # The hover card's shorter pair, the same words as the photograph captions.
    p_targeted  = "Targeted",
    p_protected = "Protected",
    fig_invasive = "Targeted",
    fig_beneficiary = "Protected",
    # After a protected species' name on cards, records, tiles and reports.
    # Never on invasive species, pickers, the form or downloads. {code} is the
    # Red List code. See fw_species_shown().
    iucn_suffix = " (IUCN: {code})",
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
  # Labels for the filter engine (R/filters.R). Keys match FW_FILTERS$copy.

  filters = list(
    continent   = "Continent",
    country     = "Country",
    taxa        = "Invasive animal",
    species     = "Invasive species",
    beneficiary = "Species protected",
    taxa_beneficiary = "Animal protected",
    family      = "Fish family",
    family_beneficiary = "Fish family protected",
    method      = "Method used",
    regime      = "Still or flowing water",
    outcome     = "Outcome",
    size        = "Size of the area treated",
    years       = "Attempt began between",
    no_year     = "Include attempts with no recorded start year",
    no_size     = "Include attempts with no recorded size",
    unit_ha     = "Hectares (still water)",
    unit_km     = "Kilometres (flowing water)",
    size_in_ha  = "in hectares",
    size_in_km  = "in kilometres",
    size_in_both = "in hectares and kilometres",
    # The unit printed after each figure on a size slider's handles and ends.
    unit_short_ha = "ha",
    unit_short_km = "km",

    # ---- The tips -------------------------------------------------------------
    # Continent has no tip, so no (i).
    tip_country = paste(
      "Country of the eradication attempt(s). Only countries with attempts",
      "recorded in FWISE are listed. If the country you are looking for is not",
      "here, filter by continent instead."
    ),
    tip_regime = paste(
      "Still water is lakes, ponds, reservoirs, etc.; flowing water is rivers,",
      "streams, canals, etc."
    ),
    tip_taxa = paste(
      "The broad group the invasive species belongs to (fish, crayfish,",
      "mussels, etc.)."
    ),
    tip_species = paste(
      "The species (often plural) targeted for eradication. You can search by",
      "common or scientific name."
    ),
    tip_beneficiary = paste(
      "The species the attempt was meant to help. This often goes unreported,",
      "and even when it is, relationships among species often mean that many",
      "more species are protected than recorded (think of a frog-focused",
      "project where birds benefit from more mayflies. That’s tens of species",
      "protected, with only one noted). So if your species of interest isn’t",
      "here, think and filter instead by where it lives, or by species it",
      "interacts with."
    ),
    tip_taxa_beneficiary = paste(
      "The broad group the species protected belongs to (fish, amphibian,",
      "bird, etc.). Recorded less consistently and less thoroughly than the",
      "invasive side."
    ),
    tip_family = paste(
      "The family the invasive fish belong(s) to - trout and salmon are",
      "Salmonidae, carp and minnows Cyprinidae, etc."
    ),
    tip_family_beneficiary = paste(
      "The family the fish protected belong(s) to."
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
      "flowing water in kilometres. The scale is logarithmic, because recorded",
      "sizes run from a fraction of a hectare to tens of thousands of them, and",
      "from a hundred metres to several hundred kilometres."
    ),
    tip_no_size = paste(
      "{n} attempts have no size recorded. Leaving this ticked keeps them in",
      "whatever range you choose, so they are not silently dropped."
    ),
    tip_years = paste(
      "The year the attempt began. The record starts at {min} but stays sparse",
      "(<100 recorded attempts per decade) until the 1990s."
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
    description = "View attempts similar to your situation or interest via filter selections, then click **Build report**.",

    # ---- The callout ---------------------------------------------------------
    # Client wording, one string per paragraph. The only place "All fields are
    # optional" is said.
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
      "That combination has nothing in it. This could be because there are no",
      "eradication attempts meeting the filtered criteria, or that potentially",
      "relevant attempts did not report all the data you filtered for."
    ),
    zero_hint_lead = "Try relaxing one of these first:",
    zero_hint_none = "Try clearing a filter and building again.",

    build   = "Build report",
    clear   = "Clear all filters",
    download_heading = "Take this away",
    download_open = "Download this report",
    download_close = "Close",
    download_lead = paste(
      "Choose what to include. More than one and they arrive together in a zip."
    ),
    download_parts = "Include in your download",
    download = "Download",
    download_xlsx = "Spreadsheet of the data (.xlsx)",
    download_xlsx_note = paste(
      "Every field (columns) of every matching attempt (rows), with the field",
      "definitions, the filters you applied, and the caveats on their own",
      "sheets."
    ),
    # Client names. Order follows FW_BUNDLE_PARTS in export.R.
    download_pdf = "Summary report (.pdf)",
    download_pdf_note = paste(
      "This report ready to read or share."
    ),
    download_records = "Detailed report (.html)",
    download_records_note = paste(
      "The summary report's charts and map, interactive, then every matching",
      "attempt written out in full with a search box. Opens in any browser."
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
    # Shown while nothing is ticked; Download stays disabled.
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
    # Under the filters table: what the report is and isn't for.
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
      "released under CC BY-NC 4.0."
    ),
    # Above the PDF's contacts table (the six busiest contacts).
    report_contacts_note = paste(
      "The up to six most involved people recorded against the attempts above.",
      "An address here means either that it is publicly associated with an",
      "online record or that they contributed the record directly and agreed",
      "to be listed in FWISE."
    ),

    # ---- Filters -------------------------------------------------------------
    f_heading   = "Describe your situation",
    # Leads the line under Build listing the filters the report used.
    f_applied   = "Built from:",
    built_announce = "Report built. {n} attempts match your description.",

    # ---- Results -------------------------------------------------------------
    r_heading    = "What the matching attempts show",
    # Singular/plural pairs, picked by fw_plural().
    r_attempts_one  = "attempt",
    r_attempts      = "attempts",
    r_countries_one = "country",
    r_countries     = "countries",
    r_species_one   = "invasive species",
    r_species       = "invasive species",
    # Shown after ">": beneficiaries are under-recorded. Counts species
    # protected by successful attempts only (fw_plan_summary()).
    r_beneficiaries_one = "species protected",
    r_beneficiaries = "species protected",
    r_years      = "year range",
    r_outcomes   = "Outcomes",
    r_outcome_note = paste(
      "All four states are shown. Failure is as important to know about as success."
    ),
    # Map title and note are maps$title and maps$note.
    r_map_missing = "{n} of these attempts have no coordinates and are not on the map.",

    # Hidden when one regime is filtered.
    r_waterbody  = "Still and flowing water (frequency and success)",
    r_waterbody_note = paste(
      "Attempts in still water (lakes, ponds, reservoirs) and flowing water",
      "(rivers, streams), with the outcomes in each. The success rate counts",
      "successful against failed attempts; ongoing and unknown are left out."
    ),
    # Own keys, though the words currently match r_method_*.
    r_waterbody_share = "Success rate",
    r_waterbody_count = "Number of attempts",

    r_method     = "Outcomes of methods within selection",
    r_method_note = paste(
      "Outcomes within each method, with the number of attempts beside it.",
      "Only attempts that used a single method are shown, so each outcome",
      "belongs to one method. The success rate counts successful against",
      "failed attempts; ongoing and unknown are left out."
    ),
    r_method_mode  = "Show",
    r_method_share = "Success rate",
    r_method_count = "Number of attempts",
    r_method_missing = "{n} of these attempts have no method recorded and are not on the method chart.",
    r_method_multi = "{n} of these attempts used more than one method and are not on the method chart.",

    # ---- The species tiles ----------------------------------------------------

    # {n_word}: tiles shown; {total}: species of that role in the selection.
    # ">" because beneficiaries are under-recorded.
    r_species_top_inv = "Top {n_word} invasive species targeted (of {total} total)",
    r_species_top_ben = "Top {n_word} species protected (of >{total} total)",
    r_tile_attempt  = "attempt",
    r_tile_attempts = "attempts",
    # Hover for a tile segment and every success-rate bar. See fw_hover_counts().
    r_tile_seg = "{outcome}: {pc}% ({n} of {total})",
    # Hover on count-view bars.
    r_bar_count_seg = "{outcome}: {n}",
    # In a tile's bar's place when none of the species' attempts has finished.
    r_tile_none = "No finished attempts yet",
    r_duration   = "How long successful eradications took",
    r_duration_note = paste(
      "How long successful eradications took, start to finish, on a log scale",
      "going from days to years (sometimes decades)."
    ),
    r_duration_missing = paste(
      "Based on the {n} successful attempts here that have a start and end,",
      "and a single recorded method. Failed, ongoing and multi-method",
      "attempts are excluded."
    ),
    r_table_showing = "Showing",

    # ---- The record search ---------------------------------------------------
    # Last results block. See fw_plan_records_ui().
    r_records = "Find a record",
    r_records_note = paste(
      "Every attempt in this report. Search by site, country, species, method",
      "or FWISE id, then click a row to read the full record, including its",
      "reference. The Detailed report download holds all of them."
    ),
    r_records_none = "No attempt in this report matches that search.",
    col_record_years = "Years",

    # ---- Potential relevant contacts -----------------------------------------
    r_contacts = "Reach out to learn more",
    r_contacts_note = paste(
      "The people recorded against the attempts above, most involved first.",
      "An address here means either that it is publicly associated with an",
      "online record or that they contributed the record directly and agreed",
      "to be listed in FWISE."
    ),
    r_contacts_none = paste(
      "None of the attempts in this selection has a contact recorded against it."
    ),
    r_contacts_size = "Contacts per page",
    # Link to the Connect page, styled like Explore's to_plan.
    r_contacts_all  = "[[networking|Browse every contact in FWISE]].",
    col_contact_name = "Name",
    col_contact_org  = "Organisation",
    col_contact_n    = "# Attempts",
    col_contact_email = "Get in touch",
    # Download progress steps (fw_write_bundle() and the PDF report).
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
      "Sign up to stay in the loop. And read below on how to cite FWISE, how it",
      "was built, and more information and resources."
    ),

    # ---- Sign-up -------------------------------------------------------------
    # The button opens the newsletter dialog (copy in R/copy_forms.R).
    signup_heading = "Keep up with FWISE",
    signup_body = paste(
      "We send an occasional update when a new release of the",
      "database goes out, or when something is published from it. No more than",
      "a few times a year."
    ),
    signup_action = "Sign up for updates",

    # ---- Citation ------------------------------------------------------------
    cite_heading = "How to cite FWISE",
    cite_summary = "The citation to use in any publication that draws on FWISE",
    cite = paste(
      "FWISE data is open source and freely available for use. Please cite it in any publication that uses it with the below citation."
    ),
    # Client wording, literal. Edit the year and version by hand at a new
    # release. Read via fw_citation_text() by About and every download.
    citation_db = paste(
      "FWISE (2026). FWISE: Freshwater Invasive Species Eradication Database",
      "(Version 1) [Web application]. https://fwise.org"
    ),

    # ---- The panels ----------------------------------------------------------
    # Each is a <details>: `*_heading` is the summary, `*_summary` the line
    # saying what's inside.

    # Caveats about the whole database. Text from fw_caveat_blocks(), also in
    # every download.
    caveats_heading = "Data caveats",
    caveats_summary = "What to keep in mind when reading this data",

    method_heading = "How it was built",
    method_summary = "Where the records come from and how they were compiled",
    # Client wording. Same as export$methods in copy_export.R: change both.
    # method_facts is a link to the project behind the Welcome page facts.
    method = paste(
      "FWISE’s data comes from two sources: a standardized literature review",
      "performed in 2026, and directly from data contributors. We will publish",
      "the full methodology in a forthcoming peer-reviewed paper summarizing",
      "main findings from FWISE."
    ),
    method_facts = "Here is the data generating the facts on the landing page.",
    facts_url = "https://github.com/freshwaterLife/FWISE-FACTS",

    # ---- Methods glossary ----------------------------------------------------
    # One entry per method in FW_METHODS, same names. dev/value_test.R checks
    # the two lists match.
    glossary_heading = "Methods glossary",
    glossary_summary = "What each eradication method means",
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
      list(term = "Other chemical methods", body = paste(
        "Like chloramine, aluminum sulfate , toxaphene, etc.")),
      list(term = "Other mechanical methods", body = paste(
        "Like shooting, angling, explosives, etc."))
    ),

    # ---- Related databases ---------------------------------------------------
    # Client list: names and URLs only.
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

    other_heading = "License and links",
    other_summary = "Species photographs, licence and links",
    images_heading = "Photographs",
    licence_heading = "Code",
    links_heading = "Data",
    link_zenodo = "The archived dataset on Zenodo",

    # ---- Feedback ------------------------------------------------------------
    # Opens the feedback dialog. Its own band, not a panel, so it's always
    # visible.
    fb_body = paste(
      "FWISE is an evolving and collective app. Please tell us how to make it better. And let us know if a record is wrong, a species is misnamed, or something on this site does not work."
    ),
    fb_action = "Send feedback"
  ),

  # ---- Contacts --------------------------------------------------------------

  networking = list(
    title = "Connect with community",
    description = paste(
      "Reach out to the people doing eradications in the places and of the",
      "species you’re interested in to learn more."
    ),
    # Blank, not drawn.
    intro = "",
    # The callout at the foot of the page.
    outro_text   = paste(
      "If you spot a problem with a contact, or need details updated or",
      "removed, please let us know:"
    ),
    # Pressing it reveals footer$contact_email (fw_email_reveal()).
    outro_action = "Contact the FWISE team",

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

    # Honest coverage, so the directory doesn't imply reach it lacks. Numbers
    # are computed from the data.
    coverage = paste(
      "This directory covers {reachable} of the {total} attempts in FWISE.",
      "{no_email} of the {contacts} people listed have no published email",
      "address, and {no_contact} attempts have no contact recorded at all.",
      "If the right person is not obvious from this list, write to the FWISE",
      "team and we will try to point you to someone who can help."
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

    email_action     = "Show email",
    email_none_label = "No public email address",
    no_organisation  = "Not recorded",

    page_size     = "Contacts per page",
    page_showing  = "Showing"
  )
)

# ---- Reading the deck --------------------------------------------------------

#' The whole copy deck as one list
#'
#' Built on every call: source order differs between Shiny and test scripts, so
#' a cached merge could miss a file.
fw_copy_all = function() {
  c(FW_COPY, FW_COPY_CONTRIBUTE, FW_COPY_EXPORT, FW_COPY_FORMS)
}

#' A string from the deck: fw_t("networking", "title")
#'
#' Stops on an undefined path. Never call at the top level of a file in R/:
#' the deck isn't complete until every file is sourced.
fw_t = function(...) {
  path = c(...)
  out = fw_copy_all()
  for (p in path) {
    out = out[[p]]
    if (is.null(out)) stop("No copy defined at: ", paste(path, collapse = " > "),
                           call. = FALSE)
  }
  out
}

#' Fill the {name} slots in a string
#'
#' Fills every occurrence; a slot with no value is left visible.
fw_fill = function(text, ...) {
  values = list(...)
  for (nm in names(values)) {
    text = gsub(paste0("{", nm, "}"), as.character(values[[nm]]), text,
                 fixed = TRUE)
  }
  text
}

#' A number from 1 to 20 in words ("ten"); digits beyond that
fw_num_word = function(n) {
  words = c("one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  n = as.integer(n)
  if (!is.na(n) && n >= 1L && n <= length(words)) words[n] else as.character(n)
}
