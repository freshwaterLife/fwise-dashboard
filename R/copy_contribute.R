# copy_contribute.R
# EVERY user-facing string on the contribute form: the page copy, the section
# titles and blurbs, every field label, help line, placeholder, tooltip,
# validation message and announcement. It is read through the same fw_t()
# as the rest of the copy deck - fw_t("contribute", "fields", "site_name") -
# because fw_copy_all() in copy.R merges this list into FW_COPY at call time.
#
# It is its own file because the form is the largest single block of copy in
# the app and copy.R was becoming hard to scan. The section name `contribute`
# must not also appear in copy.R or copy_export.R: fw_copy_all() would
# silently keep the first one it met.
#
# House style: UK spelling, sentence case, active voice. A button says what
# happens and keeps the same name through the whole flow.
#
# Field labels, prompts and tooltips follow the FWISE Upload Form field
# specification. Where the specification was ambiguous a TODO(alex) comment
# sits next to the field in mod_contribute_steps.R.

FW_COPY_CONTRIBUTE <- list(

  contribute = list(
    title = "Contribute data",
    description = paste(
      "Add an eradication attempt to FWISE. It takes about fifteen minutes and",
      "only a few fields are required."
    ),

    # THE DEFINITION AND THE SCOPE, pinned to the top of the page. Every word of
    # this is the client's (24 Sept 2026); do not reword any of it without
    # asking them. Shown by fw_preamble() on the contribute page only - it came
    # off About on 21 Sept, see the note at the top of mod_about.R.
    #
    # THE ** PAIRS ARE LOAD-BEARING: fw_emphasis() turns them into <strong>, and
    # the client chose which clause in each point is the one that has to be read
    # if nothing else is.
    #
    # `citation` is the reference `cite_short` resolves to, and the only place
    # in the app where it is written out in full. Keep the three in step, and
    # keep them in step with the outcome tooltip below, which cites the same
    # work.
    #
    # IT USED TO BE FOUR. A "How success is defined" caveat in copy_export.R
    # carried the same reference until the client replaced every caveat with a
    # placeholder for their own (24 Sept 2026), so THIS is now the only place
    # in the app that defines what success means. When the FWISE team's caveats
    # arrive, check whether theirs defines it too, and if so keep the two in
    # step - a reader who met the definition on this form must not meet a
    # different one in the download.
    preamble = list(
      heading = "What counts as an eradication",
      definition = paste(
        "**Eradication is the complete and permanent removal of all wild",
        "populations of a species from a defined area by means of a",
        "time-limited campaign**"
      ),
      cite_short = "Genovesi 2000",
      cite_url = "https://rm.coe.int/1680746248",
      citation = paste(
        "Genovesi, P. (2000). Guidelines for Eradication of Terrestrial",
        "Vertebrates: A European Contribution to the Invasive Alien Species",
        "Issue. IUCN/SSC Invasive Species Specialist Group, National Wildlife",
        "Institute, Italy."
      ),
      scope_heading = "What is the scope of contributions?",
      # A list rather than prose because the three points answer different
      # questions: which outcomes we want, what we are not, and which organisms.
      scope = c(
        paste(
          "FWISE **includes and is equally interested in all outcomes of an",
          "eradication**: successful, failed, ongoing, and unknown. Failure is",
          "as important to know about as success. Please submit both past",
          "eradication attempts (even if they occurred long ago and much data",
          "lacks) and current ones (even if unfinished, they help track global",
          "progress as new species and geographies are addressed)."
        ),
        paste(
          "FWISE **does not include control**: the reduction of population",
          "density and abundance, in order to keep damage at an acceptable",
          "level (Genovesi 2000). Though control can be important and",
          "necessary, eradication (time-limited) is cheaper and more enduring",
          "than control (long-term)."
        ),
        paste(
          "FWISE includes freshwater invasive animals: fish, mussels, crayfish,",
          "amphibians, etc. **It does not include plants.** The context and",
          "methods of freshwater animal versus plant eradications are",
          "different."
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

    # Consent controls, section 0.1 of the specification. At the foot of the
    # form since 24 Sept 2026 - see fw_step_review_ui().
    consent = list(
      heading = "Consent",
      # [NEEDS CLIENT SIGN-OFF] Reworded 24 Sept 2026 at Alex's request. The
      # specification's wording also granted permission to DISPLAY contact
      # details, which contradicted the separate opt-in for exactly that
      # (fields$contact_public), so this now covers storing and using the record
      # only.
      statement = paste(
        "By sending this record you agree that FWISE staff may store and use",
        "the information in it to review the record, publish it in the FWISE",
        "database, and contact you about your submission."
      ),
      agree_label = "I agree to FWISE storing and using this information as described.",
      agree_yes   = "Yes, I agree",
      # [PLACEHOLDER] the full terms of data use are still being drafted
      terms_link_label = "Read the full terms of data use",
      terms_url = "#"
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
      contributor = "Who to credit and contact. We need a contact so the review team can follow up.",
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
    ),
    # ---- Field labels ----------------------------------------------------------
    # One entry per question on the form, keyed by the input id it labels (or
    # the heading it names). mod_contribute_steps.R reads these through
    # fw_lab(); the question-list downloads read the rendered form, so they
    # follow automatically.
    fields = list(
      site_name        = "Site name",
      country          = "Country",
      country_other    = "Please name the country or territory",
      region           = "Region or state",
      location         = "Location on map",
      latitude         = "Latitude",
      longitude        = "Longitude",

      water_regime     = "Is it still water or flowing water?",
      waterbody_type   = "What kind of waterbody is it?",
      waterbody_other  = "Please describe the waterbody type",
      area_treated     = "Size of the area treated",
      area_unit        = "Unit",
      area_notes       = "Anything else about the area treated?",
      depth_m          = "Average or estimated depth (m)",
      depth_notes      = "Notes on depth",
      volume_m3        = "Estimated volume (m3)",
      volume_notes     = "Notes on volume",
      max_flow         = "Maximum flow (m3/s)",
      water_temp       = "Water temperature (degrees Celsius)",
      water_temp_notes = "Notes on water temperature",

      invasion_year    = "When did the invasion happen? (year)",
      start_year       = "Year the eradication attempt began",
      end_year         = "Year the eradication attempt ended",
      duration         = "Estimated total duration (days)",
      driver           = "What was the main reason for the eradication attempt?",
      driver_other     = "Please describe the reason",

      methods_heading  = "Methods used",
      method_main      = "Main eradication method",
      method_n         = "Method {n}",
      method_other     = "Please name the method",
      method_notes     = "Notes on how this method was applied",
      method_description = "A fuller description of the approach at this site",
      labour           = "Effort required (person-days)",
      cost             = "Estimated cost of the eradication attempt",
      cost_notes       = "Notes on cost",

      ingredient_basis = "Is the concentration for the active ingredient or the product?",
      toxin_conc       = "Target concentration of the chemical (mg/L)",
      conc_target_notes = "Notes on the target concentration",
      toxin_conc_measured = "Measured concentration of the chemical (mg/L), if measured",
      conc_measured_notes = "Notes on the measured concentration, if different",
      neutralising     = "Neutralizing agent used, if any",
      neutralising_other = "Please name the neutralizing agent",
      neutralising_notes = "Notes on the neutralizing agent",

      outcome          = "What was the outcome?",
      verification     = "How was the outcome verified? Method and any notes",
      reference        = "Link or citation for the underlying evidence",
      source           = "Where did this record come from?",

      contact_heading  = "Contact information",
      contact_public     = paste(
        "I give permission for my personal information (name, organization,",
        "email) to be displayed in app for people to contact me regarding",
        "eradications."
      ),
      contact_public_yes = "Yes, show",
      contact_name     = "Name",
      contact_email    = "Email",
      contact_org      = "Organisation",

      notes            = "Notes for FWISE",

      # The repeatable rows. {n} is the row number.
      target_heading   = "Target",
      target_taxa      = "What kind of animal was targeted?",
      target_taxa_other = "Please describe the group targeted",
      species          = "Which species?",
      benefit_heading  = "Beneficiary",
      benefit_taxa     = "What kind of species was protected?",
      benefit_taxa_other = "Please describe the group protected",
      remove           = "Remove"
    ),

    # Help text printed under a control. Keyed like `fields`.
    help = list(
      contact_public   = paste(
        "Tick this and your name, organization and email appear in the app, so",
        "other practitioners can contact you about eradications. Leave it",
        "unticked and only FWISE staff will see them."
      ),
      region         = "Optional. Choose the state, province or region, or type one if it is not listed.",
      location       = "Click the map to place your site, or type coordinates below.",
      waterbody_type = "The list narrows once you have answered still or flowing.",
      area_unit      = "Hectares for still water, kilometers for flowing water.",
      end_year       = "Leave blank if the work is ongoing.",
      cost           = "Include staff time and materials. Any currency is fine, just say which in the notes.",
      reference      = "A DOI or URL is ideal.",
      source         = "For our internal provenance. Not shown publicly.",
      species        = "The list narrows to the group above. If your species is not there, type it in and our review team will add it."
    ),

    placeholder = list(
      area_notes     = "e.g. five golf-course ponds",
      species_search = "Search by common or scientific name",
      select         = "Select...",
      select_or_type = "Select or type..."
    ),

    # ---- Tooltips ------------------------------------------------------------
    # One per field, behind the (i) glyph. Kept together so the client can
    # review the whole set in one place.
    tips = list(
      site_name    = "A descriptive name for the site or waterbody, as you would refer to it in a report.",
      country      = "The country the site sits in. Pick the region as well where the site is in a territory recorded separately, such as Hawaii or the Galapagos Islands.",
      location     = "Click the map to drop a pin, or type coordinates directly. Either way the two boxes stay in step with the map.",
      waterbody    = "The kind of waterbody treated. Choose the closest match, or Other (specify) if none fits.",
      water_regime = "Still water is a lake or pond, for example. Flowing water is a river or stream.",
      area         = "The size of the area treated. Hectares are usual for still water, kilometers for flowing water.",
      area_notes   = "Anything that qualifies the figure, for example five golf-course ponds.",
      depth        = "Average or estimated depth in meters.",
      volume       = "Estimated volume in cubic meters. An exact figure is preferred, but an estimate is far better than nothing.",
      max_flow     = "Maximum flow in cubic meters per second.",
      water_temp   = "Water temperature in degrees Celsius. Use the notes box if it varied, for example if the surface and the bottom of a lake differed.",
      invasive_taxa = "What kind of animal this target is. One group per target: if the eradication went after more than one, add another target below.",
      species      = "Common and scientific name, for example Common carp (Cyprinus carpio). Search by either. If your species is not listed, type it in and our review team will add it.",
      invasion_year = "The year the invasion happened, if it is known.",
      start_year   = "The year the eradication attempt began.",
      end_year     = "The year the eradication attempt ended. Leave it blank if the work is ongoing, which many attempts are.",
      duration     = "Estimated total duration of the intervention, in days.",
      driver       = "The main reason the eradication was carried out.",
      benefit_taxa = "What kind of species the eradication was meant to help.",
      benefit_sp   = "Entries such as Zooplankton or Cottidae spp. are fine. Our review team tidies these up.",
      method       = "The main eradication method used. Add further methods below if more than one was used.",
      method_notes = "Any detail on how the method was applied.",
      method_desc  = "A fuller description of the approach at this site.",
      labour       = "Effort required, in person-days. If you only have a range, put it in and we will work with it.",
      cost         = "We are not looking for a full breakdown, but an estimate helps build a picture of costs so funders and practitioners can benchmark interventions.",
      ingredient_basis = "Active means the concentration is of the active ingredient, for example rotenone itself. Product means it is of the commercial formulation as applied, for example CFT Legumine. The two differ by the product's strength, so we need to know which you mean.",
      toxin_conc   = "Target concentration of the chemical in mg/L. A text box rather than a number, because the value can vary over a treatment.",
      toxin_conc_measured = "The concentration actually measured in the water, in mg/L, if it was measured. A range is fine.",
      neutralising = "The neutralizing agent used, if any.",
      outcome      = "Eradication means the complete and permanent removal of the population (Genovesi 2000). You can record an attempt as successful without formal proof of absence; our review team records that distinction separately, so answer as you see it.",
      verification = "How the outcome was verified, and any notes on it.",
      reference    = "A link or citation for the underlying evidence. A DOI or URL is ideal.",
      source       = "Where this record came from. This is for our internal provenance and is not shown publicly.",
      contact      = "We need a contact so the review team can follow up on your submission.",
      notes        = "Anything else you would like the FWISE team to know."
    ),

    # ---- Validation messages ---------------------------------------------------
    # Shown under the field by shinyvalidate. {year} is filled at runtime.
    validate = list(
      site_name       = "Please give the site a name.",
      country         = "Please choose a country.",
      latitude        = "Place a pin on the map, or type a latitude.",
      latitude_range  = "Latitude must be between -90 and 90.",
      longitude       = "Place a pin on the map, or type a longitude.",
      longitude_range = "Longitude must be between -180 and 180.",
      waterbody_type  = "Please choose a waterbody type.",
      water_regime    = "Please choose still or flowing water.",
      area_unit       = "Please give the unit for the area you entered.",
      target_taxa     = "Please choose the kind of animal targeted.",
      target_species  = "Please name the species targeted.",
      start_year      = "Please give the year the eradication began.",
      year_range      = "Enter a year between {min} and {year}.",
      end_year_range  = "Enter a year between {min} and twenty years from now.",
      end_before_start = "The end year cannot be before the start year.",
      driver          = "Please choose a reason.",
      method          = "Please choose the main method.",
      outcome         = "Please choose an outcome.",
      contact_name    = "Please give a contact name.",
      contact_email   = "Please give a contact email.",
      email_format    = "That does not look like an email address.",
      consent         = "Please confirm you agree before sending."
    ),

    # ---- Announcements ---------------------------------------------------------
    # Written to the polite live region so a screen reader hears them.
    announce = list(
      waterbody_changed = "The waterbody types have changed to match {regime}. Please choose again.",
      regions_changed   = "The regions have changed to match {country}. Please choose again.",
      target_added      = "Target {n} added.",
      beneficiary_added = "Beneficiary {n} added.",
      location_set      = "Location set to latitude {lat}, longitude {lng}",
      nothing_entered   = "Nothing entered yet.",
      reference_prefix  = "Your reference is ",
      form_progress     = "Form progress",
      submission_received = "Submission received"
    )
  )
)
