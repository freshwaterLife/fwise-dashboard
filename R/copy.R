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
    contacts   = "Contacts",
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
    licence       = "Data released under CC BY-NC 4.0 - non-commercial data. Code released under the MIT licence.",
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
    action_contacts   = "Browse contacts"
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
      "been tried, where, and with what result."
    ),
    action_primary   = "Plan an eradication",
    action_secondary = "Explore the data",
    description = "What FWISE is, the shape of the evidence, and where to start."
  ),

  # ---- Explore ---------------------------------------------------------------

  explore = list(
    title = "Explore the data",
    description = paste(
      "Filter the full record of eradication attempts by place, species, method",
      "and outcome, and see them on the map."
    )
  ),

  # ---- Plan ------------------------------------------------------------------

  plan = list(
    title = "Plan an eradication",
    description = paste(
      "Answer a short set of questions about your site and get a tailored summary",
      "of comparable attempts, with a report you can share."
    )
  ),

  # ---- About -----------------------------------------------------------------

  about = list(
    title = "About FWISE",
    description = paste(
      "The database, how it was built, the team behind it, and how to cite it."
    )
  ),

  # ---- Contacts --------------------------------------------------------------

  contacts = list(
    title = "Contacts",
    description = "The people behind the records in FWISE.",
    # [PLACEHOLDER] framing line, to be replaced with the client's wording
    intro = paste(
      "[PLACEHOLDER] Every record in FWISE has someone behind it. These are the",
      "practitioners and researchers who ran these eradications or wrote them up.",
      "If you are planning something similar, they are the people worth talking",
      "to. Find someone working in your region, or on the species you are dealing",
      "with, and get in touch."
    ),
    # [PLACEHOLDER] closing note, to be replaced with the client's wording
    outro_heading = "Not sure who to ask?",
    outro = paste(
      "[PLACEHOLDER] If the right person is not obvious from this list, write to",
      "the FWISE team and we will try to point you to someone who has done",
      "something comparable."
    ),
    outro_action = "Email the FWISE team",
    outro_email  = "hello@example.org", # [PLACEHOLDER] awaiting the real address

    filter_continent = "Continent",
    filter_country   = "Country",
    filter_search    = "Search by name or organisation",
    filter_all       = "All",

    summary_contacts     = "contacts",
    summary_countries    = "countries",
    summary_continents   = "continents",
    summary_showing      = "Showing",

    col_name         = "Name",
    col_organisation = "Organisation",
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

    # Opening panel, before step one
    intro = list(
      heading = "Add an eradication attempt",
      what_heading = "What FWISE is",
      what = paste(
        "FWISE is a global evidence base of eradication attempts against",
        "freshwater invasive animals. Every record here came from someone who",
        "did the work or wrote it up. Adding yours makes the picture more",
        "complete for everyone planning one."
      ),
      review_heading = "Every submission is reviewed",
      review = paste(
        "Your submission goes to the FWISE team first. We check it, clean it",
        "against the rest of the database, and get in touch if anything needs",
        "clarifying. It appears in the dashboard once that is done."
      ),
      time_heading = "How long it takes",
      time = paste(
        "About fifteen minutes if you have your figures to hand. Only a few",
        "fields are required, so a partial record is far better than none."
      ),
      no_save_heading = "You cannot save and come back",
      no_save = paste(
        "There are no accounts and no logins, so there is nothing to save your",
        "progress against. Please complete the form in one sitting. If you would",
        "rather gather your answers first, download the question list below,",
        "fill it in offline, and copy your answers across when you are ready."
      ),
      scope_heading = "What belongs in FWISE",
      scope = paste(
        "Animals only for now, not plants. We define eradication as the complete",
        "and permanent removal of a population (Genovesi 2005). You can record an",
        "attempt as successful without formal proof of absence; our review team",
        "records that distinction separately, so answer as you see it."
      ),
      download_label = "Download the question list",
      download_hint  = "Plain text, opens in any editor. Every question on the form, in order.",
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
      heading = "Well done.",
      # Assembled at runtime with the live counts.
      body_template = paste(
        "You have added the {nth} eradication attempt to FWISE, and the {nth_country}",
        "in {country}."
      ),
      followup = paste(
        "We will check your submission and email you within [PLACEHOLDER: X working days]." # [PLACEHOLDER] confirm the review turnaround
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

# Small convenience so modules read as fw_t("contacts", "title") rather than a
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
