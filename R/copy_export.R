# copy_export.R
# EVERY user-facing string in the things a reader takes away: the spreadsheet
# (sheet names, provenance labels, the caveats, the data dictionary), the HTML
# report's own tables, and the two question-list downloads. Read through the
# same fw_t() as the rest of the copy deck - fw_t("export", "sheets", "attempts")
# - because fw_copy_all() in copy.R merges this list into FW_COPY at call time.
#
# The section names `export` and `questions` must not also appear in copy.R or
# copy_contribute.R: fw_copy_all() would silently keep the first one it met.

FW_COPY_EXPORT <- list(

  # ---- The spreadsheet -------------------------------------------------------

  export = list(
    # Sheet names. Excel caps these at 31 characters.
    # ORDER IS THE SHEET ORDER in the workbook, and `attempts` must stay first.
    sheets = list(
      attempts    = "Attempts",
      contacts    = "Contacts",
      caveats     = "Caveats",
      definitions = "Field definitions",
      filters     = "Filters applied"
    ),
    caveats_heading = "Caveats and limitations",
    col_field      = "Field",
    col_definition = "Definition",
    col_setting    = "Setting",
    col_value      = "Value",

    # The provenance rows at the top of the Filters sheet.
    generated_at   = "Generated at (UTC)",
    in_extract     = "Attempts in this extract",
    in_database    = "Attempts in the database",
    release        = "Data release",
    release_unknown = "unknown",

    # How a filter that was left alone is recorded, and the year range.
    filter_all     = "All",
    filter_yes     = "Yes",
    filter_no      = "No",
    range_sep      = " to ",
    range_missing  = "-",

    filename_stem  = "fwise-attempts_",
    # The one download button on the report builder produces this when more than
    # one file was ticked. See fw_write_bundle() in R/export.R.
    bundle_stem    = "fwise-report_",

    # ---- The methods and caveats text -----------------------------------------
    # The plain text file that travels inside EVERY download, whatever else the
    # reader chose. Its second half is the caveats below, from the same
    # fw_caveats() vector the workbook sheet uses.
    methods_filename = "fwise-methods-and-caveats.txt",
    methods_heading  = "How FWISE was compiled",
    # [PLACEHOLDER] The client is writing this. It is the account of how records
    # were gathered, screened and entered that a reader needs before quoting any
    # figure, and it is the one thing in this file we must not invent - a
    # plausible-sounding method statement is worse than an obvious gap, because
    # nobody will know to replace it. Grep for [PLACEHOLDER] when the real text
    # arrives; the same marker is on fw_country_burden() in R/data_load.R.
    methods = c(
      "[PLACEHOLDER - awaiting the methods statement from the FWISE team.]",
      paste(
        "This file will describe how the records in FWISE were gathered,",
        "screened and entered: the literature and reporting searched, the",
        "criteria an attempt had to meet to be included, how conflicting",
        "sources were resolved, and what was done about records that were",
        "incomplete. Until it does, treat the caveats below as the whole of",
        "what can be said about how this data came to exist."
      )
    ),

    # ---- The caveats ----------------------------------------------------------
    # THE SAME TEXT travels into every export and sits beside every result on
    # the report builder. The {numbers} are computed from the data by
    # fw_caveat_blocks() in export.R, never written here, so a caveat cannot
    # carry a stale figure. Headings are printed in capitals in the workbook.
    caveats = list(
      list(
        heading = "How success is defined",
        body = paste(
          "Success here means what Genovesi means by it, in Limits and",
          "Potentialities of Eradication as a Tool for Addressing Biological",
          "Invasions: the complete and permanent removal of every wild population",
          "of a species from a defined area, by a campaign with an end date. Work",
          "that suppressed a population without removing it is not counted as a",
          "success, however useful it was."
        )
      ),
      list(
        heading = "Claimed is not the same as validated",
        body = paste(
          "An outcome is recorded as the source reported it. Of the {successful}",
          "attempts recorded as successful, {unverified} carry no verification",
          "note. We have not returned to those sites to confirm absence, and in",
          "many cases neither has anyone else."
        )
      ),
      list(
        heading = "Why there is no success rate",
        body = paste(
          "The four outcomes are reported separately and are never combined into a",
          "single figure. A rate needs a denominator, and the honest denominator",
          "changes with every filter on this page. If you calculate one, state what",
          "you divided by."
        )
      ),
      list(
        heading = "Missing values",
        body = paste(
          "Blanks are absences, not zeros, and they are common. Here, {no_size}",
          "attempts ({pct_size}) have no treated size, {no_start} ({pct_start})",
          "have no start year and {no_end} ({pct_end}) have no end year. A good",
          "part of that last group is still running."
        )
      ),
      list(
        heading = "What this record is",
        body = paste(
          "This database holds eradication work that has been reported, which is",
          "not the same as eradication work that has been done. Write-ups favour",
          "attempts that worked, so the outcome mix here is kinder than reality and",
          "the map shows where people publish as much as where they act. An empty",
          "region is not a quiet one. If you have run an attempt, successful or",
          "not, send it in."
        )
      )
    ),

    # ---- The data dictionary ---------------------------------------------------
    # One entry per exported column, in export order. fw_field_definitions()
    # turns this into the sheet, and the tests check it covers every column in
    # FW_EXPORT_COLUMNS and nothing else.
    #
    # THE CONTACTS SHEET HAS ITS OWN, `dictionary_contacts` below. It is a
    # different frame with different columns, and folding the two together would
    # break the one-to-one relationship this list has with FW_EXPORT_COLUMNS.
    dictionary = c(
      attempt_id          = "Permanent identifier for the attempt. Minted once and never reassigned, so it is safe to join on across releases.",
      site_name           = "The treated site or waterbody, as the source described it.",
      country             = "Country the site sits in. Territories recorded separately appear in region.",
      region              = "State, province or territory, where the country alone is not specific enough.",
      continent           = "Derived from country and region, so a territory is assigned its own continent.",
      iso3                = "ISO 3166-1 alpha-3 country code.",
      latitude            = "Decimal degrees, WGS 84. Positive north.",
      longitude           = "Decimal degrees, WGS 84. Positive east.",
      waterbody_type      = "The kind of waterbody treated, e.g. Lake, Pond, Stream.",
      water_regime        = "Lentic (still water) or Lotic (flowing water).",
      area_treated        = "Size of the treated area. NOT COMPARABLE ACROSS UNITS - read area_unit.",
      area_unit           = "ha (hectares, an area) or km (kilometers, a length). Do not combine the two.",
      area_notes          = "Free text qualifying the size figure.",
      depth_m             = "Average or estimated depth, meters.",
      depth_notes         = "Free text qualifying the depth, e.g. a range or a maximum.",
      volume_m3           = "Estimated volume, cubic meters.",
      volume_notes        = "Free text qualifying the volume, or the figure in the source's own units.",
      max_flow_m3s        = "Maximum flow, cubic meters per second. Flowing water only.",
      water_temp_c        = "Water temperature, degrees Celsius.",
      water_temp_notes    = "Free text qualifying the temperature, e.g. surface and bottom readings.",
      invasive_species    = "Species targeted, as 'Common name (Scientific name)'. Semicolon-delimited.",
      invasive_taxa       = "Broad group of each target, e.g. Fish, Crayfish. Semicolon-delimited.",
      invasion_year       = "Year the invasion is recorded as having happened, where known.",
      start_year          = "Year the eradication attempt began.",
      end_year            = "Year the attempt ended. Blank where the attempt is ongoing.",
      duration_days       = "Estimated total duration of the intervention, days.",
      driver              = "The main reason the eradication was carried out.",
      beneficiary_species = "Species the eradication was intended to help. Semicolon-delimited. Under-reported - see the caveats.",
      beneficiary_taxa    = "Broad group of each beneficiary. Semicolon-delimited.",
      methods             = "Methods used, semicolon-delimited. An unordered set, not a ranking.",
      method_classes      = "chemical, mechanical or other, for each method used.",
      method_notes        = "Free text on how each method was applied.",
      method_description  = "A fuller description of the approach at this site.",
      labour_person_days  = "Effort required, person-days. Free text where the source gave a range or a description rather than a number.",
      cost_estimate       = "Estimated cost, where recorded.",
      cost_notes          = "Free text on cost, including the currency.",
      target_ingredient_basis = "Whether the target concentration refers to the Active ingredient or the commercial Product. The two differ by the product's dilution, so do not compare concentrations across this column.",
      toxin_conc_target_mg_l  = "Target toxin concentration, mg/L, on the basis given in target_ingredient_basis. Text, because the source records ranges and inequalities. Chemical methods only.",
      conc_target_notes   = "Free text on the target concentration.",
      toxin_conc_measured_mg_l = "Measured toxin concentration, mg/L, where it was measured. Text, for the same reason as the target.",
      conc_measured_notes = "Free text on the measured concentration, where it differed.",
      neutralising_agent  = "Neutralizing agent used, where any.",
      neutralising_notes  = "Free text on the neutralizing agent.",
      outcome             = "Successful, Failed, Ongoing or Unknown. See the caveats for the success definition.",
      verification_method = "How the outcome was verified.",
      verification_notes  = "Free text on verification. Blank on a successful attempt means the success is claimed rather than validated.",
      reference           = "Citation for the underlying evidence.",
      reference_link      = "DOI or URL for the reference, where one exists.",
      source              = "Where the record came from.",
      primary_contact_name  = "Person associated with the attempt.",
      primary_contact_org   = "Their organization.",
      primary_contact_email = "Their email, where they agreed to it being listed. Blank means no published address, not no contact.",
      secondary_contact_name  = "A second person associated with the attempt.",
      secondary_contact_org   = "Their organization.",
      secondary_contact_email = "Their email, where they agreed to it being listed."
    ),

    # ---- The contacts sheet's dictionary ----------------------------------------
    # Separate from `dictionary` above because it describes a different frame.
    # Keys are the columns of fw_contacts_export(), in the order it builds them.
    #
    # THE FIRST ENTRY EXISTS TO ANSWER AN OBVIOUS QUESTION: the attempts sheet
    # already carries contact columns on every row, so a reader needs telling
    # what this sheet adds rather than being left to guess which to trust.
    dictionary_contacts = c(
      contact_id    = "Permanent identifier for the person. One row per person here, unlike the attempts sheet, which repeats them on every attempt they are attached to.",
      contact_name  = "The person, as the source recorded them.",
      organisation  = "Their organization, where recorded.",
      contact_email = "Their email, where they agreed to it being listed. Blank means no published address, not no contact.",
      continents    = "Continents they have attempts in, within this extract. Semicolon-delimited.",
      countries     = "Countries they have attempts in, within this extract. Semicolon-delimited.",
      attempts_in_extract = "How many attempts IN THIS EXTRACT they are attached to, in either contact slot. Not their total in the database."
    ),

    # The heading that separates the two dictionaries on the definitions sheet.
    dict_group = "{sheet} sheet",

    # ---- The HTML report's own tables ------------------------------------------
    col_country     = "Country",
    col_attempts    = "Attempts",
    other_countries = "Other countries ({n})"
  ),

  # ---- The question-list downloads --------------------------------------------
  # The Word file and the plain-text file are generated from the rendered form,
  # so the questions themselves come from FW_COPY_CONTRIBUTE. This is only the
  # framing around them.

  questions = list(
    title_text = "FWISE - SUBMISSION QUESTION LIST",
    title_docx = "FWISE submission question list",
    intro = paste(
      "Every question on the online submission form, in the order you will",
      "meet it. Use this to gather your answers offline, then copy them across",
      "when you are ready. Only the questions marked {required} have to be",
      "answered; a partial record is far better than none."
    ),
    no_save = paste(
      "There are no accounts and no logins, so the form cannot save your",
      "progress. Please complete it in one sitting."
    ),
    generated        = "Generated",
    section          = "SECTION {n}. {title}",
    conditional_note = "This section is only shown if your earlier answers call for it.",
    repeat_note      = "You can add as many of these as you need; the form starts with one.",
    # How a required or conditional question is tagged. Words, not colour or
    # symbols: they have to survive black and white and being read aloud.
    required_text    = "*REQUIRED*",
    required_docx    = "REQUIRED",
    conditional_tag  = "[only if it applies]",
    guidance         = "Guidance: ",
    options          = "Options:",
    answer           = "Answer: ",
    more_options     = "; ... and {n} more, listed on the form"
  )
)
