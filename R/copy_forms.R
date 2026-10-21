# copy_forms.R
# The copy deck, part 4: newsletter, feedback and the privacy page's chrome
# (its text is content/privacy_and_data_terms.md). newsletter, feedback and
# privacy must not appear in another copy file.
#
# The consent sentence is stored with every sign-up: bump privacy$version when
# it changes.

FW_COPY_FORMS = list(

  newsletter = list(
    open_label = "Get FWISE updates",
    title      = "Get FWISE updates",
    name         = "Name",
    email        = "Email",
    organisation = "Organisation (optional)",
    consent = paste(
      "Yes, email me updates about FWISE from Freshwater Life. I can",
      "unsubscribe at any time."
    ),
    data_line = "We'll only use your details to send FWISE updates, and we won't share them.",
    submit    = "Sign me up",
    success   = "Thanks, you're on the list. Our first newsletter goes out in January 2027.",
    failure   = paste(
      "Something went wrong and your details weren't saved. Please try again",
      "in a moment."
    ),
    validate = list(
      name_required  = "Enter your name.",
      name_long      = "Your name must be {n} characters or fewer.",
      email          = "Enter an email address, like name@example.org.",
      email_long     = "Your email address must be {n} characters or fewer.",
      org_long       = "Your organisation must be {n} characters or fewer.",
      consent        = "Tick the box to agree to receive updates."
    )
  ),

  feedback = list(
    open_label = "Send feedback",
    title      = "Send feedback",
    page       = "Page",
    # The two non-page choices; pages use `nav` labels.
    page_general = "General",
    page_other   = "Other",
    issue        = "What's the issue or suggestion?",
    # Filled from FW_FORM_LIMITS$message, in the page and in the counter.
    counter      = "{n} of {max} characters",
    email        = "Email (optional, only if you'd like a reply)",
    data_line    = "We'll only use your email to reply about this feedback.",
    submit       = "Send feedback",
    success      = "Thanks, we've got your feedback.",
    failure      = paste(
      "Something went wrong and your feedback wasn't saved. Please try again",
      "in a moment."
    ),
    cooldown     = paste(
      "You've just sent us feedback. Please wait a few seconds before sending",
      "more."
    ),
    validate = list(
      page           = "Choose the page this is about.",
      issue_required = "Tell us the issue or suggestion.",
      issue_long     = "Please keep it to {n} characters or fewer.",
      email          = "Enter an email address, like name@example.org, or leave it blank.",
      email_long     = "Your email address must be {n} characters or fewer."
    )
  ),

  # Shared by both forms.
  forms = list(
    data_link   = "How we use your data.",
    # Read out after any link that opens a new tab.
    new_tab     = " (opens in a new tab)",
    # The honeypot's label. Never seen by a person; see fw_honeypot().
    honeypot    = "Website",
    required_note = "Fields marked * are required.",
    close       = "Close"
  ),

  privacy = list(
    title = "Privacy and data terms",
    # Terms version, stored on every newsletter and feedback row. When it
    # changes, add a dated line under "Changes to this notice" in the privacy
    # page: submissions are matched to terms by date. plan_test.R checks it.
    version       = "Draft 0.1",
    last_updated  = "2026-10-02",
    version_label = "Version",
    updated_label = "Last updated",
    contents      = "Contents",
    footer_link   = "Privacy and data terms"
  )
)
