<!--
  The text of the Privacy and data terms page. Rendered by fw_privacy_html()
  in R/mod_privacy.R, which also builds the contents list from the headings.

  HEADINGS carry their anchor in braces: "### Feedback {#feedback}". Links
  elsewhere in the app (?page=privacy&section=feedback) depend on six of them:
  newsletter, feedback, submissions, contacts, usage, data-terms. Do not rename
  those; dev/plan_test.R checks they exist.

  [TO CONFIRM: ...] marks a fact Freshwater Life has to supply. Each one is
  highlighted on the page so none can go live unnoticed.

  {contact_email} becomes the click-to-reveal FWISE address. Never type the
  address itself here: this file is public on GitHub.

  The version and date at the top of the page come from privacy$version and
  privacy$last_updated in R/copy_forms.R. When the version changes, add a dated
  line under "Changes to this notice".
-->

This page explains what personal information FWISE collects, why, where it is kept and what you can do about it (Part A), and the terms for using and contributing FWISE data (Part B).

## Part A: Privacy notice {#privacy-notice}

### Who we are {#who-we-are}

FWISE, the Freshwater Invasive Species Eradication database, is run by Freshwater Life. Freshwater Life is the data controller for the personal information described on this page. [TO CONFIRM: Freshwater Life's legal name, the country it is registered in, and its registered address]

For questions about your personal information, or to make any of the requests described under "Your rights", contact [TO CONFIRM: the email address for privacy questions and requests].

### Newsletter sign-ups {#newsletter}

**What we collect.** Your name, your email address, your organisation if you give it, the date and time you signed up, the wording of the consent you ticked, and the version of this notice at the time.

**Why, and our lawful basis.** To send you updates about FWISE. We rely on your consent, which you give by ticking the box on the sign-up form. You can withdraw it at any time.

**Where it is kept.** Until our email service is set up, your details are kept in a private Google Sheet that only named Freshwater Life staff and Freshwater Life's app developer can open. [TO CONFIRM: name the developer, if it should be named] We will then move them to an email service provider, [TO CONFIRM: provider name, once chosen], and delete the sheet before the first newsletter goes out in January 2027.

**Outside the UK and the EU.** Google is a US company, so your details may be processed outside the UK and the EU. [TO CONFIRM: the safeguard that covers this transfer, for example Google's data processing terms and standard contractual clauses]

**Unsubscribing.** Every newsletter will include a link to unsubscribe. Before the first one goes out, you can ask us to remove your details at any time using the contact under "Who we are".

**How long we keep it.** Until you unsubscribe or ask us to delete it.

### Feedback {#feedback}

**What we collect.** The page your feedback is about, your message, the date and time you sent it, and the version of this notice at the time. We collect your email address only if you give it.

**Why, and our lawful basis.** To improve FWISE and, if you gave an email address, to reply to you about your feedback. Our lawful basis is legitimate interests: it is in our interest, and yours, that problems with FWISE are reported and fixed. Giving an email address is optional. We use it only to reply about that feedback, never to send you newsletters or anything else, and that is why the form does not ask for your consent.

**Where it is kept.** In a private Google Sheet that only named Freshwater Life staff can open. As with newsletter sign-ups, Google may process it outside the UK and the EU.

**How long we keep it.** [TO CONFIRM: how long feedback is kept, for example until it has been dealt with, or deleted each 1 January if it is more than one month old]

### Eradication record submissions {#submissions}

**What we collect.** When you add a record with the "Add a record" form, we collect, alongside the details of the eradication:

- your name
- your email address
- your organisation, if you give it
- your agreement to the data-use statement on the form
- whether you allow your name, organisation and email address to be shown in FWISE
- anything you write in the notes to the FWISE team
- the date and time you sent the record

**What is published.** Once a record has been reviewed, the details of the eradication are published in FWISE. Your name, organisation and email address are shown only if you ticked the box allowing it (see "Contacts directory"). If you did not, they are kept private and left out of everything FWISE shows and every download. Your notes to the FWISE team are never published or included in a download.

**Why, and our lawful basis.** To review your record, publish it in FWISE, and contact you about it. We rely on legitimate interests for running the database, and on your consent for showing your contact details. [TO CONFIRM: these lawful bases]

**Review.** Every record is reviewed by the FWISE team before it is published. We may contact you about it using the email address you gave.

**Where it is kept.** Each submission is saved as a file in a private GitHub repository belonging to Freshwater Life, where the FWISE team reviews it. Reviewed records become part of the FWISE database, which is kept in the same repository. GitHub is a US company, so records may be processed outside the UK and the EU. [TO CONFIRM: the safeguard that covers this transfer]

**How long we keep it.** For as long as the record is part of FWISE.

### Contacts directory {#contacts}

FWISE shows contact details for people involved in eradications, so that practitioners can find each other. A contributor's contact details are shown only if they opted in when adding a record. [TO CONFIRM: where the other contacts in the directory came from, for example published papers and reports, and how those people are told their details are listed]

You can ask us to remove your contact details from FWISE at any time, using the contact under "Who we are".

### Usage analytics {#usage}

We record anonymous usage of FWISE, such as which pages and features are used, to improve it. We use no cookies for this, we do not store IP addresses, and nothing we record identifies you. [TO CONFIRM: the analytics tool, once chosen. Analytics is not running yet, so this section should not go live until it is.] [TO CONFIRM: whether the app's host, Posit Connect Cloud, keeps IP addresses in its own server logs, and for how long]

### Your rights {#your-rights}

You have the right to:

- ask for a copy of the personal information we hold about you
- ask us to correct it
- ask us to delete it
- object to how we use it
- withdraw your consent at any time, where we rely on consent

To make any of these requests, use the contact under "Who we are".

You can also complain to a data protection authority. [TO CONFIRM: which authority. There is no single US regulator for this. People in the UK can complain to the Information Commissioner's Office (ICO), and people in the EU to the data protection authority in their own country.]

### Changes to this notice {#changes}

The version and date at the top of this page change whenever this notice changes. Material changes are listed here, newest first.

- Draft 0.1, 2 October 2026: first draft.

## Part B: Data terms {#data-terms}

### Using FWISE data {#using-data}

The published FWISE data is released under [TO CONFIRM: the data licence. The site footer currently says CC BY-NC 4.0]. Please cite FWISE when you use it: the About page gives the citation.

This app always shows the current reviewed version of the data. Citable, versioned releases will be archived on Zenodo. [TO CONFIRM: the Zenodo link, once the data is published]

### Accuracy and limitations {#accuracy}

FWISE records are compiled from published sources and from records contributed by practitioners. Outcomes may be self-reported, and some successes have not been independently verified. Read the data caveats on the About page, which are also included in the report and workbook downloads, before relying on a figure.

FWISE data is provided as it is, without warranty of any kind, and Freshwater Life is not liable for decisions made using it. [TO CONFIRM: this wording, ideally reviewed by Freshwater Life's adviser]

### Contributing records {#contributing}

By submitting a record, you confirm that you are entitled to share the information in it, agree that once it has been reviewed it may be published in FWISE under the data licence above, and understand that Freshwater Life may edit records for consistency and accuracy.

To ask for a correction to a record, use "Send feedback" at the foot of any page, or email {contact_email}, quoting the site name.

### Linked sources and images {#sources-and-images}

Species photographs come from Wikimedia Commons and other open sources. Each is under its own licence and credited to its author where it appears. Linked papers and reports belong to their publishers and authors.
