# FWISE

**FWISE** is the Freshwater Invasive Species Eradication database: a public
world evidence base of eradication attempts against freshwater invasive
animals, built for [Freshwater Life](https://www.freshwaterlife.org) as an R
Shiny app. It is live at https://fwise.org.

It holds **855 eradication attempts** in **30 countries**, from **1934**, with
360 species and 217 contacts (release of 5 Oct 2026).

> **This repository holds code only.** The data lives in
> [`fwise-data`](../fwise-data), which is canonical. They are split so the
> client can publish data by pushing there, without redeploying the app.

Running the service (accounts, the review routine, publishing) is covered in
`HANDOVER-FWISE.md`, the operations handover for FWISE staff, kept outside this
repository.

---

## Contents

- [Running it locally](#running-it-locally)
- [Repository layout](#repository-layout)
- [The data layer](#the-data-layer)
- [Updating the data](#updating-the-data)
- [The pages](#the-pages)
- [Downloads](#downloads)
- [Submissions and review](#submissions-and-review)
- [Newsletter, feedback and the privacy page](#newsletter-feedback-and-the-privacy-page)
- [Environment variables](#environment-variables)
- [Design system](#design-system)
- [Deploying](#deploying)
- [How the data pieces were built](#how-the-data-pieces-were-built)
- [Open items](#open-items)
- [Decisions and gotchas](#decisions-and-gotchas)

---

## Running it locally

You need R 4.5.1 or later. Clone both repositories side by side; the app
reads its data from `../fwise-data/` by default.

```
fwise/
├── fwise-dashboard/   <- this repository
└── fwise-data/        <- the data
```

```bash
git clone <this repository>      fwise-dashboard
git clone <the data repository>  fwise-data
cd fwise-dashboard

R -e 'renv::restore()'     # rebuilds the package library from renv.lock
R -e 'shiny::runApp()'
```

No credentials are needed. With nothing configured the app reads the sibling
checkout and writes forms and submissions to `dev/forms/`. The only network
calls are the species photographs a PDF download fetches from Wikimedia.

**If your `.Renviron` holds the Google key and a sheet id, a local run writes
to the live sheet.** The tests clear these for themselves; comment them out
when trying the forms by hand.

`renv::restore()` takes a while the first time. `sf` (imported by `leaflet`)
needs GDAL, GEOS and PROJ: `brew install gdal geos proj` on macOS.

The PDF report needs the **Quarto CLI** 1.4 or later (`brew install --cask
quarto`, or point `QUARTO_PATH` at a copy). Without it the download picker
leaves the PDF out and says why.

### Checks

Run from the repository root:

```bash
Rscript dev/smoke_test.R        # contribute form: gating, validation, the write
Rscript dev/plan_test.R         # Plan page, downloads, forms, tracking, privacy page
Rscript dev/value_test.R        # every figure, bar and export row recomputed in base R
Rscript dev/check_contrast.R    # interface colour pairs against WCAG AA
Rscript dev/check_palette.R     # chart palettes: CVD separation, bands, label ink
Rscript dev/check_literals.R    # no stray colours or white; every copy key resolves
Rscript dev/reconcile_source.R  # the data holds every column and value of the export
```

All exit non-zero on failure. `plan_test.R` and `value_test.R` render real
PDFs; without Quarto they print `PDF ASSERTIONS SKIPPED`, which is a gap, not
a pass.

Run `check_contrast.R` after changing any interface colour and
`check_palette.R` after changing any chart colour.

---

## Repository layout

```
.
├── app.R                    entry point (Connect Cloud expects it at the root)
├── R/                       sourced automatically by Shiny, in byte order
│   ├── brand.R              every design value (sorts first on purpose)
│   ├── config.R             paths, env vars, data palettes, behaviour numbers
│   ├── copy.R               the copy deck: chrome and pages
│   ├── copy_contribute.R    ...the Add a record form
│   ├── copy_export.R        ...downloads and question lists
│   ├── copy_forms.R         ...newsletter, feedback, privacy page chrome
│   ├── theme.R              the bslib theme, mapped from brand.R
│   ├── ui_helpers.R         shared UI pieces and the client-side script
│   ├── data_load.R          the data contract: reads and unpacks the files
│   ├── github.R             read-only GitHub API client for the data repo
│   ├── filters.R            the one filter engine (FW_FILTERS)
│   ├── charts.R             every plotly chart
│   ├── charts_static.R      the PDF's ggplot charts and static map
│   ├── maps.R               basemaps, markers, hover cards, records
│   ├── species_images.R     photo cache and live fallback
│   ├── export.R             export frame, workbook, download bundle
│   ├── report_pdf.R         the Summary report PDF (Quarto + Typst)
│   ├── report_records.R     the Detailed report (.html)
│   ├── questions_text.R     offline question list, plain text
│   ├── questions_docx.R     the same, as Word
│   ├── submit.R             the submission write path
│   ├── forms.R              newsletter and feedback rules and records
│   ├── forms_store.R        everything that talks to Google Sheets
│   ├── tracking.R           GoatCounter tag and the event log
│   ├── qa_checks.R          review rules and import, used by dev/qa.R
│   └── mod_*.R              one module per page (contribute is split in three)
├── www/
│   ├── scss/                main.scss and records.scss (entry files),
│   │                        _tokens.scss (derived only), _components.scss
│   ├── fonts/               self-hosted Ubuntu woff2
│   ├── img/                 logos, collaborator logos, Welcome pictures
│   ├── robots.txt, sitemap.xml
├── resources/
│   ├── js/fw_map_fit.js     fits one world to each map's width
│   └── report/              Typst template, TTF fonts, Natural Earth basemap
├── content/                 privacy_and_data_terms.md (the privacy page text)
├── dev/                     checks, qa.R and helper scripts (gitignored except
│   └── archive/             those listed in .gitignore); one-off build scripts
├── manifest.json            what Connect Cloud deploys from
├── renv.lock                what local development restores from
└── .Renviron.example        every environment variable
```

`../fwise-data/` holds `attempts.csv`, `species.csv`, `contacts.csv`, the two
ISO lookups, `metadata.json`, and `source/` (the client's export and its
row-to-id map). See its README.

### Rules that hold the code together

- **Shiny sources everything in `R/` at startup.** Anything there runs on
  boot, so data scripts live in `dev/`.
- **Every design value is in `R/brand.R`.** `fw_compile_css()` passes them to
  Sass, `theme.R` maps them onto Bootstrap, and charts and exports read them
  directly. `_tokens.scss` holds no literals.
- **Every user-facing string is in the copy deck** (the four `copy*.R` files,
  read as one list). `fw_t("section", "key")` reads it, `fw_fill()` fills
  `{placeholders}`. Never call `fw_t()` at the top level of a file. The privacy
  text is `content/privacy_and_data_terms.md`.
- **Every behaviour number is in `R/config.R`** under "Behaviour".

`dev/check_literals.R` enforces the first three.

---

## The data layer

Every module reads through `R/data_load.R`; none reads a CSV directly.

**One wide table and two lookups.** `attempts.csv` has one row per attempt
with every column of the client's export (67) under snake_case names, plus
`attempt_id`, `status`, `submitted_at`, `last_updated` and
`consent_data_use`. Species and contacts are referenced by id into
`species.csv` and `contacts.csv`, so a name or a redaction is corrected once.

Multi-value cells use `FW_MULTI_SEP` (`"; "`): species are lists of ids,
`methods` a list of names. Per-method notes sit in `method_notes` as
`Rotenone: text | Draining: text` (`FW_NOTES_SEP`, since notes contain
semicolons).

**Values are stored as the source text.** `"0.010-0.020"` stays a range and
`"≥0.5"` keeps its sign. Only `FW_ATTEMPT_NUMERIC` columns are cast, in
memory.

**`fw_unpack()` builds six in-memory tables** at startup: `attempt`,
`species`, `attempt_species` (with `role`), `method` (`FW_METHODS` plus any
unlisted name), `attempt_method` and `contact`.

The load stops on:

- a file whose columns don't match `FW_ATTEMPT_COLUMNS`,
  `FW_SPECIES_COLUMNS` or `FW_CONTACT_COLUMNS` exactly;
- an id with no row in its lookup;
- an unknown `status`;
- an `iso3`, continent or region that disagrees with the ISO lookup
  (`fw_validate_geography()`).

**Approval gate.** `fw_filter_approved()` drops unapproved attempts and every
species, method and contact only they reference, before any module sees the
data. Don't reduce it to filtering the attempt table.

**Contact redaction.** Blocker to ensure no privacy leaks: a contact with
`contact_public` FALSE is dropped from `fw_contacts_summary()` (the directory
and Plan's contacts), and blanked (name, organisation and email) on map
records and in exports. `fw_assert_export_safe()` refuses any export that
still carries one. Public addresses are encoded in the page and revealed on a
click (`fw_email_reveal()`), which stops naive scrapers only.

### Where the data comes from

`fwise-data` is private, so the app reads it over the GitHub contents API.

| Mode | When | Reads |
|---|---|---|
| `local` | nothing set | `../fwise-data/` |
| `api` | `FWISE_DATA_TOKEN` set | the GitHub API |
| `url` | `FWISE_DATA_SOURCE` is an `https://` base | plain HTTPS, no credential |

The repository and branch are constants in `R/config.R`. The token is a
fine-grained PAT (owner `freshwaterLife`, that one repository, Contents:
Read). **It expires, and then the app stops serving data**: keep its expiry in
a calendar. `FWISE_DATA_SOURCE` overrides the token for a test deploy. The
startup log names the mode.

The data is read **once at startup**. New data appears after a republish.

---

## Updating the data

**After launch, `attempts.csv` is the master.** Correct a value by editing the
row. New records come through the contribute form and review
([below](#submissions-and-review)). Ids (`FW-20261005-7K3QX9`: prefix, date
first assigned, random suffix without `I`, `O`, `0` or `1`) are permanent and
never reused.

`dev/reconcile_source.R` checks the data still holds every column and value
of the export in `fwise-data/source/`. It allows only the country/region
split, the regime corrections in `FW_REGIME_BY_TYPE`, Excel float noise and
Windows line endings.

Species photos are cached in `species.csv` by `dev/fetch_species_images.R`
(credit, licence and licence URL included; no licence means no photo). The
app falls back to a memoised live lookup for a species added since.

---

## The pages

Navbar labels are in `fw_t("nav")`.

| Page | Module | What it is |
|---|---|---|
| The solution | `mod_home.R` | Welcome: the case for freshwater eradication, a figure sentence, six success-story tiles opening story cards, a before/after map reveal (static client images) and an evidence footnote. Sized for one laptop screen. |
| Explore the database | `mod_explore.R` | Live filters (continent, country, regime, invasive and protected taxa and family), a whole-database panel (attempts, countries, invasive species, >species protected, in review), the map, and three chart views (number of attempts, success rate, duration). |
| Plan an eradication | `mod_plan*.R` | The report builder: 11 filters, **Build report**, then outcomes, species tiles, the map, still/flowing water, methods, duration, contacts and a record search; downloads. |
| Add a record | `mod_contribute*.R` | One scrolling form. Contributor and consent first. |
| Connect with community | `mod_networking.R` | The contact directory, filtered and paged. |
| About | `mod_about.R` | Sign-up card, six click-to-open panels (citation, methods, caveats, glossary, related databases, licence and links), feedback band. |
| Privacy and data terms | `mod_privacy.R` | No navbar link; reached from the footer and `?page=privacy&section=...`. |

### Plan: results on Build only

Results render only when **Build report** is pressed, deliberately: no live
redraw to cherry-pick a slice from. Don't make them reactive. Clear resets the
filters and builds the default report.

Plan has **no outcome or method filter** (`FW_PLAN_DROP`): those are what a
planner comes to learn, and filtering to successes gives false optimism.
`dev/plan_test.R` checks this. Explore is live by design.

### Charts

- Counts show all four outcomes. A **success rate is Successful against
  Failed only**; Ongoing and Unknown are left out.
- Method and duration charts use **single-method attempts only** (outcomes and
  dates belong to the attempt). The duration chart is successful attempts
  only.
- No headline success rate anywhere. Rotenone (562 of 855) is never a hero
  figure.
- Colour is never the only encoding: values are printed in or beside bars.
- Charts are locked down (no zoom, pan or legend toggling); one PNG button at
  300 dpi.
- `fw_*_data()` functions count once for both the plotly charts and the PDF's
  ggplot versions.

### Maps

Leaflet, Web Mercator only (the note under each map says so). Every map shows
the band 80N–60S at its Mercator aspect, with a fractional zoom-out limit set
by `resources/js/fw_map_fit.js`. Identical coordinates group into counted
rings (`FW_MAP$cluster`). Hovering a marker shows a card; clicking opens the
full record (fetched from the server on the page, embedded in saved reports).
The Carto basemap needs its key (`FW_CARTO_KEY` in `R/maps.R`; public by
design, scoped by domain).

---

## Downloads

One **Download this report** button, floating in the bottom corner, opens a
picker. Parts (`FW_BUNDLE_PARTS`): the **Summary report (.pdf)**, the
**Detailed report (.html)** and the **spreadsheet (.xlsx)**. One part
downloads as itself; several as a zip. Names carry a UTC time stamp.

`fw_export_frame(data, attempt_ids)` is the one flattening function, for both
the filtered download and the full Zenodo dataset. Every export closes on the
methods statement, the caveats and the citation (`fw_closing_blocks()`).

- **Spreadsheet:** attempts, field definitions, the filters applied, and
  methods and caveats.
- **Summary report (PDF):** `R/report_pdf.R`. R writes Typst calls (text
  escaped by `fw_typ_str()`) against `resources/report/typst-template.typ`;
  `fwise-tokens.typ` is written per render from `brand.R`, `config.R` and the
  copy deck. Quarto renders it with no R chunks. Charts come from
  `charts_static.R`; the map uses bundled Natural Earth outlines. Species
  photos are fetched per render with a timeout. The picker warns at
  `FW_PDF$warn_pages` or `warn_mb` using `fw_pdf_size_estimate()`.
- **Detailed report (.html):** `R/report_records.R`. The summary figures
  (live), then every attempt as a card from the export frame (so the same
  redaction), with a find box. Self-contained; only tiles and photos need the
  network. Gotcha: `jsonlite::base64_enc()` wraps lines, which breaks inlined
  fonts; use `fw_html_base64()`.

The caveats are client copy in `export$caveats` (`R/copy_export.R`); add or
remove a block and every surface reflows. Their success definition matches
the contribute form's: keep them in step.

---

## Submissions and review

`fw_submit_attempt()` appends one row to the `submissions` tab of the inbox
sheet (`FWISE_INBOX_SHEET_ID`, defaulting to the forms sheet), or to
`dev/forms/submissions.csv` without a sheet. `fw_inbox_mode()` says which.

A row is readable without lookups (`FW_INBOX_COLUMNS`): species as labels,
the contact as name, organisation, email and public, plus an optional
`contact_id`. `qa_status` and `imported` lead the row. Typed-in species also
get a `new_species` row. Writes are RAW appends. **A failed write is reported
to the contributor, never saved locally**: Connect Cloud's disk is wiped on
restart.

`Rscript dev/check_sheets.R --setup-inbox` prepares the two tabs (headers,
plain-text cells, frozen header, dropdowns). Plain `dev/check_sheets.R` checks
headers.

### `dev/qa.R`

Run a section at a time in RStudio (Ctrl+Alt+T). Rules are in
`R/qa_checks.R`; new-species lookups in `dev/species_enrich.R`.

| Section | Does |
|---|---|
| 0 Setup | Loads the code; reports the data checkout's branch and state and which inbox it reads. |
| 1 Read and check | `inbox`, `new_species`, `checks` (errors and warnings), `contact_matches`, `species_matches`. Fix rows in the sheet and rerun; set `qa_status` done and `status` approved or rejected. |
| 2 Resolve | Rechecks the done rows and shows `to_add`, `species_add`, `contacts_add`. Stops on any error. |
| 3 Enrich | Fills blanks for new species: family (GBIF), IUCN status (`IUCN_API_TOKEN`), a photo with credit and licence. |
| 4 Write | The only write. Asks for `yes`; refuses if fwise-data is dirty or behind, or the sheet changed. Appends to the CSVs, rewrites `metadata.json`, marks `imported`. |
| 5 Publish | Prints the git commands. The app shows the import after a republish. |

Errors stop a row; warnings don't. Fuzzy matches (accents, case and word
order ignored; small edit distances) only suggest: a contact is reused on an
exact name and organisation or a typed `contact_id`. Private wins over public.
Nothing imports twice; rerunning is safe.

Not built: bulk import and edits to existing records (paste a bulk load into
the `submissions` tab; edit rows by hand).

---

## Newsletter, feedback and the privacy page

- **Get FWISE updates** (`mod_newsletter.R`): name, email, optional
  organisation, consent. One sign-up per visit.
- **Send feedback** (`mod_feedback.R`): page, message, optional email.
  Several per visit, with a `FW_FEEDBACK_COOLDOWN_S` pause.
- **Privacy and data terms** (`mod_privacy.R`): the text is
  `content/privacy_and_data_terms.md`.

Any `fw_form_open("newsletter" | "feedback", label, source)` button opens a
form. Both have a honeypot. With `FWISE_FORMS_SHEET_ID` and `GS4_SA_KEY_B64`
set, rows go to the `newsletter` and `feedback` tabs (RAW); otherwise to
`dev/forms/`. No fallback between them.

Sheet set-up: enable the Sheets API, create a service account and JSON key,
make the tabs with the headers in `FW_NEWSLETTER_COLUMNS` and
`FW_FEEDBACK_COLUMNS` (`R/forms_store.R`), share the sheet with the key's
`client_email` as Editor, then run `dev/check_sheets.R` (`--write-headers`
fills an empty row 1).

**Terms version:** `privacy$version` in `R/copy_forms.R`, stored on every
newsletter and feedback row. Every change needs a dated line under "Changes
to this notice" (`dev/plan_test.R` checks).

### Usage tracking

Two layers, both in `R/tracking.R`: GoatCounter counts arrivals (one script in
the page head, site code `FW_GOATCOUNTER_CODE` in `R/config.R`; count.js skips
localhost), and an event log writes one row per action to the `events` tab.
The events, their `detail` formats, the privacy rules, the `?campaign=` names
in use and the metric definitions are in the FWISE handover document (kept
outside this repo). A new event goes in `FW_TRACK_EVENTS` and the handover's
events table.

Events are held in memory per session (capped at 500) and written in one
append when the session ends, through the forms' sign-in (`fw_google_token()`
in `R/forms_store.R`): 5 s timeout, no retry. On failure the session's rows are
dropped and the server log gets one line with the reason, never row contents.
The write blocks the R process, so other sessions can wait up to the timeout,
and rows still in memory are lost if the process stops (e.g. a redeploy).

`FWISE_LOG_MODE` is `sheet` in production, `console` to print rows locally, or
`off`; unset is off. `sheet` with no sheet id or key is also off, and the
startup line names the setting at fault, e.g. `FWISE startup: event log off
(FWISE_LOG_MODE is not set)`. To test locally:

```
FWISE_LOG_MODE=console Rscript -e 'shiny::runApp(port = 8790)'
```

Use the app, close the tab, and each row prints as `FWISE event: ...`.

---

## Environment variables

All optional; the app runs with none. Copy `.Renviron.example` to `.Renviron`
(gitignored) for local use. On Connect Cloud, set them in the content
settings.

| Variable | Where | Purpose |
|---|---|---|
| `FWISE_DATA_TOKEN` | Connect Cloud | GitHub PAT (Contents: Read on `freshwaterLife/fwise-data`). Set = read over the API. Expires. |
| `FWISE_DATA_SOURCE` | rarely | Overrides the token: a local path, or an `https://` base. |
| `FWISE_FORMS_SHEET_ID` | Connect Cloud | Sheet for newsletter and feedback. |
| `GS4_SA_KEY_B64` | Connect Cloud | Service account JSON key, raw or base64. |
| `FWISE_INBOX_SHEET_ID` | optional | Sheet for submissions; defaults to the forms sheet. |
| `FWISE_LOG_MODE` | Connect Cloud | `sheet`, `console` or `off` (default). See Usage tracking. |
| `FWISE_LOG_SHEET_ID` | optional | Sheet for the `events` tab; defaults to the forms sheet. |
| `FWISE_CARTO_KEY` | optional | Overrides the built-in Carto key. |
| `QUARTO_PATH` | optional | A Quarto CLI not on the PATH. |
| `IUCN_API_TOKEN` | reviewer only | For `dev/qa.R` section 3. The app never uses it. |

Never commit a credential. `.Renviron` and `*.json` are gitignored (except
`manifest.json`, `renv.lock` and `metadata.json`).

---

## Design system

### Where to change things

| To change | Edit | Then run |
|---|---|---|
| a colour, size, radius, shadow | `R/brand.R` | `dev/check_contrast.R`, `dev/check_literals.R` |
| any wording | `R/copy.R`, `copy_contribute.R`, `copy_export.R`, `copy_forms.R` | `dev/check_literals.R` |
| privacy text | `content/privacy_and_data_terms.md` | `dev/plan_test.R` |
| a behaviour number | `R/config.R`, "Behaviour" | `dev/value_test.R` |
| chart palette | `R/config.R` (`FW_PALETTE`) | `dev/check_palette.R` |

Restart the app after editing SCSS (CSS is compiled at start-up).

### Colour

The client's two values, `#0F8B79` (teal) and `#191144` (indigo), are the
palette; everything else derives from them or is a neutral tinted towards the
indigo. **No white anywhere.** Tokens are named for their role (`ink`,
`teal_text`, `surface`, `border_input`...); see `FW_COLOURS` in `R/brand.R`.

`brand_teal` fails AA as text, so it's for non-text only; links and buttons
use `teal_text`. The navbar is light (the wordmark is indigo); indigo is the
page-title band and the footer's lower tier.

Charts use the Wong (2011) colourblind-safe palette (`FW_PALETTE`), matching
the paper. Never use data colours for interface chrome or interface teals for
data. Counts inside outcome segments are indigo (white fails AA on every Wong
colour).

### Type

Ubuntu, self-hosted (`www/fonts/`, Latin subset). Ubuntu Mono for numerals
only (`.fw-num`).

**Nothing that stays on the page goes below `$fw-size-min`** (client rule).
Exemptions, all client instructions: `size_popup` (transient overlays),
`size_credit`, `size_photo_credit` and `size_fine` (four named callers). See
`FW_TYPE`. Bootstrap's small-text variables are raised in `R/theme.R`, and
ionRangeSlider and Leaflet need longer selectors because their CSS loads
after ours. Plotly uses `FW_TYPE$floor_px` and `uniformtext` hides labels
that won't fit.

### Layout and motion

`.fw-container` is 95% of the viewport with a 1.5rem minimum gutter
(`min()`, wrapped in `unquote()` for libsass). No pixel cap and no global
measure; `.fw-measure` is opt-in. Motion only responds to user actions; none
on load or scroll, no count-ups, all off under `prefers-reduced-motion`.

---

## Deploying

The app runs on Posit Connect Cloud at https://fwise.org (credentials are
held by the client).

**Connect Cloud reads `manifest.json`, not `renv.lock`.** Regenerate it
whenever dependencies or files change:

```bash
R -e 'renv::snapshot()'
R -e 'rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")'
```

`.rscignore` keeps `dev/` and `.Renviron*` out of the bundle; a manifest
listing a file the repo lacks fails the deploy.

To publish: push to the public GitHub repository, then **Republish** on
Connect Cloud (set environment variables in its UI). Check the startup log:
it names the data mode, the forms and inbox modes, the event log, and
`FWISE startup: quarto 1.x at ...` (NOT FOUND means no PDF).

**Domain:** registered with Namecheap, held by the client. The apex points at
Connect Cloud; `www` redirects to it. Add the custom domain in Connect Cloud's
settings, then the CNAME/ALIAS and verification records in Namecheap
(Advanced DNS). TLS is automatic once verified.

**Stale pages after a deploy:** the page carries `<meta name="fw-build">`
(`FW_BUILD_ID`) and reloads itself once if the server's build differs. To
debug a report, ask for that value from the page source.

---

## How the data pieces were built

One-off scripts kept in `dev/archive/` (run from the repository root). Their
outputs are committed; none needs rerunning in normal use.

| Script | Built | Rerun when |
|---|---|---|
| `build_from_export.R` | `attempts.csv`, `species.csv`, `contacts.csv`, `metadata.json` and the row-to-id map, from the client's export in `fwise-data/source/` | Never after launch: it replaces the data and mints new ids. |
| `import_iucn.R` | The IUCN columns in `species.csv`, from a one-off download | Never; `dev/qa.R` now fills IUCN data for new species. |
| `build_iso_lookups.R` | `lookup_iso3166.csv` and `lookup_iso3166_2.csv` in `fwise-data` | The ISO standard changes. |
| `build_report_basemap.R` | `resources/report/ne_110m_basemap.rds` (Natural Earth countries, ocean, lakes) for the PDF map | The map needs new outlines. |
| `build_success_named.R` | `www/img/home/success/*-named.png`, trimmed and downsized from the client's lettered plates | New artwork arrives (its input folder, `resources/success-pic/`, is no longer in the repo). |

---

## Open items

**Needs the client:**

- **Zenodo DOI.** `footer$doi_url` in `R/copy.R` is `#`; DOI links open a
  "coming soon" card until it's set.
- **Data-use consent wording** (`[NEEDS CLIENT SIGN-OFF]` in
  `R/copy_contribute.R`), and the privacy terms version, still "Draft 0.1"
  (`privacy$version` in `R/copy_forms.R`). Any `[TO CONFIRM: ...]` written into
  the privacy text is highlighted on the page.
- **Metric definitions** in the handover document are drafts for Freshwater Life to
  confirm.
- Optional: reversed (light) logo artwork would allow an indigo navbar.

**`TODO(alex)` in the code:**

- `R/mod_contribute_steps.R`: labour effort is one text box (the spec wanted
  a number with a free-text fallback), so "20 to 30" passes to QA.
- `R/data_load.R`: some species ("Amphipoda", "Barbus sp.") have no
  scientific name and sit whole in `common_name`; the client's cleaning should
  split them.

**Known limitations:**

- `attempts.csv`, not the client's old spreadsheet, is the master. A fresh
  export can't simply be dropped in.
- PDF species photos need the network at download time (a miss prints a
  placeholder).
- The PDF needs Quarto on the server (checked in the startup log).

---

## Decisions and gotchas

- **Plan has no outcome or method filter; there is no success-rate headline.**
  Both client decisions; see [The pages](#the-pages).
- **Contacts page:** filters plus a summary strip and paging, rather than a
  grouped table.
- **Geography:** `country` is the ISO 3166-1 country or territory (Guam is a
  country); `iso3` and `continent` follow it alone; `region` is free text.
- **Methods** are an unbounded any-of set; `method_order` is not a ranking. An
  attempt has at most a primary and a secondary contact.
- **The contribute form is one page,** rendered once (re-rendering broke
  repeatable rows). Send is always enabled and takes the contributor to the
  first problem. Send gating (`all_valid()`) is separate from the validators,
  because `is_valid()` is TRUE for a disabled validator: a new required rule
  needs a line in both. Fish family is never asked; it comes from the
  species.
- **`sf` is a hard dependency** (via `leaflet`, with `raster`, `s2`,
  `units`, `sp`, `terra`). Don't drop it without dropping leaflet.
- **A private repo answers 404, not 403,** to a token that can't see it.
  `fw_gh_message()` says so in the log.
- **`\s` is not whitespace inside an R bracket expression** (default engine):
  use `[:space:]`.
- **A selectize without a leading blank selects its first option.**
  `fw_select()` and the species picker add one.
- **Fish family in the client's export** aligns to the Fish slots, not to
  every species slot; `dev/reconcile_source.R` relies on this.
- **`leaflet::addLayersControl(overlayGroups = NULL)`** shows a checkbox
  named "null"; pass `character(0)`.
- **plotly `layer = "above traces"`** doesn't lift gridlines over bars; only
  a shape with `layer = "above"` does.
- **The `.fw-plan` block in `_components.scss`** is still open around the
  block-title (i) rule, so rules placed there compile as `.fw-plan ...`.
- **libsass** evaluates `min()` itself: interpolate or `unquote()` it, or use
  width plus max-width.

---

## Licence

Code: MIT ([LICENSE](LICENSE)). Data: CC BY-NC 4.0. Both appear in the
footer (`footer$licence` in `R/copy.R`).
