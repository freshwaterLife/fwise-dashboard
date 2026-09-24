# FWISE

**FWISE** is the Freshwater Invasive Species Eradication database: a public,
world evidence base of eradication attempts against freshwater invasive animals,
built for [Freshwater Life](https://www.freshwaterlife.org). It is an R Shiny
application that launches publicly in October 2026.

It currently holds **914 eradication attempts** across **30 countries**, going
back to **1934**.

> **This repository holds code only.** The data lives in a separate repository,
> [`fwise-data`](../fwise-data), which is canonical. The two are split so the
> client can publish new data by pushing there, without touching or redeploying
> this app. See [How the data layer works](#how-the-data-layer-works).

> **Status.** Contribute, Networking, About, Explore the data (the record
> browser) and Plan an eradication (the report builder) are built. Home is a
> deliberate stub with its intended structure recorded in comments inside the
> module. See [HANDOVER.md](HANDOVER.md) for exactly what is and is not done.

---

## Contents

- [Running it locally](#running-it-locally)
- [Repository layout](#repository-layout)
- [How the data layer works](#how-the-data-layer-works)
- [Updating the data](#updating-the-data)
- [The record browser](#the-record-browser)
- [The report builder](#the-report-builder)
- [Submissions](#submissions)
- [Environment variables](#environment-variables)
- [The design system](#the-design-system)
- [Deploying to Posit Connect Cloud](#deploying-to-posit-connect-cloud)
- [Pointing the custom domain at it](#pointing-the-custom-domain-at-it)
- [Known limitations](#known-limitations)

---

## Running it locally

You need R 4.5.1 or later.

**Clone both repositories as siblings.** The app looks for its data at
`../fwise-data/` by default, so the directory layout matters:

```
fwise/
├── fwise-dashboard/   <- this repository
└── fwise-data/        <- the data, canonical
```

```bash
git clone <this repository>      fwise-dashboard
git clone <the data repository>  fwise-data
cd fwise-dashboard

R -e 'renv::restore()'     # reconstructs the package library from renv.lock
R -e 'shiny::runApp()'
```

That is the whole setup. **No credentials are required, and the app makes no
network calls in local development** (a PDF download is the one exception: it
fetches its species photographs from Wikimedia). With nothing configured it reads
`../fwise-data/attempts.csv` and its two lookups, and writes any submissions to
`../fwise-data/inbox/`,
one CSV per submission — the same shape the deployment writes over the API, so
the QA loop is identical either way.

`renv::restore()` will take a while the first time. `sf` is in the dependency
list because `leaflet` imports it, and it needs GDAL, GEOS and PROJ present on
the machine. On macOS: `brew install gdal geos proj`.

**The PDF report needs the Quarto CLI**, 1.4 or later (for its bundled Typst):
`brew install --cask quarto`, or point `QUARTO_PATH` at a copy. Without it the
app runs normally and the Plan page's download picker leaves the PDF out and
says why. The startup log's second line names the Quarto it found.

### Checking your changes

```bash
Rscript dev/smoke_test.R      # form gating, validation and the write path
Rscript dev/plan_test.R       # report builder: gate, states, filters, export
Rscript dev/value_test.R      # every number on screen, every chart bar, every
                              # export row, recomputed independently and compared
Rscript dev/check_contrast.R  # every interface colour pair against WCAG AA
Rscript dev/check_palette.R   # the chart palettes: CVD separation, bands, label ink
Rscript dev/check_literals.R  # no stray colours or strings; every copy key resolves
```

`plan_test.R` and `value_test.R` render real PDFs, so they need Quarto (see
above). Without it they print `PDF ASSERTIONS SKIPPED` rather than passing
quietly - treat that line as a gap, not a pass.

Two scripts write into `../fwise-data/` and are the only things in the project
that touch the network. The **app itself never does**, with one exception: a
PDF download fetches its species photographs from Wikimedia. Run them by hand
and review the diff before committing:

```bash
Rscript dev/fetch_species_images.R    # species photos from Wikimedia -> species.csv
Rscript dev/build_iso_lookups.R       # ISO 3166-1 and -2 -> the two lookup files
Rscript dev/build_report_basemap.R    # Natural Earth outlines -> resources/report/
```

All of them exit non-zero on failure, so they are usable from CI.

Run `check_contrast.R` after changing **any** interface colour, and
`check_palette.R` after changing **any** chart colour. Between them they are the
thing that catches an inaccessible palette before a user does, and they compute
the answer rather than leaving it to the eye - `check_palette.R` simulates
protanopia and deuteranopia (Machado-Oliveira-Fernandes 2009) and measures
separation in OKLab.

---

## Repository layout

```
.
├── app.R                       entry point, must stay at the repository root
├── R/
│   ├── brand.R                 EVERY design value: colour, type, space, radius,
│   │                           shadow, motion, breakpoints. Sorts first on purpose
│   ├── config.R                paths, environment variables, the data palettes,
│   │                           and the behaviour constants (top-n, page sizes...)
│   ├── copy.R                  EVERY user-facing string, part 1: chrome and pages
│   ├── copy_contribute.R       ...part 2: the contribute form
│   ├── copy_export.R           ...part 3: spreadsheet, report tables, question lists
│   ├── theme.R                 the bslib theme, mapped from brand.R
│   ├── ui_helpers.R            reusable UI components
│   ├── data_load.R             the data contract: reads the three data files
│   │                           and unpacks them into the six in-memory tables
│   ├── filters.R               ONE filter engine, shared by the report builder
│   │                           and the dashboard. FW_FILTERS is the registry
│   ├── charts.R                every plotly figure, shared by both pages
│   ├── maps.R                  basemaps, markers and popups, shared likewise
│   ├── species_images.R        the Wikimedia photo cache and its fallback
│   ├── submit.R                the write path, and record assembly
│   ├── export.R                the export contract. SHARED with the future
│   │                           Zenodo release, so the two cannot disagree
│   ├── questions_text.R        the offline question list, plain text
│   ├── questions_docx.R        the same list as Word, from the SAME walker
│   ├── report_pdf.R            the PDF report: Typst via Quarto, rendered
│   │                           on the server from R-drawn figures
│   ├── charts_static.R         the PDF's figures: ggplot twins of the
│   │                           plotly charts, and the static map
│   ├── report_records.R        the attempts download: every attempt in the
│   │                           selection, in full, as one .html file
│   └── mod_*.R                 one file per page. The contribute page is split
│                               into mod_contribute.R (server logic),
│                               mod_contribute_steps.R (section builders) and
│                               mod_contribute_ui.R (the three page states)
├── www/
│   ├── scss/                   main.scss and records.scss (the two entry
│   │                           files), _tokens.scss (derived only),
│   │                           _components.scss
│   ├── fonts/                  self-hosted Ubuntu woff2
│   └── img/                    logos and favicon
├── resources/report/           the PDF's Typst template partials, Ubuntu
│                               TTFs and the Natural Earth world outlines
├── dev/                        local scratch, gitignored except the scripts
│                               named under "Checking your changes" below
├── manifest.json               what Connect Cloud actually deploys from
├── renv.lock                   what local development restores from
└── .Renviron.example           documents every environment variable
```

There is deliberately **no `data/` directory here.** It lives one level up:

```
../fwise-data/
├── attempts.csv                THE DATA. One row per attempt, every column the
│                               client's export has, plus id, status and dates
├── species.csv                 one row per species: names, taxa, family, photo
├── contacts.csv                one row per person: name, org, email, redaction
├── lookup_iso3166.csv          the ISO 3166-1 country list, with continent.
│                               Built by dev/build_iso_lookups.R
├── lookup_iso3166_2.csv        ISO 3166-2 subdivisions, ditto
├── metadata.json               release date and row counts
├── inbox/                      one CSV per submission; merged/ once folded in
├── qa/                         the review files dev/qa.R writes and reads
└── source/                     the client's raw export and its row-to-id map,
                                kept so the data can be reconciled against it
```

### Two things that will bite you

**Shiny sources everything in `R/` automatically at startup.** This is a
documented Shiny feature, not something `app.R` does. Any file you drop into
`R/` runs on boot. Scripts that build or change data live in `dev/` for that
reason; if one ever has to sit in `R/`, guard its body with
`if (sys.nframe() == 0L)` or it will run every time the app starts.

**Every design value lives in `R/brand.R`, and only there.** Colour, type,
spacing, radius, shadow, motion and breakpoints. `fw_compile_css()` hands them
to Sass as variables ahead of the stylesheet, `R/theme.R` maps them onto
Bootstrap, and the charts, maps, workbook and Word output read them directly.
`www/scss/_tokens.scss` contains no literal values at all. Change a value in
`brand.R`, restart, and re-run `dev/check_contrast.R`.

**Every user-facing string lives in the copy deck**, which is three files read
as one list: `R/copy.R` (chrome and pages), `R/copy_contribute.R` (the form)
and `R/copy_export.R` (spreadsheet, report tables, question-list downloads).
`fw_t("section", "key")` reads any of them; `fw_fill()` fills `{placeholders}`.

**Every behaviour number lives in `R/config.R`** under "Behaviour": the top-n
limit charts name before gathering into Other, page sizes, map defaults, chart
heights and thresholds, the year bounds on the form.

`dev/check_literals.R` enforces all three rules and fails if a hex colour, a
white surface or an unresolved copy key creeps back in.

---

## How the data layer works

Every module reads through `R/data_load.R`. **No module reads a CSV directly.**
When the data layout changes, that file should be the only one you need to touch.

**The data is one wide table and two lookups.** `attempts.csv` holds one row per
eradication attempt with every column the client's export has - all 74 of them,
under snake_case names - plus the handful the app owns: `attempt_id`, `status`,
`submitted_at`, `last_updated`, `consent_data_use`. Species and contacts are
referenced **by id** into `species.csv` and `contacts.csv`, so a species name is
held once, a photo is held once, and an email or a redaction is corrected in one
place rather than on every attempt the person appears on (84 of 237 people sit
on more than one attempt; one sits on 208).

Multi-value cells use `FW_MULTI_SEP` (`"; "`): `invasive_species` and
`beneficiary_species` are lists of species ids, `methods` is a list of method
names. Per-method notes sit in `method_notes` as `Rotenone: text | Draining:
text`, paired by position and joined with `FW_NOTES_SEP` because 32 of the notes
contain a semicolon. Nine attempts have no method and a note saying why; the
note is kept verbatim.

**Values are stored as the source's text, verbatim.** `"0.010-0.020"` stays a
range, `"≥0.5"` keeps its sign. Casting to numbers happens in memory, for the
columns in `FW_ATTEMPT_NUMERIC` only. Two columns that look numeric are not -
120 target concentrations are ranges or inequalities and 189 labour figures are
sentences - and the old build had cast both to `NA`.

**The star schema still exists, in memory.** `fw_unpack()` splits the wide
table at startup into the six tables every module was written against:

| Table | Grain |
|---|---|
| `attempt` | one row per eradication attempt |
| `species` | `species.csv` as read |
| `attempt_species` | bridge, with `role` of `invasive` or `beneficiary`, from the two id lists |
| `method` | `FW_METHODS` in `config.R`, plus any name the data holds that it does not list |
| `attempt_method` | bridge, from the `methods` list, with the paired note |
| `contact` | `contacts.csv` as read |

Measured, the split takes under a tenth of a second at double today's row count.

Two things the load refuses, loudly, before any module sees the data:

- **A file whose columns are not the contract.** `FW_ATTEMPT_COLUMNS`,
  `FW_SPECIES_COLUMNS` and `FW_CONTACT_COLUMNS` in `data_load.R` are checked in
  both directions. A column with nowhere to go is how the ingredient-basis
  column went missing from the old six-table build, and it cannot happen again
  silently.
- **An id with no row.** Every species and contact id in `attempts.csv` must
  resolve to its lookup, or the load stops naming the attempts. That is the
  integrity check a spreadsheet cannot give itself.

Two structural points that are settled and should not be generalised:

- **Methods are an unbounded any-of set**, not a ranked hierarchy. `method_order`
  is the position in the cell and exists only so the paper's figures can be
  reproduced. Do not treat it as a ranking.
- **An attempt has at most a primary and a secondary contact**, so contacts are
  two id columns on the attempt, not a list. Do not turn this into a list.

### The functions

| Function | Returns |
|---|---|
| `fw_load_data()` | the six tables, loaded once at startup |
| `fw_contacts_summary()` | one row per contact, with countries and continents derived from their attempts, an attempt count, **and redacted emails already removed** |
| `fw_choices(data, table, column)` | sorted unique values, for dropdowns |
| `fw_startup_choices()` | every dropdown's options, built once |
| `fw_last_updated()` | the date shown in the footer |
| `fw_headline_stats()` | the five landing-page KPI figures |
| `fw_country_burden()` | the choropleth source (currently a placeholder, see below) |

### Email redaction

This matters, so it is worth being explicit. Where a contact's `email_public` is
`FALSE`, `fw_contacts_summary()` replaces the address with `NA` **before the data
reaches any session**. The contacts page reads only from that function, so there
is no code path by which a redacted address can reach the browser. The page shows
an empty cell rather than a "hidden" badge, because a badge advertises that there
is something worth going after.

Addresses that *are* public get a small speed bump: they are split across `data-`
attributes and reassembled in JavaScript when the link is clicked, so a naive
scraper reading the served HTML does not harvest them in one pass. **This is not
security.** Anyone running the page's JavaScript can recover a public address.
The real control is the `email_public` flag, held once per person in
`contacts.csv`.

---

## Updating the data

**`attempts.csv` is the master.** There is no raw export to re-drop and no
rebuild step. The client corrects a value by editing the row; a new record
arrives through the contribute form and the review loop below; a new species or
contact is a new row in its lookup, added by the fold step or by hand, whose id
the attempt row then cites.

The raw export the database was built from is kept under `source/` with its
row-to-id map, and **`dev/reconcile_source.R` proves the data still holds every
column and every value of it**, identically coded:

```bash
Rscript dev/reconcile_source.R
```

It walks all 74 source columns with one rule each - identity for the scalars,
round trips through the lookups for species, taxa, family and contacts, position
for methods and their notes - and a column with no rule is itself a failure, so
a new column in a future export cannot slip past. The only transformations it
allows are the country/region split, the water-regime corrections named in
`FW_REGIME_BY_TYPE` (printed, not skipped), and Windows line endings inside a
cell read as newlines. Run it whenever someone asks whether anything was lost.

### Identifiers are permanent

`attempt_id`, `species_id` and `contact_id` are **minted once and never
reassigned**. Format is `FW-20260908-7K3QX9`: a prefix, the date the id was first
assigned, and a random suffix drawn from an alphabet with no `I`, `O`, `0` or `1`
in it, because these get read aloud and retyped by people.

Each id lives in the row or lookup that owns it, so there is no registry to keep
in step. An attempt id is minted by the form the moment a submission is sent and
travels with the row from the inbox into `attempts.csv` unchanged; species and
contact ids are minted by `dev/qa.R fold` once the reviewer has confirmed the row
is genuinely new. Only `attempt_id` ever leaves the app, in exports.

### Serving data without redeploying

`fwise-data` is **private**, so an unauthenticated `raw.githubusercontent.com`
URL returns 404. The app therefore talks to the GitHub contents API, and the
whole of its production configuration is one environment variable:

```
FWISE_DATA_TOKEN=github_pat_...
```

Set it and the app reads `freshwaterLife/fwise-data@main` over the API and
writes submissions back to it. Leave it unset and everything stays on the
sibling checkout with no network calls at all. **The repository and branch are
constants in `R/config.R`**, not settings — neither is a secret and neither
varies, and making them configurable would only add ways to get a deployment
wrong.

The token is a fine-grained PAT, resource owner `freshwaterLife`, scoped to that
one repository, with **Contents: Read and write**. Write, not just read, because
the contribute form commits submissions to `inbox/`. It **expires**, and when it
does the app stops serving data as well as accepting submissions — so the expiry
date belongs in a calendar, not just in this file.

Every read goes through `fw_data_path()` in `R/data_load.R` and every GitHub call
through `R/github.R`, which are the only two places that know the data is not on
local disk. `fw_data_mode()` resolves one of three modes and the startup log
names which one it picked, so the first line of a Connect Cloud log tells you
whether the token was seen:

| mode | when | reads | writes |
|---|---|---|---|
| `local` | nothing set | `../fwise-data/` | `../fwise-data/inbox/` |
| `api` | `FWISE_DATA_TOKEN` set | GitHub API | GitHub API |
| `url` | `FWISE_DATA_SOURCE` is an `https://` base | plain HTTPS, no credential | not possible |

`FWISE_DATA_SOURCE` still overrides the token if set, so a test deploy can be
pointed at a fork, a branch or a local path without a code change. It is not
needed in production.

The data is read **once at startup**, not per session and not on a poll. It
changes quarterly and visibility comes from a deliberate republish, so re-reading
would spend a request per visitor to discover nothing had changed. A new release
reaches users when the app restarts.

---

## The record browser

`R/mod_explore.R`, drawing its records from `fw_attempt_records()` in `maps.R`,
its filters from `filters.R` and its record panel from the same map card script
everything else uses.

**This page is about the one attempt; the report builder is about the many.**
That is the whole distinction between them, and it is worth holding on to: the
two pages were previously the same filters over the same charts with a Build
button between them, which gave a reader no reason to pick one. Explore is where
you find a single attempt, see where it is, and read everything recorded about
it. Plan is where you ask what attempts like yours look like in aggregate and
take a file away.

The page, top to bottom:

| Block | What it is |
|---|---|
| Database panel | Six figures for the **whole database**, never filtered, above the controls so it cannot be read as the size of a selection. From `fw_headline_stats()`. |
| Filter bar | Six live filters: continent, country, invasive taxa, beneficiary taxa, method, outcome. |
| Selection strip | `fw_plan_summary_ui()`, shared with the report, showing what the filters left. |
| Map | The same map as everywhere else, markers coloured by outcome. |
| List | One card per attempt, sorted and paged. Cards look like the map's hover card with the species photograph added. |
| Record panel | The full record, opened by a card **or** a marker, with previous and next through the selection. |

**There are no charts here, on purpose.** A chart of a selection is the report
builder's answer, and it belongs there with the caveats beside it and a
downloadable file under it. Adding a second weaker copy to this page is what
made the two indistinguishable before.

**Six filters, not ten.** Place, animal on either side, method and outcome are
the questions someone has when looking for an attempt. Waterbody, regime, the
named-species pickers and the year range are questions about a *group* of
attempts and stay on the report builder. The set is `FW_EXPLORE_FILTERS`; both
pages draw from the single registry in `filters.R`, so a filter here is the same
filter there.

**One record, one route.** A card click and a marker click write the same
attempt id into the same Shiny input, and `fw_map_detail_server()` answers both
out of `fw_attempt_records()`. This is what stops a record saying one thing in
the list and another on the map. It also means previous/next follows whatever
order the reader is looking at, because the server steps through the *sorted*
selection.

**Attempts with no coordinates are in the list.** Three of them have none. They
have no marker, so the map cannot show them, but they are real records and the
list and the record panel both reach them - `fw_map_points()` is just the
located subset of `fw_attempt_records()`.

**The list is live; the report builder is gated.** Browsing is watching the list
narrow under the controls. Citing is not. Do not make the two consistent.

---

## The report builder

`R/mod_plan.R`, with filters in `mod_plan_filters.R`, rendering in
`mod_plan_results.R`, the spreadsheet export and the download bundle in
`export.R`, the PDF report in `report_pdf.R` (figures in `charts_static.R`) and
the attempts file in `report_records.R`.

**The questions sit above the report, not beside it.** This page is a form
followed by its answer. The dashboard keeps its sidebar because browsing *is*
watching the picture change under the controls; here, a sidebar would put the
questions and the results side by side and invite reading the results first,
then working the filters until they say something comfortable. Stacked, there is
nothing to read until the questions have been answered, and both halves get the
full width of the page.

**Results render only when Build report is pressed.** That is a design decision
before it is a performance one. The client's steer was "controlled, informative,
not random clicking", and the reason is credibility: a view that redraws under
the cursor invites someone to land on a narrow, unrepresentative slice by
accident and then cite it. **Do not make the results reactive to the filters.**
The gap between changing a filter and seeing a result is the feature.

Three states, all of which must keep working:

| State | When | What it does |
|---|---|---|
| Empty | before any build | nothing at all - the page's introduction is in the page header, and `dev/plan_test.R` asserts the results area is genuinely empty |
| Zero | filters match nothing | says so plainly and **names which filter to relax** |
| Results | otherwise | summary, outcomes, map, method comparison, cumulative, table |

**All four outcomes stay visible.** Successful, Failed, Ongoing and Unknown are
never collapsed into a success rate. Failure teaches as much as success, and
ongoing attempts show where the next results will come from.

**Rotenone is never a headline.** It is 544 of 914 attempts and is socially
sensitive. It appears inside the method comparison alongside every other method,
reached by the user's own filtering. Do not add a hero statistic about it.

**Colour is never the only encoding.** Every chart carries its numbers as text,
the map labels each marker with its outcome, and the table below holds the same
information. Colourblind safety is a stated client requirement.

**The caveats panel folds on the About page** and does not anywhere else. It was
always visible and never an accordion; the client later asked for it to be one of
the About page's click-to-open panels, so it is a `<details>` there whose summary
names what is inside rather than saying "caveats". In every export it is still
unfoldable text, and it comes from the same function in both places, so the two
cannot say different things.

**The caveats themselves are a placeholder** (client, 24 September 2026). Five
blocks we had written came out and one headingless block reading
`[PLACEHOLDER - ANABELL TO PROVIDE CAVEATS FOR FWISE]` went in, so the FWISE
team writes what it thinks the database needs without our wording in front of
it. Grep `[PLACEHOLDER]` in `R/copy_export.R`. The
structure is untouched: add blocks back as `list(heading =, body =)` and the
About panel, the workbook sheet, the PDF and the records HTML all reflow.

### The export

`fw_export_frame(data, attempt_ids)` is the one flattening function. The filtered
download and the future full Zenodo dataset are the same call with and without
ids — if they were built separately they could disagree, and a reader comparing
a download against the citable dataset would find different numbers with no way
to tell which was right. `dev/plan_test.R` asserts they agree cell for cell.

Multi-value fields flatten to **semicolon-delimited single columns**. Neither of
the source's own conventions survives: the eight numbered species columns become
one column, and the underscore-nested taxa strings become one column. The
underscore is a legacy convention of the client's spreadsheet; the semicolon is
the published one, and the Field definitions sheet says so.

`fw_assert_export_safe()` refuses to write a workbook containing an address
belonging to a contact who asked not to be listed. Applying the redaction is not
the same as guaranteeing it, and an export is the one place a mistake travels
outside the building and cannot be recalled.

### The downloads

One **Download this report** button, floating in the bottom-right corner from
the moment a build matches something (client, 24 September 2026), opens a
picker with three parts (client, 23 September 2026): the **PDF report**,
**every attempt in full** (`.html`) and the spreadsheet (`.xlsx`). More than
one arrive as a zip; **one arrives as itself**. The interactive HTML report
that used to sit here - live plotly charts and a leaflet map, printed to PDF
through the browser - was replaced by the PDF, and the CSV went with it.

The button has moved three times and always for the same reason: the results
are a long scroll and the download is what the page is for. It was the last
block under two tables, then a sticky row at the top of the results, and it is
now out of the flow altogether - so it costs no line of the page and is in the
same place whatever the reader has scrolled to. It is still first in the DOM,
which `dev/plan_test.R` asserts. See `.fw-plan__results-head` in
`www/scss/_components.scss` for the stacking it has to respect.

**Nothing travels uninvited** (client, 24 September 2026). A methods-and-
caveats `.txt` used to go into every download whatever was ticked, which meant
ticking the PDF handed back a zip of two files. It is gone, and each document
now closes on that section itself: the workbook's last tab, the last section of
the PDF and of the records `.html`. With nothing left that an empty selection
could produce, the Download button is disabled until something is ticked - see
`fw_download_guard()` in `R/mod_plan.R`.

### The PDF report

`R/report_pdf.R`. The report the reader built, on FWISE letterhead, with every
logo the site's footer carries (FWISE on the letterhead; Weird Fishes Advisory
and the four collaborators in the footer of every page), static charts and a
static map, on A4. **Rendered on the server by Quarto, with its Typst engine** -
no LaTeX, no Chrome, no Python.

- **R decides everything; Quarto only typesets.** The body is written in R as
  calls to the components in `resources/report/typst-template.typ`, with every
  piece of text passed as an escaped string literal (`fw_typ_str()`), never
  spliced into markup. The `.qmd` Quarto renders has no R chunks, so no second
  R process has to find this app's library.
- **No values in the template.** Colours, sizes, logo paths and fixed wording
  come from `fwise-tokens.typ`, written per render by `fw_pdf_tokens()` from
  `R/brand.R`, `R/config.R` and the copy deck. `dev/check_literals.R` refuses a
  colour typed into a `.typ` file.
- **The figures are ggplot twins** of the page's plotly charts
  (`R/charts_static.R`). Each is drawn from the same counting function as its
  original - `fw_method_data()`, `fw_category_data()`,
  `fw_method_waterbody_data()`, `fw_duration_data()` in `charts.R` - so the two
  cannot count differently; `dev/value_test.R` checks both against a base-R
  recount. They are drawn at their printed size, so the 1rem floor (12.6pt on
  paper, `FW_PRINT` in `brand.R`) holds for chart labels too. Each follows the
  reader's count/share toggle.
- **The map** is ggplot on Natural Earth 1:110m outlines bundled in
  `resources/report/` (built by `dev/build_report_basemap.R`), so it needs no
  tiles and no network.
- **The species photographs** are fetched from Wikimedia at render time, in
  parallel, each under `FW_PDF$image_timeout_s`; one that does not arrive prints
  as the page's placeholder. Credits and licences print under each.
- **The size warning.** `fw_pdf_size_estimate()` predicts pages and megabytes
  from the selection (contacts, map dots, photographs) before anything is
  drawn, and the picker warns at `FW_PDF$warn_pages` or `FW_PDF$warn_mb`. The
  whole database is about 25 pages and 2 MB, most of it the contacts table.
  The coefficients were fitted to real renders and `dev/value_test.R` fails if
  the estimate drifts more than 30% from three re-rendered cases.

### The attempts file

`R/report_records.R`. Every attempt in the selection, in the spreadsheet's
order, as one card each: all 55 exported fields, labelled
(`FW_COPY$export$record_labels`) and grouped (`record_groups`), a blank field
saying "Not noted". A contents list with a find box sits above the cards; the
caveats and the logos sit below. It is built from `fw_export_frame()`, so it
carries exactly what the spreadsheet carries, with the same redaction. The
stylesheet (`www/scss/records.scss`) and the fonts are inlined - nothing is
fetched when it opens. Cards are assembled as escaped strings rather than a
tag tree: 914 cards as tags took minutes, as strings about two seconds.

**`jsonlite::base64_enc()` wraps at 76 characters**, and CSS will not parse a
newline inside `url()` - an inlined font silently goes missing.
`fw_html_base64()` strips the wrapping; do not go around it.

---

## Submissions

`fw_submit_attempt(record, data)` is the single entry point. Where it writes is
decided by `fw_data_mode()`: with a token it commits over the GitHub API, without
one it writes to the local inbox and needs no credentials. **Both produce the
same thing** - one CSV per submission in `inbox/`, named for its `attempt_id` -
so QA does not care which wrote it.

**A submission is one row in the shape of the database.** The inbox file has
exactly the columns of `attempts.csv`, in the same order, so the reviewer
compares like with like and folding it in is an append. Two things a submission
cannot know are ids for species and contacts the database does not hold yet.
Those travel in the same cell with a `new:` prefix and what the reviewer needs
to create the row - `new:Arctic charr (Salvelinus alpinus)|Fish`,
`new:Jane Doe|University of X|jane@x.org|public` - and the review step resolves
them. A contributor's contact is always a `new:` reference, because the form has
no contact picker; the review step matches it against `contacts.csv` so a known
person needs no decision.

**One file per submission, not an append.** The GitHub API has no append: adding
a row to a shared CSV means reading it, decoding it, appending, and PUTting the
whole file back quoting the blob SHA it was read at, and two contributors
pressing Send in the same moment make the second one 409 and need retry logic. A
file per submission has no read-modify-write at all, and the attempt id makes it
idempotent for free.

**A failed GitHub write is reported to the contributor, never quietly written to
disk instead.** On Connect Cloud the container filesystem is discarded on
restart, so a local fallback would hand someone a confirmation screen for a
submission that was already gone.

### The review loop

`dev/qa.R` is the QA step. Pull `fwise-data`, stage, review in a spreadsheet,
fold, commit, push, restart the app.

```bash
Rscript dev/qa.R stage                              # inbox -> qa/review_<date>.csv
Rscript dev/qa.R fold qa/review_<date>.csv          # decisions -> attempts.csv
Rscript dev/qa.R fold qa/review_<date>.csv --keep-pending
```

1. **Stage.** Every unprocessed inbox file becomes a row of
   `qa/review_<date>.csv`. Beside each id column is a `_names` companion -
   `invasive_species_names`, `primary_contact_names` - so the reviewer reads
   "Common carp (Cyprinus carpio); [NEW] Arctic charr (Salvelinus alpinus)"
   rather than ids. Every `new:` species becomes a row of
   `qa/species_new_<date>.csv` with the names split out, the taxa the
   contributor chose, family `Unknown`, and a `same_binomial_as` warning when a
   species with that scientific name is already held. Every `new:` contact
   becomes a row of `qa/contacts_new_<date>.csv`, with `action` pre-filled as
   `use:CO-…` when the name and organisation already exist. The console lists
   what looks off: a country not in the ISO list, a method not in
   `FW_METHODS`, an off-vocabulary driver, waterbody or agent, a possible
   duplicate of an existing site and start year.
2. **Review.** In the review file, correct any value and set `status` to
   `approved` or `rejected`. In the two `_new` files, set `action` on every row:
   `add` (fix the names, taxa and family first) or `use:<existing id>`.
3. **Fold.** Refuses to run while any `action` is blank or names an id that does
   not exist. Mints an id for every `add` and appends it to the lookup, rewrites
   every `new:` reference to an id, drops the `_names` columns, fills `iso3` and
   `continent` from the ISO lookup, appends the approved rows to `attempts.csv`,
   moves their inbox files to `inbox/merged/`, and rewrites `metadata.json`.
   Rejected rows go to `qa/rejected.csv`. Rows left `pending` stay in the inbox
   unless `--keep-pending`, which folds them in as pending, invisible publicly.
   Before writing, the folded rows are passed through the same `fw_unpack()` the
   app uses, so nothing that would stop the next startup can be committed.

It is **idempotent** - an attempt id already in `attempts.csv` is skipped by
both commands - and it **moves** folded files to `inbox/merged/`, so the app can
count outstanding submissions with a single directory listing instead of opening
every file, which over the API would be one request per submission.

A species added by the fold has no photograph yet. `dev/fetch_species_images.R`
visits only rows with a blank `image_url`, so run it afterwards.

---

## Environment variables

All optional. The app runs with none of them set. See `.Renviron.example`, and
copy it to `.Renviron` (gitignored) for local use.

| Variable | Set where | Purpose |
|---|---|---|
| `FWISE_DATA_TOKEN` | Connect Cloud settings | **The only thing a deployment needs.** A fine-grained GitHub PAT with Contents: Read and write on `freshwaterLife/fwise-data`. Set means read and write over the GitHub API; unset means the local sibling checkout and no network calls. Expires — see "Serving data without redeploying". |
| `FWISE_DATA_SOURCE` | rarely, for a test deploy | Overrides the above. A path reads that directory; an `https://` base reads over plain HTTPS with no credential and cannot write. Point it at the directory holding `attempts.csv` and `metadata.json`. |

**Never commit a credential.** `.Renviron` and `*.json` are gitignored, with
`manifest.json` explicitly re-included because it is configuration rather than a
secret.

---

## The design system

### Where to change things

| To change | Edit | Then run |
|---|---|---|
| a colour, a radius, a shadow, a font size | `R/brand.R` | `dev/check_contrast.R`, `dev/check_literals.R` |
| any wording, label, tooltip, sheet name | `R/copy.R`, `R/copy_contribute.R` or `R/copy_export.R` | `dev/check_literals.R` |
| a top-n limit, page size, map default, chart height | `R/config.R`, "Behaviour" | `dev/value_test.R` |
| the Wong or Tol data palettes | `R/config.R` | `dev/check_palette.R` |

Nothing else holds a value. If you find one, it is a bug: move it.

### Colour

**The client supplied two hex values, and they are the whole palette.**

| Source | Hex | Becomes |
|---|---|---|
| FWISE teal (client) | `#0F8B79` | `brand_teal` |
| FWISE indigo (client) | `#191144` | `brand_indigo`, and `ink` |

Everything else is derived from them or is a neutral tinted towards the
indigo. **There is no white anywhere.** The page is a cool off-white, every
surface on it is one tonal step lighter, and surfaces are lifted by that step
and by a soft shadow rather than by a border.

Tokens are named for their **role**, so a value can change without the name
lying. From `FW_COLOURS` in `R/brand.R`:

| Token | Hex | Role |
|---|---|---|
| `ink` | `#191144` | body text and headings |
| `ink_muted` | `#4a4468` | secondary text, captions, help |
| `brand_indigo` | `#191144` | the page-title band, the footer's lower tier |
| `brand_teal` | `#0f8b79` | accents and markers. **Non-text only** |
| `teal_text` | `#0c7565` | links and primary buttons: the teal that passes AA |
| `teal_hover` | `#0a6152` | hover for links and buttons |
| `teal_tint` | `#c9e7e3` | chips, the progress track, tinted panels |
| `teal_wash` | `#e4f1ee` | the faintest teal: notices, hover washes |
| `teal_light` | `#7fc5bd` | links and accents on the indigo ground |
| `page` | `#e9ebf3` | the page ground |
| `surface` | `#f7f8fc` | cards, tables, inputs, navbar, popups, the report sheet |
| `sunken` | `#eef0f7` | table header rows, the citation block |
| `border` | `#d9dcea` | hairlines, dividers, the chart grid |
| `border_input` | `#6b6486` | input edges: an interactive boundary, needs 3:1 |
| `on_indigo` | `#f7f8fc` | text on the indigo ground |
| `on_indigo_muted` | `#c9c4e3` | secondary text on the indigo ground |

**The brand teal fails WCAG AA as text** (3.5:1 on the page, 4.2:1 on a
surface). It is reserved for non-text use - the active nav marker, the KPI
rule, the focus ring - and every text or button role uses `teal_text`. Do not
set body-sized text in `brand_teal`. `dev/check_contrast.R` lists every pair
the interface uses and is the arbiter.

**Where the indigo lives.** The navbar has to stay light: the FWISE lockup's
wordmark is indigo and would vanish on an indigo bar. So every page opens with
its title reversed out of an indigo band under the navbar, and the footer's
lower tier is indigo. The footer's upper tier, which holds the two logos, is
on the page ground for the same reason.

**How the tokens reach the stylesheet.** `fw_compile_css()` in
`R/ui_helpers.R` passes `fw_sass_variables()` to `sass::sass()` ahead of the
entry file, so `$fw-ink` in Sass *is* `FW_COLOURS$ink`. `_tokens.scss` holds
only derived values (`$fw-hairline: 1px solid $fw-border`). A token missing
from `brand.R` fails the compile loudly rather than falling back to a stale
copy. Bootstrap is compiled separately by bslib and cannot see those
variables, which is why `R/theme.R` maps the same tokens onto Bootstrap's own
names.

### The 1rem type floor

**No text in the app is set below 1rem**, captions and labels included. That is
a client instruction, and holding it took three things:

- `$fw-size-caption` is now `1.15rem` rather than `0.85rem`, and `$fw-size-min`
  is `1rem`. **These are no longer the same value.** The client later asked for
  subtitles - the qualifying line under a block heading, which is caption - at
  `1.15rem`, and raising caption alone would have put it above body text at
  `1.0625rem`; so the whole scale moved up together, keeping the ratios between
  its steps, and the floor stayed where it was. Caption is now separated from
  body text by size **as well as** by colour. Use `$fw-size-caption` for small
  print that should ride the scale and `$fw-size-min` only where something must
  not grow.
- **Bootstrap's own small-text variables are set in `R/theme.R`.** A dozen of
  its components size themselves off separate variables that default to
  `0.875em` or less - form help text, validation feedback, small buttons,
  badges, legends - and every one renders somewhere in this app. bslib also sets
  its own `--bs-btn-font-size` of `.9375rem`, which is 15px.
- **Three third-party stylesheets needed longer selectors, not just a value.**
  ionRangeSlider (`.irs--shiny .irs-from`, 11px) and Leaflet
  (`.leaflet-container .leaflet-control-attribution`, 11px; `.leaflet .info`,
  14px, the map legend) each score two classes and load *after* our `<style>`
  block, so a two-class rule here ties and loses. The selectors in
  `_components.scss` are deliberately one class longer. Do not shorten them.

Plotly takes pixels rather than rem, so `FW_TYPE$floor_px` is 16 and
`uniformtext` is `minsize = FW_TYPE$floor_px, mode = "hide"` - a segment too
narrow for the floor shows no number rather than an unreadable one, and the
hover still carries it. That applies to every **stacked bar** in the app, which
is now all of them: both proportion donuts were removed at the client's request
(the outcome ring first, the method ring in September 2026), so there is no
longer a chart that opts out of `uniformtext`.

**The one exemption is pop-ups**, granted explicitly by the client: the
information popovers behind the (i) glyphs and the map popup may sit under the
floor, at `$fw-size-popup` (`0.9rem`). It applies to **transient overlay text
only**. Anything that stays on the page - the map legend, the attribution line,
photo credits, table cells, captions - is page text and sits at or above
`$fw-size-min`.

One consequence worth knowing about: the CC BY credit on a species tile used to
be held down by size (`0.6rem`). The floor takes that lever away, so it is held
down by weight and colour instead and the species name is stepped up to
`$fw-size-lead` to stay clearly above it.

**To verify the floor**, load the app and run this in the browser console - it
returns any on-page text under 16px, pop-ups excluded:

```js
Array.from(document.querySelectorAll("body *")).filter(e => {
  const r = e.getBoundingClientRect(); if (r.width < 1 || r.height < 1) return false;
  if (e.closest(".popover,.tooltip,.leaflet-popup,.fw-map-card,.fw-map-detail,.fw-popup")) return false;
  if (parseFloat(getComputedStyle(e).fontSize) >= 16) return false;
  return Array.from(e.childNodes).some(n => n.nodeType === 3 && n.textContent.trim());
}).map(e => getComputedStyle(e).fontSize + " " + e.className);
```

### Data visualisation colours are separate

Charts and map layers use the Wong (2011) colourblind-safe palette from
`FW_PALETTE` in `R/config.R`, so figures match the academic paper, the app and
exported reports:

```
Successful #009E73   Failed #E69F00   Ongoing #56B4E9   Unknown #CC79A7
Neutral #999999      Emphasis #0072B2  Vermilion #D55E00
```

**Never encode data with the interface teals, and never use the Wong colours as
interface chrome.**

The count drawn inside each outcome segment is **indigo, not white**
(`FW_OUTCOME_LABEL_INK`). That is measured, not stylistic: white numerals came
to 2.25:1 on the Failed orange and 2.31:1 on the Ongoing blue, so the figure the
reader is meant to read off the bar was the least legible thing on the page.
None of the four Wong colours is dark enough to take white at 4.5:1.

### The method palette

`FW_METHOD_COLOURS` was a single-hue teal ramp, dark to light by frequency.
**The client rejected it and they were right**: method is a nominal category,
not a magnitude, and a ramp tells a reader the segments are ordered when they
are not. Measured, the old ramp failed outright - its worst adjacent pair came
to dE 9.0 against a floor of 15 under normal vision, so neighbouring segments
genuinely were not separable.

The replacement is **seven distinct hues**, taken from Paul Tol's *muted*
qualitative palette (designed for colour-vision deficiency) with each hue
stepped into the usable lightness band (OKLCH L 0.43–0.77) and lifted over the
chroma floor (C ≥ 0.10). Tol's teal slot is deliberately unused, because the
brand teal is interface chrome and must never encode data.

```
ME07 Rotenone #007da4   ME04 Netting/Trapping #a58a22   ME02 Draining #8e2a72
ME03 Electrofishing #3f9b3f   ME01 Antimycin-A #8c4a1f
ME05 Other chemical #534bb4   ME06 Other mechanical #cf5f6f
```

**The order of that vector is load-bearing, twice over.** It is still frequency
order, so the stack reads most-used first; it is *also* the arrangement, out of
all 5040, that maximises the distance between segments that physically touch.
Reordering the entries re-colours the chart **and** degrades it.

| Pairlist | Worst pair (CVD / normal) | Floors | |
|---|---|---|---|
| Adjacent - what a stacked bar is judged on | 12.5 / 23.0 | 8 / 15 | **pass** |
| All pairs - the harder test | 2.7 / 12.4 | 8 / 15 | fails |

The all-pairs result is expected and is **not** a defect to fix by re-picking
colours: seven categories cannot be made pairwise-distinct at that floor by any
palette. It only bites where two non-neighbouring segments end up touching,
which needs an intervening method to be absent from that waterbody. The white
separator rule and the in-segment counts are what carry that case, which is why
both are mandatory rather than decoration.

Run `Rscript dev/check_palette.R` after touching any colour or the order. It
checks the lightness band, the chroma floor, CVD separation under simulated
protanopia and deuteranopia, separation under normal vision, contrast against
the surface, and the label ink inside every segment.

### Width

**Nothing is capped in pixels.** `.fw-container` is the one content-column rule
and every page uses it. It takes 95% of the viewport, with a floor that keeps a
24px gutter on a phone, resolved in one `min()` rather than a media query:

```scss
width: min(95%, calc(100% - 3rem));
```

`min()` takes whichever is smaller, so at 375px the fixed gutter wins and at
1920px the proportion does; the crossover happens on its own at around 960px.
The container used to stop at `max-width: 1180px`, which meant a 27-inch monitor
showed the same column as a laptop with empty page either side.

The `unquote()` around that value in `_components.scss` is load-bearing: libsass
has its own `min()` and fails the build trying to compare `95%` with a `calc()`.

**The prose measures went too.** About, the eradication preamble, the citation
block and the feedback box used to cap their paragraphs at 68–78 `ch`. That is a
character measure rather than a pixel one and it is normally right, but inside a
full-width tinted box it left two thirds of the box empty and read as a bug
rather than as typography.

The honest trade is stated here so nobody re-adds them by accident: **a
paragraph on About now runs to around 200 characters a line on a 27-inch
monitor, which is longer than is comfortable to read.** Two ways back, in
increasing order of effort:

- `.fw-measure` is still in the stylesheet and `$fw-measure` is still a token.
  Put the class on a block that genuinely reads better narrow.
- Better: **give the text a narrower box rather than giving a wide box narrower
  text.** `.fw-caveats__grid` is the worked example — short blocks laid out in
  `repeat(auto-fit, minmax(26rem, 1fr))`, so the column sets the line length,
  the panel fills, and nothing is capped. `auto-fit` means the block count is
  never written down: `fw_caveat_blocks()` parses whatever `fw_caveats()`
  returns and the grid reflows. About's sections would take the same treatment.

### Type

Ubuntu throughout, self-hosted in `www/fonts/` as woff2 (latin subset, 67KB for
all five faces). No CDN request is made for type, so the app renders correctly
with the network blocked. Ubuntu Mono is reserved for numerals in KPI figures,
counts, coordinates and record identifiers, via the `.fw-num` class. Never use it
for labels or body text.

### Motion

There is no page-load or scroll motion anywhere, and no count-up on any figure.
Numbers appear at their value. This is an evidence base, and figures that perform
their own arrival read as persuasion to exactly the audience this most needs to
convince. Motion exists only in response to a user action, at 150 to 200ms, and
all of it is disabled under `prefers-reduced-motion`.

---

## Deploying to Posit Connect Cloud

**Connect Cloud does not read `renv.lock`.** It reads `manifest.json`, which
carries both the package set and the R version. `renv.lock` is kept for local
development. Both are committed.

Regenerate the manifest whenever dependencies change:

```bash
R -e 'renv::snapshot()'
R -e 'rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")'
git add renv.lock manifest.json && git commit -m "Update dependencies"
```

`.rscignore` keeps `dev/` out of the bundle. Without it the manifest lists
gitignored files that Connect Cloud cannot find, and the deploy fails.

### Publishing

1. Push to a **public** GitHub repository, with `app.R` and `manifest.json` at
   the root.
2. At [connect.posit.cloud](https://connect.posit.cloud), sign in with GitHub and
   choose **Publish**.
3. Pick the repository, branch and `app.R` as the primary file.
4. Add the environment variables from the table above in the deployment settings.
   They are set in Connect Cloud's UI, never in a committed file.
5. Publish. The first build takes a while because `sf` and its system
   dependencies have to be resolved.
6. Check the second line of the log: `FWISE startup: quarto 1.x at ...`. Connect
   Cloud provides the Quarto CLI to Shiny apps; if the line says NOT FOUND, the
   PDF report is unavailable (the other downloads still work) and `QUARTO_PATH`
   is the setting to point at one.

Deployment credentials are held by the client and were not available when this
was written, so the steps above have not been executed.

## Pointing the custom domain at it

The domain is registered with Namecheap and is held by the client.

1. In Connect Cloud, open the content's settings and add the custom domain. It
   will give you a target hostname and a verification record.
2. In Namecheap, go to **Domain List**, then **Manage**, then **Advanced DNS**.
3. Add a `CNAME` record with the host you want (`www`, or `fwise` for a
   subdomain) pointing at the target hostname Connect Cloud gave you.
4. Add the verification record exactly as Connect Cloud specifies, usually a
   `TXT` record.
5. A bare apex domain (`example.org` with no subdomain) cannot take a CNAME.
   Use Namecheap's ALIAS record, or redirect the apex to `www`.
6. Wait for propagation, up to 48 hours but usually far less, then confirm in
   Connect Cloud. TLS is issued automatically once verification passes.

---

## Known limitations

- **There is no remote submission backend.** Submissions land in a local file
  only. The GitHub write path needs a token that does not exist yet. See
  [Submissions](#submissions).
- **`fw_country_burden()` returns a placeholder.** The landing page map is meant
  to contrast the *burden* (invasive fish species per country) against the
  *response* (recorded eradication attempts), and the mismatch between them is
  the whole point. No per-country invasive fish file exists in the repository, so
  the function currently derives its figures from FWISE's own records, which is
  the response and not the burden. It does not yet tell the story. The function
  is isolated so replacing it is a one-line change.
- **The client's spreadsheet is no longer the master.** `attempts.csv` is. A
  fresh export from the old spreadsheet cannot simply be dropped in; a value
  changed there has to be changed in the row, or a new migration written and
  reconciled with `dev/reconcile_source.R`.
- **The FWISE lockup's wordmark is indigo**, so the navbar and the footer's
  upper tier stay on a light ground and the indigo sits in the page-title band
  and the footer's lower tier. Reversed artwork would open up an indigo navbar.
- **Carto now watermarks its keyless tiles.** Every map in the app is currently
  drawn over "API KEY REQUIRED" repeated across the basemap. `fw_carto_url()`
  already appends `FWISE_CARTO_KEY` if it is set, so a Carto account fixes it
  without a code change; the alternative is changing the default ground in
  `fw_add_basemaps()`. See HANDOVER.md section 5.5, which records a September
  2026 check that no longer holds.
- **The PDF's species photographs need the network at download time.** They
  are Wikimedia's, fetched when the PDF is built; one that does not arrive
  prints as the placeholder, and the rest of the report is unaffected.
- **The PDF needs Quarto on the server.** Assumed present on Connect Cloud; the
  startup log confirms it. Without it the PDF is left out of the picker.
- Every remaining placeholder and open decision is listed in
  [HANDOVER.md](HANDOVER.md).

---

## Licence

Code is released under the MIT licence, see [LICENSE](LICENSE). The data is
released under CC BY-NC 4.0: attribution required, non-commercial use only. Both
lines appear in the footer on every page, from `R/copy.R` `footer$licence`.
