# FWISE

**FWISE** is the Freshwater Invasive Species Eradication database: a public,
world evidence base of eradication attempts against freshwater invasive animals,
built for [Freshwater Life](https://www.freshwaterlife.org). It is an R Shiny
application that launches publicly in October 2026.

It currently holds **914 eradication attempts** across **29 countries**, going
back to **1934**.

> **This repository holds code only.** The data lives in a separate repository,
> [`fwise-data`](../fwise-data), which is canonical. The two are split so the
> client can publish new data by pushing there, without touching or redeploying
> this app. See [How the data layer works](#how-the-data-layer-works).

> **Status.** Contribute, Networking and Plan an eradication (the report
> builder) are built. Home, Explore the data and About are deliberate stubs with
> their intended structure recorded in comments inside each module. See
> [HANDOVER.md](HANDOVER.md) for exactly what is and is not done.

---

## Contents

- [Running it locally](#running-it-locally)
- [Repository layout](#repository-layout)
- [How the data layer works](#how-the-data-layer-works)
- [Updating the data](#updating-the-data)
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
network calls in local development.** With nothing configured it reads
`../fwise-data/schema/` and writes any submissions to `../fwise-data/inbox/`,
one CSV per submission — the same shape the deployment writes over the API, so
the QA loop is identical either way.

`renv::restore()` will take a while the first time. `sf` is in the dependency
list because `leaflet` imports it, and it needs GDAL, GEOS and PROJ present on
the machine. On macOS: `brew install gdal geos proj`.

### Checking your changes

```bash
Rscript dev/smoke_test.R      # form gating, validation and the write path
Rscript dev/plan_test.R       # report builder: gate, states, filters, export
Rscript dev/check_contrast.R  # every colour pair against WCAG AA
```

Two scripts write into `../fwise-data/` and are the only things in the project
that touch the network. The **app itself never does.** Run them by hand and
review the diff before committing:

```bash
Rscript dev/fetch_species_images.R    # species photos from Wikimedia -> species.csv
Rscript dev/build_iso_lookups.R       # ISO 3166-1 and -2 -> the two lookup files
```

Both test scripts exit non-zero on failure, so they are usable from CI.

Run `check_contrast.R` after changing **any** colour. It is the thing that
catches an inaccessible palette before a user does.

---

## Repository layout

```
.
├── app.R                       entry point, must stay at the repository root
├── R/
│   ├── config.R                paths, environment variables, the Wong palette
│   ├── copy.R                  EVERY user-facing string
│   ├── theme.R                 the bslib theme
│   ├── ui_helpers.R            reusable UI components
│   ├── data_load.R             the data contract, read by every module
│   ├── data_prep.R             BUILD SCRIPT, not part of the running app
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
│   ├── report_html.R           the report builder's document output: one
│   │                           self-contained HTML file, live charts and
│   │                           map inside it, printable to PDF
│   └── mod_*.R                 one file per page. The contribute page is split
│                               into mod_contribute.R (server logic),
│                               mod_contribute_steps.R (section builders) and
│                               mod_contribute_ui.R (the three page states)
├── www/
│   ├── scss/                   _tokens.scss, _components.scss, main.scss
│   ├── fonts/                  self-hosted Ubuntu woff2
│   └── img/                    logos and favicon
├── dev/                        local scratch, gitignored except the scripts
│                               named under "Checking your changes" below
├── manifest.json               what Connect Cloud actually deploys from
├── renv.lock                   what local development restores from
└── .Renviron.example           documents every environment variable
```

There is deliberately **no `data/` directory here.** It lives one level up:

```
../fwise-data/
├── fwise_2026-09-06.csv        the raw client export, never modified
├── lookup_country.csv          maps the EXPORT'S messy country values onto
│                               clean names. About this dataset
├── lookup_iso3166.csv          the ISO 3166-1 country list. About the STANDARD.
│                               Built by dev/build_iso_lookups.R
├── lookup_iso3166_2.csv        ISO 3166-2 subdivisions, ditto
├── metadata.json               release date and row counts
├── id_registry/                the permanent id crosswalk - never hand-edit
└── schema/                     the six star-schema tables, generated
```

### Two things that will bite you

**Shiny sources everything in `R/` automatically at startup.** This is a
documented Shiny feature, not something `app.R` does. Any file you drop into
`R/` runs on boot. That is why `R/data_prep.R` wraps its work in
`fw_build_schema()` and only executes under a direct `Rscript` call. If you add
another build script to `R/`, give it the same guard or it will run every time
the app starts.

**Colour values live in two places.** Sass variables cannot cross into R, so the
palette is defined in `www/scss/_tokens.scss` and mirrored in `FW_COLOURS` in
`R/theme.R`. Change both, then re-run `dev/check_contrast.R`.

---

## How the data layer works

Every module reads through `R/data_load.R`. **No module reads a CSV directly.**
When the real schema changes, that file should be the only one you need to touch.

The data is a star schema of six tables:

| Table | Grain |
|---|---|
| `attempt.csv` | one row per eradication attempt (the fact table) |
| `species.csv` | one row per species |
| `attempt_species.csv` | bridge, with `role` of `invasive` or `beneficiary` |
| `method.csv` | one row per method, classed `chemical` / `mechanical` / `other` |
| `attempt_method.csv` | bridge |
| `contact.csv` | one row per contact |

Two structural points that are settled and should not be generalised:

- **Methods are an unbounded any-of set**, not a ranked hierarchy, which is why
  they use a bridge table. `method_order` exists only so the paper's figures can
  be reproduced. Do not treat it as a ranking.
- **An attempt has at most a primary and a secondary contact**, so contacts are
  two foreign keys on `attempt`, not a bridge. Do not turn this into a bridge.

### The functions

| Function | Returns |
|---|---|
| `fw_load_data()` | the six tables, loaded once at startup |
| `fw_attempts_wide()` | one row per attempt, with species, methods and contacts as list columns |
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
The real control is the `email_public` flag.

---

## Updating the data

The client's cleaned export is a single flat table. `R/data_prep.R` transforms it
into the six star-schema tables.

```bash
# 1. Drop the new export into ../fwise-data/
# 2. Point FW_SOURCE_CSV in R/config.R at the new filename
# 3. Rebuild - reads from and writes back to ../fwise-data/
Rscript R/data_prep.R
```

The transform is code, so it lives here; its inputs and outputs are data, so they
live in `fwise-data`. Commit the regenerated `schema/` there, not here.

It never writes to the source file. Correct values in the source export and
re-run, so the cleaning stays in one place.

The script **stops with a clear error** if the export contains a country not
listed in `../fwise-data/lookup_country.csv`. Add a row for the new country, giving it a
continent and ISO3, and re-run. This is deliberate: silently dropping a country
would quietly remove records from the map.

### Two source quirks the transform handles

- `Invasive Taxa` is underscore-joined and aligns one-to-one with the eight
  species slots.
- `Invasive Fish Family` is also underscore-joined but lists **only the fish
  entries**, in order. So family *n* attaches to the *n*th slot whose taxa is
  `Fish`, not to slot *n*. Getting this wrong silently assigns fish families to
  crayfish.

### Identifiers are permanent

`attempt_id`, `species_id` and `contact_id` are **minted once and never
reassigned**. Format is `FW-20260908-7K3QX9`: a prefix, the date the id was first
assigned, and a random suffix drawn from an alphabet with no `I`, `O`, `0` or `1`
in it, because these get read aloud and retyped by people.

`../fwise-data/id_registry/` holds the crosswalk that guarantees it. A rebuild
looks each row up there, reuses the id it already has, and mints a new one only
for a genuinely new row. **Do not hand-edit those files.**

This replaced ids that were row positions — `FW0001` from `row_number()`, and for
species and contacts a row number assigned *after* an alphabetical sort. Adding
one species beginning with "A" renumbered every species after it, and re-sorting
the export renumbered everything. Since those ids are the join key for QA
writeback, Zenodo versioning and reference linkage, that silently corrupted all
three.

`R/data_prep.R` also writes `id_registry/key_backfill.csv`, a pasteable `Key`
column for the client's master spreadsheet. Once `Key` is populated it becomes
authoritative and the natural-key registry is only a fallback.

If you change how the natural key is built, **every row looks new** and every id
is re-minted. The build's registry check will not save you — it only catches a
key resolving to a *different* id, not a key that no longer matches anything.

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

## The report builder

`R/mod_plan.R`, with filters in `mod_plan_filters.R`, rendering in
`mod_plan_results.R`, the spreadsheet export in `export.R` and the HTML report
in `report_html.R`.

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
| Empty | before any build | explains what the page does and what you will get |
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

**The caveats panel is always visible**, never an accordion, and the same text
goes into every export from the same function, so the two cannot say different
things.

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

### The HTML report

`R/report_html.R`. The same report the reader is looking at, on FWISE
letterhead, as **one self-contained `.html` file**: every stylesheet, script,
webfont, the logo and the data itself are inlined, so nothing is fetched when
the file is opened. Around 4 MB, nearly all of it plotly.

**This replaced a Word export.** Both produced a document with the charts in it;
the difference is what a chart *is* in each format.

- **No round trip to the browser.** A `.docx` can only hold a chart as a
  picture, and rendering a plotly figure to a PNG server-side needs kaleido
  (Python) or webshot2 (Chrome), neither of which belongs on Connect Cloud. So
  the Word route asked the browser to photograph every figure with
  `Plotly.toImage()`, posted the base64 PNGs back over the websocket into a
  Shiny input, stashed them server-side and then *clicked a hidden download
  button* on the reader's behalf. All of that is gone. This is an ordinary
  `downloadHandler`.
- **Vector, not a screenshot.** The figures travel as live plotly graphs. They
  stay crisp at any zoom and at print resolution, and they keep their hover
  readouts.
- **The map travels.** Leaflet could never be captured — its tiles are
  cross-origin and taint the canvas — so the Word file had a country table
  standing in for it. Here the real map is in the document, with the country
  table kept beside it for print and for readers with no connection.
- **The page's own components.** The summary strip, the outcome bars, the
  attempts table and the caveats panel are the *same functions* the page
  renders (`fw_plan_summary_ui()`, `fw_outcome_bars_ui()`, `fw_plan_table()`,
  `fw_plan_caveats_ui()`), under the *same compiled stylesheet*
  (`fw_html_app_css()` runs the same `main.scss` the app serves). Nothing is
  translated into Word table primitives, so the report cannot drift away from
  the screen, and a component restyled in `_components.scss` is restyled in
  every report built afterwards.
- **No new infrastructure.** No kaleido, no Python, no webshot2, no headless
  Chrome, no LaTeX — the same constraint that deferred PDF output in the first
  place. No new package either: `sass`, `htmltools`, `jsonlite` and `openxlsx`
  were all already dependencies.

**PDF is the browser's print engine.** The report carries a **Save as PDF**
button that calls `window.print()`. The `@media print` rules in
`fw_html_report_css()` are the export: they drop the toolbar, force
`print-color-adjust: exact` so the outcome bars keep their fills, repeat table
headers across pages, keep figures and table rows from splitting, and let
plotly's SVG scale down to the paper width. A bundled `html2pdf.js` or `jsPDF`
was considered and rejected — both rasterise the DOM to a canvas, which throws
away the vector output that is half the point of the file.

**The data is inside the report.** Two more buttons hand back the selection as
a UTF-8 CSV and as the full four-sheet workbook — built by `fw_write_workbook()`,
the *same* function the spreadsheet button serves, so the copy inside the report
and the copy downloaded beside it are identical. Both are carried as base64 in
inert `<script type="application/base64">` elements and handed out as Blobs, which
is the route that works from a `file://` URL and offline.

**What you see is what you get.** The method chart has a share/count toggle, and
`input$method_mode` travels into the download, so the document shows whichever
mode the reader is looking at rather than re-deciding for them. A chart that had
too little data to draw returns `NULL` and is skipped, heading and all.

Two things to know before editing:

- **`jsonlite::base64_enc()` wraps at 76 characters.** CSS will not parse a
  newline inside `url()`, so a wrapped data URI keeps its rules and silently
  loses every inlined image — leaflet's zoom and layer icons come out as empty
  white boxes with nothing in the console. `fw_html_base64()` strips the
  wrapping; do not go around it.
- **Relative URLs do not survive inlining.** `leaflet.css` asks for
  `images/marker-icon.png` and `main.css` for `../fonts/ubuntu-400.woff2`;
  pasted into a `<style>` block those resolve against wherever the reader saved
  the file. `fw_html_inline_css_urls()` rewrites every one of them to a data
  URI.

---

## Submissions

`fw_submit_attempt(record, data)` is the single entry point. Where it writes is
decided by `fw_data_mode()`: with a token it commits over the GitHub API, without
one it writes to the local inbox and needs no credentials. **Both produce the
same thing** — one CSV per submission in `inbox/`, named for its `submission_id`
— so QA does not care which wrote it.

**One file per submission, not an append.** The GitHub API has no append: adding
a row to a shared CSV means reading it, decoding it, appending, and PUTting the
whole file back quoting the blob SHA it was read at, and two contributors
pressing Send in the same moment make the second one 409 and need retry logic. A
file per submission has no read-modify-write at all, and `submission_id` makes it
idempotent for free.

**A failed GitHub write is reported to the contributor, never quietly written to
disk instead.** On Connect Cloud the container filesystem is discarded on
restart, so a local fallback would hand someone a confirmation screen for a
submission that was already gone.

**The Google Sheets backend has been removed.** Submissions moved to GitHub, so
the Sheets path was deleted rather than left in place as untested code with
credential handling in its documentation. It had never been run against a real
Sheet.

The inbox is a **raw submissions list, not the schema**. One flat row per
submission, with the repeatable species, methods and beneficiaries serialised
into single pipe-delimited cells, because a person reads them in a spreadsheet
during QA. Normalisation into the star schema happens manually in that review.

Every row is auto-populated with a `submission_id`, `status = "pending"` and a
`submitted_at` timestamp, so the QA and publishing pipeline can work without
duplicates.

### Testing the submit-review-publish loop locally

`dev/merge_submissions.R` is now **the QA step**, not scaffolding: pull
`fwise-data`, run it, review, commit, push, restart the app. It reads
`inbox/*.csv` and still reads the older `dev/submissions_local.csv` if one is
present, so submissions made before the GitHub write path landed are not
stranded.

```bash
Rscript dev/merge_submissions.R --list      # read-only: what would be merged
Rscript dev/merge_submissions.R             # merge as pending
Rscript dev/merge_submissions.R --approve   # merge and approve in one step
```

The loop:

1. Submit a record through the Contribute page. It lands in
   `../fwise-data/inbox/<submission_id>.csv`.
2. Merge it. It enters the schema as **`pending`**, mints ids through the same
   registry as everything else, and creates any new species, method or contact
   rows it needs.
3. Check it is invisible. A pending record must not appear in any dropdown, the
   Networking directory, the report builder's filters, or an export — while its
   foreign keys still resolve. That boundary is the thing worth testing.
4. Approve it: set `status` to `approved` in `fwise-data/schema/attempt.csv`.
   Restart the app and it appears everywhere.

It is **idempotent** — `submission_id` is the natural key, so re-running merges
nothing twice — and it **moves** merged files to `inbox/merged/` so the in-review
count does not count them again once they are in the schema. Moving rather than
rewriting a status cell is what lets the app count outstanding submissions with a
single directory listing instead of opening every file, which over the API would
be one request per submission.

It **warns about near-duplicate species**. The picker deliberately lets a
contributor type a name we do not hold, which means "Arctic charr" arrives as a
new species when "Arctic char" is already there on 24 attempts: same binomial,
different common name, two rows. The merge points at it rather than silently
creating the split.

To discard everything merged, re-run `Rscript R/data_prep.R` — the schema is
regenerated from the raw export.

---

## Environment variables

All optional. The app runs with none of them set. See `.Renviron.example`, and
copy it to `.Renviron` (gitignored) for local use.

| Variable | Set where | Purpose |
|---|---|---|
| `FWISE_DATA_TOKEN` | Connect Cloud settings | **The only thing a deployment needs.** A fine-grained GitHub PAT with Contents: Read and write on `freshwaterLife/fwise-data`. Set means read and write over the GitHub API; unset means the local sibling checkout and no network calls. Expires — see "Serving data without redeploying". |
| `FWISE_DATA_SOURCE` | rarely, for a test deploy | Overrides the above. A path reads that directory; an `https://` base reads over plain HTTPS with no credential and cannot write. Point it at the directory holding `metadata.json` and `schema/`, not at `schema/` itself. |

**Never commit a credential.** `.Renviron` and `*.json` are gitignored, with
`manifest.json` explicitly re-included because it is configuration rather than a
secret.

---

## The design system

### Colour

Every value is sampled from the two logo files in `www/img/`, not invented.

| Sampled from | Hex | Becomes |
|---|---|---|
| FWISE circle mark | `#108978` | `--fw-primary`, and the hue the whole ramp is built on |
| FWISE wordmark | `#191044` | deliberately **not** an interface token |
| Weird Fishes Advisory ink | `#00222b` | reconciled against `--fw-abyss` |

| Token | Hex | Role |
|---|---|---|
| `--fw-abyss` | `#0a2e29` | primary text, footer ground |
| `--fw-deep` | `#0d574c` | headings, links, primary buttons |
| `--fw-primary` | `#108978` | the brand teal. **Non-text roles only** |
| `--fw-shallow` | `#1c9484` | hover, progress fill, focus ring |
| `--fw-shoal` | `#c7ede8` | subtle fills, decorative hairlines |
| `--fw-line-input` | `#65948d` | input borders |
| `--fw-silt` | `#f7f4ef` | page background, the warm neutral |
| `--fw-paper` | `#ffffff` | cards and panels |
| `--fw-ink-muted` | `#4a6a64` | secondary text |

**The true brand teal fails WCAG AA as text.** `#108978` on the page background
is 3.93:1 and white on it is 4.31:1, both under the 4.5:1 floor. So
`--fw-primary` is reserved for non-text use and every text or button role falls
back to `--fw-deep`. Do not set body-sized text in the brand teal.

`--fw-line-input` is an addition to the brief's token list. `--fw-shoal` is a
decorative hairline and is correctly below the 3:1 floor, because WCAG 1.4.11
applies to interactive component boundaries rather than dividers. Input borders
*are* interactive boundaries, so they need their own darker token.

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
- **Attempt identifiers are positional.** The source `Key` column is empty, so
  `attempt_id` is generated from row order and is stable only while that order
  is. If the client starts populating `Key`, switch to it in `data_prep.R`.
- **Both logo files are dark ink for light backgrounds**, and the FWISE lockup is
  not knocked out of its own, so on the dark footer the pair share one white
  plaque at matching height. Reversed artwork would let the plaque go.
- **Carto now watermarks its keyless tiles.** Every map in the app is currently
  drawn over "API KEY REQUIRED" repeated across the basemap. `fw_carto_url()`
  already appends `FWISE_CARTO_KEY` if it is set, so a Carto account fixes it
  without a code change; the alternative is changing the default ground in
  `fw_add_basemaps()`. See HANDOVER.md section 5.5, which records a September
  2026 check that no longer holds.
- **The report is around 4 MB**, nearly all of it the bundled `plotly.js`. That
  is the price of interactive vector figures that work with the network down,
  and it is comparable to the Word file it replaced once that file's PNGs were
  counted. `plotly::partial_bundle()` would cut it substantially, but the
  partial bundles are not shipped in the installed package and fetching them
  would reintroduce a network dependency at build time.
- **The report has no running header or page numbers.** The letterhead is a
  block at the top of page one. `@page` margin boxes would give both, but
  browser support for their content is uneven enough that a header which
  renders in Chrome and vanishes in Safari is worse than none. See
  `fw_html_report_css()`.
- **The map inside the report needs a connection for its tiles.** The markers,
  popups and legend are in the file; the ground underneath them is not. This is
  stated on the face of the document, and the attempts-by-country table below
  it is what remains when the tiles cannot load.
- Every remaining placeholder and open decision is listed in
  [HANDOVER.md](HANDOVER.md).

---

## Licence

Code is released under the MIT licence, see [LICENSE](LICENSE). The data is
released under CC BY-NC 4.0: attribution required, non-commercial use only. Both
lines appear in the footer on every page, from `R/copy.R` `footer$licence`.
