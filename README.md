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

> **Status: skeleton.** The Contribute and Contacts pages are built. Home,
> Explore the data, Plan an eradication and About are deliberate stubs with
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
`../fwise-data/schema/` and writes any submissions to
`dev/submissions_local.csv`.

`renv::restore()` will take a while the first time. `sf` is in the dependency
list because `leaflet` imports it, and it needs GDAL, GEOS and PROJ present on
the machine. On macOS: `brew install gdal geos proj`.

### Checking your changes

```bash
Rscript dev/smoke_test.R      # form gating, validation and the write path
Rscript dev/check_contrast.R  # every colour pair against WCAG AA
```

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
│   ├── submit.R                the write path, and record assembly
│   └── mod_*.R                 one file per page. The contribute page is split
│                               into mod_contribute.R (server logic),
│                               mod_contribute_steps.R (section builders) and
│                               mod_contribute_ui.R (the three page states)
├── www/
│   ├── scss/                   _tokens.scss, _components.scss, main.scss
│   ├── fonts/                  self-hosted Ubuntu woff2
│   └── img/                    logos and favicon
├── dev/                        local scratch, gitignored except two scripts
├── manifest.json               what Connect Cloud actually deploys from
├── renv.lock                   what local development restores from
└── .Renviron.example           documents every environment variable
```

There is deliberately **no `data/` directory here.** It lives one level up:

```
../fwise-data/
├── fwise_2026-09-06.csv        the raw client export, never modified
├── lookup_country.csv          country to continent, ISO3 and region
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

Set `FW_DATA_URL` to the raw GitHub URL of a directory holding the six CSVs (no
trailing slash) and the app reads from there instead of `data/schema/`. That lets
the client publish new data by pushing a commit, with no redeploy.

---

## Submissions

`fw_submit_attempt(record, data)` is the single entry point. It appends to
`dev/submissions_local.csv`, creating it with headers if absent, and needs no
credentials.

**The Google Sheets backend has been removed.** Submissions are moving to GitHub,
so the Sheets path was deleted rather than left in place as untested code with
credential handling in its documentation. It had never been run against a real
Sheet.

**The GitHub write path is not built.** It needs a token that does not exist yet
and is a separate job. Until it lands, the local file is the whole story.

The inbox is a **raw submissions list, not the schema**. One flat row per
submission, with the repeatable species, methods and beneficiaries serialised
into single pipe-delimited cells, because a person reads them in a spreadsheet
during QA. Normalisation into the star schema happens manually in that review.

Every row is auto-populated with a `submission_id`, `status = "pending"` and a
`submitted_at` timestamp, so the QA and publishing pipeline can work without
duplicates.

---

## Environment variables

All optional. The app runs with none of them set. See `.Renviron.example`, and
copy it to `.Renviron` (gitignored) for local use.

| Variable | Set where | Purpose |
|---|---|---|
| `FW_DATA_URL` | Connect Cloud settings | Read the six CSVs from a URL instead of `../fwise-data/schema/` |

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
- Every remaining placeholder and open decision is listed in
  [HANDOVER.md](HANDOVER.md).

---

## Licence

Code is released under the MIT licence, see [LICENSE](LICENSE). The data is
released under CC BY-NC 4.0: attribution required, non-commercial use only. Both
lines appear in the footer on every page, from `R/copy.R` `footer$licence`.
