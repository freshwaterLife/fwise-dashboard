# FWISE handover

What is built, what is not, every placeholder awaiting client copy, every
`TODO(alex)`, and every decision made that the brief did not cover.

Read alongside [README.md](README.md), which covers running, deploying and the
data layer.

---

## 1. What is built

| Page | State | Notes |
|---|---|---|
| Home | **Stub** | Full specification pasted into `R/mod_home.R` as comments |
| Explore the data | **Stub** | Intended structure in `R/mod_explore.R` |
| Plan an eradication | **Stub** | Intended structure in `R/mod_plan.R` |
| Contribute data | **Built** | One scrolling form, eleven sections, built to the field specification |
| Contacts | **Built** | Reads the real 237-contact table |
| About | **Stub** | Intended structure in `R/mod_about.R` |

Also built: the design system, navigation and footer, the data layer and its
transform, and the submission write path.

### Verified working

- `shiny::runApp()` from a clean checkout with no credentials, no errors.
- All six navigation items with correct active states.
- The form completes end to end at 375px and writes to
  `dev/submissions_local.csv`.
- Send stays disabled until every required field passes; errors are shown *and*
  announced through a live region.
- Contacts renders from the real table, rolls up by continent and country, shows
  attempt counts, and omits redacted emails from the served markup as well as the
  visible table. Tested by flipping a contact to not-public and confirming
  neither the address nor its local-part appears anywhere in the rendered HTML.
- Ubuntu renders from local files with no CDN request.
- Keyboard focus is visible on every interactive element.
- `prefers-reduced-motion` disables all motion.
- Every colour pair meets WCAG AA (`Rscript dev/check_contrast.R`).

### Not done, and deliberately out of scope

Landing page content, dashboard charts, maps of attempt data, filters and
cross-filtering, the report generation engine and PDF output, data cleaning,
taxonomy matching, species image scraping, authentication, and tests beyond the
smoke check.

---

## 2. Blocking on the client

These need someone else before the page can be finished.

| Item | Needed for | Where |
|---|---|---|
| Final headline and supporting copy | Landing page hero | `R/copy.R` `home$title`, `home$lead` |
| Per-country invasive fish species file | The landing map's whole point | `fw_country_burden()` in `R/data_load.R` |
| Case study content, before and after | Landing page | `R/mod_home.R` |
| Google service account and Sheet | Live submissions | `fw_write_sheets()` in `R/submit.R` |
| Zenodo DOI | Footer, About | `R/copy.R` `footer$doi_url` |
| Public GitHub repository URL | Footer | `R/copy.R` `footer$github_url` |
| FWISE team email address | Contacts page | `R/copy.R` `contacts$outro_email` |
| Terms of data use | Contribute consent | `R/copy.R` `contribute$consent$terms_url` |
| Review turnaround time | Confirmation screen | `R/copy.R` `contribute$confirm$followup` |
| Reversed (light) logo artwork | Footer | would remove the plaque, see 5.6 |
| Weird Fishes Advisory website URL | Footer logo link | `R/copy.R` `footer$wfa_url` |
| Deployment credentials and domain | Going live | see README |

---

## 3. Every `[PLACEHOLDER]`

All in `R/copy.R` unless stated. Search the file for `[PLACEHOLDER]` to find them.

| Location | What it is |
|---|---|
| `home$title` | Landing headline. Final wording belongs to the client |
| `home$lead` | Supporting sentence under the headline |
| `footer$doi_url` | Zenodo DOI, currently `#` |
| `footer$github_url` | Public repository URL |
| `footer$wfa_url` | Weird Fishes Advisory website, currently a guess |
| `contacts$intro` | Framing line at the top of the contacts page |
| `contacts$outro` | Closing note offering a route to the FWISE team |
| `contacts$outro_email` | Currently `hello@example.org` |
| `contribute$consent$terms_link_label` / `terms_url` | Full terms of data use |
| `contribute$confirm$followup` | "within X working days" |
| `R/data_load.R` `fw_country_burden()` | Placeholder choropleth source |

The question list is no longer a file anyone maintains. `R/questions_text.R`
walks the same section builders the form renders and writes the download as plain
text, so adding a field to a section puts it in the download with no second edit.
There is nothing to replace.

---

## 4. Every `TODO(alex)`

| File | Line | Ambiguity, and the simplest reading implemented |
|---|---|---|
| `R/mod_contribute_steps.R` | ~135 | Spec asks for "a searchable dropdown of Sovereign ISO and Location ISO". Implemented as a country dropdown plus a free-text region box. `data/lookup_country.csv` already carries `iso3` and `region` and can drive a true two-level picker if the client wants one |
| `R/mod_contribute_steps.R` | ~299 | Spec asks for labour effort as "a numeric box with a free-text fallback for ranges". Implemented as one text box, so "20 to 30" is accepted and passed to QA |
| `R/mod_contribute_steps.R` | ~319 | Spec lists "Measured concentration notes" but no measured concentration *value*. Implemented as specified, notes only. Worth checking this was intended |
| `R/data_load.R` | ~99 | 35 of 390 species have no parenthetical scientific name (e.g. `Amphipoda`, `Barbus sp.`). The whole string sits in `common_name` and the dropdown label coalesces both fields so nothing is unfindable. The client's QA cleaning should split these properly |

---

## 5. Decisions made that the brief did not cover

### 5.1 Contacts layout: filters plus a summary strip

The brief asked me to choose between a collapsible grouped table and filters plus
a summary strip, once I had the real row count. **Chose filters plus a strip.**
237 contacts across 29 countries would produce roughly 29 accordion groups of
which most hold one to three people, so the visitor spends the visit expanding
things. Filters let someone jump straight to their region and the strip keeps the
roll-up visible.

Added pagination at 25 rows, which the brief did not ask for. Rendering all 237
rows produced a 17,000 pixel scroll, which defeats the purpose of a page meant to
help someone find one person.

### 5.2 The landing map source: placeholder, not the real burden

The brief said to use a per-country invasive fish file if present and a synthetic
layer if not, and to say which. **Neither, strictly.** No such file is in the
repository, so `fw_country_burden()` derives species counts from FWISE's own
records. That is the *response*, not the *burden*, so the section does not yet
tell the mismatch story it exists to tell. I preferred real data with a stated
caveat over synthetic numbers that could be mistaken for real. It is isolated
behind one function so replacing it is a one-line change.

### 5.3 Continent, ISO3 and region are derived, not sourced

The schema requires `continent`, `iso3` and `region`; the source export has none
of them. `data/lookup_country.csv` supplies them, keyed on the raw country string
so each territory can take its **own** continent rather than inheriting the
sovereign state's. `United States (Guam)` maps to Oceania, not North America.
The transform stops with a clear error on any country not in the lookup.

### 5.4 An extra colour token, and the brand teal restricted

`--fw-line-input` (`#65948d`) is an addition to the brief's fixed token list.
`--fw-shoal` is a decorative hairline at 1.2:1 and is correctly below the 3:1
floor, since WCAG 1.4.11 governs interactive component boundaries rather than
dividers. Input borders *are* interactive boundaries and needed their own token.

Separately: the sampled brand teal `#108978` **fails AA as text** (3.93:1 on the
page, 4.31:1 for white on it). Following the brief's own instruction, text and
button roles fall back to `--fw-deep` and the brand teal is kept for non-text
use. The FWISE wordmark indigo `#191044` was deliberately not promoted to a
token, as that would introduce the second accent hue the brief rules out.

### 5.5 Base map is Esri, not Carto

`CartoDB.Positron` is the usual muted choice and was the first pick, but Carto
now watermarks keyless requests with "API KEY REQUIRED" across every tile.
Switched to `Esri.WorldGrayCanvas`, equally muted and still keyless. If the client
obtains a Carto key, switch back.

### 5.6 Footer logos share one white plaque

Both supplied logo files are dark ink drawn for light backgrounds, the FWISE
lockup is not knocked out of its own, and the footer is `--fw-abyss`. The two sit
together on one white rounded plaque at matching height, with "Built by Weird
Fishes Advisory" on its own line underneath, and each links out to its
organisation. Reversed artwork would let the plaque be deleted.

### 5.7 `manifest.json` is what actually deploys

The brief said to verify Connect Cloud's requirements rather than assume. **It
does not read `renv.lock`**; it reads `manifest.json`, which carries the package
set and the R version. Both are committed: the manifest for deployment,
`renv.lock` for local `renv::restore()`. `.rscignore` keeps `dev/` out of the
bundle, without which the manifest lists gitignored files the deploy cannot find.

### 5.8 `sf` is in the dependency list

The brief said `sf` only if genuinely needed. It is: `leaflet` **imports** it, so
it is a hard dependency rather than optional. It needs GDAL, GEOS and PROJ on the
deployment host, which lengthens the first Connect Cloud build.

### 5.9 Two dev scripts are kept in version control

`dev/` is gitignored, but `dev/check_contrast.R` and `dev/smoke_test.R` are
re-included. `_tokens.scss` cites the contrast checker by name, so ignoring it
would leave a dangling reference. This needed `dev/*` rather than `dev/` in the
gitignore, because git cannot re-include a file whose parent directory is
excluded.

### 5.10 The contribute page is split across four files

The brief asks for modules under roughly 300 lines. A form this size does not fit
that in one file, so it is split by concern: `mod_contribute.R` holds server
logic, `mod_contribute_steps.R` the section UI builders, `mod_contribute_ui.R`
the three page states, and record assembly sits in `submit.R` beside the column
list it fills.

### 5.11 The form is one scrolling page, not a wizard

It was a stepped wizard, and the step body was rendered through `renderUI`. Every
Back and every Next therefore destroyed and rebuilt the inputs on the step being
left, which is what broke going back, doubled the repeatable rows, and left the
server-side species selectize being populated against elements that were not in
the DOM yet. Every section is now rendered once and never re-rendered, and there
is no UI rendering on the form after the first paint.

Two consequences worth knowing. **The progress rail is entirely client-side**
(`fw_form_script()`): the bar tracks scroll position and the label counts
answered required fields, and neither touches the server, because recomputing
them per keystroke is exactly the lag a single long page is meant to avoid.
**The review table renders on demand**, behind "Check my answers", for the same
reason.

### 5.12 Send-gating is computed separately from the validators

`shinyvalidate` reports a **disabled** validator as valid. The validators start
disabled so a contributor is not shown errors for fields they have not reached,
which means `is_valid()` cannot be used for gating. `all_valid()` therefore
checks the required inputs explicitly. **If you add a required rule, add a line
to `all_valid()` as well**; a comment in the file says so.

Send is deliberately always enabled. Pressing it on an incomplete form enables
every validator, scrolls to the first field that needs attention and announces
the failure, which is more use than a disabled button that will not say why.

### 5.13 The fish family is derived, never asked

`fw_family_for_species()` looks the family up from the species the contributor
picked and falls back to `"Unknown"`. The form used to ask for it. Anything the
database does not hold - including a species someone typed in themselves -
arrives as "Unknown" for QA to fill in.

---

## 6. Traps worth knowing about

**Shiny auto-sources everything in `R/`.** Any file dropped in there runs on
boot. `R/data_prep.R` was rebuilding the star schema on every app start until its
body was wrapped in `fw_build_schema()` behind a direct-invocation guard. Give
any new build script the same guard.

**`\s` is not a whitespace shorthand in R's default regex engine.** The email
pattern originally used `[^@\s]`, which excludes the *letter* s, so
`tester@example.org` was rejected as malformed. Use POSIX classes
(`[^[:space:]@]`) or pass `perl = TRUE`. The smoke test covers this.

**A `selectize` with choices and no blank first option auto-selects the first
one.** The country dropdown silently defaulted to Argentina, so any contributor
who did not touch it would have had their record filed under the wrong country.
`fw_select()` now always prepends a blank option.

**Fish family aligns to fish slots, not to species slots.** In the source,
`Invasive Taxa` is underscore-joined one-to-one with the eight species slots, but
`Invasive Fish Family` lists only the *fish* entries in order. Family *n*
attaches to the *n*th `Fish` slot. Getting this wrong silently assigns fish
families to crayfish.

**Colour values live in two places**, `www/scss/_tokens.scss` and `FW_COLOURS` in
`R/theme.R`, because Sass variables cannot cross into R. Change both and re-run
`dev/check_contrast.R`.

---

## 7. Suggested next steps

1. Get the per-country invasive fish file. The landing page's central argument
   depends on it and everything else there is ready.
2. Wire up the Google Sheet and actually run the Sheets backend. It has never
   been executed.
3. Replace the placeholder copy in `R/copy.R`, working down section 3.
4. Confirm the Weird Fishes Advisory URL behind the footer logo
   (`footer$wfa_url`). The FWISE logo already links to freshwaterlife.org.
5. Build the Explore page, which unblocks the "see their attempts" route from
   Contacts. `fw_contacts_summary()` already returns an `attempt_ids` list column
   per contact.
6. Deploy to Connect Cloud early, before the October webinars, so the `sf` system
   dependencies and the custom domain are not a launch-day surprise.
