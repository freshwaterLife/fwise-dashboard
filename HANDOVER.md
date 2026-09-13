# FWISE handover

What is built, what is not, every placeholder awaiting client copy, every
`TODO(alex)`, and every decision made that the brief did not cover.

Read alongside [README.md](README.md), which covers running, deploying and the
data layer.

---

## 1. Pages

| Page                | Description                                                                                                                                                                                                                                                                                                                                                                              |
| ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Home                | Landing page offering case studies, connections to events, and map displaying current work and opportunties.                                                                                                                                                                                                                                                                             |
| Explore the data    | Dashboard page - users are encouraged to explore and filter the data.                                                                                                                                                                                                                                                                                                                    |
| Plan an eradication | This outputs data and a simple report for users based on filters they have applied. Idea is that users will use this to understand similar attempts that they might want to plan, or identify contacts in their area to apply to active conservation work. Users apply filters, click build, read, and can download as a spreadsheet or a self-contained HTML report that prints to PDF. |
| Contribute data     | One scrolling form allowing users to input data on **their** eradication attempt, whether it failed, is ongoing, or successful.                                                                                                                                                                                                                                                          |
| Networking          | Reads and displays the contact table.                                                                                                                                                                                                                                                                                                                                                    |
| About               | Project description, citation block, feedback box                                                                                                                                                                                                                                                                                                                                        |






---

## 2. Blocking on the client

These need someone else before the page can be finished.

| Item | Needed for | Where |
|---|---|---|
| Final headline and supporting copy | Landing page hero | `R/copy.R` `home$title`, `home$lead` |
| Per-country invasive fish species file | The landing map's whole point | `fw_country_burden()` in `R/data_load.R` |
| Case study content, before and after | Landing page | `R/mod_home.R` |
| Zenodo DOI | Footer, About | `R/copy.R` `footer$doi_url` |
| Public GitHub repository URL | Footer | `R/copy.R` `footer$github_url` |
| FWISE team email address | Networking page | `R/copy.R` `networking$outro_email` |
| Terms of data use | Contribute consent | `R/copy.R` `contribute$consent$terms_url` |
| Review turnaround time | Confirmation screen | `R/copy.R` `contribute$confirm$followup` |
| Reversed (light) logo artwork | Navbar | would allow an indigo navbar, see 5.6 |
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
| `networking$intro` | Framing line at the top of the networking page |
| `networking$outro` | Closing note offering a route to the FWISE team |
| `networking$outro_email` | Currently `hello@example.org` |
| `about$feedback_email` | Where the About feedback box addresses its mail. Currently `hello@example.org` |
| `about$method2` | The search strategy and review protocol |
| `about$citation` | The DOI inside the citation block |
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
| `R/mod_contribute_steps.R` | ~299 | Spec asks for labour effort as "a numeric box with a free-text fallback for ranges". Implemented as one text box, so "20 to 30" is accepted and passed to QA |
| `R/mod_contribute_steps.R` | ~319 | **Resolved.** The spec listed "Measured concentration notes" but no value; the source data holds 86 measured values, so the form now asks for both, and for the ingredient basis (Active or Product) that every chemical record carries |
| `R/data_load.R` | ~232 | 35 of 390 species have no parenthetical scientific name (e.g. `Amphipoda`, `Barbus sp.`). The whole string sits in `common_name` and the dropdown label coalesces both fields so nothing is unfindable. The client's QA cleaning should split these properly |

**Resolved since.** The "Sovereign ISO and Location ISO" ambiguity is closed:
the country field is now the full ISO 3166-1 list (countries with records first,
then the rest, then "Other (specify)"), and the region field is that country's
ISO 3166-2 subdivisions, narrowed as soon as a country is chosen. Both lists are
built offline by `dev/build_iso_lookups.R` and committed to `fwise-data/`.

The subdivision picker allows free text (`create = TRUE`) on purpose. ISO does
not name every catchment, county or district a treated site might sit in, and a
picker that refuses the true answer is worse than a text box.

`lookup_country.csv` is gone with the six-table build. Its job - splitting
"United States (Hawaii)" into country and region and giving Guam its own
continent - was done once by the migration, and `country`, `region`, `iso3` and
`continent` are now stored columns on the attempt, visible and editable in QA.
New rows get `iso3` and `continent` from `lookup_iso3166.csv` at fold time.

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

### 5.3 Continent, ISO3 and region are stored, derived from the country, and checked

The source export has none of them. They are ordinary columns of `attempts.csv`
now, and they follow one rule, enforced at every load by
`fw_validate_geography()` in `R/data_load.R`:

- `country` is the ISO 3166-1 name of the country or territory the site is in.
  A territory with its own ISO entry is **its own country**: the export's
  `United States (Guam)` is stored as country `Guam`, iso3 `GUM`, Oceania.
- `iso3` and `continent` are properties of the country and of nothing else.
  Hawaii is a state of the United States, so its rows are `USA` and North
  America wherever they sit on the globe. The earlier build filed Hawaii under
  Oceania and Guam under the United States, which was two rules at once.
- `region` is free text within the country - a state, a province, an island
  group - and never moves a row between countries or continents.

`dev/qa.R` writes `iso3` and `continent` from `lookup_iso3166.csv` when a row is
folded in, and refuses a row whose region is itself a country in the standard.
The load stops on any stored row that disagrees with the lookup, naming it. A
country the lookup does not know ("Other (specify)") is left to the reviewer.

The lookup itself was rebuilt on 13 September 2026: the builder had read the UN
sub-region column instead of the intermediate region, which left fifty
countries - Mexico and all of South America among them - with no continent.
See 5.27.

### 5.4 An extra colour token, and the brand teal restricted

`--fw-line-input` (`#6b6486`) is an addition to the brief's fixed token list.
`--fw-shoal` is a decorative hairline and is correctly below the 3:1 floor,
since WCAG 1.4.11 governs interactive component boundaries rather than
dividers. Input borders *are* interactive boundaries and needed their own token.

Separately: the brand teal `#0F8B79` **fails AA as text** (3.47:1 on the cream
page, 4.21:1 for white on it). Following the brief's own instruction, text and
button roles fall back to `--fw-deep` and the brand teal is kept for non-text
use.

> **SUPERSEDED, second round of client feedback.** The note above originally
> read that the wordmark indigo `#191044` was deliberately *not* promoted to a
> token, because that would introduce a second accent hue the brief ruled out.
>
> The client has since supplied `#0F8B79` and `#191144` as the two brand values
> and the workshop launch flyer as the scheme to follow, and that flyer is built
> on **indigo and teal together**. So the one-accent-hue rule no longer holds,
> and the reasoning in this section is kept only so the change is traceable.
>
> **SUPERSEDED AGAIN, third round.** The cream page went too: the scheme is
> now the indigo-tinted off-white described in section 5.26 and in the design
> system section of `README.md`. Token names changed to roles at the same time
> (`--fw-line-input` is `border_input`, `--fw-shoal` is `border` or
> `teal_tint` depending on the use).

### 5.5 Base map is Carto Voyager, with three alternatives

Superseded twice, and **now wrong again.** The first pick was `CartoDB.Positron`;
it was dropped on a claim that Carto watermarks keyless tiles with "API KEY
REQUIRED". That claim was rechecked on 9 September 2026 and did not hold at the
time - the unauthenticated `basemaps.cartocdn.com` endpoints returned ordinary
200 PNG tiles.

**It holds now.** Rechecked on 10 September 2026 by fetching a tile directly and
by screenshotting the report builder in a real browser: the tiles still return
200, but "API KEY REQUIRED / carto.com/basemaps/apikey" is drawn diagonally
across each one, so every map in the app currently reads as broken. The
200-response check that was used before is not sufficient - **look at the
pixels.**

Two ways out, and this is a client decision rather than a code one:

- Get a Carto account and set `FWISE_CARTO_KEY`. `fw_carto_url()` already
  appends it, so this is an environment variable and no code change.
- Change the default ground in `fw_add_basemaps()`. The Esri layers already
  wired up there are unwatermarked, but the grey canvas was dropped once on
  client feedback that it read as drab, so do not swap it back silently.

The grey Esri canvas was then dropped on client feedback that it read as drab.
`fw_add_basemaps()` in `R/maps.R` now offers four grounds through one layer
control - Plain (Carto Voyager, default), Water (`Esri.OceanBasemap`), Terrain
(`Esri.WorldTopoMap`) and Satellite (`Esri.WorldImagery`). Terrain is not
decoration: the metrics framework asks for it so a reader can eyeball whether a
waterbody is hydrologically isolated.

`FWISE_CARTO_KEY` is read by `fw_carto_url()` and appended if set, so moving to
a keyed Carto plan is an environment variable rather than a code change. It is
unset and does not need to be.

### 5.17 Point maps stay in Web Mercator; choropleths must not

Leaflet renders raster tiles in Web Mercator and nothing else. That is fine for
the attempt maps - a reader is zooming in on one site, and Mercator says nothing
false about a dot. It is **not** fine for a country choropleth: Mercator makes
Greenland and Canada shout and Africa and Indonesia whisper, which inverts the
gap story the landing page exists to tell. When the burden layer arrives, draw
it in Equal Earth from a countries GeoJSON rather than adding a choropleth to
the existing tile maps.

### 5.28 Stacked markers are grouped, and the record is fetched on click

Two changes to the attempt maps on 13 September 2026, both measured first.

**Grouping.** 96 of the 911 located attempts share their exact coordinates with
another, and only the top dot of a stack could ever be hovered or clicked. The
markers now go through Leaflet.markercluster, but with the radius set to zero
until the reader is zoomed well in (`FW_MAP$cluster` in `R/config.R`), which the
plugin reads as "group identical coordinates only". So the coarse view is still
coloured dots; a stack shows as a marker-sized indigo dot with its count in
its title, because a counted ring at world zoom piled thirty of them over
Norway; a click on it zooms in, and fans the members out once zooming cannot
separate them. From zoom 13 up, markers within a marker's width of each other
group too, and a group is drawn as a counted ring.

**The record on click.** A full build was sending 6.2 MB over the websocket, of
which 4.7 MB was the detail panel of every marker, and the server spent 2.6 s
rendering species figures for it - 2,646 figures for 306 species. Two fixes:
figures are rendered once per species (`fw_map_figure_cache()`), and the page
now sends only the hover card and fetches the record when a marker is clicked
(`detail = "lazy"` in `fw_add_attempt_markers()`, answered by
`fw_map_detail_server()`). The HTML report still embeds every record, because a
saved file has no server to ask.

### 5.6 The footer is two tiers, and the navbar stays light

The FWISE lockup (now a transparent PNG) has an indigo wordmark, so it cannot
sit on the indigo. The navbar is therefore on the light surface, the page title
sits in an indigo band directly under it, and the footer is two full-width
tiers: logos and attribution on the page ground, then the indigo band with the
release date, links and licence. The white plaque this section used to
describe is gone.

### 5.26 One place for each kind of value, and the tests that hold it

Third round of client feedback, and the refactor that went with it. Three
rules, each enforced by a script rather than remembered:

- **Design values live in `R/brand.R`.** Colour, type, spacing, radius, shadow,
  motion, breakpoints. They reach Sass through `fw_compile_css()`, which hands
  `fw_sass_variables()` to the compiler ahead of the stylesheet, so
  `_tokens.scss` holds no literals and cannot drift. Bootstrap is compiled
  separately by bslib, so `theme.R` maps the same tokens onto its names.
  Chart chrome, map markers, the workbook header and the Word output read
  `FW_COLOURS` and `FW_TYPE` directly. Tokens are named for their **role**
  (`teal_text`, `surface`, `border_input`), not their hue; the poetic names
  (`abyss`, `shoal`, `silt`) are gone.
- **Copy lives in the deck**: `copy.R`, `copy_contribute.R`, `copy_export.R`,
  read as one list by `fw_copy_all()` on every `fw_t()` call - on every call
  because Shiny sources `R/` in one order and a test script's `list.files()`
  in another, and a merge cached on the first call could be missing a file.
  `fw_fill()` fills `{placeholders}`; the caveats and the sentence that says
  "the ten most common" are templates filled from the data and from
  `FW_TOP_N`. **Never call `fw_t()` at the top level of a file.** The one
  string deliberately left as a constant is `FW_OTHER` in `data_load.R`,
  because it is compared against stored values.
- **Behaviour numbers live in `config.R`** under "Behaviour".

`dev/check_literals.R` fails on a hex literal outside the two token files, on
any white, and on any `fw_t()` key the deck does not define.
`dev/value_test.R` recomputes every number on screen, every chart bar and
every export row in plain base R and compares. Two stale assertions in
`plan_test.R` were fixed at the same time: the empty state before a build is
now genuinely empty, and the widget count is read from the document rather
than written down.

**Palette.** No white anywhere: the page is `#e9ebf3`, surfaces `#f7f8fc`,
lifted by tone and by `FW_SHADOW$card`. `teal_text` moved from `#0a5d50` to
`#0c7565` (4.7:1 on the page, so still AA) and the hover state is now a step
*darker*, because the old lighter hover fell under the floor at exactly the
moment the reader was about to click. Stacked-bar separators are gone from the
outcome charts and reduced to a hairline on the method chart, which keeps them
because of the CVD note in `config.R`. One latent Sass bug was found on the way:
`.fw-field:has(.is-invalid)` compiled to `margin-inline-start: -1rem - 3px`,
which is not CSS; it is a `calc()` now.

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

Rechecked when the lockfile was pruned, because `renv.lock` records leaflet's
`Requirements` as empty and that makes `sf` look like an orphan it would be safe
to drop. It is not. Reading the installed DESCRIPTIONs instead of the lockfile
shows `leaflet` hard-imports both `sf` and `raster`, which in turn pull `s2`,
`units`, `sp` and `terra`. **Do not try to drop them without dropping leaflet.**

### 5.14 Identifiers are minted, not positional

`attempt_id`, `species_id` and `contact_id` used to be row positions, the last
two assigned after an alphabetical sort. They are minted once and never
reassigned. The registry that used to guarantee that is gone (5.27): each id now
lives in the row or lookup that owns it, the attempt id is minted by the form at
submission time, and species and contact ids by `dev/qa.R fold`. Every id that
existed before the flatten is unchanged.

### 5.22 The report builder is stacked, and its Word output is photographed

Two changes made together, on client feedback, and they are related.

**The filter panel moved from a sidebar to a full-width panel above the
results.** The report builder is a form followed by its answer. Side by side,
the questions and the results are visible at once, which invites reading the
results first and then working the filters until they say something comfortable;
stacked, there is nothing to read until the questions have been answered. It
also gives both halves the whole page, which is where the method and duration
charts were being crushed. `mod_plan.R` scrolls to the results on Build and
announces the count, because a result below the fold looks like nothing
happening. **The dashboard keeps its sidebar** - browsing is watching the picture
change under the controls, so there the controls have to stay in reach.

**The report is one self-contained HTML file, and it replaced a Word export.**
The constraint that deferred PDF output still holds - kaleido needs Python,
webshot2 needs Chrome, and neither belongs on this deployment. The Word route
worked around it by asking the browser to photograph every figure with
`Plotly.toImage()`, posting the base64 PNGs back into a Shiny input, stashing
them server-side and clicking a hidden download button on the reader's behalf.

**None of that is needed to put a chart in an HTML file.** The figures travel as
live plotly widgets and the map as a live leaflet widget, so the download is an
ordinary `downloadHandler` and the two-beat capture, the stash and the hidden
button are all gone. Full explanation at the top of `R/report_html.R`.

What follows from the change, all of it a gain rather than a trade:

- **The map is in the document.** It could not be captured for Word - leaflet
  tiles are cross-origin and taint the canvas - so the Word file had a country
  table standing in for it. The table is still there, because the map's tile
  background needs a connection and a printed page wants a list.
- **The figures are vector and still interactive.** Crisp at any zoom and at
  print resolution, and they keep their hover readouts.
- **The report is the page.** The summary strip, outcome bars, attempts table
  and caveats panel are the same functions the page renders, under the same
  compiled `main.scss`. There is no second implementation to drift.
- **The attempts table carries every row**, not the Word file's first forty, and
  stays upright rather than needing landscape pages.
- **PDF is the browser's own print engine**, driven by the `@media print` rules
  in `fw_html_report_css()` and a Save as PDF button. No PDF library is bundled;
  `html2pdf.js` and `jsPDF` rasterise the DOM, which would throw away the vector
  output.
- **The data travels inside the report** - a CSV and the full four-sheet
  workbook, the latter built by the same `fw_write_workbook()` the spreadsheet
  button serves.
- **What you see is what you get.** `input$method_mode` travels into the
  download, so the method chart appears in whichever mode the reader is looking
  at.

Two traps that cost real time, both now guarded in code and in `dev/plan_test.R`:

- `jsonlite::base64_enc()` wraps at 76 characters. CSS will not parse a newline
  inside `url()`, so a wrapped data URI keeps its rules and **silently loses
  every inlined image**, with nothing in the console. `fw_html_base64()` strips
  it.
- Relative URLs inside a stylesheet do not survive being inlined.
  `fw_html_inline_css_urls()` rewrites them to data URIs.

### 5.23 The content column is a proportion, not a pixel cap

`.fw-container` was `max-width: 1180px`, so above about 1250px the app stopped
growing and sat as a fixed column with empty page either side. It is now
`min(95%, calc(100% - 3rem))`: 95% of the viewport, with a floor that keeps a
24px gutter on a phone, and no media query. Every page uses that one rule, so
this was a one-line change.

**The prose measures went with it**, which is the part worth a second look. The
About page, the eradication preamble, the citation block and the feedback box
capped their paragraphs at 68-78 `ch`. That is a character measure rather than a
pixel one and is normally the right call, but inside a box that now spans the
page it left two thirds of the box empty and read as a bug rather than as
typography.

The cost, stated plainly so nobody reverts it by accident: **an About paragraph
now runs to roughly 200 characters a line on a 27-inch monitor.** That is longer
than is comfortable. `.fw-measure` and `$fw-measure` still exist as the opt-in
way back for a single block.

The better fix, if the client raises it, is not to re-cap the text. It is to give
the text a narrower BOX rather than giving a wide box narrower text.
`.fw-caveats__grid` is the worked example - the caveat blocks now lay out in
`repeat(auto-fit, minmax(26rem, 1fr))`, so the column sets the line length, the
tinted panel fills completely, and nothing has a max-width at all. The count is
nowhere in the CSS or the prose: `fw_caveat_blocks()` parses whatever
`fw_caveats()` returns, so adding or removing a caveat needs no other edit.
About's sections would take the same treatment.

### 5.15 The report builder does not react to its filters; the dashboard does

`R/mod_plan.R` gates rendering behind an explicit Build report press. This is
deliberate design, not an oversight or a performance hack. Making the results
live would be a one-line change and should not be made.

`R/mod_explore.R` is live for the opposite reason: browsing is watching the
picture change. The two pages look inconsistent and are meant to - and since
5.22 they no longer share a layout either.

### 5.18 One filter engine, one registry

`R/filters.R` holds `FW_FILTERS` and every function that reads it. The filter
set used to be enumerated in four independent places - the clear-all vector, the
state snapshot, the zero-result hints and the workbook's Filters sheet - so
adding a filter meant four edits and any one could be missed. All four are now
driven from the registry. **Add a filter there and it appears everywhere.**

### 5.19 The report builder has no outcome filter, on purpose

The dashboard has one; the report builder does not. Graden's reasoning, from the
metrics framework: given their situation, someone planning an eradication should
see *everything* tried there and its association with success and failure.
Filtering to successes only produces false optimism about their own site.

`fw_plan_filter_ids()` is `fw_filter_ids(drop = "outcome")`. `dev/plan_test.R`
asserts the asymmetry in both directions so it cannot be quietly undone.

### 5.20 No success-rate headline

The metrics framework proposes "X% of attempts lead to successful eradication"
as a landing and dashboard headline. **Declined, and confirmed with the client.**
It collides with two standing rules: no single hero statistic on socially
sensitive methods, and never collapsing the four outcome states into one rate.
The outcome-stacked visuals carry the same information without inviting someone
to quote the number on its own. Raise it with Graden before revisiting.

### 5.21 Species photographs are cached, not fetched

`dev/fetch_species_images.R` resolves each species against Wikipedia and
Wikimedia Commons and writes the URL, credit, licence and file-page link into
`species.csv`. 308 of 390 resolve; the rest are group names ("Cyprinidae spp."),
hybrids and subspecies that have no article, and they render a labelled
placeholder.

The app reads the cache and makes **no** network call for an image it already
has, which keeps `config.R`'s promise that local development is offline.
`fw_species_image_fetch()` is a memoised live fallback for a species added since
the last run.

Two traps, both already sprung once:

- The old six-table build used to blank the image columns on every rebuild,
  silently discarding twenty minutes of Wikimedia's time. There is no rebuild
  any more: the image columns live in `species.csv`, which is maintained, not
  generated. Do not write a script that regenerates it.
- The resolver originally required a Commons `Artist` field and skipped anything
  without one, which threw away properly licensed photographs of common species
  (*Lota lota*, *Ameiurus nebulosus*). The **licence** is mandatory; the credit
  falls back to the source. Twenty species were recovered by that fix alone.

### 5.24 One token, two directions, and the repository is a constant

`fwise-data` is private, so `raw.githubusercontent.com` returns 404 and the
original plan of a raw base URL cannot work. Both directions go through the
GitHub contents API instead, in `R/github.R`, which is the only file that knows
GitHub exists.

**The deployment sets one variable, `FWISE_DATA_TOKEN`.** The repository and
branch are constants in `config.R`. Neither is secret and neither varies, and an
env var for each would only add ways to misconfigure a deploy without adding
anything you could do with it. The token is also the mode switch: set it and the
app is remote, unset it and local development makes no network calls at all,
which is the promise `config.R` has always made.

**One token does read and write.** Splitting them was considered and dropped. The
argument for splitting was that a write-capable token sits in a public-facing
process, but the submission path is built from a server-minted `attempt_id`
with nothing user-supplied in it, so reaching `attempts.csv` would need the
token exfiltrated from the process first. The cost of the split — two credentials
to rotate, two expiry dates — was real and the benefit was not.

**The consequence to write on a calendar: when the token expires the app stops
serving data, not just submissions.**

### 5.25 Submissions are one file each, and merging moves them

`inbox/<submission_id>.csv`, not an append to a shared inbox. The GitHub API has
no append: a shared file means read, decode, append, PUT quoting the blob SHA,
and two contributors pressing Send in the same second make the second one 409.
One file per submission has no read-modify-write to lose, and `submission_id`
makes it idempotent for free.

`dev/qa.R fold` **moves** folded files to `inbox/merged/` rather than
rewriting a status cell. That is what lets the in-review count list one directory
instead of opening every file — which over the API would be a request per
submission, to render a number.

**A failed GitHub write shows the contributor an error and does not fall back to
a local file.** On Connect Cloud the container filesystem is discarded on
restart, so the fallback would print a confirmation screen for a submission that
had already ceased to exist. That trap was live: `dev` is in `.rscignore`, so the
old `fw_write_local()` created the directory inside the container, wrote the row,
returned success, and lost it at the next restart.

### 5.16 The approval gate is structural

`fw_filter_approved()` in `data_load.R` removes unapproved rows AND every
species and contact nothing approved refers to any more, before any module sees
the data. A new page cannot leak a pending species or contact even if its author
never thinks about approval. Do not "optimise" this by filtering only the
attempt table. It runs on the in-memory tables `fw_unpack()` builds, so it did
not change when the files did.

### 5.27 The data is one wide table, and the star schema lives in memory

Fourth round, September 2026, and the one that changed the data's shape. Two
problems arrived together. `Target Ingredient Basis` - Active or Product, filled
on every one of the 325 chemical rows, and the thing that makes a concentration
comparable - was missing from the published data. And the six-table star schema
made manual QA impossible: one attempt was spread across five files joined by
opaque ids, and adding one row meant five file edits plus a registry append.

The basis column had vanished because the build's attempt table was a
hand-written column list and nothing checked the source against it. It was not
alone: `Measured Toxin Concentration (mg/L)`, three notes columns, `Sent` and
`Eradication or Control` had gone the same way, the numeric cast had turned 120
concentration ranges and 189 labour descriptions into `NA`, and nine method
notes on attempts with no method were dropped outright. The merge script lost
four of the form's notes fields for the same reason.

**What changed.** `fwise-data` now holds `attempts.csv` (one row per attempt,
all 74 source columns plus the app's five, values stored as the source's text),
`species.csv` and `contacts.csv` (lookups referenced by id), and `source/` with
the raw export and its row-to-id map. `data_load.R` unpacks the wide table into
the same six in-memory tables at startup, so no module changed. The migration
rebuilt everything from the raw export, resolved every id through the old
registries so all 914 attempt, 390 species and 237 contact ids are exactly what
they were, and was gated on two checks: `dev/reconcile_source.R` (every source
column and value present, identically coded, 74 of 74, zero differences) and an
old-schema equivalence test by id (identical). Load time did not move: 0.087s
before, 0.075s after at double the rows.

**Decisions made along the way, with the reasoning:**

- **Species and contacts stayed as lookups**, referenced by id, rather than
  being written as text into the row. Alex's call, and the numbers support it:
  a typo cannot mint a phantom species, and 84 of 237 contacts sit on more than
  one attempt, one on 208. The cost is long cells; the review file carries
  `_names` companions so nobody reads ids by eye.
- **Methods stayed as names.** Seven values fixed by the paper, held in
  `FW_METHODS` in `config.R`, with the ids `ME01`-`ME07` kept so
  `FW_METHOD_COLOURS` did not re-key. A method the data holds that the list does
  not loads as class `other` and `qa.R stage` flags it.
- **Taxa and family moved to `species.csv`.** They are properties of a species,
  the data holds no name with two taxa, and the reconciliation round-trips the
  source's per-attempt strings through the lookup to prove nothing was lost.
- **Verbatim text, cast in memory.** So the reconciliation is string identity
  and so the ranges survive. Two labour cells hold the export's own
  `10.199999999999999`; that is what the client's file says.
- **`FW_NOTES_SEP` is a pipe** because 32 method notes contain a semicolon and
  none contain a pipe. The nine method-less notes are kept verbatim in the
  notes cell; `method_notes` also stays on the in-memory attempt table because
  they have no bridge row to live in.
- **Line endings.** The export carries Windows line endings inside quoted
  paragraphs; the migration normalised them to `\n`, and the reconciliation
  applies the same normalisation to the source before comparing. It is the
  only normalisation there is.

`dev/reconcile_source.R` stays in the repository as the standing answer to "was
anything lost". The migration script and its equivalence test were deleted once
they had run.

### 5.9 Several dev scripts are kept in version control

`dev/` is gitignored, but `dev/check_contrast.R`, `dev/check_palette.R`,
`dev/check_literals.R`, `dev/smoke_test.R`, `dev/plan_test.R`,
`dev/value_test.R`, `dev/fetch_species_images.R` and `dev/build_iso_lookups.R`
are re-included. The last two write into `fwise-data/` and are the only things
in the project that make a network call; the app itself never does. `_tokens.scss` cites the contrast checker by name, so ignoring it
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

**A private repo answers 404, not 403.** GitHub will not admit that a repository
a token cannot see exists, so a mis-scoped token — resource owner left as a
personal account, or the repository not selected — looks exactly like a missing
file. `fw_gh_message()` in `R/github.R` spells this out in the error rather than
letting the log say "not found".

**Shiny auto-sources everything in `R/`.** Any file dropped in there runs on
boot. An earlier build script was rebuilding the whole schema on every app start
until its body was put behind a direct-invocation guard. Data scripts live in
`dev/` now; if one ever has to sit in `R/`, guard it with
`if (sys.nframe() == 0L)`.

**`\s` is not a whitespace shorthand in R's default regex engine.** The email
pattern originally used `[^@\s]`, which excludes the *letter* s, so
`tester@example.org` was rejected as malformed. Use POSIX classes
(`[^[:space:]@]`) or pass `perl = TRUE`. The smoke test covers this.

**A `selectize` with choices and no blank first option auto-selects the first
one.** The country dropdown silently defaulted to Argentina, so any contributor
who did not touch it would have had their record filed under the wrong country.
`fw_select()` now always prepends a blank option.

**Fish family aligns to fish slots, not to species slots.** In the client's
export, `Invasive Taxa` is underscore-joined one-to-one with the eight species
slots, but `Invasive Fish Family` lists only the *fish* entries in order. Family
*n* attaches to the *n*th `Fish` slot. The data no longer carries either
convention - taxa and family are columns of `species.csv` - but
`dev/reconcile_source.R` re-derives both from the export to check them, and
anyone reading a fresh export from the client needs to know.

**Colour values live in ONE place, `R/brand.R`.** `_tokens.scss` has no
literals; if the compiler says "Undefined variable", the token is missing from
`brand.R`. Re-run `dev/check_contrast.R` and `dev/check_literals.R` after any
change there.

---

## 7. Suggested next steps

1. Get the per-country invasive fish file. The landing page's central argument
   depends on it and everything else there is ready.
2. Watch the first live submission land in `fwise-data/inbox/` and run the
   `dev/qa.R stage` / `fold` loop against it once, before the webinars. The loop
   is built and tested end to end locally, but it has not yet been exercised
   against the real repository with the real token.
3. Replace the placeholder copy in `R/copy.R`, working down section 3.
4. Confirm the Weird Fishes Advisory URL behind the footer logo
   (`footer$wfa_url`). The FWISE logo already links to freshwaterlife.org.
5. Build the Explore page, which unblocks the "see their attempts" route from
   Contacts. `fw_contacts_summary()` already returns an `attempt_ids` list column
   per contact.
6. Deploy to Connect Cloud early, before the October webinars, so the `sf` system
   dependencies and the custom domain are not a launch-day surprise.
