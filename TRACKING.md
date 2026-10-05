# Usage tracking

FWISE records how it is used so that Freshwater Life can report usage to funders. There are two layers:

1. **GoatCounter** counts arrivals: visits, referrers, campaign source, country and device. It does nothing else. There are no click or event calls.
2. **The event log** records actions inside the app, one row per event, in the `events` tab of a private Google Sheet. All funder headline figures come from this log.

The code is in `R/tracking.R`. The settings are in `R/config.R`.

## The source parameter: `ref`

Tag inbound links with `?ref=<source>`, for example:

- `https://fwise.org/?ref=webinar`
- `https://fwise.org/?ref=issg`
- `https://fwise.org/?ref=internal`

GoatCounter reads `ref` as the visit's source, and the event log stores it on `session_start`. Before it is stored, the value is lowercased. It is kept only if it uses letters, digits and hyphens and is 30 characters or fewer; anything else is recorded as `other`. A visit with no `ref` is recorded as `direct`. Keep a list of the values in use here:

| ref | Used for |
|---|---|
| webinar | Links shared in webinars |
| issg | Links from ISSG |
| internal | Links from Freshwater Life's own channels |

## Events

Each row has five columns:

| Column | Contents |
|---|---|
| timestamp | UTC, ISO 8601, e.g. `2026-10-07T14:02:11Z` |
| session_id | Shiny's random per-session token. It is not stored anywhere else and not linked to any other visit. |
| event | One of the six names below |
| detail | A JSON object |
| app_version | `FW_APP_VERSION` in `R/config.R` |

| Event | When | Detail |
|---|---|---|
| session_start | A visit opens the app | `{"source": "webinar"}`, or `"direct"` / `"other"` |
| report_built | The report builder makes a report (Build, or Clear, which builds the default report) | Each filter as an array of the values chosen (`[]` if none), plus `year_from`, `year_to`, `include_no_year`, `size_ha`, `size_km` (log10 slider ends), `include_no_size` and `n_records` |
| download | A download file has been written | Report builder: `{"type": "zip"/"pdf"/"html"/"xlsx", "parts": [...], "n_records": n}`. Contribute questions: `{"type": "docx"}` or `{"type": "txt"}` |
| contact_click | A contributor's revealed address is used: its mailto link or Copy button is pressed (Networking, report builder contacts, map record panel). Revealing the address is not logged. Each press is one row. | `{"contact_id": "CO-..."}` |
| form_submit | A form has saved successfully | `{"form": "submission"}`, `"newsletter"` or `"feedback"` |
| session_end | The visit ends | `{}` |

Multi-select filters are always JSON arrays, even with one value. The FWISE address (footer, Networking, privacy page) is not logged at all. A spam submission caught by the honeypot is not logged as `form_submit`.

## Privacy rules

- No IP addresses, user agents, referrer URLs, cookies, local storage or persistent identifiers.
- No personal data: no names, email addresses, organisation names or form field contents. A contact is identified only by its `contact_id`, and the server checks that id against the contacts table.
- No free text. Filter values come from the pickers only and are checked against the choices the page offered; anything unexpected is dropped.
- `ref` is cleaned as described above.

## How it is written

Events are held in memory for each session and written in one append request when the session ends. The write reuses the forms' Google service account sign-in (`fw_google_token()` in `R/forms_store.R`). It has a 5 second timeout and no retry. If anything fails, the session's rows are dropped, and the server log records one line giving the reason but no row contents. Visitors never see an error.

The write blocks the R process while it runs, so other sessions on the same process wait for up to the timeout, plus up to 5 seconds more if the hourly token needs renewing. Rows still in memory are lost if the process is stopped before its sessions end, for example during a redeploy.

A session is capped at 500 events.

## Settings

| Variable | Value |
|---|---|
| `FWISE_LOG_MODE` | `sheet` in production, `console` to print rows locally, `off`. **Unset means off.** |
| `FWISE_LOG_SHEET_ID` | The ID of the sheet holding the `events` tab |
| `GS4_SA_KEY_B64` | Already set for the forms. The same key is used. |

If `sheet` mode is missing the sheet ID or the key, logging is off. The startup log says which mode is running (`FWISE startup: event log ...`).

The GoatCounter site code is `FW_GOATCOUNTER_CODE` in `R/config.R`. While it holds the placeholder `GOATCOUNTER_CODE`, no script is added. GoatCounter does not count visits from localhost.

To test locally, run `FWISE_LOG_MODE=console Rscript -e 'shiny::runApp(port = 8790)'`, use the app, then close the tab. Each row is printed as `FWISE event: ...`.

## Metric definitions

Draft definitions, to be confirmed by Freshwater Life. Every figure is counted over a reporting period, by UTC timestamp.

| Metric | Definition |
|---|---|
| Visits | GoatCounter visits |
| Sessions | Distinct `session_id` with a `session_start` row |
| Sessions by source | Sessions grouped by `detail.source` |
| Reports built | Count of `report_built` rows |
| Sessions that built a report | Distinct `session_id` with at least one `report_built` |
| Downloads | Count of `download` rows, split by `detail.type` |
| Records downloaded | Sum of `detail.n_records` over report builder downloads |
| Contact clicks | Count of `contact_click` rows (mailto or Copy presses). Contacts reached is the distinct `detail.contact_id` per session, summed. |
| Form submissions | Count of `form_submit` rows, split by `detail.form` |
| Most-used filters | For each filter in `report_built`, the share of reports where its array is not empty |

When reporting, do not publish figures small enough to point to one person or organisation, as the privacy notice promises.
