# smoke_test.R
# A basic check that the app launches and that the contribute form gates,
# validates and writes correctly. Run it from the repository root:
#
#     Rscript dev/smoke_test.R
#
# It writes a real submission through the real write path - into a LOCAL inbox
# in a temporary directory, never the live sheet, whatever .Renviron holds -
# and asserts against the row it wrote.
#
# NOTE: this file tests the ONE SCROLLING PAGE form. It previously tested a
# stepped wizard - next_step, step_i(), live_steps() - none of which has existed
# since the form was rebuilt as a single page. Those assertions failed on every
# run, but partway through, so the output still looked like progress. If you add
# a test here, make sure a failure is loud.

library(shiny)
source("R/brand.R"); source("R/config.R"); source("R/copy.R"); source("R/copy_contribute.R")
source("R/copy_export.R"); source("R/copy_forms.R"); source("R/data_load.R")
for (f in c("ui_helpers.R","theme.R","submit.R","forms.R","forms_store.R","mod_explore.R",
            "mod_contribute_steps.R","mod_contribute.R","tracking.R")) source(file.path("R", f))
d = fw_load_data(); ch = fw_startup_choices(d)

# NO SHEET. The inbox is pointed at a temporary directory for the whole run, so
# a developer whose .Renviron holds the real key cannot send a test row to the
# reviewer's sheet by running this.
FWISE_INBOX_SHEET_ID = NULL
FW_DEV_DIR = file.path(tempdir(), "fw-smoke"); unlink(FW_DEV_DIR, recursive = TRUE)

failures = 0L
ok = function(lbl, got, want) {
  pass = identical(got, want)
  if (!pass) failures <<- failures + 1L
  cat(sprintf("  %-52s %-6s %s\n", lbl, format(got), if (pass) "PASS" else "*** FAIL ***"))
}

testServer(mod_contribute_server, args = list(data = d, choices = ch), {
  cat("\n-- gating on an empty form --\n")
  ok("all_valid with nothing filled", all_valid(), FALSE)
  # CONSENT GATES SEND, NOT START (24 Sept 2026). Both boxes sit in the
  # form's first section, with the contact details (29 Sept 2026).
  ok("start opens the form without consent", { session$setInputs(start=1); stage() }, "form")
  # CONTACT AND CONSENT FIRST (client, 29 Sept 2026).
  ok("the contributor section leads the form", FW_STEPS[[1]]$id, "contributor")
  contributor_html = as.character(fw_step_contributor_ui(NS("c"), ch))
  ok("both consent boxes sit with the contact details",
     all(vapply(c("c-primary_contact_email", "c-consent_data_use", "c-contact_public"),
                function(id) grepl(id, contributor_html, fixed = TRUE), logical(1))), TRUE)
  ok("and the email comes before the consent",
     regexpr("c-primary_contact_email", contributor_html) < regexpr("c-consent_data_use", contributor_html), TRUE)
  ok("the review section carries no consent",
     grepl("consent_data_use", as.character(fw_step_review_ui(NS("c"), ch)), fixed = TRUE), FALSE)
  ok("the data-use box is a sending error while unticked",
     "consent_data_use" %in% vapply(fw_check()$errors, `[[`, "", "id"), TRUE)

  session$setInputs(consent_data_use = TRUE)
  ok("all_valid still false (fields empty)", all_valid(), FALSE)
  ok("and consent is no longer among the errors once ticked",
     "consent_data_use" %in% vapply(fw_check()$errors, `[[`, "", "id"), FALSE)

  cat("\n-- country must not silently default --\n")
  ok("country starts empty", is.null(input$country) || input$country == "", TRUE)

  cat("\n-- required fields, filled one group at a time --\n")
  session$setInputs(site_name="Test Tarn", country="Norway",
                    latitude=59.8833, longitude=10.5333)
  ok("site alone is not enough", all_valid(), FALSE)

  session$setInputs(waterbody_type="Pond", water_regime="Lentic", area_treated=2)
  ok("area entered without a unit blocks", all_valid(), FALSE)
  session$setInputs(area_unit="ha")

  # The targets block: one taxa + species pair per target row.
  session$setInputs(target_taxa_1="Fish",
                    target_species_1="Common carp (Cyprinus carpio)")

  cat("\n-- year validation --\n")
  session$setInputs(start_year=2009, end_year=2005, driver="Fisheries")
  ok("end year before start year blocks", all_valid(), FALSE)
  session$setInputs(end_year=2011)

  cat("\n-- contact validation --\n")
  session$setInputs(method_1="Rotenone", outcome="Successful",
                    primary_contact_name="A Tester", primary_contact_email="nope",
                    contact_public = TRUE)
  ok("malformed email blocks", all_valid(), FALSE)
  # Guards the POSIX character-class fix: an address containing the letter "s"
  # was rejected when the pattern used [^@\\s].
  session$setInputs(primary_contact_email="tester@essex.org")
  ok("address containing 's' is accepted", all_valid(), TRUE)
  # The data-use box is the one consent that gates Send; the display one never does.
  session$setInputs(consent_data_use = FALSE)
  ok("a complete form without data-use consent blocks", all_valid(), FALSE)
  session$setInputs(consent_data_use = TRUE, contact_public = FALSE)
  ok("a complete form without display consent sends", all_valid(), TRUE)
  session$setInputs(contact_public = TRUE)

  cat("\n-- conditional chemical section --\n")
  ok("chemical section live for Rotenone", chemical_selected(), TRUE)
  session$setInputs(method_notes_1 = "CFT Legumine", target_ingredient_basis = "Product")

  cat("\n-- the write path --\n")
  session$setInputs(send = 1)
  r = result()
  ok("submission succeeded", r$success, TRUE)

  ok("the inbox is local for this run", fw_inbox_mode(), "local")
  inbox = fw_inbox_read(FW_INBOX_TABS$submissions)
  row = inbox[inbox$attempt_id %in% r$attempt_id, , drop = FALSE]
  ok("the submission is in the inbox under the id it was given", nrow(row), 1L)
  ok("the row has exactly the inbox's columns",
     identical(setdiff(names(inbox), "sheet_row"), FW_INBOX_COLUMNS), TRUE)
  ok("it waits for QA", c(row$qa_status, row$imported, row$status), c("not done", "no", "pending"))
  ok("a known species is written as its label",
     row$invasive_species, "Common carp (Cyprinus carpio)")
  ok("the contributor is split into columns",
     c(row$contact_name, row$contact_email, row$contact_public),
     c("A Tester", "tester@essex.org", "yes"))
  ok("contact_id is left for the reviewer", is.na(row$contact_id), TRUE)
  ok("no derived column is written",
     any(c("iso3", "continent", "last_updated", "secondary_contact_id") %in% names(inbox)), FALSE)
  ok("the method note is paired with its method",
     identical(row$method_notes, "Rotenone: CFT Legumine"), TRUE)
  ok("ingredient basis is recorded", identical(row$target_ingredient_basis, "Product"), TRUE)
  ok("a held species adds no new_species row",
     nrow(fw_inbox_read(FW_INBOX_TABS$new_species)), 0L)
  ok("the confirmation counts include the waiting row", r$total_attempts, nrow(d$attempt) + 1L)
  cat("  counts: total", r$total_attempts, "| Norway", r$country_attempts, "\n")
  ok("stage is done", stage(), "done")
})

cat("\n-- the email permission is opt-in --\n")
# Built straight from fw_collect_submission(), the function the write path
# uses, so the default is tested without sending a second record.
contact_of = function(extra) {
  inp = c(list(primary_contact_name = "B Tester", primary_contact_email = "b@x.org",
                primary_contact_org = "Org"), extra)
  fw_collect_submission(inp, rows = list(), choices = ch)$contact_public
}
ok("unticked -> the address is private", contact_of(list()), "no")
ok("ticked -> the address is public", contact_of(list(contact_public = TRUE)), "yes")

cat("\n-- a typed species is written as typed and listed for the reviewer --\n")
typed = fw_collect_submission(
  list(target_taxa_1 = "Fish", target_species_1 = "Arctic charr (Salvelinus alpinus)",
       benefit_taxa_1 = "Frog", benefit_species_1 = "Common carp (Cyprinus carpio)"),
  rows = list(target = 1, benefit = 1), choices = ch)
ok("the label is the cell", typed$invasive_species, "Arctic charr (Salvelinus alpinus)")
ok("only the species we do not hold is listed", typed$new_species$label, "Arctic charr (Salvelinus alpinus)")
ok("with the taxa the contributor chose", typed$new_species$taxa, "Fish")
nsr = fw_new_species_rows(typed$new_species, "FW-X")[[1]]
ok("its new_species row splits the names",
   unname(nsr[c("common_name", "scientific_name", "taxa", "from_attempt")]),
   c("Arctic charr", "Salvelinus alpinus", "Fish", "FW-X"))

cat("\n-- a new species' names, as the QA import builds them --\n")
source("R/qa_checks.R")
nm_of = function(label, common, scientific) {
  n = fw_new_species_names(label, common, scientific)
  fw_species_label(tibble::tibble(common_name = n$common, scientific_name = n$scientific))$label
}
ok("both names typed rebuild the label",
   nm_of("Arctic charr (Salvelinus alpinus)", "Arctic charr", "Salvelinus alpinus"),
   "Arctic charr (Salvelinus alpinus)")
ok("names left blank come from the label",
   nm_of("Arctic charr (Salvelinus alpinus)", NA_character_, NA_character_),
   "Arctic charr (Salvelinus alpinus)")
ok("a bare scientific label rebuilds itself",
   nm_of("Oreochromis grahami", NA_character_, "Oreochromis grahami"), "Oreochromis grahami")
ok("a bare label with no names stays a common name",
   fw_new_species_names("Amphipoda", NA_character_, NA_character_)$common, "Amphipoda")

cat("\n-- check my answers: hard errors block, soft warnings never do --\n")
testServer(mod_contribute_server, args = list(data = d, choices = ch), {
  strip = function(x) gsub("\\s+", " ", gsub("<[^>]*>", " ", as.character(x$html %||% x)))
  session$setInputs(consent_data_use = TRUE, start = 1)

  session$setInputs(check_answers = 1)
  ok("empty form reports errors",
     grepl("need your attention", strip(output$review_summary)), TRUE)

  # Every REQUIRED field, and deliberately no optional ones.
  session$setInputs(
    site_name = "Test Tarn", country = "Norway", latitude = 59.88, longitude = 10.53,
    water_regime = "Lentic", waterbody_type = "Pond",
    target_taxa_1 = "Fish", target_species_1 = "Common carp (Cyprinus carpio)",
    start_year = 2009, driver = "Fisheries", method_1 = "Rotenone",
    outcome = "Successful", primary_contact_name = "A Tester",
    primary_contact_email = "tester@example.org")
  session$setInputs(check_answers = 2)
  h = strip(output$review_summary)
  ok("required-only form reports clear", grepl("This all looks good", h), TRUE)
  ok("soft warnings are still shown",    grepl("Worth adding if you have it", h), TRUE)
  # THE POINT OF THE SPLIT. A record with no end year, no size, no beneficiary
  # and no reference is a real record - most ongoing attempts look like this.
  # Blocking it would bias the database towards the tidy ones.
  ok("soft warnings do not block sending", all_valid(), TRUE)

  session$setInputs(end_year = 2005, check_answers = 3)
  ok("end before start is a hard error",
     grepl("cannot have ended before it began", strip(output$review_summary)), TRUE)
  ok("and blocks sending", all_valid(), FALSE)

  session$setInputs(end_year = 2011, check_answers = 4)
  ok("clears once corrected",
     grepl("This all looks good", strip(output$review_summary)), TRUE)

  session$setInputs(start_year = 1650, check_answers = 5)
  ok("implausible year is a note, not an error",
     grepl("unusually early", strip(output$review_summary)), TRUE)
  ok("and does not block sending", all_valid(), TRUE)
})

cat("\n-- non-chemical path --\n")
testServer(mod_contribute_server, args = list(data = d, choices = ch), {
  session$setInputs(consent_data_use=TRUE, start=1, method_1="Netting / Trapping")
  ok("chemical section stays hidden", chemical_selected(), FALSE)
})

unlink(FW_DEV_DIR, recursive = TRUE)

# THE LINK PREVIEW (client, 14 Oct 2026): the short name under the title, the
# explicit lockup as its picture, and the longer sentence kept for the
# structured data, which Google Dataset Search wants at 50 characters or more.
cat("\n-- the link preview --\n")
head_html = as.character(fw_head_meta())
ok("description is the database's name",
   grepl('<meta name="description" content="Freshwater Invasive Species Eradication Database"/>',
         head_html, fixed = TRUE), TRUE)
ok("og:description matches it",
   grepl('<meta property="og:description" content="Freshwater Invasive Species Eradication Database"/>',
         head_html, fixed = TRUE), TRUE)
ok("og:image is the explicit lockup",
   grepl(basename(FW_LOGO$explicit_file), head_html, fixed = TRUE) &&
     file.exists(FW_LOGO$explicit_file), TRUE)
ld = jsonlite::fromJSON(sub("(?s).*<script type=\"application/ld\\+json\">(.*?)</script>.*", "\\1",
                             head_html, perl = TRUE), simplifyDataFrame = FALSE)
ok("the Dataset description is 50+ characters",
   nchar(ld[["@graph"]][[2]]$description) >= 50, TRUE)

cat("\n")
if (failures > 0L) {
  # A non-zero exit is what makes this usable from CI or a pre-commit hook.
  # Without it a broken form reads as a successful run.
  stop(failures, " smoke test assertion(s) failed", call. = FALSE)
}
cat("All smoke tests passed.\n")
