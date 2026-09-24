# smoke_test.R
# A basic check that the app launches and that the contribute form gates,
# validates and writes correctly. Run it from the repository root:
#
#     Rscript dev/smoke_test.R
#
# It writes a real submission through the real write path and then deletes the
# file it made, so the sibling data checkout is left as it was found. It asserts
# against THAT file rather than against whatever is already in the inbox - an
# earlier version read line 2 of the shared inbox, which passed on a stale row
# from a previous run and would have gone on passing if the write had broken.
#
# NOTE: this file tests the ONE SCROLLING PAGE form. It previously tested a
# stepped wizard - next_step, step_i(), live_steps() - none of which has existed
# since the form was rebuilt as a single page. Those assertions failed on every
# run, but partway through, so the output still looked like progress. If you add
# a test here, make sure a failure is loud.

library(shiny)
source("R/brand.R"); source("R/config.R"); source("R/copy.R"); source("R/copy_contribute.R")
source("R/copy_export.R"); source("R/data_load.R")
for (f in c("ui_helpers.R","theme.R","submit.R","mod_explore.R",
            "mod_contribute_steps.R","mod_contribute.R")) source(file.path("R", f))
d <- fw_load_data(); ch <- fw_startup_choices(d)

failures <- 0L
ok <- function(lbl, got, want) {
  pass <- identical(got, want)
  if (!pass) failures <<- failures + 1L
  cat(sprintf("  %-52s %-6s %s\n", lbl, format(got), if (pass) "PASS" else "*** FAIL ***"))
}

testServer(mod_contribute_server, args = list(data = d, choices = ch), {
  cat("\n-- gating on an empty form --\n")
  ok("all_valid with nothing filled", all_valid(), FALSE)
  # CONSENT GATES SEND, NOT START (24 Sept 2026). Both boxes sit at the foot
  # of the form now - see fw_step_review_ui().
  ok("start opens the form without consent", { session$setInputs(start=1); stage() }, "form")
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
                    email_public = TRUE)
  ok("malformed email blocks", all_valid(), FALSE)
  # Guards the POSIX character-class fix: an address containing the letter "s"
  # was rejected when the pattern used [^@\\s].
  session$setInputs(primary_contact_email="tester@essex.org")
  ok("address containing 's' is accepted", all_valid(), TRUE)
  # The data-use box is the one consent that gates Send; the display one never does.
  session$setInputs(consent_data_use = FALSE)
  ok("a complete form without data-use consent blocks", all_valid(), FALSE)
  session$setInputs(consent_data_use = TRUE, email_public = FALSE)
  ok("a complete form without display consent sends", all_valid(), TRUE)
  session$setInputs(email_public = TRUE)

  cat("\n-- conditional chemical section --\n")
  ok("chemical section live for Rotenone", chemical_selected(), TRUE)
  session$setInputs(method_notes_1 = "CFT Legumine", target_ingredient_basis = "Product")

  cat("\n-- the write path --\n")
  session$setInputs(send = 1)
  r <- result()
  ok("submission succeeded", r$success, TRUE)

  written <- file.path(fw_inbox_dir(), paste0(r$attempt_id, ".csv"))
  ok("the submission is on disk where it says it is", file.exists(written), TRUE)
  row <- utils::read.csv(written, colClasses = "character", check.names = FALSE)
  ok("the row has exactly the database's columns",
     identical(names(row), FW_ATTEMPT_COLUMNS), TRUE)
  ok("a known species is written as its id",
     identical(row$invasive_species, unname(ch$species_ids[["Common carp (Cyprinus carpio)"]])), TRUE)
  ok("the contributor travels as a new: reference marked public",
     grepl("^new:A Tester\\|", row$primary_contact_id) && grepl("public$", row$primary_contact_id), TRUE)
  # write_csv(na = "") puts NA on disk as an empty cell, which the loader
  # reads back as NA.
  ok("the secondary contact is written as NA (a blank cell)",
     is.na(row$secondary_contact_id) || !nzchar(row$secondary_contact_id), TRUE)
  ok("the method note is paired with its method",
     identical(row$method_notes, "Rotenone: CFT Legumine"), TRUE)
  ok("ingredient basis is recorded", identical(row$target_ingredient_basis, "Product"), TRUE)
  # Leave no litter in fwise-data. The row has been checked; keeping it would
  # inflate the in-review count of whoever runs the app next.
  unlink(written)
  cat("  counts: total", r$total_attempts, "| Norway", r$country_attempts, "\n")
  ok("stage is done", stage(), "done")
})

cat("\n-- the email permission is opt-in --\n")
# Built straight from fw_collect_submission(), the function the write path
# uses, so the default is tested without sending a second record.
contact_of <- function(extra) {
  inp <- c(list(primary_contact_name = "B Tester", primary_contact_email = "b@x.org",
                primary_contact_org = "Org"), extra)
  fw_collect_submission(inp, rows = list(), choices = ch)$primary_contact_id
}
ok("unticked -> the address is private", grepl("\\|private$", contact_of(list())), TRUE)
ok("ticked -> the address is public",
   grepl("\\|public$", contact_of(list(email_public = TRUE))), TRUE)

cat("\n-- check my answers: hard errors block, soft warnings never do --\n")
testServer(mod_contribute_server, args = list(data = d, choices = ch), {
  strip <- function(x) gsub("\\s+", " ", gsub("<[^>]*>", " ", as.character(x$html %||% x)))
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
  h <- strip(output$review_summary)
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

cat("\n")
if (failures > 0L) {
  # A non-zero exit is what makes this usable from CI or a pre-commit hook.
  # Without it a broken form reads as a successful run.
  stop(failures, " smoke test assertion(s) failed", call. = FALSE)
}
cat("All smoke tests passed.\n")
