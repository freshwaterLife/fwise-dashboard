# smoke_test.R
# A basic check that the app launches and that the contribute form gates,
# validates and writes correctly. Run it from the repository root:
#
#     Rscript dev/smoke_test.R
#
# It writes a real row to dev/submissions_local.csv, so delete that file
# afterwards if you want the confirmation counts to start from the database
# figure again.
#
# NOTE: this file tests the ONE SCROLLING PAGE form. It previously tested a
# stepped wizard - next_step, step_i(), live_steps() - none of which has existed
# since the form was rebuilt as a single page. Those assertions failed on every
# run, but partway through, so the output still looked like progress. If you add
# a test here, make sure a failure is loud.

library(shiny)
source("R/config.R"); source("R/copy.R"); source("R/data_load.R")
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
  ok("consent not given -> start blocked", { session$setInputs(start=1); stage() }, "intro")

  session$setInputs(consent_data_use = TRUE, email_private = FALSE, start = 2)
  ok("consent given -> form starts", stage(), "form")
  ok("all_valid still false (fields empty)", all_valid(), FALSE)

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
                    primary_contact_name="A Tester", primary_contact_email="nope")
  ok("malformed email blocks", all_valid(), FALSE)
  # Guards the POSIX character-class fix: an address containing the letter "s"
  # was rejected when the pattern used [^@\\s].
  session$setInputs(primary_contact_email="tester@essex.org")
  ok("address containing 's' is accepted", all_valid(), TRUE)

  cat("\n-- conditional chemical section --\n")
  ok("chemical section live for Rotenone", chemical_selected(), TRUE)

  cat("\n-- the write path --\n")
  session$setInputs(send = 1)
  r <- result()
  ok("submission succeeded", r$success, TRUE)
  ok("email_public recorded as yes",
     grepl("yes", readLines("dev/submissions_local.csv")[2]), TRUE)
  cat("  counts: total", r$total_attempts, "| Norway", r$country_attempts, "\n")
  ok("stage is done", stage(), "done")
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
