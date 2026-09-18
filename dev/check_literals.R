# check_literals.R
# Guards the "one place for each kind of value" rule. Exits non-zero if:
#
#   1. a hex colour literal appears in R/ outside R/brand.R and R/config.R
#      (config.R holds the data palettes, which are deliberately literal)
#   2. a hex colour literal appears anywhere in www/scss/ outside a comment
#   3. pure white (#fff, #ffffff, or the word white as a colour) appears in
#      either, because the design has no white surface
#   4. any fw_t("a", "b") call in R/ names a key the copy deck does not define
#
#     Rscript dev/check_literals.R
#
# Lives in dev/ because it is a build-time check, not part of the app.

fails <- character(0)
fail <- function(...) fails <<- c(fails, paste0(...))

strip_comments_r    <- function(x) sub("#.*$", "", x)
strip_comments_scss <- function(x) sub("//.*$", "", x)

# ---- 1 and 3: R -------------------------------------------------------------
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  if (basename(f) %in% c("brand.R", "config.R")) next
  lines <- strip_comments_r(readLines(f, warn = FALSE))
  hits <- grep('"#[0-9a-fA-F]{3,8}"', lines)
  for (i in hits) fail(f, ":", i, "  hex literal: ", trimws(lines[i]))
}
for (f in c("R/brand.R", "R/config.R")) {
  lines <- strip_comments_r(readLines(f, warn = FALSE))
  hits <- grep('"#(fff|ffffff)"', lines, ignore.case = TRUE)
  for (i in hits) fail(f, ":", i, "  pure white: ", trimws(lines[i]))
}

# ---- 2 and 3: Sass ----------------------------------------------------------
for (f in list.files("www/scss", pattern = "[.]scss$", full.names = TRUE)) {
  lines <- strip_comments_scss(readLines(f, warn = FALSE))
  hits <- grep("#[0-9a-fA-F]{3,8}\\b", lines)
  for (i in hits) fail(f, ":", i, "  hex literal: ", trimws(lines[i]))
  # `white` as a colour value, not white-space and friends.
  hits <- grep("(^|[^a-z-])white($|[^a-z-])", lines)
  for (i in hits) fail(f, ":", i, "  the word white: ", trimws(lines[i]))
}

# ---- 4: every fw_t() key resolves ------------------------------------------
source("R/copy.R"); source("R/copy_contribute.R"); source("R/copy_export.R")
pattern <- 'fw_t\\(("[^"]+"(, *"[^"]+")*)\\)'
keys <- character(0)
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  lines <- strip_comments_r(readLines(f, warn = FALSE))
  m <- regmatches(lines, gregexpr(pattern, lines))
  keys <- c(keys, unlist(m))
}
keys <- unique(keys)
for (k in keys) {
  path <- gsub('"| ', "", strsplit(sub("^fw_t\\((.*)\\)$", "\\1", k), ",")[[1]])
  ok <- tryCatch({ do.call(fw_t, as.list(path)); TRUE }, error = function(e) FALSE)
  if (!ok) fail("unresolved copy key: ", k)
}
dups <- names(fw_copy_all())[duplicated(names(fw_copy_all()))]
if (length(dups)) fail("section defined in more than one copy file: ", paste(dups, collapse = ", "))

cat(length(keys), "distinct fw_t() keys checked\n")
if (length(fails)) {
  cat(paste0("  ", fails, collapse = "\n"), "\n")
  cat(length(fails), "problem(s).\n")
  quit(status = 1)
}
cat("No stray literals; every copy key resolves.\n")
