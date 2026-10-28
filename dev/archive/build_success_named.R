# build_success_named.R
# ARCHIVED. Made the web copies of the client's lettered success plates (the
# story pop-up images): trimmed the transparent A4 margin and downsized to
# ~1700px. Its input, resources/success-pic/, is no longer in the repo; get the
# originals from the client to rerun.
#
#     Rscript dev/archive/build_success_named.R
#
# Reads  resources/success-pic/*.png
# Writes www/img/home/success/*-named.png

TARGET_PX = 1600   # long side; covers 2x density plus a 150% zoom
PAD_PX    = 12     # matches the thumbnails' alpha-bbox padding
ALPHA_MIN = 0.01   # anything fainter is margin, not drawing

src_dir = "resources/success-pic"
out_dir = "www/img/home/success"

if (!dir.exists(src_dir)) stop("run me from fwise-dashboard/, not dev/")
stopifnot(nzchar(Sys.which("sips")))

# Beneficiary plates only.
srcs = setdiff(list.files(src_dir, pattern = "[.]png$", full.names = TRUE),
                list.files(src_dir, pattern = "_greyscale[.]png$", full.names = TRUE))

for (src in srcs) {
  # africa_fiery_redfin.png -> africa-fiery-redfin-named.png (as FW_HOME_IMG).
  stem = gsub("_", "-", sub("[.]png$", "", basename(src)))
  out  = file.path(out_dir, paste0(stem, "-named.png"))

  a = png::readPNG(src)
  if (dim(a)[3] != 4) stop("no alpha channel, cannot trim: ", src)

  al   = a[, , 4]
  rows = which(apply(al, 1, function(r) any(r > ALPHA_MIN)))
  cols = which(apply(al, 2, function(c) any(c > ALPHA_MIN)))
  if (!length(rows) || !length(cols)) stop("fully transparent: ", src)

  r0 = max(1, min(rows) - PAD_PX); r1 = min(nrow(al), max(rows) + PAD_PX)
  c0 = max(1, min(cols) - PAD_PX); c1 = min(ncol(al), max(cols) + PAD_PX)
  png::writePNG(a[r0:r1, c0:c1, , drop = FALSE], out)

  # Resize after the trim.
  system2("sips", c("-Z", TARGET_PX, shQuote(out), "--out", shQuote(out)),
          stdout = NULL, stderr = NULL)

  b = png::readPNG(out)
  cat(sprintf("%-34s %4.1fMB %dx%d -> %5.0fKB %dx%d\n",
              basename(out), file.info(src)$size / 1024^2,
              ncol(al), nrow(al), file.info(out)$size / 1024,
              dim(b)[2], dim(b)[1]))
}
