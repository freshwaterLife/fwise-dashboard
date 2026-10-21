# build_report_basemap.R
# ARCHIVED. Built resources/report/ne_110m_basemap.rds, the PDF map's world
# (countries, ocean and lakes as separate layers), so the PDF needs no tiles.
#
#     Rscript dev/archive/build_report_basemap.R
#
# Source: Natural Earth 1:110m, public domain. Only geometry is kept, plus
# iso3 on countries (to drop Antarctica). Read by fw_gg_basemap().

suppressPackageStartupMessages(library(sf))

base = paste0("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/",
               "master/geojson/ne_110m_")

# s2 off: spherical repair draws lines across the map at the antimeridian.
sf::sf_use_s2(FALSE)

#' Fetch one Natural Earth layer, keeping only the columns named
fetch = function(name, keep = character(0)) {
  src = tempfile(fileext = ".geojson")
  utils::download.file(paste0(base, name, ".geojson"), src,
                       mode = "wb", quiet = TRUE)
  x = sf::st_read(src, quiet = TRUE)
  x = x[, keep]
  sf::st_make_valid(x)
}

world = fetch("admin_0_countries", "ISO_A3")
names(world)[1] = "iso3"

# No attributes worth keeping on either water layer - they are drawn as one
# flat fill. st_geometry() keeps the sfc alone, which is smaller than an sf
# with a column of nothing in it.
ocean = sf::st_geometry(fetch("ocean"))
lakes = sf::st_geometry(fetch("lakes"))

out = file.path("resources", "report", "ne_110m_basemap.rds")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(world = world, ocean = ocean, lakes = lakes), out, compress = "xz")

cat("Wrote", out, "-", nrow(world), "countries,",
    length(ocean), "ocean,", length(lakes), "lakes,",
    format(file.size(out), big.mark = ","), "bytes\n")
cat("The old ne_110m_countries.rds is superseded and can be deleted.\n")
