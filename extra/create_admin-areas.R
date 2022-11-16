library(data.table)
library(sf)

# Non-overwriting download operation
download_file <- function(url, destfile, ...) {
  if (!file.exists(destfile)) {
    download.file(url, destfile, ...)
  } else {
    warning('File already exists. Download operation skipped.', call.=FALSE)
  }
}

# get the stem of a file
# (basename of a file without the ext): e.g. data/foo.csv -> foo
file_stem <- function(path) {
  tools::file_path_sans_ext(basename(path))
}


###--------------------------------------------------------------------------###
# This script:
# 1) downloads shapes of Germany at all admin levels, and grid
# 2) cleans it and produce shape files for states, districts and municipalities
# 3) creates administrative levels: district & municipality names and codes
### ------------------------------------------------------------------------ ###

# Downloading ------------------------------------------------------------------

## Territorial Codes References, Administrative areas -------
## Federal Agency for Cartography and Geodesy (gdz.BKG-bund.de)
### website: https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete.html?___store=default
### Administrative areas 1:250 000 (levels), as of December 31st (VG250 31.12.)
### It is based on the territorial definition of 2019 (end of the year).

furl = 'https://daten.gdz.bkg.bund.de/produkte/vg/vg250_kompakt_1231/2019/vg250_12-31.utm32s.shape.kompakt.zip'
dname = 'extra/admin-areas/'
dir.create(dname, showWarnings = FALSE)
zfpath = sprintf('%s/%s', dname, basename(furl))

try(download_file(furl, zfpath))

# unzipping
suppressWarnings(unzip(zfpath, exdir = dname, overwrite = FALSE))

fpath = sprintf('%s/%s/dokumentation/struktur_und_attribute_vg250.xls',dname, file_stem(zfpath))
dname = sprintf('%s/%s', dname, file_stem(zfpath)) # the extracted folder

## States, Bundesländer ---------------------
# source:https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Glossar/bundeslaender.html
stats = read.csv("extra/admin-areas/states.csv")


## Geographic grids for Germany in Lambert projection (GeoGitter Inspire) ----

grid_furl = "https://daten.gdz.bkg.bund.de/produkte/sonstige/geogitter/aktuell/DE_Grid_ETRS89-LAEA_1km.gpkg.zip"
grid_dname = 'extra/admin-areas/germany-grid'
grid_zfpath = sprintf('%s/%s', grid_dname, basename(grid_furl))
dir.create(grid_dname, showWarnings = FALSE)
try(download_file(grid_furl, grid_zfpath))
suppressWarnings(unzip(grid_zfpath, exdir = grid_dname, overwrite = FALSE))

de_grid = st_read(dir(grid_dname, '.*_1km.gpkg$', recursive = TRUE, full.names = TRUE))[, c('id', 'geom')]
## EW_NS (note the rearrange \2_\1)
de_grid$grid_id = sub('1km[NS](\\d{4})[EW](\\d{4})', "\\2_\\1", de_grid$id)
st_geometry(de_grid) = 'geometry'
de_grid = de_grid[, c('grid_id', 'geometry')]


# Cleaning ---------------------------------------------------------------

## administrative units ----
admin_areas = readxl::read_excel(fpath, sheet='VG250')
names(admin_areas) = tolower(names(admin_areas))
ibz = readxl::read_excel(fpath, sheet="VG250_IBZ" ) # attribute table

# make class data.table in place
setDT(admin_areas)
setDT(ibz)
setDT(states)

admin_areas = admin_areas[, c(
  "ade", "ars", "ags", "gen", "ibz", "sn_l", "sn_r", "sn_k", "sn_v1", "sn_v2",
  "sn_g", "ars_0", "ags_0", "debkg_id"
), with = FALSE]

setnames(
  admin_areas,
  c("sn_l", "sn_r", "sn_k", "sn_v1", "sn_v2", "sn_g"),
  c("state", "admin_district", "district", "admin_assoc_frontpart","admin_assoc_rearpart", "municipality")
)
setnames(admin_areas, c('ade', 'gen', 'ibz'), c('admin_level', 'name', 'admin_unit'))

ibz = ibz[, .(admin_unit = IBZ, bez = BEZ)]
ibz = unique(ibz, by = c('admin_unit', "bez"))
admin_areas = merge(admin_areas, ibz, "admin_unit")

### districts ----
districts = admin_areas[admin_level == 4L, !'admin_level'] # Kreise

## since we kept only ADE == 4 (i.e. districts), ARS should be the same as AGS
if (with(districts, all(ags == ars))) {
  message('AGS == ARS')
  districts[, `:=`(did = ars, ags = NULL, ars = NULL)]
  setcolorder(districts, 'did')
}

districts = districts[, .(did, name, state, admin_unit=paste0(admin_unit,"-", bez), debkg_id)]


# merge with the states data.frame for state_abb
districts = districts[states[, c('state_code', 'state_abb')], on = 'state==state_code'][, !'state']

setnames(districts, 'state_abb', 'state')
setcolorder(districts, 'state', after = 'name')

### municipalities ----
municipals = admin_areas[admin_level == 6L,
.(ars, ags, geo_name = name, did = paste0(state, admin_district, district),
  admin_unit = paste0(admin_unit, "-", bez), debkg_id
  )
]

# merge with the districts data.frame for district id and name
municipals = municipals[districts[, .(did, district_name = name, state)], on='did']
setcolorder(municipals, c('district_name', 'state'), after = 'did')

## Shapes ---------------------------------------------------------------------
# The compact version comes redundancy-free i.e. the shapes are the lower admin
# levels i.e. municipalities. For districts and states, we need to aggregate
# the shapes to the respective levels.

shape_path = sprintf("%s/VG250_F.shp", dir(dname, 'vg250_kompakt_[0-9]{4}', full.names = TRUE))
municipality_shape = read_sf(shape_path)

## Filter by GF = Geofactor : Survey of values
                          # 1 = Waters without structures
                          # 2 = Waters with structures
                          # 3 = Land without structure
                          # 4 = Land with structure
names(municipality_shape) = tolower(names(municipality_shape))
municipality_shape = subset(municipality_shape, gf == 4L, select = c("ars", 'geometry'))
municipality_shape = merge(municipality_shape, municipals, 'ars')

## aggregate to the district and state levels
district_shape = aggregate(
  municipality_shape[, "geometry"],
  list(did = municipality_shape$did), length
) |>
  merge(districts[, .(did, name, state)])

state_shape = aggregate(district_shape[, "geometry"],
  list(state = district_shape$state), length
)


### add municipality info to the grid--via spatial join ------------------------
de_grid = st_transform(de_grid, st_crs(municipality_shape))
de_grid = st_join(de_grid, municipality_shape, join=st_contains,left=FALSE, largest=TRUE)
de_grid = de_grid[, c("grid_id", "ars", "ags", "geo_name", "did", "district_name", "geometry")]


# write to disk -----
## admin area names, codes
fwrite(municipals, 'extra/admin-areas/municipalities_bkg.csv')
fwrite(districts, 'extra/admin-areas/districts_bkg.csv')

## admin area shapes
st_write(municipality_shape, "extra/admin-areas/municipalities_bkg.gpkg", append = FALSE)
st_write(district_shape, "extra/admin-areas/districts_bkg.gpkg", append = FALSE)

## grids with admin info (municipalities)
st_write(de_grid, 'extra/admin-areas/grid-germany_with-admin-areas.gpkg', append = FALSE)
fwrite(st_drop_geometry(de_grid), 'extra/admin-areas/grid-germany_with-admin-areas.csv')
