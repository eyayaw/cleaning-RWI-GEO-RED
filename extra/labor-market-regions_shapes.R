library(sf)

# district shape
districts = st_read("extra/admin-areas/districts_bkg.gpkg")
districts$state = NULL

# labor market regions (LMRs)
lmrs = read.csv(
  "extra/Labor-Market-Regions_Kosfeld-Werner-2012.csv",
  col.names = c("amr_id", "amr_name", "district"), row.names = NULL
)

# split the district code and name into two columns
lmrs = transform(lmrs,
  district_id = sub("^(\\d{4,5}) (.+)$", "\\1", district),
  district_name = sub("^(\\d{4,5}) (.+)$", "\\2", district),
  district = NULL) |>
  transform(district_id = sprintf("%05i", as.integer(district_id)))

# we can construct the shapes of the labor market regions from the spatial aggregate of
# the shapes of the constitute districts: by aggregating the polygons of the the member districts

## however, territorial changes over time prevent us from direct merges
## districts are merged into another district or dissolved, or went
## through administrative border changes (or just district key change),
## so we do some matching between the old and the new manually
nomatch = lmrs[
  with(lmrs, district_id %in% setdiff(district_id, districts$did)),
  c("district_id", "district_name")
]

# compiled from consulting bbsr.bund.de's "Umstiegsschlüssel für konsistente Zeitreihe" (see the `Extra` below), Google/Wikipedia
correction = tibble::tribble(
  ~district_id, ~district_name, ~new_district_id, ~new_district_name,

  "03152", "Göttingen", "03159", "Göttingen",
  # "03156", "Osterode am Harz", "03159", "Göttingen", # drop

  # https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/deutschland/kreisgebietsreformen/kreisreform-meck-pomm.html
  "13101", "Rostock", "13003", "Rostock",
  "13102", "Schwerin", "13004", "Schwerin",
  "13104", "Mittleres Mecklenburg", "13072", "Rostock Landkreis",
  "13103", "Mecklenburgische Seenplatte", "13071", "Mecklenburgische Seenplatte",
  "13106", "Nordwestmecklenburg", "13074", "Nordwestmecklenburg",
  "13107", "Nordvorpommern", "13073", "Vorpommern-Rügen",
  "13108", "Südvorpommern", "13075", "Vorpommern-Greifswald",
  "13105", "Südwestmecklenburg", "13076", "Ludwigslust-Parchim",

  # merged into one district
  "05313", "SK Aachen", "05334", "Städteregion Aachen",
  # "05354", "LK Aachen", "05334", "Städteregion Aachen"

)


new = merge(lmrs, correction, by = c("district_id", "district_name"))
new = new[, c("amr_id", "amr_name", "new_district_id", "new_district_name")] |>
  setNames(c("amr_id", "amr_name", "district_id", "district_name"))

new_lmrs = rbind(lmrs[!(lmrs$district_id %in% nomatch$district_id), ], new)
stopifnot(
  `There're still old districts that need to be taken care of` =
    nrow(districts[!districts$did %in% new_lmrs$district_id, ]) == 0L
)

# labor market regions updated for the 2019 district border definitions
write.csv(new_lmrs, "extra/Labor-Market-Regions_Kosfeld-Werner-2012_2019.csv",
  quote = FALSE, row.names = FALSE
)

## construct the shapes of the labor market regions

lmrs_shape = districts |>
  merge(new_lmrs, by.x = "did", by.y = "district_id") |>
  {\(.x) aggregate(
    x = .x[, c("did", "name", "geometry")],
    by = list(amr_id = .x$amr_id, amr_name = .x$amr_name),
    FUN = \(x) paste0(x, collapse = "|")
    )
  }()

names(lmrs_shape)[match(c("did", "name"), names(lmrs_shape))] = c("district_id", "district_name")

# write the shapes
st_write(lmrs_shape,
  "extra/Labor-Market-Regions_Kosfeld-Werner-2012_2019.gpkg", append = FALSE
)


# ------------------------------------------------------------------------------
#                        Extra                                                 #
# ------------------------------------------------------------------------------
# Below gives more information about the history of district changes in Germany.
# It can be used, for example, to decide which labor market region to use for
# Göttingen 2019 (03159), which was created out of Göttingen (03152) or Osterode am Harz (03156).

## https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/umstiegsschluessel.html
url = "https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/umstiegsschluessel/ref-kreise-umrech-2019-1990-2018.xlsx?__blob=publicationFile&v=3"
fpath = sub("(.+)(\\?.+)", "\\1", paste0('extra/', basename(url)))
if (!file.exists(fpath)) download.file(url, fpath)
ref_year = 2019
sheets = readxl::excel_sheets(fpath) |> trimws()
# there is a typo in one of the sheet names, I manually renamed sheet "2016-2018" to "2016-2019"
# by opening the excel file, or the below lines take care of it
if (!all(substr(sheets, 6, 9) == ref_year)) {
  message("There is/are wrongly named sheet(s).")
  substr(sheets,6,9) = as.character(ref_year)
}
## Since Kostfeld and Werner (2012) is based on the 2007-2011 territorial changes
## Deutsche Arbeitsmarktregionen – Neuabgrenzung nach den Kreisgebietsreformen 2007–2011
# let's keep 2011 and on
sheets = grep("^201[1-8]-", sheets, value=TRUE)
names(sheets) = substr(sheets, 1, 4)
# nms = c('Kreise',"Kreisname", "flächen", "bevölkerungs", "Kreise", "kreisname")
nms = c("id", "name", "area", "pop", "new_id", "new_name")
pat = "(Kreis)|((Fl.che|Bev.lkerung)\\s?am\\s?31\\.12\\.\\d{4})"

ref = lapply(sheets, \(sheet) readxl::read_xlsx(fpath, sheet = sheet)) |>
  lapply(\(d) d[, grep(pat, names(d))]) |>
  lapply(setNames, nm = nms) |>
  data.table::rbindlist(idcol = "year") |>
  as.data.frame() |>
  # rm 3 trailing zeros and make 5 digit long
  transform(
    id = sprintf("%05i", as.integer(id) / 1000L),
    new_id = sprintf("%05i", as.integer(new_id) / 1000L)
  )
