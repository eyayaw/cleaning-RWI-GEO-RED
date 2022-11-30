library(data.table)
library(sf)

source("helpers/helpers.R")
# count missing values, which are flagged by any negative number
count_missing <- function(df) {
  lapply(df, \(x) sum(x < 0)) |>
    {\(x) data.frame(var = names(x), count = unname(unlist(x)))}()
}

sales_homes = fread(sprintf("data/homes-sales_%s-%s.csv", Sys.getenv("YEAR_START"), Sys.getenv("YEAR_END")))
sales_aparts = fread(sprintf("data/apartments-sales_%s-%s.csv", Sys.getenv("YEAR_START"), Sys.getenv("YEAR_END")))


# rm duplicates
nrh = nrow(sales_homes)
idx = sales_homes[, .(idx=.I[which.max(spell)]), .(obid, year)][, idx]
sales_homes = sales_homes[idx, ]
message(sprintf("%.2f%% were duplicates.", 100-100*nrow(sales_homes)/nrh))
sales_homes[, spell := NULL]
rm(idx)

nra = nrow(sales_aparts)
idx = sales_aparts[, .(idx=.I[which.max(spell)]), .(obid, year)][, idx]
sales_aparts = sales_aparts[idx, ]
message(sprintf("%.2f%% were duplicates.", 100-100*nrow(sales_aparts)/nra))
sales_aparts[, spell := NULL]
rm(idx)


# how to combine houses and flats?
# assign a special value for vars that do not apply to one or another (flats/homes)
not4Homes = setdiff(names(sales_aparts), names(sales_homes))
not4Aparts = setdiff(names(sales_homes), names(sales_aparts))
special_code = 999L # by construction

sales_homes[, (not4Homes) := special_code]
sales_aparts[, (not4Aparts) := special_code]
sales = rbindlist(list(sales_homes, sales_aparts), use.names=TRUE)

# housekeeping
rm(sales_homes, sales_aparts)

if (!("grid_id" %in% names(sales))) {
  if ("ergg_1km" %in% names(sales)) {
    setnames(sales, "ergg_1km", "grid_id")
  } else if ("grid_id_char" %in% names(sales)) {
    setnames(sales, "grid_id_char", "grid_id")
  }
}

# reorder variables
order_of_vars = c(
  "obid", "uniqueid_gen", "kid2019", "zipcode", "state", "year", "ad_begin_year",
  "ad_begin_mon", "ad_end_mon", "price", "floor_space", "usable_floor_space", "plot_size",
  "num_rooms", "num_bedrooms", "num_bathrooms", "num_floors", "house_type",
  "protected_building", "holiday_house", "heating_type", "basement",
  "guest_washroom", "constr_year", "renov_year", "constr_phase",
  "equipment", "condition", "parking_space", "parking_space_price",
  "grid_id", "lab_mrkt_reg", "kid2015", "number_hits_of_ad"
)
setcolorder(sales, order_of_vars)
setnames(sales, 'kid2019', 'did') # rename kid2019 to `did` (district id)


# cleaning ---------------------------------------------------------------------
(missings=count_missing(sales) |> {\(x) x[order(-x$count), ]}())

## dealing with missing values of many forms -----------------------------------
n = nrow(sales)
sales = sales[grid_id > 0 & price > 0 & floor_space > 0 & num_rooms > 0, ]
message(sprintf("%.2f%% observations dropped", 100-100*nrow(sales)/n))

### house type ----
# the combination of categories follows 'Klick & Schaffner' (2019)
sales[, house_type := fcase(
  house_type == -7 | house_type == -9, 0L,
  house_type == 1 | house_type == 2, 1L,
  house_type == 11 | house_type == 12, 2L,
  house_type == 3L, 3L, # semi-detached
  between(house_type, 4, 6), 4L,
  house_type == 13 | house_type == 15, 5L,
  between(house_type, 7, 10) | house_type == 14, 6L
)]
sales[, house_type := factor(
  house_type, c(0L:6L, special_code),
  c(
    "na",            # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "single-family", #  1 Single-family house (detached) + 2 Single-family house
    "two-family",    # 11 two-family houses + 12 block of flats
    "semi-detached", # itself, 3 semi-detached
    "terraced",      # 4 terraced + 5 terraced (middle unit) + 6 terraced (end unit)
    "other",         # 13 other property for living + 15 other
    "special",       # 7 Bungalow + 8 Farmhouse + 9 Castle + 10 Mansion + 14 Special property
    "apartments"     # 0-6 are for homes, 7 is for flats (WK_SUF)--a special type by construction
  )
)]


### apartment type ----
# category 11 has no label, this needs attention
sales[flat_type == 11 | flat_type == -7 | flat_type == -9, flat_type := 0L]
sales[, flat_type := factor(
  flat_type, c(0L:10L, special_code),
  c(
    "na",                  # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "attic",               #  1 Attic flat
    "ground-floor",        #  2 Ground floor flat
    "flat",                #  3 Flat
    "raised-ground-floor", #  4 Raised ground floor flat
    "loft",                #  5 Loft
    "Maisonette",          #  6 Maisonette
    "penthouse",           #  7 Penthouse
    "souterrain",          #  8 Souterrain
    "flat-with-terrace",   #  9 Flat with terrace
    "others",              # 10
    "houses"               # 0-10 are for flats, `special_code` is for houses (HK_SUF)--a special type by construction
  )
)]


### condition of the object ----
sales[condition < 0, condition := 0L]
sales[, condition := factor(
  condition,
  0L:10L,
  c("na", get_value_labels('objektzustand', include_missing=FALSE)$label)
)]


### number of bedrooms ----
sales[, num_bedrooms := fcase(
  num_bedrooms <= 0, 0L,
  num_bedrooms >= 7, 7L,
  # basically else [ifelse(TRUE)]:
  rep_len(TRUE, length(num_bedrooms)), num_bedrooms
)]

sales[, num_bedrooms := factor(
  num_bedrooms,
  0:7,
  c("na or 0", 1:6, "7+")
)]


### number of bathrooms ----
sales[, num_bathrooms := fcase(
  num_bathrooms <= 0, 0L,
  num_bathrooms >= 4, 4L,
  # basically else [ifelse(TRUE)]:
  rep_len(TRUE, length(num_bathrooms)), num_bathrooms
)]

sales[, num_bathrooms := factor(
  num_bathrooms,
  0:4,
  c("na or 0", 1:3, "4+")
)]


### total number of floors, create 5 categories ----
sales[, num_floors := fcase(
  between(num_floors, -11, 0), 0L,
  between(num_floors, 4, max(num_floors)), 4L,
  # basically else [ifelse(TRUE)]:
  rep_len(TRUE, length(num_floors)), num_floors
)]

sales[, num_floors := factor(num_floors, 0:4, c("na", 1:3, "4+"))]

### facilities of the house, create categories ----
sales[between(equipment, -11, 0), equipment := 0]
sales[, equipment := factor(
  equipment,
  0:4,
  c("na", get_value_labels("ausstattung", include_missing=FALSE)$label)
)]

### year of construction, year of renovation ----
maxYear = max(sales$year)
sales[, c("constr_year_cat", "renov_year_cat") := lapply(.SD, function(x) {
  fcase(
    x <= 0, 0,
    x < 1900, 1,
    between(x, 1900, 1945), 2,
    between(x, 1946, 1959), 3,
    between(x, 1960, 1969), 4,
    between(x, 1970, 1979), 5,
    between(x, 1980, 1989), 6,
    between(x, 1990, 1999), 7,
    between(x, 2000, 2009), 8,
    between(x, 2010, maxYear), 9
  )
}), .SDcols = c("constr_year", "renov_year")]

sales[, c("constr_year_cat", "renov_year_cat") := lapply(.SD,
  factor,
  levels = 0:9,
  labels = c(
    "na", "<1900", "1900-1945", "1946-1959", "1960-1969", "1970-1979",
    "1980-1989", "1990-1999", "2000-2009", "2010+"
  )
), .SDcols = c("constr_year_cat", "renov_year_cat")]


### Type of heating ----
sales[heating_type < 0, heating_type := 0]
# get_value_labels('heizungsart', TRUE) |> subset(value>=0, )
sales[, heating_type := factor(
  heating_type,
  0L:13L,
  c("na", get_value_labels("heizungsart", include_missing=FALSE)$label)
)]


### construction phase ----
### D.N.A. = Does Not Apply (because `constr_phase` is in `not4Aparts`)
sales[constr_phase <= 0, constr_phase := 0L]
sales[, constr_phase := factor(
  constr_phase, c(0L:3L,special_code),
  c("na", get_value_labels("bauphase", include_missing=FALSE)$label, "D.N.A.")
)]

### binary variables -----
# Following ('Klick & Schaffner' 2019, p. 12), for binary variables, we replace
# missing values by 0, i.e., by absence of the feature. Absence is denoted by `Nein` (== no ==0) in binary variables.

binary_vars = c(
  "basement", "protected_building", "guest_washroom", "holiday_house",
  "elevator", "balcony", "kitchen", "public_housing_cert", "betreut", "garden"
)

for (i in seq_along(binary_vars)) {
  # check if absence of the info in a binary variable is denoted by 0
  if (0 %in% unique(sales[[binary_vars[[i]]]])) {
    sales[, (binary_vars[[i]]) := lapply(.SD, function(v) {
      fcase(v == -9 | v == -7, 0L,
            between(v, -11, -1), NA_integer_,
            rep_len(TRUE, length(v)), v)
      # replace '-9'--other missing or '-7'--not specified, by '0'.
    }),
    .SDcols = binary_vars[[i]]
    ]
  } else {
    warning(sprintf(
      "Variable `%s` does not have `0` in its levels/categories.",
      binary_vars[[i]]
    ))
  }
}
sales[, (binary_vars) := lapply(.SD, as.factor), .SDcols = binary_vars]

# house keeping
rm(order_of_vars, binary_vars, i)

# remove protected buildings
# sales = sales[protected_building == 0, ]
# sales[, protected_building := NULL]

# drop districts that do not exist under the BKG (2019.12.31) definition, if any
districts = fread("extra/admin-areas/districts_bkg.csv", select="did")
sales = merge(sales, districts, 'did')
rm(districts)


# compute distance to the CBD -----
de_grid = st_read('extra/admin-areas/germany-grid/de-grid.gpkg')
lmrs = fread(
  "extra/Labor-Market-Regions_Kosfeld-Werner-2012_2019.csv",
  select = c("amr_id", "district_id"),
  col.names = c("amr_id", "did")
)
cbds = st_read('extra/Labor-Market-Regions_Kosfeld-Werner-2012_2019.gpkg')[, c('amr_id', 'geom')] |> st_centroid(cbds)
cbds = st_transform(cbds, st_crs(de_grid))
cbds = merge(cbds, lmrs, 'amr_id')
rm(lmrs)
# geometry now is the centroid of the grid cell
st_geometry(de_grid) = st_centroid(st_geometry(de_grid)) |> st_geometry()
de_grid = merge(de_grid, unique(sales[, .(grid_id, did)]), by="grid_id")

dids = as.integer(unique(cbds$did))
dist2cbd = vector("list", length(dids))
for (did in dids) {
  grid_ids = which(as.integer(de_grid$did) == did)
  dist2cbd[[did]] = data.frame(
    grid_id = de_grid[grid_ids, ]$grid_id,
    did = did,
    dist2cbd = st_distance(de_grid[grid_ids, ], cbds[as.integer(cbds$did) == did, ])
  )
}
dist2cbd = rbindlist(dist2cbd, use.names = TRUE)
sales = merge(sales, dist2cbd, c('grid_id', 'did'))


## import consumer price index (CPI) for inflation adjustment ----
# source: https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61121&language=en#abreadcrumb
cpi = fread("extra/cpi_61121-0002.csv", skip = 6, header = FALSE, select = 1:3, na.strings = "...", col.names = c('year', 'mon', 'cpi'))
cpi[, year := as.integer(year)][, mon := match(mon, month.name)]
# fwrite(cpi, 'data/consumer-price-index_monthly_base-year-2015.csv')

## adjust by the GDP deflator (CPI) --------
sales = merge(sales, cpi[year >= min(sales$year), ],
              by.x = c("year", "ad_end_mon"), by.y = c("year", "mon")
              )
sales[, price := price / (cpi/100)]  # divide by the deflator
sales[, cpi := NULL] # remove cpi column

# zipcodes ----
sales = sales[zipcode > 0, ] # there are some -9 zipcodes
sales[, `:=`(zipcode = sprintf('%05i', zipcode))] # make 5 digit

# Define new vars ----
sales[, c('lnprice', 'price_sqm') := .(log(price), price/floor_space)]
sales[, lnprice_sqm := log(price_sqm)]
setkeyv(sales, c("did", "zipcode", "ad_end_mon", "year"))

# optional: further cleaning for  hedonic model-----

# problematic construction and renovation years
sales[constr_year < 0, constr_year := NA][renov_year < 0, renov_year := NA]

# perhaps houses not finished or built yet
maxYear = sales[, max(year)]
sales[constr_year > maxYear, constr_year := (maxYear)]
# If renovated before built, swap construction year with renovation year
sales[renov_year < constr_year, `:=`(renov_year = constr_year, constr_year = renov_year)]

# keep houses built since 1900
sales = sales[constr_year >= 1900 | is.na(constr_year), ]

# imputation of NAs with the overall median value by house type

# fill missing construction year by renovation year:
# could be that not renovated yet, thus construction year == renovation year
sales[is.na(constr_year) & !is.na(renov_year), constr_year := renov_year]
sales[is.na(renov_year) &  !is.na(constr_year), renov_year := constr_year]

sales[, `:=`(
  constr_year = fifelse(
    is.na(constr_year),
    as.integer(median(constr_year, na.rm = TRUE)),
    constr_year
  )
), house_type]

# impute renovation year by the median value,
# if not, replace it with construction year
sales[, `:=`(
  renov_year = fifelse(
    is.na(renov_year),
    max(constr_year, as.integer(median(renov_year, na.rm = TRUE))),
    renov_year
  )
), house_type]


# sanity checks
if (any(idx <- sales[, constr_year < 1900])) {
  message(sprintf("The imputation produced %i very old houses: built before 1900. Removing them...", sum(idx)))
  sales = sales[!idx, ]
}

if (any(sales[, renov_year > maxYear])) {
  message("The imputation produced for some homes renov.year > max.year possible.\
          Replaced them by the max.year")
  sales[renov_year > maxYear, renov_year := (maxYear)]
}

# compute age of houses, and renovation speed
sales[, age0 := maxYear - constr_year]
sales[, age1 := renov_year - constr_year]

# drop not-finished houses: House in process of planning or building
# sales = sales[!(constr_phase %like% "(House in process of )?(planning|building)"), ]


# handle outliers
# discard properties with
# (i) a sales price below 250e/m2 or above 25000e/m2
# (ii) floor space below 30m2 or above 500m2
n = nrow(sales)
sales = sales[exp(lnprice_sqm) >= 250 & exp(lnprice_sqm) <= 25000 & floor_space >= 30 & floor_space <= 500, ]
nrow(sales)/n # without duplicates

# write to disk ----
if (!dir.exists("data/processed/")) {
  dir.create("data/processed")
}

if (file.exists('data/processed/homes_ready.csv')) {
  warning("File has been overwritten!", call. = FALSE)
}
fwrite(sales, 'data/processed/homes_ready.csv')


