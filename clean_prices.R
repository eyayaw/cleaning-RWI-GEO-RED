library(data.table)
library(sf)
source("helpers/helpers.R")
if (!dir.exists("data/processed/")) {
  dir.create("data/processed")
}
GRID_data_path = "../.GRID_v11/Raster_shp/ger_1km_rectangle.shp"

# cleaning ---------------------------------------------------------------------
fpath="data/homes-sales_2016-2021.csv"
homes_sales = fread(fpath) # KeepLeadingZeros = TRUE is not required, rwi data comes without the leading zeros

# reorder variables
order_of_vars = c(
  "obid", "uniqueid_gen", "spell", "kid2019", "zipcode", "state", "year", "ad_begin_year",
  "ad_begin_mon","ad_end_mon", "price", "floor_space", "usable_floor_space", "plot_size",
  "num_rooms", "num_bedrooms", "num_bathrooms", "num_floors", "house_type",
  "protected_building", "holiday_house", "heating_type", "basement",
  "guest_washroom", "constr_year", "renov_year", "constr_phase",
  "equipment", "condition", "parking_space", "parking_space_price",
  "grid_id", "lab_mrkt_reg", "kid2015", "number_hits_of_ad"
)

if (!("grid_id" %in% names(homes_sales))) {
  if ("ergg_1km" %in% names(homes_sales)) {
    setnames(homes_sales, "ergg_1km", "grid_id")
  } else if ("grid_id_char" %in% names(homes_sales)) {
    setnames(homes_sales, "grid_id_char", "grid_id")
  }
}

setcolorder(homes_sales, c(order_of_vars, setdiff(names(homes_sales), order_of_vars)))
setnames(homes_sales, 'kid2019', 'did') # rename kid2019 to `did` (district id)

# rm duplicates
n = nrow(homes_sales)
idx = homes_sales[, .(idx=.I[which.max(spell)]), .(obid, year)][, idx]
homes_sales = homes_sales[idx, ]
nrow(homes_sales)/n
homes_sales[, spell := NULL]
rm(idx)

## dealing with missing values of many forms -----------------------------------
homes_sales = homes_sales[zipcode>0, ]
### drop observations with missing district identifier ----
count_missing = list(did = homes_sales[did < 0, .N])
homes_sales = homes_sales[did > 0, ]

### drop obs with missing 1km grid identifier, i.e., that are annonymized
count_missing$grid_id = homes_sales[grid_id<0, .N]
homes_sales = homes_sales[grid_id > 0, ]

### drop rows if price is missing ----
count_missing$price = homes_sales[price <= 0, .N]
homes_sales = homes_sales[price > 0, ]

### drop if missing floor space ----
count_missing$floor_space = homes_sales[floor_space <= 0, .N]
homes_sales = homes_sales[floor_space > 0, ]

### plot area ----
# count_missing$plot_size = homes_sales[plot_size <= 0, .N]
# homes_sales = homes_sales[plot_size > 0, ]

### number of rooms -----
count_missing$num_rooms = homes_sales[num_rooms < 0, .N]
homes_sales = homes_sales[num_rooms > 0, ]

### house type ----
count_missing$house_type = homes_sales[house_type < 0, .N]
homes_sales[, house_type := fcase(
  house_type == -7 | house_type == -9, 0L,
  house_type == 1 | house_type == 2, 1L,
  house_type == 11 | house_type == 12, 2L,
  house_type == 3L, 3L, # semi-detached
  between(house_type, 4, 6), 4L,
  house_type == 13 | house_type == 15, 5L,
  between(house_type, 7, 10) | house_type == 14, 6L
)]
homes_sales[, house_type := factor(
  house_type, 0L:6L,
  c(
    "na",            # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "single-family", #  1 Single-family house (detached) + 2 Single-family house
    "two-family",    # 11 two-family houses + 12 block of flats
    "semi-detached", # itself, 3 semi-detached
    "terraced",      # 4 terraced + 5 terraced (middle unit) + 6 terraced (end unit)
    "other",         # 13 other property for living + 15 other
    "special"        # 7 Bungalow + 8 Farmhouse + 9 Castle + 10 Mansion + 14 Special property
  )
)]


### condition of the object ----
count_missing$condition = homes_sales[condition < 0, .N]
homes_sales[condition < 0, condition := 0L]
homes_sales[, condition := factor(
  condition,
  0L:10L,
  c(
    "na", "First occupancy", "First occupancy after reconstruction", "Like new",
    "Reconstructed", "Modernised", "Completely renovated", "Well kempt",
    "Needs renovation", "By arrangement", "Dilapidated"
  )
)]


### number of bedrooms ----
count_missing$num_bedrooms = homes_sales[num_bedrooms <= 0, .N]
homes_sales[, num_bedrooms := fcase(
  num_bedrooms <= 0, 0L,
  num_bedrooms >= 7, 7L,
  rep_len(TRUE, length(num_bedrooms)), num_bedrooms
)]

homes_sales[, num_bedrooms := factor(
  num_bedrooms,
  0:7,
  c("na or 0", 1:6, "7+")
)]


### number of bathrooms ----
count_missing$num_bathrooms = homes_sales[num_bathrooms <= 0, .N]
homes_sales[, num_bathrooms := fcase(
  num_bathrooms <= 0, 0L,
  num_bathrooms >= 4, 4L,
  rep_len(TRUE, length(num_bathrooms)), num_bathrooms
)]

homes_sales[, num_bathrooms := factor(
  num_bathrooms,
  0:4,
  c("na or 0", 1:3, "4+")
)]


### total number of floors, create 5 categories ----
homes_sales[, num_floors := fcase(
  between(num_floors, -11, 0), 0L,
  between(num_floors, 4, max(num_floors)), 4L,
  rep_len(TRUE, length(num_floors)), num_floors
)]

homes_sales[, num_floors := factor(num_floors, 0:4, c("na", 1:3, "4+"))]

### facilities of the house, create categories ----
homes_sales[between(equipment, -11, 0), equipment := 0]
homes_sales[, equipment := factor(
  equipment,
  0:4,
  c("na", "Simple", "Normal", "Sophisticated", "Deluxe")
)]

### year of construction, year of renovation ----
homes_sales[, c("constr_year_cat", "renov_year_cat") := lapply(.SD, function(x) {
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
    between(x, 2010, 2020), 9
  )
}), .SDcols = c("constr_year", "renov_year")]

homes_sales[, c("constr_year_cat", "renov_year_cat") := lapply(.SD,
                                                         factor,
                                                         levels = 0:9,
                                                         labels = c(
                                                           "na", "<1900", "1900-1945", "1946-1959", "1960-1969", "1970-1979",
                                                           "1980-1989", "1990-1999", "2000-2009", "2009+"
                                                         )
), .SDcols = c("constr_year_cat", "renov_year_cat")]


### Type of heating ----
homes_sales[heating_type < 0, heating_type := 0]
# heating_type_labs = get_value_labels('heating_type', TRUE)
# datapasta::vector_paste(heating_type_labs$label)
homes_sales[, heating_type := factor(
  heating_type,
  0L:13L,
  c("na", "Cogeneration/combined heat and power plant", "Electric heating",
    "Self-contained central heating", "District heating", "Floor heating",
    "Gas heating", "Wood pellet heating", "Night storage heaters", "Heating by stove",
    "Oil heating", "Solar heating", "Thermal heat pump", "Central heating")
)]


### construction phase ----
homes_sales[constr_phase <= 0, constr_phase := 0L]
homes_sales[, constr_phase := factor(
  constr_phase,
  0L:3L,
  c("na", "House in process of planning", "House in process of building", "House built")
)]

### binary variables -----
# Following ('Klick & Schaffner' 2019, p. 12), for binary variables, we replace
# missing values by 0, i.e., by absence of the feature. Absence is denoted by `Nein` (== no ==0) in binary variables.

binary_vars = c("basement", "protected_building", "guest_washroom", "holiday_house")

# check if absence of the info in a binary variable is denoted by 0
check_for_0 = logical(length(binary_vars))
for (i in seq_along(binary_vars)) {
  check_for_0[[i]] =
    all(0 %in% unique(homes_sales[[binary_vars[[i]]]]))
}

for (i in seq_along(binary_vars)) {
  if (check_for_0[[i]]) {
    homes_sales[, (binary_vars[[i]]) := lapply(.SD, function(v) {
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
homes_sales[, (binary_vars) := lapply(.SD, as.factor), .SDcols = binary_vars]

# house keeping
rm(order_of_vars, binary_vars, i, check_for_0)

# remove protected buildings
homes_sales = homes_sales[protected_building == 0, ]
homes_sales[, protected_building := NULL]

# keep only districts that are defined in BKG end of the year i.e. 2019.12.31
districts = fread('data/processed/districts_destasis.csv',
                  select = 'did', colClasses = 'integer')
homes_sales = merge(homes_sales, districts, 'did') # two districts c('3152', '3156') will be dropped

if (FALSE)
  message(sprintf('%.2f%% of the observations dropped.', 100 * homes_sales[, .N]/fread(fpath)[, .N]))

# compute distance to the CBD -----
grid1km = st_read(GRID_data_path)[, c("idm", "geometry")]
cbds = st_read('data/geodata/CBDs.shp')[, c('did', 'geometry')]
cbds$geometry = st_centroid(cbds[, 'geometry'])$geometry
cbds = st_transform(cbds, st_crs(grid1km))
cbds$did = as.integer(cbds$did)

# geometry now is the centroid of the grid cell
grid1km$geometry = st_centroid(grid1km[, 'geometry'])$geometry
names(grid1km)[names(grid1km) == 'idm'] = 'grid_id'
grid1km = merge(grid1km, unique(homes_sales[, .(grid_id, did)]), by="grid_id")

dids = unique(cbds$did)
dist2cbd = vector("list", length(dids))
for (did in dids) {
  grid_ids = which(grid1km$did == did)
  dist2cbd[[did]] = data.frame(
    grid_id = grid1km[grid_ids, ]$grid_id,
    did=did,
    dist2cbd = st_distance(grid1km[grid_ids, ], cbds[cbds$did == did, ])
  )
}
dist2cbd = rbindlist(dist2cbd, use.names = TRUE)
rm(dids)
homes_sales = merge(homes_sales, dist2cbd, c('grid_id', 'did'))


## import consumer price index (CPI) for inflation adjustment ----
# source: https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61121&language=en#abreadcrumb
cpi = fread("data/cpi_61121-0002.csv", skip = 6, header = FALSE, select = 1:3, na.strings = "...", col.names = c('year', 'mon', 'cpi'))
cpi[, year := as.integer(year)][, mon := match(mon, month.name)]
# fwrite(cpi, 'data/consumer-price-index_monthly_base-year-2015.csv')

## adjust by the GDP deflator (CPI) --------
homes_sales = merge(homes_sales, cpi[year >= min(homes_sales$year), ],
              by.x = c("year", "ad_end_mon"), by.y = c("year", "mon")
              )
homes_sales[, price := price / (cpi/100)]  # divide by the deflator
homes_sales[, cpi := NULL]


# zipcodes ----
homes_sales[, `:=`(zipcode = sprintf('%05i', zipcode))] # make 5 digit

# Define new vars ----
homes_sales[, c('lnprice', 'price_sqm') := .(log(price), price/floor_space)]
homes_sales[, lnprice_sqm := log(price_sqm)]
setkeyv(homes_sales, c("did", "zipcode", "ad_end_mon", "year"))

# optional: further cleaning for  hedonic model-----

# problematic construction and renovation years
homes_sales[constr_year < 0, constr_year := NA][renov_year < 0, renov_year := NA]

# perhaps houses not finished or built yet
max_year = homes_sales[, max(year)]
homes_sales[constr_year > max_year, constr_year := (max_year)]
# If renovated before built, swap construction year with renovation year
homes_sales[renov_year < constr_year,
      `:=`(renov_year = constr_year, constr_year = renov_year)]

# keep houses built since 1900
homes_sales = homes_sales[constr_year >= 1900 | is.na(constr_year), ]


# imputation of NAs with the overall median value by house type

# fill missing construction year by renovation year:
# could be that not renovated yet, thus construction year == renovation year
homes_sales[is.na(constr_year) & !is.na(renov_year), constr_year := renov_year]
homes_sales[is.na(renov_year) &  !is.na(constr_year), renov_year := constr_year]

homes_sales[, `:=`(
  constr_year = fifelse(
    is.na(constr_year),
    as.integer(median(constr_year, na.rm = TRUE)),
    constr_year
  )
), house_type]

# impute renovation year by the median value,
# if not, replace it with construction year
homes_sales[, `:=`(
  renov_year = fifelse(
    is.na(renov_year),
    max(constr_year, as.integer(median(renov_year, na.rm = TRUE))),
    renov_year
  )
), house_type]


# sanity checks
if (any(idx <- homes_sales[, constr_year < 1900])) {
  message(sprintf("The imputation produced %i very old houses: built before 1900. Removing them...", sum(idx)))
  homes_sales = homes_sales[!idx, ]
}

if (any(homes_sales[, renov_year > max_year])) {
  message("The imputation produced for some homes renov.year > max.year possible.\
          Replaced them by the max.year")
  homes_sales[renov_year > max_year, renov_year := (max_year)]
}

# compute age of houses, and renovation speed
homes_sales[, age0 := max_year - constr_year]
homes_sales[, age1 := renov_year - constr_year]

# drop not-finished houses: House in process of planning or building
# homes_sales = homes_sales[!(constr_phase %like% "(House in process of )?(planning|building)"), ]


# handle outliers
# discard properties with
# (i) a monthly rental price below 250e/m2 or above 25000e/m2
# (ii) floor space below 30m2 or above 500m2
n = nrow(homes_sales)
homes_sales = homes_sales[exp(lnprice_sqm) >= 250 & exp(lnprice_sqm) <= 25000 &
                            floor_space >= 30 & floor_space <= 500, ]

nrow(homes_sales)/n # without duplicates

# write to disk ----
if (file.exists('data/processed/homes_ready.csv')) {
  warning("File has been overwritten!", call. = FALSE)
  fwrite(homes_sales, 'data/processed/homes_ready.csv')
} else{
  fwrite(homes_sales, 'data/processed/homes_ready.csv')
}
if (FALSE) haven::write_dta(homes_sales, 'data/processed/homes_ready.dta')


## single family homes only ----
fwrite(homes_sales[house_type == 'single-family', !'house_type'],
       'data/processed/homes_single-family_ready.csv')
if (FALSE) {
  homes_sales[house_type == 'single-family', !'house_type'] |>
    haven::write_dta('data/processed/homes_single-family_ready.dta')
}

# for hedonic regression look at "script/hedonic-model/hedonic-model_prices.R"
