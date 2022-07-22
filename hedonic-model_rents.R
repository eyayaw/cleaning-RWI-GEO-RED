library(fixest)
library(data.table)

# tidy fixed effects from fixest::fixef()
tidy_fixeff <- function(fe_obj) {
  stopifnot(inherits(fe_obj, "fixest.fixef") && names(fe_obj) != "")
  fe = data.frame(id = names(fe_obj[[1]]), eff = fe_obj[[1]])
  fixeffs = strsplit(names(fe_obj), "^", fixed = TRUE)[[1]]
  fe[, fixeffs] = do.call("rbind", strsplit(fe$id, "_"))
  fe = fe[, c(fixeffs, "eff")]
  rownames(fe) = NULL
  fe
}

rents = fread("data/processed/rents_homes-apartments_ready.csv")
rents = rents[!(zipcode %like% "^-(000)?"), ] # rm missing zip codes, if any
rents[, lndist2cbd := log(1 + dist2cbd)] # adding 1 avoids log(dist->0) becoming -Inf
setnames(rents, 'ad_end_mon', 'mon')

# handle outliers
# discard properties with
# (i) a monthly rental price below 1e/m2 or above 50e/m2
# (ii) floor space below 30m2 or above 500m2
n = nrow(rents)
rents = rents[exp(lnrent_sqm) >= 1 & exp(lnrent_sqm) <= 50 & floor_space >= 30 & floor_space <= 500, ]
nrow(rents)/n

## required variables
dep_var = 'lnrent_sqm'
fixeffs = c('zipcode', 'did', 'mon','year')
# num_vars = c("lndist2cbd", "floor_space", "num_rooms", "age0", "age1", "lnutilities")
num_vars = c("floor_space", "num_rooms", "age0", "age1", "lnutilities")
### factor variables
cats_rents_flat = c(fixeffs,
  "num_bedrooms", "num_bathrooms", "num_floors", "flat_type",
  "heating_type", "basement", "equipment", "condition", "balcony", 'garden',
  'kitchen', 'floor'
)

cats_rents_home = c(
  fixeffs,
  "num_bedrooms", "num_bathrooms", "num_floors", "house_type",
  "heating_type", "basement", "guest_washroom", "equipment", "condition"
)

var_list_rents_flat = c(fixeffs, dep_var, num_vars, setdiff(cats_rents_flat, fixeffs))
var_list_rents_home = c(fixeffs, dep_var, num_vars, setdiff(cats_rents_home, fixeffs))

# Estimation -------------------------------------------------------------------
# rents of flats vs homes
rents_flat = rents[type==4, c('uniqueid_gen', 'grid_id', var_list_rents_flat), with=FALSE]
rents_home = rents[type==2, c('uniqueid_gen', 'grid_id', var_list_rents_home), with=FALSE]

rents_flat[, (cats_rents_flat) := lapply(.SD, as.factor), .SDcols=cats_rents_flat]
rents_home[, (cats_rents_home) := lapply(.SD, as.factor), .SDcols=cats_rents_home]
rents = list(flats=rents_flat, homes=rents_home)

# Estimate the fixed effects model with dummy variable estimator
parts = lapply(list(var_list_rents_flat, var_list_rents_home), \(vlist)
paste0("lnrent_sqm ~ 0 + ", paste(setdiff(vlist, c(dep_var, fixeffs)), collapse = " + ")
))
## all homes ----
### using `fixest` package ---------------------------------------------------------
form_feols_all = lapply(parts,
  \(part) as.formula(sprintf("%s | %s", part, paste(fixeffs, collapse = "^")))
)

rental_index = vector('list', length(parts))
names(rental_index) = names(rents)
for (i in seq_along(rental_index)) {
  rental_index[[i]] = feols(form_feols_all[[i]], rents[[i]], combine.quick = FALSE)
}

hri = Map(\(x, y) cbind(x[, .(uniqueid_gen, grid_id, did, zipcode, year, mon, lnrent_sqm)], lnhri = predict(y), lnresid=residuals(y)), rents, rental_index)


fwrite(hri$flats, 'data/processed/HRI_exact-geoloc_flats.csv')
fwrite(hri$homes, 'data/processed/HRI_exact-geoloc_homes.csv')

# collect fixed effects
fe_all_tidy = lapply(rental_index, fixef) |>
  lapply(tidy_fixeff)  # extract fixed effects

fwrite(fe_all_tidy$flats, 'data/processed/HRI_zipcode_flats.csv')
fwrite(fe_all_tidy$homes, 'data/processed/HRI_zipcode_homes.csv')

fe_out = rbindlist(fe_all_tidy, use.names = TRUE, idcol = 'type') |>
  dcast(zipcode + did + mon + year ~ type, value.var = 'eff')
setnames(fe_out, names(fe_all_tidy), paste0("lnr_", names(fe_all_tidy)))
fe_out[, lnr := rowMeans(cbind(lnr_flats, lnr_homes), na.rm = TRUE)]

# write to disk
fwrite(fe_out, 'data/processed/HRI_zipcode_flats-and-homes.csv')
