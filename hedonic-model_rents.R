library(fixest)
library(data.table)
source('helpers/helpers.R')

rents = fread("data/processed/rents_homes-apartments_clean.csv")
setnames(rents, 'ad_end_mon', 'mon')
rents[, lndist := log(1 + dist2cbd/1000)] # adding 1 avoids log(dist->0) becoming -Inf

## required variables
dep_var = 'lnrent_sqm'
fixeffs = c('did', 'mon','year')
num_vars = c("lndist", "floor_space", "num_rooms", "age0", "age1", "lnutilities")
### factor variables
cats = c(
  fixeffs, "num_bedrooms", "num_bathrooms", "num_floors", "htype",
  "heating_type", "basement", "equipment", "condition", "balcony", "garden",
  "kitchen", "floor", "guest_washroom"
)

var_list = c(fixeffs, dep_var, num_vars, setdiff(cats, fixeffs))
rents = rents[,
c("uniqueid_gen", "grid_id", "zipcode", "lab_mrkt_reg", var_list), with = FALSE
]

# Estimation -------------------------------------------------------------------
# Estimate the fixed effects model with dummy variable estimator
rents[, (cats) := lapply(.SD, as.factor), .SDcols = c(cats)]
rhs = paste(setdiff(var_list, c(dep_var, fixeffs)), collapse = " + ") # rhs = X, in Y ~ Xb + fe
part = paste0("lnrent_sqm ~ 0 + ", rhs)
form = sprintf("%s | %s", part, paste(fixeffs, collapse = "^")) |> as.formula()
hedonic = feols(form, rents, combine.quick = FALSE);gc()
fe = fixef(hedonic) # extract fixed effects
fe = tidy_fixeff(fe) # a data frame containing `fixeffs` and `eff` (the estimated fixeffs)

hri = data.table(
  lnhri = predict(hedonic),
  lnresid = residuals(hedonic),
  fe = hedonic$sumFE # the sum of the fixed-effects coeffs for each observation
)
# add vars from the original data
hri = cbind(rents[, c("uniqueid_gen", "grid_id", "zipcode", fixeffs, dep_var), with = FALSE], hri)


# write to disk
fwrite(hri, 'data/processed/HRI_exact-geoloc_all-homes.csv')
fwrite(fe,'data/processed/HRI_all-homes.csv')


# export regression output
etable(hedonic,
  depvar = FALSE,
  headers = "All homes",
  tex = TRUE,
  title = "Hedonic Rental Index",
  label = "hedonic",
  file = "data/processed/hedonic-output_rents.tex",
  style.df = style.df("aer"),
  replace = TRUE
)
