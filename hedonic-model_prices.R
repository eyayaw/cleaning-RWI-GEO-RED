library(fixest)
library(data.table)
source('helpers/helpers.R')

purchases = fread("data/processed/purchases_homes-apartments_clean.csv")
setnames(purchases, "ad_end_mon", "mon")
purchases[, lndist := log(1 + dist2cbd/1000)] # dist to the cbd in km

# required variables
dep_var = 'lnprice_sqm'
fixeffs = c('did', 'mon','year')
num_vars = c("lndist", "floor_space", "num_rooms", "age0", "age1")
# factor variables
cats = c(
  fixeffs, "num_bedrooms", "num_bathrooms", "num_floors", "htype", "holiday_house",
  "heating_type", "basement", "guest_washroom", "constr_phase", "equipment",
  "condition", "balcony", "garden", "kitchen", "floor"
)

var_list = c(fixeffs, dep_var, num_vars, setdiff(cats, fixeffs))
purchases = purchases[,
c("uniqueid_gen", "grid_id", "zipcode", "lab_mrkt_reg", var_list), with = FALSE
]


# Estimation -------------------------------------------------------------------
# Estimate the fixed effects model with dummy variable estimator
purchases[, (cats) := lapply(.SD, as.factor), .SDcols = cats]
rhs = paste(setdiff(var_list, c(dep_var, fixeffs)), collapse = " + ") # rhs = X, in Y ~ Xb + fe
part = paste0("lnprice_sqm ~ 0 + ", rhs)
form = sprintf("%s | %s", part, paste(fixeffs, collapse = "^")) |> as.formula()
hedonic = feols(form, purchases, combine.quick=FALSE);gc()
fe = fixef(hedonic) # extract fixed effects
fe = tidy_fixeff(fe) # a data frame containing `fixeffs` and `eff` (the estimated fixeffs)

hpi = data.table(lnhpi = predict(hedonic), lnresid = residuals(hedonic), fe=hedonic$sumFE)
# add vars from the original data
hpi = cbind(
  purchases[, c("uniqueid_gen", "grid_id", "zipcode", fixeffs, dep_var), with = FALSE],
  hpi
)


# write to disk
fwrite(hpi, 'data/processed/HPI_exact-geoloc_all-homes.csv')
fwrite(fe,'data/processed/HPI_all-homes.csv')


# export regression output
etable(hedonic,
  depvar = FALSE,
  headers = "All homes",
  tex = TRUE,
  title = "Hedonic Price Index",
  label = "hedonic",
  file = "data/processed/hedonic-output_prices.tex",
  style.df = style.df("aer"),
  replace = TRUE
)
