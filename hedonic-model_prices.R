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

homes_sales = fread("data/processed/homes_ready.csv", keepLeadingZeros = TRUE)
setnames(homes_sales, "ad_end_mon", "mon")

# required variables
dep_var = 'lnprice_sqm'
fixeffs = c('zipcode', 'did', 'mon','year')
# num_vars = c("dist2cbd", "floor_space", "plot_size", "num_rooms", "age0", "age1")
num_vars = c("floor_space", "plot_size", "num_rooms", "age0", "age1")

# factor variables
cats = c(fixeffs,
  "num_bedrooms", "num_bathrooms", "num_floors","house_type", "holiday_house",
  "heating_type", "basement", "guest_washroom","constr_phase", "equipment", "condition"
)
var_list = c(fixeffs, dep_var, num_vars, setdiff(cats, fixeffs))

homes_sales = homes_sales[, c('uniqueid_gen', 'grid_id', var_list), with=FALSE]


# Estimation -------------------------------------------------------------------
homes_sales[, (cats) := lapply(.SD, as.factor), .SDcols = cats]

# Estimate the fixed effects model with dummy variable estimator
part = paste0("lnprice_sqm ~ 0 + ",
             paste(setdiff(var_list, c(dep_var, fixeffs)), collapse = " + "))

## all homes ----
### using `fixest` package ---------------------------------------------------------

form_feols_all = sprintf("%s | %s", part, paste(fixeffs, collapse = "^")) |>
  as.formula()

hedonic_all = feols(form_feols_all, homes_sales, combine.quick = FALSE)
fe_all = fixef(hedonic_all) # extract fixed effects
gc()
fe_all_tidy = tidy_fixeff(fe_all)

hpi = homes_sales[, .(uniqueid_gen, grid_id, did, zipcode, year, mon, lnprice_sqm, lnhpi = predict(hedonic_all), lnresid=residuals(hedonic_all))]


# write to disk
fwrite(hpi, 'data/processed/HPI_exact-geoloc_all-homes.csv')
fwrite(fe_all_tidy,'data/processed/HPI_zipcode_all-homes.csv')


## single-family homes -------
form_feols_single =
  sprintf("%s | %s", sub("house_type\\s*\\+", "", part), paste(fixeffs, collapse = "^")) |>
  as.formula()

hedonic_single = feols(form_feols_single,
                       homes_sales[house_type == 'single-family', !'house_type'],
                       combine.quick = FALSE)
fe_single = fixef(hedonic_single)
fe_single_tidy = tidy_fixeff(fe_single)

hpi_single = homes_sales[house_type == "single-family",
  .(uniqueid_gen, did, grid_id, zipcode, year, mon, lnprice_sqm, lnhpi = predict(hedonic_single), lnresid = residuals(hedonic_single))
]

# write to disk
fwrite(hpi_single, 'data/processed/HPI_exact-geoloc_single-family-homes.csv')
fwrite(fe_single_tidy,'data/processed/HPI_zipcode_single-family-homes.csv')

# export regression output
etable(hedonic_all, hedonic_single,
       depvar = FALSE,
       headers = c("All homes", "Single-family homes"),
       tex = TRUE,
       title = "Hedonic Price Indexes",
       label = "hedonic",
       file = "data/processed/hedonic-output_fixest.tex",
       style.df = style.df("aer"),
       replace = TRUE
)
