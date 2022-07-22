# Helpers for cleaning <a href="https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-sets/rwi-geo-red/x-real-estate-data-and-price-indices">Immoscout24/RWI-GEO-RED</a> housing data
This repo contains a set of R scripts for cleaning housing data provided by RWI-GEO-RED, and for constructing hedonic price/rent indexes. 

The data cleaning steps I followed in these scripts are ad hoc and thus you may need to adapt them to your need should you find them useful. 
Somes of the scripts might not be relevant for your particular use, for example, extracting the zip files. You may read the scripts in the following order. 
## Steps
1. [parse-labels_from_Stata-log-files.R](parse-labels_from_Stata-log-files.R)
2. [prepare_prices-rents_data.R](prepare_prices-rents_data.R)
3. [clean_prices.R](clean_prices.R)
4. [clean_rents.R](clean_rents.R)
5. [hedonic-model_prices.R](hedonic-model_prices.R)
6. [hedonic-model_rents.R](hedonic-model_rents.R)
