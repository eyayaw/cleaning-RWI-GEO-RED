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

### A note on hedonic index

The data come with a rich set of property characteristics which enable us to compute a hedonic price index to quality-adjust house prices. I construct a mix-adjusted house price index from the following panel hedonic regression
<img src="https://render.githubusercontent.com/render/math?math={\ln P_{hit} = \delta_{it} + \textbold{X}_{hit}\beta + \varepsilon_{hit},}">
where ℎ indexes houses, 𝑖 districts and 𝑡 years, 𝑃 price of houses in euros per m^2^, 𝛿_𝑖𝑡 denotes district-year fixed effects that are of main interest to estimate, and X includes a set of house characteristics. The estimated intercepts 𝛿 𝑖𝑡̂ represent the quality adjusted prices for each district i in every year 𝑡. After estimating (1) with fixed effects, the hedonic price index is given by 𝛿 i𝑡 ̂ = ln 𝑃 ℎi𝑡 − X𝛽.
