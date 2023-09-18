# Helpers for cleaning <a href="https://www.rwi-essen.de/en/research-advice/further/research-data-center-ruhr-fdz/data-sets/rwi-geo-red/x-real-estate-data-and-price-indices">Immoscout24/RWI-GEO-RED</a> housing data
This repo contains a set of R scripts for cleaning housing data provided by RWI-GEO-RED, and for constructing hedonic price/rent indexes. 

The data cleaning steps I followed in these scripts are ad hoc and thus you may need to adapt them to your need should you find them useful. 
Somes of the steps in the scripts might not be relevant for your particular use case, for example, extracting the zip files and parsing the variable/value labels, and translating variable names. You may read the scripts in the following order. 
## Steps
1. [parse-labels_from_Stata-log-files.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/parse-labels_from_Stata-log-files.R)
2. [prepare_prices-rents_data.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/prepare_prices-rents_data.R)
3. [clean_prices.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/clean_prices.R)
4. [clean_rents.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/clean_rents.R)
5. [hedonic-model_prices.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/hedonic-model_prices.R)
6. [hedonic-model_rents.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/hedonic-model_rents.R)

## Usage

### Requirements

- `R 4.2` or higher

- Development version of data.table (v 1.14.7) or higher:

  ```r
  install.packages("data.table")
  
  # latest development version
  data.table::update_dev_pkg()
  ```

### Preparation

Besides installing packages, you may need to create a `.Renviron` file for the location of the RWI-GEO-RED data and the desired start and end year, for example:

```bash
RED_FOLDER=C:/Users/x/RWI-GEO-RED_v6
YEAR_START=2007
YEAR_END=2021
```
> Note: Alternatively, you can define those constants inside the respective scripts. 

**Furthermore, you need to download additional data:**

- For the CPI to work, [download the monthly CPI from Destatis](https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61121&language=en#abreadcrumb) (make sure to select the years you need) and save it as `extra/cpi_61121-0002.csv`

## Features

[clean_prices.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/clean_prices.R) and [clean_rents.R](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/clean_rents.R) filter and clean the data for house/apartment prices and rents respectively. Furthermore, datasets for houses and apartments are combined, 

- Filtering:
  - [for each observation which is classified as 'likely duplicate' (`dupID_gen == 1`), drop the previous one](https://github.com/eyayaw/cleaning-RWI-GEO-RED/blob/main/clean_rents.R#L15)
  - rents: only keep observations with `grid_id > 0 & rent > 0 & floor_space > 0 & num_rooms > 0 & utilities > 0`
  - prices: only keep observations with `grid_id > 0 & price > 0 & floor_space > 0 & num_rooms > 0` 

- Combining rents for apartments and houses:
  - assign code `999L` to object properties that only apply to the other category (apartments/houses)

- Cleaning:
  - missing values (with FDZ codes < 0) are recoded to "na"
  - `num_bedrooms`: limited between 1 and 7 bedrooms (`>=7` recoded to `7+`), missing recoded to "na or 0"
  - `num_bathrooms`, `num_floors`: limited between 1 and 4 (rest see `num_bedrooms`)

Additional processing at the end of `clean_rents.R`:

- construction/renovation year
  - construction year <-> renovation year in case the other one is NA
  - drop construction years below 1900
    - if NA, impute construction year and drop imputations < 1900
- discard properties with
  - (i) a monthly rental price below 1e/m2 or above 50e/m2
  - (ii) floor space below 30m2 or above 500m2
  
And finally:

- the distance to the central business district (CBD) is computed and
- the consumer price index (CPI) is used to adjust rents/prices for inflation

### Open issues

- kategorie_Wohnung has label value "11" that has no label (neither Stata nor csv, in the csv the label is "11")

## A note on constructing a hedonic index

The data come with a rich set of property characteristics which enable us to compute a hedonic (price) index to quality-adjust house prices. I construct a mix-adjusted house price index from the following panel hedonic regression

$$
\ln P_{hit} = \textbf X_{hit}\boldsymbol\beta + \delta_{it} + \varepsilon_{hit}, \quad \quad \quad \quad (1)
$$

where $h$ indexes houses, $i$ districts and $t$ years, $P$ is the price of houses in euros per $m^2$, $\delta_{it}$ denotes district-year fixed effects that are of main interest to estimate, and $\textbf{X}$ includes a set of house characteristics. The estimated intercepts $\widehat{\delta_{it}}$ represent the quality adjusted prices for each district $i$ in every year $t$. After estimating (1) with fixed effects, the **log hedonic price index** per $m^2$ is given by $\widehat{\delta_{it}} = \ln P_{hit} − \textbf{X}_{hit}\widehat{\boldsymbol{\beta}}$. House prices are adjusted for inflation using the German [consumer price index (CPI)](https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61121&language=en#abreadcrumb).

Additionally, in the `extra/` folder, labor market regions ([Kosfeld and Werner (2012)](https://link.springer.com/article/10.1007/s13147-011-0137-8 "German Labour Markets—New Delineation after the Reforms of German District Boundaries 2007–2011")), the (1kmx1km) grid, municipality, and district information for Germany are provided. Note that the Kosfeld and Werner (2012)'s labor market regions are updated for the 2019 (end of the year) administrative structure ([Verwaltungsgliederung am 31.12.2019](https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/Verwaltungsgliederung/31122019_Jahr.html)) of districts.
