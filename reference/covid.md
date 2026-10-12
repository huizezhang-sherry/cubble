# Daily COVID count data (in `tsibble`) and Victoria LGA (in `sf`)

Daily COVID count data (`covid`) from 2022-01-01 to 2020-03-23 in a
tsibble object (`date`, `lga`, `n`, and `avg_7day`). Victoria Local
Government Area (LGA) spatial geometry in an sf object (`lga_name_2018`
and `geometry`)

## Usage

``` r
covid

lga
```

## Format

An object of class `tbl_ts` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 6806 rows and 4 columns.

An object of class `sf` (inherits from `data.frame`) with 80 rows and 2
columns.

## Details

- date:

  date object, from 2022-01-01 to 2020-03-23

- lga:

  Victoria Local Government Area (LGA) in Australia

- n:

  COVID-19 case count

- avg_7day:

  rolling mean of `n` in a 7 day window. Calculate with
  `mutate(avg_7day = slider::slide_dbl(n, mean, .before = 6))`

- lga_name_2018:

  LGA encoding by Australia Bureau of Statistics, slightly differ from
  the encoding used by the Department of Health in the `covid` data

- geometry:

  multipolygon geometry of each LGA

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
# prompt msg on the key mismatch between the two datasets
make_cubble(lga, covid, by = c("lga_name_2018" = "lga"))
#> Warning: st_centroid assumes attributes are constant over geometries
#> ! Some sites in the spatial table don't have temporal information
#> ! Some sites in the temporal table don't have spatial information
#> ! Use `check_key()` to check on the unmatched key
#> The cubble is created only with sites having both spatial and
#> temporal information
#> # cubble:   key: lga [78], index: date, nested form, [sf]
#> # spatial:  [140.96, -39.13, 149.98, -34], WGS 84
#> # temporal: date [date], n [dbl], avg_7day [dbl]
#>    lga             long   lat                                  geometry ts      
#>    <chr>          <dbl> <dbl>                            <GEOMETRY [°]> <list>  
#>  1 Alpine (S)      147. -36.9 POLYGON ((146.7258 -36.45922, 146.7198 -… <tbl_ts>
#>  2 Ararat (RC)     143. -37.5 POLYGON ((143.1807 -37.73152, 143.0609 -… <tbl_ts>
#>  3 Ballarat (C)    144. -37.5 POLYGON ((143.6622 -37.57241, 143.68 -37… <tbl_ts>
#>  4 Banyule (C)     145. -37.7 POLYGON ((145.1357 -37.74091, 145.1437 -… <tbl_ts>
#>  5 Bass Coast (S)  146. -38.5 MULTIPOLYGON (((145.5207 -38.30667, 145.… <tbl_ts>
#>  6 Baw Baw (S)     146. -38.0 POLYGON ((145.765 -37.89858, 145.7414 -3… <tbl_ts>
#>  7 Bayside (C)     145. -37.9 POLYGON ((144.985 -37.891, 144.9855 -37.… <tbl_ts>
#>  8 Benalla (RC)    146. -36.6 POLYGON ((146.1314 -36.25512, 146.0369 -… <tbl_ts>
#>  9 Boroondara (C)  145. -37.8 POLYGON ((145.1067 -37.79216, 145.0953 -… <tbl_ts>
#> 10 Brimbank (C)    145. -37.7 POLYGON ((144.8862 -37.70775, 144.8594 -… <tbl_ts>
#> # ℹ 68 more rows
check_res <- check_key(lga, covid, by = c("lga_name_2018" = "lga"))

# fix mismatch
lga2 <- lga |>
  rename(lga = lga_name_2018) |>
  mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
         lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga)) |>
  filter(!lga %in% check_res$others$spatial)
covid2 <- covid |> filter(!lga %in% check_res$others$temporal)

make_cubble(spatial = lga2, temporal = covid2)
#> Warning: st_centroid assumes attributes are constant over geometries
#> # cubble:   key: lga [80], index: date, nested form, [sf]
#> # spatial:  [140.96, -39.13, 149.98, -34], WGS 84
#> # temporal: date [date], n [dbl], avg_7day [dbl]
#>    lga             long   lat                                  geometry ts      
#>    <chr>          <dbl> <dbl>                            <GEOMETRY [°]> <list>  
#>  1 Alpine (S)      147. -36.9 POLYGON ((146.7258 -36.45922, 146.7198 -… <tbl_ts>
#>  2 Ararat (RC)     143. -37.5 POLYGON ((143.1807 -37.73152, 143.0609 -… <tbl_ts>
#>  3 Ballarat (C)    144. -37.5 POLYGON ((143.6622 -37.57241, 143.68 -37… <tbl_ts>
#>  4 Banyule (C)     145. -37.7 POLYGON ((145.1357 -37.74091, 145.1437 -… <tbl_ts>
#>  5 Bass Coast (S)  146. -38.5 MULTIPOLYGON (((145.5207 -38.30667, 145.… <tbl_ts>
#>  6 Baw Baw (S)     146. -38.0 POLYGON ((145.765 -37.89858, 145.7414 -3… <tbl_ts>
#>  7 Bayside (C)     145. -37.9 POLYGON ((144.985 -37.891, 144.9855 -37.… <tbl_ts>
#>  8 Benalla (RC)    146. -36.6 POLYGON ((146.1314 -36.25512, 146.0369 -… <tbl_ts>
#>  9 Boroondara (C)  145. -37.8 POLYGON ((145.1067 -37.79216, 145.0953 -… <tbl_ts>
#> 10 Brimbank (C)    145. -37.7 POLYGON ((144.8862 -37.70775, 144.8594 -… <tbl_ts>
#> # ℹ 70 more rows
```
