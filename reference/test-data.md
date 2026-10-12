# Toy climate data

Daily measure (2020-01-01 to 2020-01-10) on precipitation (prcp),
maximum temperature (tmax), and minimum temperature (tmin) for 3
melbourne airport stations. `stations` is the spatial component,
(`stations_sf` as an sf object), `meteo` has the temporal component
(`meteo_ts` as a tsibble object), `climate_flat` has both in a single
joined table, and `climate_mel` is the cubble object. See `climate_aus`
on the full dataset.

## Usage

``` r
stations

stations_sf

meteo

meteo_ts

climate_flat

climate_mel
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 3
rows and 6 columns.

An object of class `sf` (inherits from `tbl_df`, `tbl`, `data.frame`)
with 3 rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 30
rows and 5 columns.

An object of class `tbl_ts` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 30 rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 30
rows and 10 columns.

An object of class `spatial_cubble_df` (inherits from `cubble_df`,
`tbl_df`, `tbl`, `data.frame`) with 3 rows and 7 columns.

## See also

climate_aus

## Examples

``` r
cb <- make_cubble(
  spatial = stations, temporal = meteo,
  key = id, index = date, coords = c(long, lat)
)
identical(cb, climate_mel)
#> [1] TRUE
cb2 <- climate_flat |> 
  as_cubble(key = id, index = date, coords = c(long, lat))
identical(cb, climate_mel)
#> [1] TRUE
```
