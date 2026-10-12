# Create a cubble object

Create a cubble object

## Usage

``` r
cubble(..., key, index, coords)

make_cubble(
  spatial,
  temporal,
  by = NULL,
  key,
  index,
  coords,
  potential_match = NULL,
  key_use = "temporal"
)
```

## Arguments

- ...:

  a set of name-value pairs to create a cubble, need to include the
  `key`, `index`, and `coords` variables.

- key:

  a character (or symbol), the spatial identifier. See the Key section
  in
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html)

- index:

  a character (or symbol), the temporal identifier. Currently support
  base R classes `Date`, `POSIXlt`, `POSIXct` and tsibble's
  [`tsibble::yearmonth()`](https://tsibble.tidyverts.org/reference/year-month.html),
  [`tsibble::yearweek()`](https://tsibble.tidyverts.org/reference/year-week.html),
  and
  [`tsibble::yearquarter()`](https://tsibble.tidyverts.org/reference/year-quarter.html)
  class. See the Index section in
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html)

- coords:

  a vector of character (or symbol) of length two, in the order of
  longitude first and then latitude, the argument can be omitted if
  created from an sf and its subclasses. In case the sf geometry column
  is not POINT, coords will be the centroid coordinates.

- spatial:

  a tibble object or an sf object, the spatial component containing the
  `key` and `coords` variable (`coords` can be automatically created
  from an `sf` object if not supplied).

- temporal:

  a tibble object or a tsibble object, the temporal component containing
  the `key` and `index` variable.

- by:

  in the syntax of the `by` argument in
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  used in `make_cubble()` when the key variable has different names in
  the `spatial` and `temporal` data.

- potential_match:

  a `key_tbl` object from
  [`check_key()`](https://huizezhang-sherry.github.io/cubble/reference/check_key.md).
  When unmatched key values appear in spatial and temporal data,
  `make_cubble` will prompt the user to use
  [`check_key()`](https://huizezhang-sherry.github.io/cubble/reference/check_key.md)
  for checking. This argument allow the check result to be parsed back
  to `make_cubble` to also match the `potential_pairs` found by the
  check.

- key_use:

  a character of either "spatial" or "temporal". When `potential_math`
  is activated, this argument specifies which key column in the
  potential match to use. Default to "temporal".

## Value

a cubble object

## Examples

``` r
cubble(
  id = rep(c("perth", "melbourne", "sydney"), each = 3),
  date = rep(as.Date("2020-01-01") + 0:2, times = 3),
  long = rep(c(115.86, 144.96, 151.21), each = 3),
  lat = rep(c(-31.95, -37.81, -33.87), each = 3),
  value = rnorm(n = 9),
  key = id, index = date, coords = c(long, lat)
  )
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [115.86, -37.81, 151.21, -31.95], Missing CRS!
#> # temporal: date [date], value [dbl]
#>   id         long   lat ts              
#>   <chr>     <dbl> <dbl> <list>          
#> 1 perth      116. -32.0 <tibble [3 × 2]>
#> 2 melbourne  145. -37.8 <tibble [3 × 2]>
#> 3 sydney     151. -33.9 <tibble [3 × 2]>

# stations and climate are in-built data in cubble
make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
```
