# Match stations in two cubbles by spatial distance/ temporal similarity

The spatial matching is calculated using
[`sf::st_distance()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
with different distance (in meter or degree) available depending on the
coordinate reference system and parameter (`which` and `par`). The
temporal matching is based on a temporal matching function
(`temporal_match_fn`) that can be customised.

## Usage

``` r
match_sites(
  df1,
  df2,
  crs = sf::st_crs("OGC:CRS84"),
  which = NULL,
  par = 0,
  spatial_n_each = 1,
  spatial_n_group = 4,
  data_id,
  match_id,
  temporal_matching = TRUE,
  temporal_by,
  temporal_match_fn = match_peak,
  temporal_n_highest = 20,
  temporal_window = 5,
  ...
)

match_spatial(
  df1,
  df2,
  crs = sf::st_crs("OGC:CRS84"),
  which = NULL,
  par = 0,
  spatial_n_each = 1,
  spatial_n_group = 4,
  return_cubble = FALSE
)

match_temporal(
  data,
  data_id,
  match_id = NULL,
  temporal_by,
  return_cubble = FALSE,
  temporal_match_fn = match_peak,
  temporal_n_highest = 30,
  temporal_window = 5,
  ...
)
```

## Arguments

- df1, df2:

  the two cubble objects to match

- crs:

  a crs object from
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)

- which:

  character; for Cartesian coordinates only: one of `Euclidean`,
  `Hausdorff` or `Frechet`; for geodetic coordinates, great circle
  distances are computed; see details

- par:

  for `which` equal to `Hausdorff` or `Frechet`, optionally use a value
  between 0 and 1 to densify the geometry

- spatial_n_each:

  integer, the number of matched "station" in `df2` for each `df1`
  record

- spatial_n_group:

  integer, the number of matched group (pair) return

- data_id:

  a character (or symbol), the variable differentiates `df1` and `df2`

- match_id:

  a character (or symbol), the variable differentiate each group of
  match

- temporal_matching:

  logical, whether to match temporally

- temporal_by:

  in the `by` syntax in `dplyr::*_join()`, the variables to match
  temporally in `df1` and `df2`.

- temporal_match_fn:

  character, the function name on how two time series should be matched

- temporal_n_highest:

  numeric, the number of highest peak used for temporal matching in
  `match_peak`

- temporal_window:

  The temporal window allowed in `match_peak`

- ...:

  parameters passing to temporal match

- return_cubble:

  logical (default to false), whether to return the cubble object or a
  matching summary table

- data:

  the resulting cubble object from spatial matching (with
  `return_cubble = TRUE` in spatial matching)

## Examples

``` r
library(dplyr)
climate_aus <- mutate(climate_aus, type = "climate")
match_spatial(climate_aus, river)
#> Use OGC:CRS84 by default for distance calculation...
#> # A tibble: 4 × 4
#>   from        to      dist group
#>   <chr>       <chr>    [m] <int>
#> 1 ASN00088051 406213 1838.     1
#> 2 ASN00084145 222201 2185.     2
#> 3 ASN00085072 226027 3282.     3
#> 4 ASN00080015 406704 4034.     4
# turn with different distance calculation:
match_spatial(climate_aus, river, which = "Hausdorff")
#> Use OGC:CRS84 by default for distance calculation...
#> # A tibble: 4 × 4
#>   from        to       dist group
#>   <chr>       <chr>     [°] <int>
#> 1 ASN00088051 406213 0.0204     1
#> 2 ASN00084145 222201 0.0219     2
#> 3 ASN00085072 226027 0.0296     3
#> 4 ASN00080015 406704 0.0364     4
# tune the number of matches in each group
match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2)
#> Use OGC:CRS84 by default for distance calculation...
#> # A tibble: 10 × 4
#>    from        to       dist group
#>    <chr>       <chr>     [m] <int>
#>  1 ASN00088051 406213  1838.     1
#>  2 ASN00088051 406215  6963.     1
#>  3 ASN00088051 406235 15133.     1
#>  4 ASN00088051 405240 47673.     1
#>  5 ASN00088051 405212 48364.     1
#>  6 ASN00084145 222201  2185.     2
#>  7 ASN00084145 222203 13022.     2
#>  8 ASN00084145 223209 54797.     2
#>  9 ASN00084145 221224 63127.     2
#> 10 ASN00084145 224217 79816.     2

a1 <- match_spatial(climate_aus, river, return_cubble = TRUE) |> bind_rows()
#> Use OGC:CRS84 by default for distance calculation...
match_temporal(a1, data_id = type, match_id = group,
               temporal_by = c("prcp" = "Water_course_level"))
#> # A tibble: 4 × 2
#>   group match_res
#>   <int>     <dbl>
#> 1     1        30
#> 2     2         5
#> 3     3        14
#> 4     4        20
```
