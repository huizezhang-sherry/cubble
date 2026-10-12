# Augment spatial component into the long (temporal) form

Some spatio-temporal transformation, i.e. glyph maps, uses both spatial
and temporal variables. `unfold()` allows you to temporarily moves
spatial variables into the long form for these transformations.

## Usage

``` r
unfold(data, ...)

# S3 method for class 'spatial_cubble_df'
unfold(data, ...)

# S3 method for class 'temporal_cubble_df'
unfold(data, ...)
```

## Arguments

- data:

  a long cubble object

- ...:

  spatial variables to move into the long form, support tidyselect
  syntax

## Value

a cubble object in the long form

## Examples

``` r
climate_mel |> face_temporal() |> unfold(long, lat)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin  long   lat
#>    <chr>       <date>     <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11    145. -37.7
#>  2 ASN00086038 2020-01-02     0  26.3  12.2  145. -37.7
#>  3 ASN00086038 2020-01-03     0  34.5  12.7  145. -37.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8  145. -37.7
#>  5 ASN00086038 2020-01-05    18  16.1  12.5  145. -37.7
#>  6 ASN00086038 2020-01-06   104  17.5  11.1  145. -37.7
#>  7 ASN00086038 2020-01-07    14  20.7  12.1  145. -37.7
#>  8 ASN00086038 2020-01-08     0  26.4  16.4  145. -37.7
#>  9 ASN00086038 2020-01-09     0  33.1  17.4  145. -37.7
#> 10 ASN00086038 2020-01-10     0  34    19.6  145. -37.7
#> # ℹ 20 more rows
climate_mel |> face_temporal() |> unfold(dplyr::starts_with("l"))
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin  long   lat
#>    <chr>       <date>     <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11    145. -37.7
#>  2 ASN00086038 2020-01-02     0  26.3  12.2  145. -37.7
#>  3 ASN00086038 2020-01-03     0  34.5  12.7  145. -37.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8  145. -37.7
#>  5 ASN00086038 2020-01-05    18  16.1  12.5  145. -37.7
#>  6 ASN00086038 2020-01-06   104  17.5  11.1  145. -37.7
#>  7 ASN00086038 2020-01-07    14  20.7  12.1  145. -37.7
#>  8 ASN00086038 2020-01-08     0  26.4  16.4  145. -37.7
#>  9 ASN00086038 2020-01-09     0  33.1  17.4  145. -37.7
#> 10 ASN00086038 2020-01-10     0  34    19.6  145. -37.7
#> # ℹ 20 more rows
```
