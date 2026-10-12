# Predicate functions on the object class

Predicate functions on the object class

## Usage

``` r
is_cubble(data)

is_cubble_spatial(data)

is_cubble_temporal(data)

is_sf(data)

is_tsibble(data)
```

## Arguments

- data:

  an object to test for the class

## Value

a logical value of TRUE/FALSE

## Examples

``` r
is_cubble(stations)
#> [1] FALSE
is_cubble(meteo)
#> [1] FALSE
is_cubble(climate_flat)
#> [1] FALSE
is_cubble(climate_mel)
#> [1] TRUE
is_cubble(climate_aus)
#> [1] TRUE
is_cubble_spatial(climate_aus)
#> [1] TRUE
is_cubble_temporal(climate_aus)
#> [1] FALSE
```
