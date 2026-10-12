# Update the spatial cubble to include the sf class

add geometry list column to cubble_df object

## Usage

``` r
make_spatial_sf(x, sfc = NULL, crs, silent = FALSE)
```

## Arguments

- x:

  object of class `spatial_cubble_df`

- sfc:

  object of class `sfc` (see package sf)

- crs:

  object of class `crs` (see package sf); if missing 'OGC:CRS84' is
  assumed (WGS84) and a message is emitted

- silent:

  logical; suppress message?

## See also

[make_temporal_tsibble](https://huizezhang-sherry.github.io/cubble/reference/make_temporal_tsibble.md)

## Examples

``` r
climate_mel |> make_spatial_sf()
#> CRS missing: using OGC:CRS84 (WGS84) as default
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84 (CRS84)
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name   wmo_id ts                  geometry
#>   <chr>       <dbl> <dbl> <dbl> <chr>   <dbl> <list>           <POINT [°]>
#> 1 ASN00086038  145. -37.7  78.4 essen…  95866 <tibble> (144.9066 -37.7276)
#> 2 ASN00086077  145. -38.0  12.1 moora…  94870 <tibble>   (145.0964 -37.98)
#> 3 ASN00086282  145. -37.7 113.  melbo…  94866 <tibble> (144.8321 -37.6655)
```
