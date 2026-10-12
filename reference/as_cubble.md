# Coerce foreign objects into a cubble object

Coerce foreign objects into a cubble object

## Usage

``` r
as_cubble(data, key, index, coords, ...)

# S3 method for class 'data.frame'
as_cubble(data, key, index, coords, ...)

# S3 method for class 'tbl_df'
as_cubble(data, key, index, coords, crs, dimensions, ...)

# S3 method for class 'sf'
as_cubble(data, key, index, ...)

# S3 method for class 'ncdf4'
as_cubble(
  data,
  key,
  index,
  coords,
  vars,
  lat_range = NULL,
  long_range = NULL,
  ...
)

# S3 method for class 'stars'
as_cubble(data, key, index, coords, ...)

# S3 method for class 'sftime'
as_cubble(data, key, index, coords, ...)
```

## Arguments

- data:

  an object to be converted into an cubble object. Currently support
  objects of classes `tibble`, `ncdf4`, `stars`, and `sftime`.

- key:

  a character (symbol), the spatial identifier, see
  [`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md)

- index:

  a character (symbol), the temporal identifier, see
  [`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md).

- coords:

  a vector of character (symbol) of length 2, see
  [`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md).

- ...:

  other arguments.

- crs:

  used in `as_cubble.tbl_df()` to set the crs. the data to read in
  `as_cubble.netcdf()`.

- dimensions:

  used when creating a cubble from a stars object

- vars:

  a vector of variables to read in (with quote), used in
  `as_cubble.netcdf()` to select the variable to read in.

- lat_range, long_range:

  in the syntax of `seq(FROM, TO, BY)` to downsample

## Value

a cubble object

## Examples

``` r
climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>

# only need `coords` if create from a tsibble
dt <- climate_flat |>  tsibble::as_tsibble(key = id, index = date)
dt |>  as_cubble(coords = c(long, lat))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tbl_ts [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tbl_ts [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tbl_ts [10 × 4]>

# netcdf
path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
raw <- ncdf4::nc_open(path)
dt <- as_cubble(raw)
#> ℹ More than 10,000 keys: only use the first key to test spatial &
#> temporal variables.
# subset degree
dt <- as_cubble(raw,vars = c("q", "z"),
                long_range = seq(113, 153, 3),
                lat_range = seq(-53, -12, 3))

if (FALSE) { # \dontrun{
# stars - take a few seconds to run
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <-  stars::read_stars(tif)
x |> as_cubble(index = band)
} # }

# don't have to supply coords if create from a sftime
dt <- climate_flat |>
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84")) |>
  sftime::st_as_sftime()
dt |> as_cubble(key = id, index = date)
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84 (CRS84)
#> # temporal: prcp [dbl], tmax [dbl], tmin [dbl], date [date]
#>   id           elev name   wmo_id            geometry  long   lat ts      
#>   <chr>       <dbl> <chr>   <dbl>         <POINT [°]> <dbl> <dbl> <list>  
#> 1 ASN00086038  78.4 essen…  95866 (144.9066 -37.7276)  145. -37.7 <tibble>
#> 2 ASN00086077  12.1 moora…  94870   (145.0964 -37.98)  145. -38.0 <tibble>
#> 3 ASN00086282 113.  melbo…  94866 (144.8321 -37.6655)  145. -37.7 <tibble>
```
