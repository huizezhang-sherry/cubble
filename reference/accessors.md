# Accessors to a cubble object

Accessors to a cubble object

## Usage

``` r
# S3 method for class 'spatial_cubble_df'
data[i, j, drop = FALSE]

# S3 method for class 'temporal_cubble_df'
data[i, j, drop = FALSE]

# S3 method for class 'spatial_cubble_df'
names(x) <- value

# S3 method for class 'temporal_cubble_df'
names(x) <- value

# S3 method for class 'cubble_df'
x[[i]] <- value
```

## Arguments

- data:

  an object of class `spatial_cubble_df` or `temporal_cubble_df`

- i, j:

  row and column selector

- drop:

  logical. If `TRUE` the result is coerced to the lowest possible
  dimension. The default is to drop if only one column is left, but
  **not** to drop if only one row is left.

- x:

  data frame.

- value:

  a suitable replacement value: it will be repeated a whole number of
  times if necessary and it may be coerced: see the Coercion section. If
  `NULL`, deletes the column if a single column is selected.

## Details

For nested cubbles, `[` will return a cubble object if the `key`
variable, the`coords` variables, and the `ts` column all present. If the
cubble object is also an sf object, the sticky select behavior on the sf
column will preserve. For long cubbles, `[` will return a cubble object
if the `key` and `index` variable both present. When a cubble can't be
created and the data is not an sf class, `[` will always return a
tibble, even with single index selection.

## Examples

``` r
climate_mel[c(1:3, 7)] # a nested cubble
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat ts               
#>   <chr>       <dbl> <dbl> <list>           
#> 1 ASN00086038  145. -37.7 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 <tibble [10 × 4]>
make_spatial_sf(climate_mel)[1:3] # an sf
#> CRS missing: using OGC:CRS84 (WGS84) as default
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 144.8321 ymin: -37.98 xmax: 145.0964 ymax: -37.6655
#> Geodetic CRS:  WGS 84 (CRS84)
#> # A tibble: 3 × 4
#>   id           long   lat            geometry
#>   <chr>       <dbl> <dbl>         <POINT [°]>
#> 1 ASN00086038  145. -37.7 (144.9066 -37.7276)
#> 2 ASN00086077  145. -38.0   (145.0964 -37.98)
#> 3 ASN00086282  145. -37.7 (144.8321 -37.6655)

long <- climate_mel |> face_temporal()
long[1:3] # a long cubble
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp
#>    <chr>       <date>     <dbl>
#>  1 ASN00086038 2020-01-01     0
#>  2 ASN00086038 2020-01-02     0
#>  3 ASN00086038 2020-01-03     0
#>  4 ASN00086038 2020-01-04     0
#>  5 ASN00086038 2020-01-05    18
#>  6 ASN00086038 2020-01-06   104
#>  7 ASN00086038 2020-01-07    14
#>  8 ASN00086038 2020-01-08     0
#>  9 ASN00086038 2020-01-09     0
#> 10 ASN00086038 2020-01-10     0
#> # ℹ 20 more rows

climate_mel[1:3] # tibble
#> # A tibble: 3 × 3
#>   id           long   lat
#>   <chr>       <dbl> <dbl>
#> 1 ASN00086038  145. -37.7
#> 2 ASN00086077  145. -38.0
#> 3 ASN00086282  145. -37.7
long[2:5] # tibble
#> # A tibble: 30 × 4
#>    date        prcp  tmax  tmin
#>    <date>     <dbl> <dbl> <dbl>
#>  1 2020-01-01     0  26.8  11  
#>  2 2020-01-02     0  26.3  12.2
#>  3 2020-01-03     0  34.5  12.7
#>  4 2020-01-04     0  29.3  18.8
#>  5 2020-01-05    18  16.1  12.5
#>  6 2020-01-06   104  17.5  11.1
#>  7 2020-01-07    14  20.7  12.1
#>  8 2020-01-08     0  26.4  16.4
#>  9 2020-01-09     0  33.1  17.4
#> 10 2020-01-10     0  34    19.6
#> # ℹ 20 more rows
climate_mel[1] # still tibble
#> # A tibble: 3 × 1
#>   id         
#>   <chr>      
#> 1 ASN00086038
#> 2 ASN00086077
#> 3 ASN00086282
long[1] # and still tibble
#> # A tibble: 30 × 1
#>    id         
#>    <chr>      
#>  1 ASN00086038
#>  2 ASN00086038
#>  3 ASN00086038
#>  4 ASN00086038
#>  5 ASN00086038
#>  6 ASN00086038
#>  7 ASN00086038
#>  8 ASN00086038
#>  9 ASN00086038
#> 10 ASN00086038
#> # ℹ 20 more rows
```
