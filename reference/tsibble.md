# Gap-filling on the temporal component of a cubble object

Gap-filling on the temporal component of a cubble object

## Usage

``` r
# S3 method for class 'temporal_cubble_df'
fill_gaps(.data, ..., .full = FALSE, .start = NULL, .end = NULL)

# S3 method for class 'temporal_cubble_df'
scan_gaps(.data, ...)

# S3 method for class 'temporal_cubble_df'
index_by(.data, ...)
```

## Arguments

- .data:

  A tsibble.

- ...:

  A set of name-value pairs. The values provided will only replace
  missing values that were marked as "implicit", and will leave
  previously existing `NA` untouched.

  - empty: filled with default `NA`.

  - filled by values or functions.

- .full:

  - `FALSE` inserts `NA` for each keyed unit within its own period.

  - `TRUE` fills `NA` over the entire time span of the data (a.k.a.
    fully balanced panel).

  - [`start()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    starting point (i.e. `min(<index>)`) across units.

  - [`end()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    ending point (i.e. `max(<index>)`) across units.

- .start, .end:

  Set custom starting/ending time that allows to expand the existing
  time spans.

## Value

a cubble object

## Examples

``` r
library(tsibble)
climate_aus |> face_temporal() |> fill_gaps()
#> # cubble:   key: id [639], index: date, long form
#> # temporal: 2020-01-01 -- 2020-12-31 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00001006 2020-01-01   164  38.3  25.3
#>  2 ASN00001006 2020-01-02     0  40.6  30.5
#>  3 ASN00001006 2020-01-03    16  39.7  27.2
#>  4 ASN00001006 2020-01-04     0  38.2  27.3
#>  5 ASN00001006 2020-01-05     2  39.3  26.7
#>  6 ASN00001006 2020-01-06    60  32.9  25.6
#>  7 ASN00001006 2020-01-07   146  34.1  25.5
#>  8 ASN00001006 2020-01-08    40  36.6  26.2
#>  9 ASN00001006 2020-01-09     0  38.2  27.6
#> 10 ASN00001006 2020-01-10     0  38.9  29.7
#> # ℹ 231,453 more rows
climate_aus |> face_temporal() |> scan_gaps()
#> # cubble:   key: id [639], index: date, long form, [tsibble]
#> # temporal: 2020-03-01 -- 2020-10-31 [1D], has gaps!
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date      
#>    <chr>       <date>    
#>  1 ASN00004090 2020-03-01
#>  2 ASN00004090 2020-03-02
#>  3 ASN00004090 2020-03-03
#>  4 ASN00004090 2020-03-04
#>  5 ASN00004090 2020-03-05
#>  6 ASN00004090 2020-03-06
#>  7 ASN00004090 2020-03-07
#>  8 ASN00004090 2020-03-08
#>  9 ASN00004090 2020-03-09
#> 10 ASN00004090 2020-03-10
#> # ℹ 693 more rows
```
