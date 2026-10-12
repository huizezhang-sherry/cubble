# Australia river data

Australia river data

## Usage

``` r
river
```

## Format

An object of class `spatial_cubble_df` (inherits from `cubble_df`,
`tbl_df`, `tbl`, `data.frame`) with 71 rows and 6 columns.

## Examples

``` r
river
#> # cubble:   key: station [71], index: date, nested form
#> # spatial:  [141.28, -38.7, 149.52, -35.91], Missing CRS!
#> # temporal: date [date], Water_course_level [dbl]
#>    station name                                lat  long ts       type 
#>    <chr>   <chr>                             <dbl> <dbl> <list>   <chr>
#>  1 221224  CANN RIVER U/S CANN RIVER OFFTAKE -37.5  149. <tibble> river
#>  2 222201  SNOWY RIVER @ ORBOST              -37.7  148. <tibble> river
#>  3 222203  SNOWY RIVER @ MARLO JETTY         -37.8  149. <tibble> river
#>  4 223209  TAMBO RIVER @ BATTENS LANDING     -37.8  148. <tibble> river
#>  5 224217  MITCHELL RIVER @ ROSEHILL         -37.8  148. <tibble> river
#>  6 225232  THOMSON RIVER @ BUNDALAGUAH       -38.0  147. <tibble> river
#>  7 226027  LA TROBE RIVER AT SWING BRIDGE    -38.1  147. <tibble> river
#>  8 226041  LAKE WELLINGTON @ BULL BAY        -38.1  147. <tibble> river
#>  9 226227  LATROBE RIVER @ KILMANY SOUTH     -38.1  147. <tibble> river
#> 10 232204  MOORABOOL RIVER @ MORRISONS       -37.8  144. <tibble> river
#> # ℹ 61 more rows
```
