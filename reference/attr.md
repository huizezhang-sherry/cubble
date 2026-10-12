# Extract cubble attributes

Extract cubble attributes

## Usage

``` r
# S3 method for class 'cubble_df'
key_vars(x)

# S3 method for class 'cubble_df'
key(x)

# S3 method for class 'cubble_df'
key_data(.data)

coords(data)

spatial(data)

# S3 method for class 'spatial_cubble_df'
spatial(data)

# S3 method for class 'temporal_cubble_df'
spatial(data)

index(data)

index_var(data)
```

## Arguments

- x, .data, data:

  a cubble object

## Examples

``` r
library(tsibble)
#> 
#> Attaching package: ‘tsibble’
#> The following objects are masked from ‘package:cubble’:
#> 
#>     index, index_var, is_tsibble
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, union
key(climate_mel)
#> [[1]]
#> id
#> 
key_vars(climate_mel)
#> [1] "id"
key_data(climate_mel)
#> # A tibble: 3 × 2
#>   id                .rows
#>   <chr>       <list<int>>
#> 1 ASN00086038         [1]
#> 2 ASN00086077         [1]
#> 3 ASN00086282         [1]
cubble::index(climate_mel)
#> date
cubble::index_var(climate_mel)
#> [1] "date"
coords(climate_mel)
#> [1] "long" "lat" 
spatial(climate_mel)
#> # A tibble: 3 × 6
#>   id           long   lat  elev name              wmo_id
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866
```
