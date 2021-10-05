
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cubble

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/cubble/workflows/R-CMD-check/badge.svg)](https://github.com/huizezhang-sherry/cubble/actions)
<!-- badges: end -->

Cubble is a vector spatio-temporal data structure for data analysis and
visualisation.

## Installation

You can install the released version of cubble from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cubble")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("huizezhang-sherry/cubble")
```

## Example

Using `as_cubble()` to create a cubble in the nested form by supplying
the site identifier `key`, temporal identifier `index`, and the spatial
coordinates that defines the site `coords`.

``` r
library(cubble)
library(dplyr)
climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat))
#> # Cubble: id-wise: nested form
#> # Key:    id [5]
#> # Leaves: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id            lat  long  elev name           wmo_id ts                
#>   <chr>       <dbl> <dbl> <dbl> <chr>           <dbl> <list>            
#> 1 ASN00009021 -31.9  116.  15.4 perth airport   94610 <tibble [366 × 4]>
#> 2 ASN00010311 -31.9  117. 179   york            94623 <tibble [366 × 4]>
#> 3 ASN00010614 -32.9  117. 338   narrogin        94627 <tibble [366 × 4]>
#> 4 ASN00014015 -12.4  131.  30.4 darwin airport  94120 <tibble [366 × 4]>
#> 5 ASN00015131 -17.6  134. 220   elliott         94236 <tibble [366 × 4]>
```

Use `stretch()` to switch from the nested form to the long form. Long
form is convenient for time-wise computation, for exmaple, we can filter
to January records:

``` r
climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  stretch() %>% 
  filter(lubridate::month(date) == 1)
#> # Cubble: time-wise: long form
#> # Key:    id [5]
#> # Leaves: id [chr], lat [dbl], long [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00009021 2020-01-01     0   319   153
#>  2 ASN00009021 2020-01-02     0   249   164
#>  3 ASN00009021 2020-01-03     6   232   130
#>  4 ASN00009021 2020-01-04     0   284   124
#>  5 ASN00009021 2020-01-05     0   353   116
#>  6 ASN00009021 2020-01-06     0   348   131
#>  7 ASN00009021 2020-01-07     0   328   151
#>  8 ASN00009021 2020-01-08     0   304   174
#>  9 ASN00009021 2020-01-09     0   287   173
#> 10 ASN00009021 2020-01-10     0   326   158
#> # … with 145 more rows
```

Switch back to the list-column form with `tamp()`. List-column form is
suitable for site-wise manipulation, for example, we can add the count
of no-raining days for each station:

``` r
climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  stretch() %>% 
  filter(lubridate::month(date) == 1) %>% 
  tamp() %>% 
  mutate(zero_rain = sum(ts$prcp == 0, na.rm = TRUE))
#> # Cubble: id-wise: nested form
#> # Key:    id [5]
#> # Leaves: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id            lat  long  elev name           wmo_id ts                zero_rain
#>   <chr>       <dbl> <dbl> <dbl> <chr>           <dbl> <list>                <int>
#> 1 ASN00009021 -31.9  116.  15.4 perth airport   94610 <tibble [31 × 4]>        29
#> 2 ASN00010311 -31.9  117. 179   york            94623 <tibble [31 × 4]>        31
#> 3 ASN00010614 -32.9  117. 338   narrogin        94627 <tibble [31 × 4]>        30
#> 4 ASN00014015 -12.4  131.  30.4 darwin airport  94120 <tibble [31 × 4]>        12
#> 5 ASN00015131 -17.6  134. 220   elliott         94236 <tibble [31 × 4]>        14
```
