
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cubble

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/cubble/workflows/R-CMD-check/badge.svg)](https://github.com/huizezhang-sherry/cubble/actions)
<!-- badges: end -->

Cubble is a vector spatio-temporal data structure for data analysis.

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

Using `tamp()` to create a cubble in the nested form by supply the
variable that identifies each site:

``` r
library(cubble)
library(dplyr)
climate_flat %>% 
  tamp(station) 
#> # Cubble: station-wise: nested form
#> # Group:  station [2]
#> # Leaves: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   station       lat  long elevation name                ts                
#>   <fct>       <dbl> <dbl>     <dbl> <fct>               <list>            
#> 1 ASN00001019 -14.3  127.        23 kalumburu           <tibble [366 × 4]>
#> 2 ASN00002012 -18.2  128.       422 halls creek airport <tibble [366 × 4]>
```

Use `stretch()` to switch to the long form and filter to January
records:

``` r
climate_flat %>% 
  tamp(station) %>% 
  stretch() %>% 
  filter(lubridate::month(date) == 1)
#> # Cubble: time-wise: long form
#> # Group:  station [2]
#> # Leaves: station [fct], lat [dbl], long [dbl], elevation [dbl], name [fct]
#>    station     date        prcp  tmax  tmin
#>    <fct>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00001019 2020-01-01    46  38.6  25.1
#>  2 ASN00001019 2020-01-02     0  38.8  28.1
#>  3 ASN00001019 2020-01-03   266  37.9  23.6
#>  4 ASN00001019 2020-01-04     0  34.3  26.2
#>  5 ASN00001019 2020-01-05    46  35.4  26.7
#>  6 ASN00001019 2020-01-06   760  27.5  24.8
#>  7 ASN00001019 2020-01-07  1168  31.6  23.2
#>  8 ASN00001019 2020-01-08  1178  32.7  24  
#>  9 ASN00001019 2020-01-09    48  34.2  25.1
#> 10 ASN00001019 2020-01-10     0  35.4  26.7
#> # … with 52 more rows
```

Switch back to the list-column form with `tamp()` and add a count on the
number of day with no rain for each station:

``` r
climate_flat %>% 
  tamp(station) %>% 
  stretch() %>% 
  filter(lubridate::month(date) == 1) %>% 
  tamp() %>% 
  mutate(zero_rain = sum(ts$prcp == 0, na.rm = TRUE))
#> # Cubble: station-wise: nested form
#> # Group:  station [2]
#>   station       lat  long elevation name               ts              zero_rain
#>   <fct>       <dbl> <dbl>     <dbl> <fct>              <list>              <int>
#> 1 ASN00001019 -14.3  127.        23 kalumburu          <tibble [31 × …        12
#> 2 ASN00002012 -18.2  128.       422 halls creek airpo… <tibble [31 × …        13
```
