
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cubble

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/cubble/workflows/R-CMD-check/badge.svg)](https://github.com/huizezhang-sherry/cubble/actions)
<!-- badges: end -->

Cubble provides a new data structure to manipulate spatio-temporal
vector data. It arranges variables into two forms: nested form and long
form. The nested form shows each site in a row and time invariant
variables as columns. The time varying variables are nested into a `ts`
column. In the long form, each row is cross-identified by the site and
time, time varying variables are presented, and time invariant variables
are stored as an attribute. The two forms can be switched back and forth
for manipulation on the spatial and temporal dimension of the data.

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

`as_cubble()` creates a cubble in the nested form by supplying the
spatial identifier, `key`, temporal identifier, `index`, and the spatial
coordinates that defines the site, `coords`.

``` r
library(cubble)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.2
nested <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat))
nested
#> # cubble:   id [5]: nested form
#> # bbox:     [115.97, -32.94, 133.55, -12.42]
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id            lat  long  elev name           wmo_id ts                
#>   <chr>       <dbl> <dbl> <dbl> <chr>           <dbl> <list>            
#> 1 ASN00009021 -31.9  116.  15.4 perth airport   94610 <tibble [366 × 4]>
#> 2 ASN00010311 -31.9  117. 179   york            94623 <tibble [366 × 4]>
#> 3 ASN00010614 -32.9  117. 338   narrogin        94627 <tibble [366 × 4]>
#> 4 ASN00014015 -12.4  131.  30.4 darwin airport  94120 <tibble [366 × 4]>
#> 5 ASN00015131 -17.6  134. 220   elliott         94236 <tibble [366 × 4]>
```

`face_temporal()` switches a cubble from the nested form to the long
form. The long form cubble is for operations whose output is
cross-identified by `key` and `index`, for example, filtering January
records:

``` r
long <- nested %>% 
  face_temporal() %>% 
  filter(lubridate::month(date) == 1)
long
#> # cubble:  date, id [5]: long form
#> # bbox:    [115.97, -32.94, 133.55, -12.42]
#> # spatial: lat [dbl], long [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00009021 2020-01-01     0  31.9  15.3
#>  2 ASN00009021 2020-01-02     0  24.9  16.4
#>  3 ASN00009021 2020-01-03     6  23.2  13  
#>  4 ASN00009021 2020-01-04     0  28.4  12.4
#>  5 ASN00009021 2020-01-05     0  35.3  11.6
#>  6 ASN00009021 2020-01-06     0  34.8  13.1
#>  7 ASN00009021 2020-01-07     0  32.8  15.1
#>  8 ASN00009021 2020-01-08     0  30.4  17.4
#>  9 ASN00009021 2020-01-09     0  28.7  17.3
#> 10 ASN00009021 2020-01-10     0  32.6  15.8
#> # … with 145 more rows
```

`face_spatial()` switches the long cubble back to the nested cubble. The
nested form is for operations whose output is only identified by the
`key`, for example, mutating the average maximum temperature in January:

``` r
long %>% 
  face_spatial() %>% 
  mutate(avg_max = mean(ts$tmax, na.rm = TRUE))
#> # cubble:   id [5]: nested form
#> # bbox:     [115.97, -32.94, 133.55, -12.42]
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id            lat  long  elev name           wmo_id ts                avg_max
#>   <chr>       <dbl> <dbl> <dbl> <chr>           <dbl> <list>              <dbl>
#> 1 ASN00009021 -31.9  116.  15.4 perth airport   94610 <tibble [31 × 4]>    31.6
#> 2 ASN00010311 -31.9  117. 179   york            94623 <tibble [31 × 4]>    34.6
#> 3 ASN00010614 -32.9  117. 338   narrogin        94627 <tibble [31 × 4]>    31.4
#> 4 ASN00014015 -12.4  131.  30.4 darwin airport  94120 <tibble [31 × 4]>    32.8
#> 5 ASN00015131 -17.6  134. 220   elliott         94236 <tibble [31 × 4]>    38.5
```

## Misc

- **Naming**: Cubble stands for “cubical tibble” and you can think of
multivariate spatio-temporal data as a *cube* with three axes: variable,
location, and time.

- **weatherdata**: Some examples use larger climate data, which are stored
in the `weatherdata` repo, you can download the repo with

``` r
remotes::install_github("huizezhang-sherry/weatherdata")
```
