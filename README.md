
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cubble <a href='https://huizezhang-sherry.github.io/cubble/'><img src='man/figures/logo.svg' align="right" height="138.5" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/cubble/workflows/R-CMD-check/badge.svg)](https://github.com/huizezhang-sherry/cubble/actions)
<!-- badges: end -->

Cubble provides a data structure to manipulate spatio-temporal vector
data in two forms: nested form (spatial cubble) and long form (temporal
cubble). The nested cubble shows spatial variables as columns and nests
temporal variables in a `ts` column. The long cubble presents temporal
variables as columns and stores spatial variables as an attribute. The
two forms can be switched back and forth for manipulation on the spatial
and temporal dimension of the data.

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

## Create a cubble

`as_cubble()` creates a cubble in the nested form by supplying the
spatial identifier, `key`, temporal identifier, `index`, and the spatial
coordinates that defines the site, `coords`.

``` r
library(cubble)
library(dplyr)
nested <- climate_flat %>% as_cubble(key = id, index = date, coords = c(long, lat))
nested
#> # cubble:   key: id [3], index: date, nested form
#> # extent:   [144.8321, -37.98, 145.0964, -37.6655]
#> # temporal: prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
```

Check the vignette [1. The cubble class](articles/cb1class.html) for
more details on the class deisgn.

You can also create a cubble object by supplying the spatial and
temporal component separately in `make_cubble()`. This allows you to use
an `sf` object for the spatial component and a `tsibble` object for the
temporal component. The `sf` and `tsibble` class will be carried over to
the cubble object:

``` r
cb_nested <- make_cubble(spatial = stations_sf, temporal = meteo_ts)
class(cb_nested)
#> [1] "spatial_cubble_df" "cubble_df"         "sf"               
#> [4] "tbl_df"            "tbl"               "data.frame"
class(cb_nested$ts[[1]])
#> [1] "tbl_ts"     "tbl_df"     "tbl"        "data.frame"
```

Check the vignette [2. Creation and coercion](articles/cb2create.html)
for more on `make_cubble()` and coerce from other objects: netcdf,
[stars objects](https://r-spatial.github.io/stars/), and [sftime
objects](https://r-spatial.github.io/sftime/).

## Pivot between the spatial and temporal cubble

The pair of verbs, `face_temporal()` and `face_spatial()`, pivot the
cubble object between the spatial and temporal sides.

Check [Getting started](articles/cubble.html) and the vignette [3.
Making a glyph map](articles/cb3glyph.html) for operating on the spatial
and temporal cubble to make spatio-temporal visualisation.

## Misc

Cubble stands for “cubical tibble” as in a *tibble* object for
multivariate spatio-temporal data *cube*.
