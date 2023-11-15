
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cubble <a href='https://huizezhang-sherry.github.io/cubble/'><img src='man/figures/logo.svg' align="right" height="138.5" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/cubble/workflows/R-CMD-check/badge.svg)](https://github.com/huizezhang-sherry/cubble/actions)
<!-- badges: end -->

The term spatio-temporal data can incorporate various spatial and
temporal characteristics and different data may require different data
structures for wrangling and analysis. The spatio-temporal data that
cubble addresses are those collected at unique fixed locations, allowing
for irregularity in the temporal dimension, such as the weather station
data.

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

## How cubble organises spatio-temporal data

Cubble organises spatio-temporal data in two structures: In a nested
cubble (spatial cubble), spatial variables are organised as columns and
temporal variables are nested within a specialised `ts` column:

    #> # cubble:   key: id [3], index: date, nested form
    #> # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
    #> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
    #>   id           long   lat  elev name              wmo_id ts               
    #>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
    #> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
    #> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
    #> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>

In a long cubble (temporal cubble), the temporal variables are expanded
into the long form, while the spatial variables are stored as a data
attribute:

    #> # cubble:   key: id [3], index: date, long form
    #> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
    #> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
    #>    id          date        prcp  tmax  tmin
    #>    <chr>       <date>     <dbl> <dbl> <dbl>
    #>  1 ASN00086038 2020-01-01     0  26.8  11  
    #>  2 ASN00086038 2020-01-02     0  26.3  12.2
    #>  3 ASN00086038 2020-01-03     0  34.5  12.7
    #>  4 ASN00086038 2020-01-04     0  29.3  18.8
    #>  5 ASN00086038 2020-01-05    18  16.1  12.5
    #>  6 ASN00086038 2020-01-06   104  17.5  11.1
    #>  7 ASN00086038 2020-01-07    14  20.7  12.1
    #>  8 ASN00086038 2020-01-08     0  26.4  16.4
    #>  9 ASN00086038 2020-01-09     0  33.1  17.4
    #> 10 ASN00086038 2020-01-10     0  34    19.6
    #> # ℹ 20 more rows

The two forms can be pivoted back and forth with the pair of verb:
`face_spatial()` and `face_temporal()`.

<img src="man/figures/cubble-operations.png" width="80%" style="display: block; margin: auto;" />

## Roadmap

- To learn more about the cubble class: [1. The cubble
  class](https://huizezhang-sherry.github.io/cubble/articles/cb1class.html)
- To create a cubble or coerce an existing R object into a cubble: [2.
  Creation and
  coercion](https://huizezhang-sherry.github.io/cubble/articles/cb2create.html)
- To incorporate sf or tsibble in a cubble: [3. Compatibility with
  tsibble and
  sf](https://huizezhang-sherry.github.io/cubble/articles/cb3tsibblesf.html)
- To create glyph map, match multiple data sources, and create
  interactive graphics with cubble: [4. Making a glyph
  map](https://huizezhang-sherry.github.io/cubble/articles/cb4glyph.html),
  [5. Matching different data
  sources](https://huizezhang-sherry.github.io/cubble/articles/cb5match.html),
  and [6. Interactive
  graphics](https://huizezhang-sherry.github.io/cubble/articles/cb6interactive.html)
