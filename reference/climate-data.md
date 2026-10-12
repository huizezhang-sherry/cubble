# Australia climate data

`climate_aus`: daily measure on precipitation (`prcp`), maximum
temperature (`tmax`), and minimum temperature (`tmin`) in 2020 for 639
stations. `historical_tmax`: daily maximum temperature (`tmax`) for 75
stations in Victoria and New South Wales for two periods: 1971-1975 and
2016-2020.

## Usage

``` r
climate_aus

historical_tmax
```

## Format

An object of class `spatial_cubble_df` (inherits from `cubble_df`,
`tbl_df`, `tbl`, `data.frame`) with 639 rows and 7 columns.

An object of class `spatial_cubble_df` (inherits from `cubble_df`,
`tbl_df`, `tbl`, `data.frame`) with 75 rows and 7 columns.

## Details

- id:

  station ID, "ASN000" are international paddings, the next two digits
  (digit 8-9) indicates the states the station is in: Western Australia:
  01-13, Northern Territory: 14-15, South Australia: 16-26, Queensland:
  27-45, New South Wales: 46-75, Victoria: 76-90, Tasmania: 91-99. See
  http://www.bom.gov.au/climate/cdo/about/site-num.shtml

- lat:

  latitude of the stations, in degree

- long:

  longitude of the stations, in degree

- elev:

  elevation of the stations

- name:

  station name

- wmo_id:

  the world meteorological organisation (WMO) station number

- ts:

  For `climate_aus`: date, prcp, tmax, and tmin, for `historical_tmax`:
  date and tmax

## Examples

``` r
climate_aus |> face_temporal() |> face_spatial()
#> # cubble:   key: id [639], index: date, nested form
#> # spatial:  [113.53, -43.66, 153.64, -10.05], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>    id           long   lat  elev name                wmo_id ts                
#>    <chr>       <dbl> <dbl> <dbl> <chr>                <dbl> <list>            
#>  1 ASN00001006  128. -15.5   3.8 wyndham aero         95214 <tibble [366 × 4]>
#>  2 ASN00001007  126. -13.8   6   troughton island     94102 <tibble [366 × 4]>
#>  3 ASN00001018  126. -16.4 546   mount elizabeth      94211 <tibble [366 × 4]>
#>  4 ASN00001019  127. -14.3  23   kalumburu            94100 <tibble [366 × 4]>
#>  5 ASN00001020  126. -14.1  51   truscott             95101 <tibble [366 × 4]>
#>  6 ASN00001025  126. -15.4 385   doongan              94215 <tibble [366 × 4]>
#>  7 ASN00002012  128. -18.2 422   halls creek airport  94212 <tibble [366 × 4]>
#>  8 ASN00002032  128. -17.0 203   warmun               94213 <tibble [366 × 4]>
#>  9 ASN00002056  129. -15.8  44   kununurra aero       94216 <tibble [366 × 4]>
#> 10 ASN00002064  128. -16.6 164   argyle aerodrome     94217 <tibble [366 × 4]>
#> # ℹ 629 more rows
```
