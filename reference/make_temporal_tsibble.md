# Update the temporal cubble to include the tsibble class (`tbl_ts`)

Update the temporal cubble to include the tsibble class (`tbl_ts`)

## Usage

``` r
make_temporal_tsibble(x)
```

## Arguments

- x:

  object of class `temporal_cubble_df`

## Examples

``` r
climate_mel |> face_temporal() |> make_temporal_tsibble()
#> # cubble:   key: id [3], index: date, long form, [tsibble]
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
```
