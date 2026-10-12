# Check on key when create cubble from two components (spatial/temporal)

When creating a cubble from separate spatial and temporal component,
[`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md)
will informed users about potential disagreement of the key values in
the two datasets (some sites appear in one table but not the other).
This function summarises the key values into those match, potentially
can be matched, and can't be matched.

## Usage

``` r
check_key(spatial, temporal, by = NULL)
```

## Arguments

- spatial:

  a tibble object or an sf object, the spatial component containing the
  `key` and `coords` variable (`coords` can be automatically created
  from an `sf` object if not supplied).

- temporal:

  a tibble object or a tsibble object, the temporal component containing
  the `key` and `index` variable.

- by:

  in the syntax of the `by` argument in
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  used in
  [`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md)
  when the key variable has different names in the `spatial` and
  `temporal` data.

## Value

a list with three elements: 1) paired: a tibble of paired ID from
spatial and temporal data, 2) potential_pairs: a tibble of pairs that
could potentially match from both datasets, 3) others: other key values
that can't be matched in a list: others\$temporal and others\$spatial

## Examples

``` r
check_key(stations, meteo)
#> $paired
#> # A tibble: 3 × 2
#>   spatial     temporal   
#>   <chr>       <chr>      
#> 1 ASN00086038 ASN00086038
#> 2 ASN00086077 ASN00086077
#> 3 ASN00086282 ASN00086282
#> 
#> $potential_pairs
#> # A tibble: 0 × 0
#> 
#> $others
#> $others$temporal
#> character(0)
#> 
#> $others$spatial
#> character(0)
#> 
#> 
#> attr(,"class")
#> [1] "key_tbl" "list"   

# make_cubble() will prompt to use check_key if there are key mis-match:
colnames(lga) <- c("lga", "geometry")
cb <- make_cubble(spatial = lga, temporal = covid)
#> Warning: st_centroid assumes attributes are constant over geometries
#> ! Some sites in the spatial table don't have temporal information
#> ! Some sites in the temporal table don't have spatial information
#> ! Use `check_key()` to check on the unmatched key
#> The cubble is created only with sites having both spatial and
#> temporal information
(check_res <- check_key(lga, covid))
#> $paired
#> # A tibble: 78 × 2
#>    spatial        temporal      
#>    <chr>          <chr>         
#>  1 Alpine (S)     Alpine (S)    
#>  2 Ararat (RC)    Ararat (RC)   
#>  3 Ballarat (C)   Ballarat (C)  
#>  4 Banyule (C)    Banyule (C)   
#>  5 Bass Coast (S) Bass Coast (S)
#>  6 Baw Baw (S)    Baw Baw (S)   
#>  7 Bayside (C)    Bayside (C)   
#>  8 Benalla (RC)   Benalla (RC)  
#>  9 Boroondara (C) Boroondara (C)
#> 10 Brimbank (C)   Brimbank (C)  
#> # ℹ 68 more rows
#> 
#> $potential_pairs
#> # A tibble: 2 × 2
#>   spatial             temporal    
#>   <chr>               <chr>       
#> 1 Kingston (C) (Vic.) Kingston (C)
#> 2 Latrobe (C) (Vic.)  Latrobe (C) 
#> 
#> $others
#> $others$spatial
#> character(0)
#> 
#> $others$temporal
#> [1] "Interstate" "Overseas"   "Unknown"   
#> 
#> 
#> attr(,"class")
#> [1] "key_tbl" "list"   
make_cubble(spatial = lga, temporal = covid, potential_match = check_res)
#> Warning: st_centroid assumes attributes are constant over geometries
#> ! Some sites in the temporal table don't have spatial information
#> ! Use `check_key()` to check on the unmatched key
#> The cubble is created only with sites having both spatial and
#> temporal information
#> # cubble:   key: lga [80], index: date, nested form, [sf]
#> # spatial:  [140.96, -39.13, 149.98, -34], WGS 84
#> # temporal: date [date], n [dbl], avg_7day [dbl]
#>    lga             long   lat                                  geometry ts      
#>    <chr>          <dbl> <dbl>                            <GEOMETRY [°]> <list>  
#>  1 Alpine (S)      147. -36.9 POLYGON ((146.7258 -36.45922, 146.7198 -… <tbl_ts>
#>  2 Ararat (RC)     143. -37.5 POLYGON ((143.1807 -37.73152, 143.0609 -… <tbl_ts>
#>  3 Ballarat (C)    144. -37.5 POLYGON ((143.6622 -37.57241, 143.68 -37… <tbl_ts>
#>  4 Banyule (C)     145. -37.7 POLYGON ((145.1357 -37.74091, 145.1437 -… <tbl_ts>
#>  5 Bass Coast (S)  146. -38.5 MULTIPOLYGON (((145.5207 -38.30667, 145.… <tbl_ts>
#>  6 Baw Baw (S)     146. -38.0 POLYGON ((145.765 -37.89858, 145.7414 -3… <tbl_ts>
#>  7 Bayside (C)     145. -37.9 POLYGON ((144.985 -37.891, 144.9855 -37.… <tbl_ts>
#>  8 Benalla (RC)    146. -36.6 POLYGON ((146.1314 -36.25512, 146.0369 -… <tbl_ts>
#>  9 Boroondara (C)  145. -37.8 POLYGON ((145.1067 -37.79216, 145.0953 -… <tbl_ts>
#> 10 Brimbank (C)    145. -37.7 POLYGON ((144.8862 -37.70775, 144.8594 -… <tbl_ts>
#> # ℹ 70 more rows
```
