# unfold() works

    Code
      unfold(face_temporal(climate_mel), long, lat)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp  tmax  tmin  long   lat
         <chr>       <date>     <dbl> <dbl> <dbl> <dbl> <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11    145. -37.7
       2 ASN00086038 2020-01-02     0  26.3  12.2  145. -37.7
       3 ASN00086038 2020-01-03     0  34.5  12.7  145. -37.7
       4 ASN00086038 2020-01-04     0  29.3  18.8  145. -37.7
       5 ASN00086038 2020-01-05    18  16.1  12.5  145. -37.7
       6 ASN00086038 2020-01-06   104  17.5  11.1  145. -37.7
       7 ASN00086038 2020-01-07    14  20.7  12.1  145. -37.7
       8 ASN00086038 2020-01-08     0  26.4  16.4  145. -37.7
       9 ASN00086038 2020-01-09     0  33.1  17.4  145. -37.7
      10 ASN00086038 2020-01-10     0  34    19.6  145. -37.7
      # i 20 more rows

---

    Code
      unfold(face_temporal(climate_mel), dplyr::starts_with("l"))
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp  tmax  tmin  long   lat
         <chr>       <date>     <dbl> <dbl> <dbl> <dbl> <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11    145. -37.7
       2 ASN00086038 2020-01-02     0  26.3  12.2  145. -37.7
       3 ASN00086038 2020-01-03     0  34.5  12.7  145. -37.7
       4 ASN00086038 2020-01-04     0  29.3  18.8  145. -37.7
       5 ASN00086038 2020-01-05    18  16.1  12.5  145. -37.7
       6 ASN00086038 2020-01-06   104  17.5  11.1  145. -37.7
       7 ASN00086038 2020-01-07    14  20.7  12.1  145. -37.7
       8 ASN00086038 2020-01-08     0  26.4  16.4  145. -37.7
       9 ASN00086038 2020-01-09     0  33.1  17.4  145. -37.7
      10 ASN00086038 2020-01-10     0  34    19.6  145. -37.7
      # i 20 more rows

---

    Code
      unfold(climate_mel)
    Condition
      Error in `unfold()`:
      ! `unfold()` currently can only be used on a long form cubble (class `spatial_cubble_df`)

---

    Code
      unfold(make_temporal_tsibble(face_temporal(climate_mel)), long, lat)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key)
      
        # Now:
        data %>% select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key)
      
        # Now:
        data %>% select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, long form, [tsibble]
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp  tmax  tmin  long   lat
         <chr>       <date>     <dbl> <dbl> <dbl> <dbl> <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11    145. -37.7
       2 ASN00086038 2020-01-02     0  26.3  12.2  145. -37.7
       3 ASN00086038 2020-01-03     0  34.5  12.7  145. -37.7
       4 ASN00086038 2020-01-04     0  29.3  18.8  145. -37.7
       5 ASN00086038 2020-01-05    18  16.1  12.5  145. -37.7
       6 ASN00086038 2020-01-06   104  17.5  11.1  145. -37.7
       7 ASN00086038 2020-01-07    14  20.7  12.1  145. -37.7
       8 ASN00086038 2020-01-08     0  26.4  16.4  145. -37.7
       9 ASN00086038 2020-01-09     0  33.1  17.4  145. -37.7
      10 ASN00086038 2020-01-10     0  34    19.6  145. -37.7
      # i 20 more rows

