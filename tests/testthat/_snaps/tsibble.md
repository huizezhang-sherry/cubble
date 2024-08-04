# cubble works for tsibble objects

    Code
      make_temporal_tsibble(face_temporal(climate_mel))
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
         id          date        prcp  tmax  tmin
         <chr>       <date>     <dbl> <dbl> <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11  
       2 ASN00086038 2020-01-02     0  26.3  12.2
       3 ASN00086038 2020-01-03     0  34.5  12.7
       4 ASN00086038 2020-01-04     0  29.3  18.8
       5 ASN00086038 2020-01-05    18  16.1  12.5
       6 ASN00086038 2020-01-06   104  17.5  11.1
       7 ASN00086038 2020-01-07    14  20.7  12.1
       8 ASN00086038 2020-01-08     0  26.4  16.4
       9 ASN00086038 2020-01-09     0  33.1  17.4
      10 ASN00086038 2020-01-10     0  34    19.6
      # i 20 more rows

---

    Code
      fill_gaps(face_temporal(climate_aus))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key)
      
        # Now:
        data %>% select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [639], index: date, long form
      # temporal: 2020-01-01 -- 2020-12-31 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp  tmax  tmin
         <chr>       <date>     <dbl> <dbl> <dbl>
       1 ASN00001006 2020-01-01   164  38.3  25.3
       2 ASN00001006 2020-01-02     0  40.6  30.5
       3 ASN00001006 2020-01-03    16  39.7  27.2
       4 ASN00001006 2020-01-04     0  38.2  27.3
       5 ASN00001006 2020-01-05     2  39.3  26.7
       6 ASN00001006 2020-01-06    60  32.9  25.6
       7 ASN00001006 2020-01-07   146  34.1  25.5
       8 ASN00001006 2020-01-08    40  36.6  26.2
       9 ASN00001006 2020-01-09     0  38.2  27.6
      10 ASN00001006 2020-01-10     0  38.9  29.7
      # i 231,453 more rows

---

    Code
      scan_gaps(face_temporal(climate_aus))
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
      # cubble:   key: id [639], index: date, long form, [tsibble]
      # temporal: 2020-03-01 -- 2020-10-31 [1D], has gaps!
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date      
         <chr>       <date>    
       1 ASN00004090 2020-03-01
       2 ASN00004090 2020-03-02
       3 ASN00004090 2020-03-03
       4 ASN00004090 2020-03-04
       5 ASN00004090 2020-03-05
       6 ASN00004090 2020-03-06
       7 ASN00004090 2020-03-07
       8 ASN00004090 2020-03-08
       9 ASN00004090 2020-03-09
      10 ASN00004090 2020-03-10
      # i 693 more rows

---

    Code
      res <- tsibble::index_by(face_temporal(make_cubble(stations_sf, meteo_ts)),
      week = lubridate::week(date))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key)
      
        # Now:
        data %>% select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

---

    Code
      summarise(res, tmax = mean(tmax, na.rm = TRUE))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key)
      
        # Now:
        data %>% select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: week, long form, [tsibble]
      # temporal: 1 -- 2 [1], no gaps
      # spatial:  elev [dbl], name [chr], wmo_id [dbl], long [dbl], lat [dbl],
      #   geometry [POINT [°]]
         week id           tmax
        <dbl> <chr>       <dbl>
      1     1 ASN00086038  24.5
      2     1 ASN00086077  23.7
      3     1 ASN00086282  24.7
      4     2 ASN00086038  31.2
      5     2 ASN00086077  30.5
      6     2 ASN00086282  31.2

