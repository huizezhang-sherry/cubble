# face_spatial() and face_temporal() works

    Code
      face_temporal(climate_mel)
    Output
      # cubble:   key: id [3], index: date, long form
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
      face_spatial(face_temporal(climate_mel))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

