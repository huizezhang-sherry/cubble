# creating cubble works

    Code
      make_cubble(spatial = stations, temporal = meteo, key = id, index = date,
        coords = c(long, lat))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(by)
      
        # Now:
        data |> select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

