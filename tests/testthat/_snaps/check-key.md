# check_key works

    Code
      check_key(stations, meteo)
    Output
      $paired
      # A tibble: 3 x 2
        spatial     temporal   
        <chr>       <chr>      
      1 ASN00086038 ASN00086038
      2 ASN00086077 ASN00086077
      3 ASN00086282 ASN00086282
      
      $potential_pairs
      # A tibble: 0 x 0
      
      $others
      $others$temporal
      character(0)
      
      $others$spatial
      character(0)
      
      
      attr(,"class")
      [1] "key_tbl" "list"   

---

    Code
      (check_res <- check_key(lga, covid))
    Output
      $paired
      # A tibble: 78 x 2
         spatial        temporal      
         <chr>          <chr>         
       1 Alpine (S)     Alpine (S)    
       2 Ararat (RC)    Ararat (RC)   
       3 Ballarat (C)   Ballarat (C)  
       4 Banyule (C)    Banyule (C)   
       5 Bass Coast (S) Bass Coast (S)
       6 Baw Baw (S)    Baw Baw (S)   
       7 Bayside (C)    Bayside (C)   
       8 Benalla (RC)   Benalla (RC)  
       9 Boroondara (C) Boroondara (C)
      10 Brimbank (C)   Brimbank (C)  
      # i 68 more rows
      
      $potential_pairs
      # A tibble: 2 x 2
        spatial             temporal    
        <chr>               <chr>       
      1 Kingston (C) (Vic.) Kingston (C)
      2 Latrobe (C) (Vic.)  Latrobe (C) 
      
      $others
      $others$spatial
      character(0)
      
      $others$temporal
      [1] "Interstate" "Overseas"   "Unknown"   
      
      
      attr(,"class")
      [1] "key_tbl" "list"   

---

    Code
      make_cubble(spatial = lga, temporal = covid, potential_match = check_res)
    Condition
      Warning:
      st_centroid assumes attributes are constant over geometries
    Message
      ! Some sites in the temporal table don't have spatial information
      ! Use `check_key()` to check on the unmatched key
      The cubble is created only with sites having both spatial and
      temporal information
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
      # cubble:   key: lga [80], index: date, nested form, [sf]
      # spatial:  [140.961682, -39.1339581, 149.976291, -33.9960517], WGS 84
      # temporal: date [date], n [dbl], avg_7day [dbl]
         lga             long   lat                                  geometry ts      
         <chr>          <dbl> <dbl>                            <GEOMETRY [°]> <list>  
       1 Alpine (S)      147. -36.9 POLYGON ((146.7258 -36.45922, 146.7198 -~ <tbl_ts>
       2 Ararat (RC)     143. -37.5 POLYGON ((143.1807 -37.73152, 143.0609 -~ <tbl_ts>
       3 Ballarat (C)    144. -37.5 POLYGON ((143.6622 -37.57241, 143.68 -37~ <tbl_ts>
       4 Banyule (C)     145. -37.7 POLYGON ((145.1357 -37.74091, 145.1437 -~ <tbl_ts>
       5 Bass Coast (S)  146. -38.5 MULTIPOLYGON (((145.5207 -38.30667, 145.~ <tbl_ts>
       6 Baw Baw (S)     146. -38.0 POLYGON ((145.765 -37.89858, 145.7414 -3~ <tbl_ts>
       7 Bayside (C)     145. -37.9 POLYGON ((144.985 -37.891, 144.9855 -37.~ <tbl_ts>
       8 Benalla (RC)    146. -36.6 POLYGON ((146.1314 -36.25512, 146.0369 -~ <tbl_ts>
       9 Boroondara (C)  145. -37.8 POLYGON ((145.1067 -37.79216, 145.0953 -~ <tbl_ts>
      10 Brimbank (C)    145. -37.7 POLYGON ((144.8862 -37.70775, 144.8594 -~ <tbl_ts>
      # i 70 more rows

