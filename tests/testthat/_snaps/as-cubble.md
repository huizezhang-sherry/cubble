# as_cubble() works

    Code
      as_cubble(as.data.frame(climate_flat), key = id, index = date, coords = c(long,
        lat))
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      as_cubble(climate_flat, key = id, index = date, coords = c(long, lat))
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      as_cubble(nest(climate_flat, data = date:tmin), key = id, index = date, coords = c(
        long, lat))
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      as_cubble(nest(climate_flat, data = prcp:tmin), key = id, index = date, coords = c(
        long, lat))
    Condition
      Error in `as_cubble()`:
      ! Can't' find the index variable in the data. Please check.

---

    Code
      as_cubble(dt, coords = c(long, lat))
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tbl_ts [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tbl_ts [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tbl_ts [10 x 4]>

---

    Code
      as_cubble(dt, key = id, index = date)
    Output
      # cubble:   key: id [3], index: date, nested form, [sf]
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], WGS 84
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           elev name   wmo_id  long   lat            geometry ts      
      * <chr>       <dbl> <chr>   <dbl> <dbl> <dbl>         <POINT [°]> <list>  
      1 ASN00086038  78.4 essen~  95866  145. -37.7 (144.9066 -37.7276) <tibble>
      2 ASN00086077  12.1 moora~  94870  145. -38.0   (145.0964 -37.98) <tibble>
      3 ASN00086282 113.  melbo~  94866  145. -37.7 (144.8321 -37.6655) <tibble>

---

    Code
      res
    Output
      # cubble:   key: id [122848], index: band, nested form
      # spatial:  [288790.500000803, 9110743.00002899, 298708.50000055,
      #   9120746.50002874], Missing CRS!
      # temporal: band [int], L7_ETMs.tif [dbl]
               x        y     id ts              
           <dbl>    <dbl>  <int> <list>          
       1 288791. 9120747. 122500 <tibble [6 x 2]>
       2 288819. 9120747. 122501 <tibble [6 x 2]>
       3 288848. 9120747. 122502 <tibble [6 x 2]>
       4 288876. 9120747. 122503 <tibble [6 x 2]>
       5 288905. 9120747. 122504 <tibble [6 x 2]>
       6 288933. 9120747. 122505 <tibble [6 x 2]>
       7 288962. 9120747. 122506 <tibble [6 x 2]>
       8 288990. 9120747. 122507 <tibble [6 x 2]>
       9 289019. 9120747. 122508 <tibble [6 x 2]>
      10 289047. 9120747. 122509 <tibble [6 x 2]>
      # i 122,838 more rows

---

    Code
      face_temporal(res)
    Output
      # cubble:   key: id [122848], index: band, long form
      # temporal: 1 -- 6 [1], no gaps
      # spatial:  x [dbl], y [dbl]
             id  band L7_ETMs.tif
          <int> <int>       <dbl>
       1 122500     1          69
       2 122500     2          56
       3 122500     3          46
       4 122500     4          79
       5 122500     5          86
       6 122500     6          46
       7 122501     1          69
       8 122501     2          57
       9 122501     3          49
      10 122501     4          75
      # i 737,078 more rows

---

    Code
      as_cubble(raw)
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"id"` instead of `.data$id`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"long"` instead of `.data$long`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"lat"` instead of `.data$lat`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"time"` instead of `.data$time`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`
    Message
      i More than 10,000 keys: only use the first key to test spatial &
      temporal variables.
    Output
      # cubble:   key: id [26565], index: time, nested form
      # spatial:  [113, -53, 153, -12], Missing CRS!
      # temporal: time [date], q [dbl], z [dbl]
            id  long   lat ts              
         <int> <dbl> <dbl> <list>          
       1     1  113    -12 <tibble [6 x 3]>
       2     2  113.   -12 <tibble [6 x 3]>
       3     3  114.   -12 <tibble [6 x 3]>
       4     4  114.   -12 <tibble [6 x 3]>
       5     5  114    -12 <tibble [6 x 3]>
       6     6  114.   -12 <tibble [6 x 3]>
       7     7  114.   -12 <tibble [6 x 3]>
       8     8  115.   -12 <tibble [6 x 3]>
       9     9  115    -12 <tibble [6 x 3]>
      10    10  115.   -12 <tibble [6 x 3]>
      # i 26,555 more rows

---

    Code
      dt <- as_cubble(raw, vars = c("q", "z"), long_range = seq(113, 153, 3),
      lat_range = seq(-53, -12, 3))
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"id"` instead of `.data$id`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"long"` instead of `.data$long`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"lat"` instead of `.data$lat`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"time"` instead of `.data$time`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"var"` instead of `.data$var`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"Freq"` instead of `.data$Freq`

---

    Code
      as_cubble(dt, key = id, index = date)
    Output
      # cubble:   key: id [3], index: date, nested form, [sf]
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], WGS 84
      # temporal: prcp [dbl], tmax [dbl], tmin [dbl], date [date]
        id           elev name   wmo_id            geometry  long   lat ts      
        <chr>       <dbl> <chr>   <dbl>         <POINT [°]> <dbl> <dbl> <list>  
      1 ASN00086038  78.4 essen~  95866 (144.9066 -37.7276)  145. -37.7 <tibble>
      2 ASN00086077  12.1 moora~  94870   (145.0964 -37.98)  145. -38.0 <tibble>
      3 ASN00086282 113.  melbo~  94866 (144.8321 -37.6655)  145. -37.7 <tibble>

---

    Code
      as_cubble(data_stars, key = id, index = time)
    Output
      # cubble:   key: id [2], index: time, nested form, [sf]
      # spatial:  [130, -44, 140, -38], WGS 84
      # temporal: time [date], var1 [dbl]
           id  long   lat     station ts              
      * <int> <dbl> <dbl> <POINT [°]> <list>          
      1     1   130   -38   (130 -38) <tibble [5 x 2]>
      2     2   140   -44   (140 -44) <tibble [5 x 2]>

