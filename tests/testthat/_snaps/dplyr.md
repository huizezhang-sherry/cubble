# dplyr verbs work

    Code
      filter(cb_nested, elev > 40)
    Output
      # cubble:   key: id [2], index: date, nested form
      # spatial:  [144.8321, -37.7276, 144.9066, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      filter(cb_long, prcp > 0)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-05 -- 2020-01-07 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
        id          date        prcp  tmax  tmin
        <chr>       <date>     <dbl> <dbl> <dbl>
      1 ASN00086038 2020-01-05    18  16.1  12.5
      2 ASN00086038 2020-01-06   104  17.5  11.1
      3 ASN00086038 2020-01-07    14  20.7  12.1
      4 ASN00086077 2020-01-05    20  17.4  12.7
      5 ASN00086077 2020-01-06   122  17.8  11.8
      6 ASN00086077 2020-01-07     6  20.3  12.6
      7 ASN00086282 2020-01-05    16  15.7  12  
      8 ASN00086282 2020-01-06    90  17.3  11.5
      9 ASN00086282 2020-01-07     6  19.9  11.8

---

    Code
      mutate(cb_nested, elev2 = elev + 10)
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts                elev2
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>            <dbl>
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>  88.4
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>  22.1
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]> 123. 

---

    Code
      mutate(cb_long, prcp2 = prcp + 10)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp  tmax  tmin prcp2
         <chr>       <date>     <dbl> <dbl> <dbl> <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11      10
       2 ASN00086038 2020-01-02     0  26.3  12.2    10
       3 ASN00086038 2020-01-03     0  34.5  12.7    10
       4 ASN00086038 2020-01-04     0  29.3  18.8    10
       5 ASN00086038 2020-01-05    18  16.1  12.5    28
       6 ASN00086038 2020-01-06   104  17.5  11.1   114
       7 ASN00086038 2020-01-07    14  20.7  12.1    24
       8 ASN00086038 2020-01-08     0  26.4  16.4    10
       9 ASN00086038 2020-01-09     0  33.1  17.4    10
      10 ASN00086038 2020-01-10     0  34    19.6    10
      # i 20 more rows

---

    Code
      arrange(cb_nested, wmo_id)
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>

---

    Code
      arrange(cb_long, prcp)
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
       5 ASN00086038 2020-01-08     0  26.4  16.4
       6 ASN00086038 2020-01-09     0  33.1  17.4
       7 ASN00086038 2020-01-10     0  34    19.6
       8 ASN00086077 2020-01-01     0  24.7  10  
       9 ASN00086077 2020-01-02     0  24.8  11.8
      10 ASN00086077 2020-01-03     0  35    12.2
      # i 20 more rows

---

    Code
      cb_long |> group_by(first_5 = ifelse(lubridate::day(date) <= 5, 1, 2)) |> 
        summarise(tmax = mean(tmax))
    Output
      # cubble:   key: id [3], index: first_5, long form, groups: first_5 [2]
      # temporal: 1 -- 2 [1], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
        first_5 id           tmax
          <dbl> <chr>       <dbl>
      1       1 ASN00086038  26.6
      2       1 ASN00086077  25.5
      3       1 ASN00086282  27.1
      4       2 ASN00086038  26.3
      5       2 ASN00086077  25.9
      6       2 ASN00086282  26.2

---

    Code
      cb_long |> mutate(first_5 = ifelse(lubridate::day(date) <= 5, 1, 2)) |> 
        summarise(t = mean(tmax), .by = first_5)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.by)
      
        # Now:
        data |> select(all_of(.by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date           t
         <chr>       <date>     <dbl>
       1 ASN00086038 2020-01-01  26.8
       2 ASN00086038 2020-01-02  26.3
       3 ASN00086038 2020-01-03  34.5
       4 ASN00086038 2020-01-04  29.3
       5 ASN00086038 2020-01-05  16.1
       6 ASN00086038 2020-01-06  17.5
       7 ASN00086038 2020-01-07  20.7
       8 ASN00086038 2020-01-08  26.4
       9 ASN00086038 2020-01-09  33.1
      10 ASN00086038 2020-01-10  34  
      # i 20 more rows

---

    Code
      select(cb_nested, name)
    Message
      i Missing attribute `id`, `long`, `lat`, and `ts`, add it back.
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat ts                name             
        <chr>       <dbl> <dbl> <list>            <chr>            
      1 ASN00086038  145. -37.7 <tibble [10 x 4]> essendon airport 
      2 ASN00086077  145. -38.0 <tibble [10 x 4]> moorabbin airport
      3 ASN00086282  145. -37.7 <tibble [10 x 4]> melbourne airport

---

    Code
      select(cb_nested, -id, -name)
    Message
      i Missing attribute `id`, add it back.
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts               
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.   94866 <tibble [10 x 4]>

---

    Code
      select(cb_long, prcp)
    Message
      i Missing attribute `id` and `date`, add it back.
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date        prcp
         <chr>       <date>     <dbl>
       1 ASN00086038 2020-01-01     0
       2 ASN00086038 2020-01-02     0
       3 ASN00086038 2020-01-03     0
       4 ASN00086038 2020-01-04     0
       5 ASN00086038 2020-01-05    18
       6 ASN00086038 2020-01-06   104
       7 ASN00086038 2020-01-07    14
       8 ASN00086038 2020-01-08     0
       9 ASN00086038 2020-01-09     0
      10 ASN00086038 2020-01-10     0
      # i 20 more rows

---

    Code
      select(cb_long, -prcp, -date)
    Message
      i Missing attribute `date`, add it back.
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         date       id           tmax  tmin
         <date>     <chr>       <dbl> <dbl>
       1 2020-01-01 ASN00086038  26.8  11  
       2 2020-01-02 ASN00086038  26.3  12.2
       3 2020-01-03 ASN00086038  34.5  12.7
       4 2020-01-04 ASN00086038  29.3  18.8
       5 2020-01-05 ASN00086038  16.1  12.5
       6 2020-01-06 ASN00086038  17.5  11.1
       7 2020-01-07 ASN00086038  20.7  12.1
       8 2020-01-08 ASN00086038  26.4  16.4
       9 2020-01-09 ASN00086038  33.1  17.4
      10 2020-01-10 ASN00086038  34    19.6
      # i 20 more rows

---

    Code
      rename(cb_nested, elev2 = elev)
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat elev2 name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      rename(cb_long, prcp2 = prcp)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date       prcp2  tmax  tmin
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
      rename(cb_nested, id2 = id)
    Output
      # cubble:   key: id2 [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id2          long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      rename(cb_long, date2 = date)
    Output
      # cubble:   key: id [3], index: date2, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
         id          date2       prcp  tmax  tmin
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
      left_join(nested, df1, by = "id")
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts                name             
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
      1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 x 4]> essendon airport 
      2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 x 4]> moorabbin airport
      3 ASN00086282  145. -37.7 113.   94866 <tibble [10 x 4]> <NA>             

---

    Code
      right_join(nested, df1, by = "id")
    Output
      # cubble:   key: id [2], index: date, nested form
      # spatial:  [144.9066, -37.98, 145.0964, -37.7276], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts                name             
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
      1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 x 4]> essendon airport 
      2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 x 4]> moorabbin airport

---

    Code
      inner_join(nested, df1, by = "id")
    Output
      # cubble:   key: id [2], index: date, nested form
      # spatial:  [144.9066, -37.98, 145.0964, -37.7276], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts                name             
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
      1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 x 4]> essendon airport 
      2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 x 4]> moorabbin airport

---

    Code
      full_join(nested, df1, by = "id")
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts                name             
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
      1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 x 4]> essendon airport 
      2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 x 4]> moorabbin airport
      3 ASN00086282  145. -37.7 113.   94866 <tibble [10 x 4]> <NA>             

---

    Code
      anti_join(nested, df1, by = "id")
    Output
      # cubble:   key: id [1], index: date, nested form
      # spatial:  [144.8321, -37.6655, 144.8321, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev wmo_id ts               
        <chr>       <dbl> <dbl> <dbl>  <dbl> <list>           
      1 ASN00086282  145. -37.7  113.  94866 <tibble [10 x 4]>

---

    Code
      bind_rows(df1, df2)
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
      bind_rows(df1, df2)
    Output
      # cubble:   key: id [1], index: date, long form
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
      relocate(cb_nested, ts, .before = name)
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev ts                name              wmo_id
        <chr>       <dbl> <dbl> <dbl> <list>            <chr>              <dbl>
      1 ASN00086038  145. -37.7  78.4 <tibble [10 x 4]> essendon airport   95866
      2 ASN00086077  145. -38.0  12.1 <tibble [10 x 4]> moorabbin airport  94870
      3 ASN00086282  145. -37.7 113.  <tibble [10 x 4]> melbourne airport  94866

---

    Code
      relocate(face_temporal(cb_nested), tmin)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
          tmin id          date        prcp  tmax
         <dbl> <chr>       <date>     <dbl> <dbl>
       1  11   ASN00086038 2020-01-01     0  26.8
       2  12.2 ASN00086038 2020-01-02     0  26.3
       3  12.7 ASN00086038 2020-01-03     0  34.5
       4  18.8 ASN00086038 2020-01-04     0  29.3
       5  12.5 ASN00086038 2020-01-05    18  16.1
       6  11.1 ASN00086038 2020-01-06   104  17.5
       7  12.1 ASN00086038 2020-01-07    14  20.7
       8  16.4 ASN00086038 2020-01-08     0  26.4
       9  17.4 ASN00086038 2020-01-09     0  33.1
      10  19.6 ASN00086038 2020-01-10     0  34  
      # i 20 more rows

---

    Code
      slice_head(cb_nested, n = 2)
    Output
      # cubble:   key: id [2], index: date, nested form
      # spatial:  [144.9066, -37.98, 145.0964, -37.7276], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>

---

    Code
      slice_tail(cb_nested, n = 2)
    Output
      # cubble:   key: id [2], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      2 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      slice_max(cb_nested, elev)
    Output
      # cubble:   key: id [1], index: date, nested form
      # spatial:  [144.8321, -37.6655, 144.8321, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086282  145. -37.7  113. melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      slice_min(cb_nested, elev)
    Output
      # cubble:   key: id [1], index: date, nested form
      # spatial:  [145.0964, -37.98, 145.0964, -37.98], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>

---

    Code
      rowwise(cb_nested)
    Output
      # cubble:   key: id [3], index: date, nested form, groups: rowwise
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts               
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 x 4]>
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 x 4]>
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 x 4]>

---

    Code
      rowwise(cb_long)
    Output
      # cubble:   key: id [3], index: date, long form, groups: rowwise
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
      (res <- group_by(mutate(cb_nested, group1 = c(1, 1, 2)), group1))
    Output
      # cubble:   key: id [3], index: date, nested form, groups: group1 [2]
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts       group1
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>    <dbl>
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble>      1
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble>      1
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble>      2

---

    Code
      ungroup(res)
    Output
      # cubble:   key: id [3], index: date, nested form
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], Missing CRS!
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name              wmo_id ts       group1
        <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>    <dbl>
      1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble>      1
      2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble>      1
      3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble>      2

---

    Code
      (res2 <- group_by(unfold(face_temporal(res), group1), group1))
    Output
      # cubble:   key: id [3], index: date, long form, groups: group1 [2]
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl], group1
      #   [dbl]
         id          date        prcp  tmax  tmin group1
         <chr>       <date>     <dbl> <dbl> <dbl>  <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11        1
       2 ASN00086038 2020-01-02     0  26.3  12.2      1
       3 ASN00086038 2020-01-03     0  34.5  12.7      1
       4 ASN00086038 2020-01-04     0  29.3  18.8      1
       5 ASN00086038 2020-01-05    18  16.1  12.5      1
       6 ASN00086038 2020-01-06   104  17.5  11.1      1
       7 ASN00086038 2020-01-07    14  20.7  12.1      1
       8 ASN00086038 2020-01-08     0  26.4  16.4      1
       9 ASN00086038 2020-01-09     0  33.1  17.4      1
      10 ASN00086038 2020-01-10     0  34    19.6      1
      # i 20 more rows

---

    Code
      ungroup(res2)
    Output
      # cubble:   key: id [3], index: date, long form
      # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
      # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl], group1
      #   [dbl]
         id          date        prcp  tmax  tmin group1
         <chr>       <date>     <dbl> <dbl> <dbl>  <dbl>
       1 ASN00086038 2020-01-01     0  26.8  11        1
       2 ASN00086038 2020-01-02     0  26.3  12.2      1
       3 ASN00086038 2020-01-03     0  34.5  12.7      1
       4 ASN00086038 2020-01-04     0  29.3  18.8      1
       5 ASN00086038 2020-01-05    18  16.1  12.5      1
       6 ASN00086038 2020-01-06   104  17.5  11.1      1
       7 ASN00086038 2020-01-07    14  20.7  12.1      1
       8 ASN00086038 2020-01-08     0  26.4  16.4      1
       9 ASN00086038 2020-01-09     0  33.1  17.4      1
      10 ASN00086038 2020-01-10     0  34    19.6      1
      # i 20 more rows

