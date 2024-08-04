# matching examples work

    Code
      match_spatial(climate_aus, river)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Message
      Use OGC:CRS84 by default for distance calculation...
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"dist"` instead of `.data$dist`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
    Output
      # A tibble: 4 x 4
        from        to      dist group
        <chr>       <chr>    [m] <int>
      1 ASN00088051 406213 1838.     1
      2 ASN00084145 222201 2185.     2
      3 ASN00085072 226027 3282.     3
      4 ASN00080015 406704 4034.     4

---

    Code
      match_spatial(climate_aus, river, which = "Hausdorff")
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Message
      Use OGC:CRS84 by default for distance calculation...
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"dist"` instead of `.data$dist`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
    Output
      # A tibble: 4 x 4
        from        to       dist group
        <chr>       <chr>     [°] <int>
      1 ASN00088051 406213 0.0204     1
      2 ASN00084145 222201 0.0219     2
      3 ASN00085072 226027 0.0296     3
      4 ASN00080015 406704 0.0364     4

---

    Code
      match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Message
      Use OGC:CRS84 by default for distance calculation...
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"dist"` instead of `.data$dist`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
    Output
      # A tibble: 10 x 4
         from        to       dist group
         <chr>       <chr>     [m] <int>
       1 ASN00088051 406213  1838.     1
       2 ASN00088051 406215  6963.     1
       3 ASN00088051 406235 15133.     1
       4 ASN00088051 405240 47673.     1
       5 ASN00088051 405212 48364.     1
       6 ASN00084145 222201  2185.     2
       7 ASN00084145 222203 13022.     2
       8 ASN00084145 223209 54797.     2
       9 ASN00084145 221224 63127.     2
      10 ASN00084145 224217 79816.     2

---

    Code
      match_spatial(climate_aus, river, spatial_n_each = 5, spatial_n_group = 2,
        return_cubble = TRUE)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Message
      Use OGC:CRS84 by default for distance calculation...
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"dist"` instead of `.data$dist`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"group"` instead of `.data$group`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
    Output
      [[1]]
      # cubble:   key: id [6], index: date, nested form, [sf]
      # spatial:  [144.49, -37.1, 145.06, -36.95], WGS 84
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
         id     long   lat  elev name  wmo_id ts       type              geometry
         <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <list>   <chr>          <POINT [°]>
       1 ASN0~  145. -37.0   290 rede~  94859 <tibble> clim~  (144.5203 -37.0194)
       2 ASN0~  145. -37.0   290 rede~  94859 <tibble> clim~  (144.5203 -37.0194)
       3 ASN0~  145. -37.0   290 rede~  94859 <tibble> clim~  (144.5203 -37.0194)
       4 ASN0~  145. -37.0   290 rede~  94859 <tibble> clim~  (144.5203 -37.0194)
       5 ASN0~  145. -37.0   290 rede~  94859 <tibble> clim~  (144.5203 -37.0194)
       6 4062~  145. -37.0    NA CAMP~     NA <tibble> river (144.5403 -37.01512)
       7 4062~  144. -37.0    NA COLI~     NA <tibble> river (144.4906 -36.96145)
       8 4062~  145. -36.9    NA WILD~     NA <tibble> river (144.6647 -36.94718)
       9 4052~  145. -37.1    NA SUGA~     NA <tibble> river  (145.055 -37.05963)
      10 4052~  145. -37.1    NA SUND~     NA <tibble> river (145.0561 -37.09895)
      # i 2 more variables: group <int>, dist [m]
      
      [[2]]
      # cubble:   key: id [6], index: date, nested form, [sf]
      # spatial:  [147.57, -37.81, 149.16, -37.54], WGS 84
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
         id     long   lat  elev name  wmo_id ts       type              geometry
         <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <list>   <chr>          <POINT [°]>
       1 ASN0~  148. -37.7  62.7 orbo~  95918 <tibble> clim~  (148.4667 -37.6922)
       2 ASN0~  148. -37.7  62.7 orbo~  95918 <tibble> clim~  (148.4667 -37.6922)
       3 ASN0~  148. -37.7  62.7 orbo~  95918 <tibble> clim~  (148.4667 -37.6922)
       4 ASN0~  148. -37.7  62.7 orbo~  95918 <tibble> clim~  (148.4667 -37.6922)
       5 ASN0~  148. -37.7  62.7 orbo~  95918 <tibble> clim~  (148.4667 -37.6922)
       6 2222~  148. -37.7  NA   SNOW~     NA <tibble> river  (148.451 -37.70739)
       7 2222~  149. -37.8  NA   SNOW~     NA <tibble> river (148.5278 -37.79889)
       8 2232~  148. -37.8  NA   TAMB~     NA <tibble> river (147.8491 -37.75731)
       9 2212~  149. -37.5  NA   CANN~     NA <tibble> river (149.1567 -37.53865)
      10 2242~  148. -37.8  NA   MITC~     NA <tibble> river   (147.5722 -37.815)
      # i 2 more variables: group <int>, dist [m]
      

---

    Code
      match_sites(climate_aus, river, data_id = type, match_id = group, temporal_by = c(
        prcp = "Water_course_level"))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key2)
      
        # Now:
        data %>% select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Message
      Use OGC:CRS84 by default for distance calculation...
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"dist"` instead of `.data$dist`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"group"` instead of `.data$group`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"from"` instead of `.data$from`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"to"` instead of `.data$to`
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(key_name)
      
        # Now:
        data %>% select(all_of(key_name))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      [[1]]
      [[1]][[1]]
      # cubble:   key: id [2], index: date, nested form, [sf]
      # spatial:  [144.52, -37.02, 144.54, -37.02], WGS 84
      # temporal: date [date], matched [dbl]
        id         long   lat  elev name  wmo_id type              geometry group
        <chr>     <dbl> <dbl> <dbl> <chr>  <dbl> <chr>          <POINT [°]> <int>
      1 ASN00088~  145. -37.0   290 rede~  94859 clim~  (144.5203 -37.0194)     1
      2 406213     145. -37.0    NA CAMP~     NA river (144.5403 -37.01512)     1
      # i 3 more variables: dist [m], ts <list>, match_res <dbl>
      
      
      [[2]]
      [[2]][[1]]
      # cubble:   key: id [2], index: date, nested form, [sf]
      # spatial:  [148.45, -37.71, 148.47, -37.69], WGS 84
      # temporal: date [date], matched [dbl]
        id         long   lat  elev name  wmo_id type             geometry group
        <chr>     <dbl> <dbl> <dbl> <chr>  <dbl> <chr>         <POINT [°]> <int>
      1 ASN00084~  148. -37.7  62.7 orbo~  95918 clim~ (148.4667 -37.6922)     2
      2 222201     148. -37.7  NA   SNOW~     NA river (148.451 -37.70739)     2
      # i 3 more variables: dist [m], ts <list>, match_res <dbl>
      
      
      [[3]]
      [[3]][[1]]
      # cubble:   key: id [2], index: date, nested form, [sf]
      # spatial:  [147.13, -38.14, 147.13, -38.12], WGS 84
      # temporal: date [date], matched [dbl]
        id         long   lat  elev name  wmo_id type              geometry group
        <chr>     <dbl> <dbl> <dbl> <chr>  <dbl> <chr>          <POINT [°]> <int>
      1 ASN00085~  147. -38.1   4.6 east~  94907 clim~  (147.1322 -38.1156)     3
      2 226027     147. -38.1  NA   LA T~     NA river (147.1278 -38.14491)     3
      # i 3 more variables: dist [m], ts <list>, match_res <dbl>
      
      
      [[4]]
      [[4]][[1]]
      # cubble:   key: id [2], index: date, nested form, [sf]
      # spatial:  [144.76, -36.16, 144.77, -36.13], WGS 84
      # temporal: date [date], matched [dbl]
        id         long   lat  elev name  wmo_id type              geometry group
        <chr>     <dbl> <dbl> <dbl> <chr>  <dbl> <chr>          <POINT [°]> <int>
      1 ASN00080~  145. -36.2    96 echu~  94861 clim~  (144.7642 -36.1647)     4
      2 406704     145. -36.1    NA DEAK~     NA river (144.7693 -36.12866)     4
      # i 3 more variables: dist [m], ts <list>, match_res <dbl>
      
      

---

    Code
      match_temporal(a1, data_id = type, match_id = group, temporal_by = c(prcp = "Water_course_level"))
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
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
        data %>% select(index)
      
        # Now:
        data %>% select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(loc)
      
        # Now:
        data %>% select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(.y)
      
        # Now:
        data %>% select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # A tibble: 4 x 2
        group match_res
        <int>     <dbl>
      1     1        30
      2     2         5
      3     3        14
      4     4        20

