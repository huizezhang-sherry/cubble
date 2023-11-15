# matching examples work

    Code
      match_spatial(climate_aus, river)
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
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
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
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
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key2)
      
        # Now:
        data |> select(all_of(key2))
      
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
      match_temporal(a1, data_id = type, match_id = group, temporal_by = c(prcp = "Water_course_level"))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(key)
      
        # Now:
        data |> select(all_of(key))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(index)
      
        # Now:
        data |> select(all_of(index))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(loc)
      
        # Now:
        data |> select(all_of(loc))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data |> select(.y)
      
        # Now:
        data |> select(all_of(.y))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # A tibble: 4 x 2
        group match_res
        <int>     <dbl>
      1     1        30
      2     2         5
      3     3        14
      4     4        20

