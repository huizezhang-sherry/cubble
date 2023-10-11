# make cubble with partial match

    Code
      dplyr::pull(make_cubble(lga2, covid2, potential_match = check_res), lga)
    Warning <simpleWarning>
      st_centroid assumes attributes are constant over geometries
    Warning <lifecycle_warning_deprecated>
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      [1] "Kingston (C)" "Latrobe (C)" 

---

    Code
      dplyr::pull(make_cubble(lga2, covid2, potential_match = check_res, key_use = "spatial"),
      lga)
    Warning <simpleWarning>
      st_centroid assumes attributes are constant over geometries
    Warning <lifecycle_warning_deprecated>
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(by)
      
        # Now:
        data %>% select(all_of(by))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      [1] "Kingston (C) (Vic.)" "Latrobe (C) (Vic.)" 

