# multiplication works

    Code
      make_cubble(spatial = lga, temporal = covid, potential_match = check_res)
    Condition
      Warning:
      st_centroid assumes attributes are constant over geometries
      Error in `check_key_tbl()`:
      ! The obj need to be the result from `check_key()`.

---

    Code
      make_cubble(lga, covid, potential_match = check_res, key_use = "aaa")
    Condition
      Warning:
      st_centroid assumes attributes are constant over geometries
      Error in `check_key_tbl()`:
      ! The obj need to be the result from `check_key()`.

