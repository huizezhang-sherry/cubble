# cubble 1.0.0

* version update for JSS publication
* fix the `st_transformation` on `spatial_cubble_df` (#32)

# cubble 0.3.2

* fix the conversion between cubble and stars object (#30)

# cubble 0.3.1

* small fix on `group_by` with temporal cubble, reported from CRAN

# cubble 0.3.0

* new `make_cubble()` for constructing a cubble object from separate spatial & temporal components (replace `as_cubble(list(spatial = ..., temporal = ...))` syntax)
* `as_cubble()` for stars and sftime objects (#5)
* refactor the cubble class: nested cubble has class: `spatial_cubble_df` and `cubble_df`,
the long cubble has class: `temporal_cubble_df` and `cubble_df`. 
* refactor the matching function
* better integration with sf and tsibble 

# cubble 0.2.1

* add pkg logo
* fix mismatch of arguments in slice_*() with the latest dplyr generics

# cubble 0.2.0

* update on `geom_glyph` to take regular function
* a new vignette on *making a linked map and time series plot*
* add a converter between `stars` and `cubble` object #5

# cubble 0.1.1

* remove dependency (suggest) on `rnoaa`

# cubble 0.1.0

* Added a `NEWS.md` file to track changes to the package.
