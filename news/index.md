# Changelog

## cubble 1.0.0

CRAN release: 2024-08-27

- version update for JSS publication
- fix the `st_transformation` on `spatial_cubble_df`
  ([\#32](https://github.com/huizezhang-sherry/cubble/issues/32))

## cubble 0.3.2

CRAN release: 2024-07-18

- fix the conversion between cubble and stars object
  ([\#30](https://github.com/huizezhang-sherry/cubble/issues/30))

## cubble 0.3.1

CRAN release: 2024-07-02

- small fix on `group_by` with temporal cubble, reported from CRAN

## cubble 0.3.0

CRAN release: 2023-06-30

- new
  [`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md)
  for constructing a cubble object from separate spatial & temporal
  components (replace `as_cubble(list(spatial = ..., temporal = ...))`
  syntax)
- [`as_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/as_cubble.md)
  for stars and sftime objects
  ([\#5](https://github.com/huizezhang-sherry/cubble/issues/5))
- refactor the cubble class: nested cubble has class:
  `spatial_cubble_df` and `cubble_df`, the long cubble has class:
  `temporal_cubble_df` and `cubble_df`.
- refactor the matching function
- better integration with sf and tsibble

## cubble 0.2.1

CRAN release: 2023-03-21

- add pkg logo
- fix mismatch of arguments in slice\_\*() with the latest dplyr
  generics

## cubble 0.2.0

CRAN release: 2022-11-17

- update on `geom_glyph` to take regular function
- a new vignette on *making a linked map and time series plot*
- add a converter between `stars` and `cubble` object
  [\#5](https://github.com/huizezhang-sherry/cubble/issues/5)

## cubble 0.1.1

CRAN release: 2022-06-02

- remove dependency (suggest) on `rnoaa`

## cubble 0.1.0

CRAN release: 2022-04-22

- Added a `NEWS.md` file to track changes to the package.
