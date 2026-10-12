# Pivot a cubble object between the nested/long (spatial/temporal) form

While `face_temporal()` switches a cubble object into a long cubble,
suitable for temporal operations, `face_spatial()` turns a long cubble
back into a nest cubble for spatial operations. The two operations are
exact inverse.

## Usage

``` r
face_temporal(data, col)

# S3 method for class 'temporal_cubble_df'
face_temporal(data, col)

# S3 method for class 'spatial_cubble_df'
face_temporal(data, col)

face_spatial(data)

# S3 method for class 'spatial_cubble_df'
face_spatial(data)

# S3 method for class 'temporal_cubble_df'
face_spatial(data)
```

## Arguments

- data:

  a cubble object

- col:

  a character (or a symbol), the list column to be expanded, `col` is
  required to be specified if there are more than one list column and
  the list column name is not `ts`.

## Value

a cubble object

## References

[cubble: An R Package for Organizing and Wrangling Multivariate
Spatio-Temporal Data](doi:10.18637/jss.v110.i07)

## Examples

``` r
cb_long <- climate_mel |> face_temporal()
cb_back <- cb_long |> face_spatial()
identical(climate_mel, cb_back)
#> [1] TRUE
```
