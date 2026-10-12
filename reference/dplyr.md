# `dplyr` methods

Verbs supported for both nested and long cubble include:
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),.
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html),
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html),
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html),
`dplyr::slice_*()`, `dplyr::*_join()`,
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)

## Usage

``` r
# S3 method for class 'temporal_cubble_df'
arrange(.data, ...)

# S3 method for class 'spatial_cubble_df'
select(.data, ...)

# S3 method for class 'temporal_cubble_df'
select(.data, ...)

# S3 method for class 'spatial_cubble_df'
group_by(.data, ..., .add, .drop)

# S3 method for class 'temporal_cubble_df'
group_by(.data, ..., .add, .drop)

# S3 method for class 'spatial_cubble_df'
ungroup(x, ...)

# S3 method for class 'temporal_cubble_df'
ungroup(x, ...)

# S3 method for class 'spatial_cubble_df'
summarise(.data, ..., .by = NULL, .groups = NULL)

# S3 method for class 'temporal_cubble_df'
summarise(.data, ..., .by = key_vars(.data), .groups = NULL)

# S3 method for class 'spatial_cubble_df'
rename(.data, ...)

# S3 method for class 'temporal_cubble_df'
rename(.data, ...)

bind_rows.temporal_cubble_df(..., .id = NULL)

bind_cols.spatial_cubble_df(..., .name_repair)

bind_cols.temporal_cubble_df(..., .name_repair)

# S3 method for class 'spatial_cubble_df'
rowwise(data, ...)

# S3 method for class 'temporal_cubble_df'
rowwise(data, ...)

# S3 method for class 'cubble_df'
dplyr_col_modify(data, cols)

# S3 method for class 'spatial_cubble_df'
dplyr_row_slice(data, i, ...)

# S3 method for class 'temporal_cubble_df'
dplyr_row_slice(data, i, ...)

# S3 method for class 'spatial_cubble_df'
dplyr_reconstruct(data, template)

# S3 method for class 'temporal_cubble_df'
dplyr_reconstruct(data, template)

# S3 method for class 'spatial_cubble_df'
mutate(.data, ...)

# S3 method for class 'temporal_cubble_df'
mutate(.data, ...)

# S3 method for class 'spatial_cubble_df'
filter(.data, ...)

# S3 method for class 'spatial_cubble_df'
arrange(.data, ...)
```

## Arguments

- ...:

  In
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables or computations to group by. Computations are always done on
  the ungrouped data frame. To perform computations on the grouped data,
  you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) step
  before the
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Computations are not allowed in
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html). In
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables to remove from the grouping.

- .add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

- x:

  A [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - "drop_last": dropping the last level of grouping. This was the only
    supported option before version 1.0.0.

  - "drop": All levels of grouping are dropped.

  - "keep": Same grouping structure as `.data`.

  - "rowwise": Each row is its own group.

  When `.groups` is not specified, it is chosen based on the number of
  rows of the results:

  - If all the results have 1 row, you get "drop_last".

  - If the number of rows varies, you get "keep" (note that returning a
    variable number of rows was deprecated in favor of
    [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html),
    which also unconditionally drops all levels of grouping).

  In addition, a message informs you of that choice, unless the result
  is ungrouped, the option "dplyr.summarise.inform" is set to `FALSE`,
  or when
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  is called from a function in a package.

- .id:

  The name of an optional identifier column. Provide a string to create
  an output column that identifies each input. The column will use names
  if available, otherwise it will use positions.

- .name_repair:

  One of `"unique"`, `"universal"`, or `"check_unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

- data, .data:

  a cubble object of class `spatial_cubble_df` or `temporal_cubble_df`

- cols:

  A named list used to modify columns. A `NULL` value should remove an
  existing column.

- i:

  A numeric or logical vector that indexes the rows of `data`.

- template:

  Template data frame to use for restoring attributes.

## Details

You may find not all the verbs have a `verb.spatial_cubble_df` or
`verb.temporal_cubble_df` implemented. These verbs call the dplyr
extending trios: `dplyr_row_slice`, `dplyr_col_modify`, and
`dplyr_reconstruct` under the hood. See
https://dplyr.tidyverse.org/reference/dplyr_extending.html

## Examples

``` r
library(dplyr)
cb_nested <- climate_mel
cb_long <- face_temporal(climate_mel)

# filter - currently filter.spatial_cubble_df, dply_row_slice
cb_nested |> filter(elev > 40)
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.83, -37.73, 144.91, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
cb_long |> filter(prcp > 0)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-05 -- 2020-01-07 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>   id          date        prcp  tmax  tmin
#>   <chr>       <date>     <dbl> <dbl> <dbl>
#> 1 ASN00086038 2020-01-05    18  16.1  12.5
#> 2 ASN00086038 2020-01-06   104  17.5  11.1
#> 3 ASN00086038 2020-01-07    14  20.7  12.1
#> 4 ASN00086077 2020-01-05    20  17.4  12.7
#> 5 ASN00086077 2020-01-06   122  17.8  11.8
#> 6 ASN00086077 2020-01-07     6  20.3  12.6
#> 7 ASN00086282 2020-01-05    16  15.7  12  
#> 8 ASN00086282 2020-01-06    90  17.3  11.5
#> 9 ASN00086282 2020-01-07     6  19.9  11.8

# mutate - curerntly mutate.spatial_cubble_df, dply_col_modify
cb_nested |> mutate(elev2 = elev + 10)
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts                elev2
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>            <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>  88.4
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>  22.1
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]> 123. 
cb_long |> mutate(prcp2 = prcp + 10)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin prcp2
#>    <chr>       <date>     <dbl> <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11      10
#>  2 ASN00086038 2020-01-02     0  26.3  12.2    10
#>  3 ASN00086038 2020-01-03     0  34.5  12.7    10
#>  4 ASN00086038 2020-01-04     0  29.3  18.8    10
#>  5 ASN00086038 2020-01-05    18  16.1  12.5    28
#>  6 ASN00086038 2020-01-06   104  17.5  11.1   114
#>  7 ASN00086038 2020-01-07    14  20.7  12.1    24
#>  8 ASN00086038 2020-01-08     0  26.4  16.4    10
#>  9 ASN00086038 2020-01-09     0  33.1  17.4    10
#> 10 ASN00086038 2020-01-10     0  34    19.6    10
#> # ℹ 20 more rows

# arrange - currently arrange.spatial_cubble_df, arrange.temporal_cubble_df
cb_nested |> arrange(wmo_id)
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
cb_long |> arrange(prcp)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-08     0  26.4  16.4
#>  6 ASN00086038 2020-01-09     0  33.1  17.4
#>  7 ASN00086038 2020-01-10     0  34    19.6
#>  8 ASN00086077 2020-01-01     0  24.7  10  
#>  9 ASN00086077 2020-01-02     0  24.8  11.8
#> 10 ASN00086077 2020-01-03     0  35    12.2
#> # ℹ 20 more rows

# summarise - summarise.spatial_cubble_df,  summarise.temporal_cubble_df
cb_long |>
  group_by(first_5 = ifelse(lubridate::day(date) <=5, 1, 2 )) |>
  summarise(tmax = mean(tmax))
#> # cubble:   key: id [3], index: first_5, long form, groups: first_5 [2]
#> # temporal: 1 -- 2 [1], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>   first_5 id           tmax
#>     <dbl> <chr>       <dbl>
#> 1       1 ASN00086038  26.6
#> 2       1 ASN00086077  25.5
#> 3       1 ASN00086282  27.1
#> 4       2 ASN00086038  26.3
#> 5       2 ASN00086077  25.9
#> 6       2 ASN00086282  26.2
cb_long |>
  mutate(first_5 = ifelse(lubridate::day(date) <=5, 1, 2)) |>
  summarise(t = mean(tmax), .by = first_5)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date           t
#>    <chr>       <date>     <dbl>
#>  1 ASN00086038 2020-01-01  26.8
#>  2 ASN00086038 2020-01-02  26.3
#>  3 ASN00086038 2020-01-03  34.5
#>  4 ASN00086038 2020-01-04  29.3
#>  5 ASN00086038 2020-01-05  16.1
#>  6 ASN00086038 2020-01-06  17.5
#>  7 ASN00086038 2020-01-07  20.7
#>  8 ASN00086038 2020-01-08  26.4
#>  9 ASN00086038 2020-01-09  33.1
#> 10 ASN00086038 2020-01-10  34  
#> # ℹ 20 more rows

# select -  select.spatial_cubble_df,  select.temporal_cubble_df
cb_nested |> select(name)
#> ℹ Missing attribute `id`, `long`, `lat`, and `ts`, add it back.
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat ts                name             
#>   <chr>       <dbl> <dbl> <list>            <chr>            
#> 1 ASN00086038  145. -37.7 <tibble [10 × 4]> essendon airport 
#> 2 ASN00086077  145. -38.0 <tibble [10 × 4]> moorabbin airport
#> 3 ASN00086282  145. -37.7 <tibble [10 × 4]> melbourne airport
cb_nested |> select(-id, -name)
#> ℹ Missing attribute `id`, add it back.
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.   94866 <tibble [10 × 4]>
cb_long |> select(prcp)
#> ℹ Missing attribute `id` and `date`, add it back.
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp
#>    <chr>       <date>     <dbl>
#>  1 ASN00086038 2020-01-01     0
#>  2 ASN00086038 2020-01-02     0
#>  3 ASN00086038 2020-01-03     0
#>  4 ASN00086038 2020-01-04     0
#>  5 ASN00086038 2020-01-05    18
#>  6 ASN00086038 2020-01-06   104
#>  7 ASN00086038 2020-01-07    14
#>  8 ASN00086038 2020-01-08     0
#>  9 ASN00086038 2020-01-09     0
#> 10 ASN00086038 2020-01-10     0
#> # ℹ 20 more rows
cb_long |> select(-prcp, -date)
#> ℹ Missing attribute `date`, add it back.
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    date       id           tmax  tmin
#>    <date>     <chr>       <dbl> <dbl>
#>  1 2020-01-01 ASN00086038  26.8  11  
#>  2 2020-01-02 ASN00086038  26.3  12.2
#>  3 2020-01-03 ASN00086038  34.5  12.7
#>  4 2020-01-04 ASN00086038  29.3  18.8
#>  5 2020-01-05 ASN00086038  16.1  12.5
#>  6 2020-01-06 ASN00086038  17.5  11.1
#>  7 2020-01-07 ASN00086038  20.7  12.1
#>  8 2020-01-08 ASN00086038  26.4  16.4
#>  9 2020-01-09 ASN00086038  33.1  17.4
#> 10 2020-01-10 ASN00086038  34    19.6
#> # ℹ 20 more rows

# rename - rename.spatial_cubble_df, rename.temporal_cubble_df
cb_nested |> rename(elev2 = elev)
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat elev2 name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
cb_long |> rename(prcp2 = prcp)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date       prcp2  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows
# rename on key attributes
cb_nested |> rename(id2 = id)
#> # cubble:   key: id2 [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id2          long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
cb_long |> rename(date2 = date)
#> # cubble:   key: id [3], index: date2, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date2       prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows

# join - mutate_join - dplyr_reconstruct()
# join - filter_join - dplyr_row_slice()
df1 <- cb_nested |> as_tibble() |> select(id, name) |> head(2)
nested <- cb_nested |> select(-name)
nested |> left_join(df1, by = "id")
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts                name             
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
#> 1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 × 4]> essendon airport 
#> 2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 × 4]> moorabbin airport
#> 3 ASN00086282  145. -37.7 113.   94866 <tibble [10 × 4]> NA               
nested |> right_join(df1, by = "id")
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.91, -37.98, 145.1, -37.73], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts                name             
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
#> 1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 × 4]> essendon airport 
#> 2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 × 4]> moorabbin airport
nested |> inner_join(df1, by = "id")
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.91, -37.98, 145.1, -37.73], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts                name             
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
#> 1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 × 4]> essendon airport 
#> 2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 × 4]> moorabbin airport
nested |> full_join(df1, by = "id")
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts                name             
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>            <chr>            
#> 1 ASN00086038  145. -37.7  78.4  95866 <tibble [10 × 4]> essendon airport 
#> 2 ASN00086077  145. -38.0  12.1  94870 <tibble [10 × 4]> moorabbin airport
#> 3 ASN00086282  145. -37.7 113.   94866 <tibble [10 × 4]> NA               
nested |> anti_join(df1, by = "id")
#> # cubble:   key: id [1], index: date, nested form
#> # spatial:  [144.83, -37.67, 144.83, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl>  <dbl> <list>           
#> 1 ASN00086282  145. -37.7  113.  94866 <tibble [10 × 4]>

# bind_rows - dplyr_reconstruct, bind_rows.temporal_cubble_df
df1 <- cb_nested |> head(1)
df2 <- cb_nested |> tail(2)
bind_rows(df1, df2)
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
df1 <- cb_long |> head(10)
df2 <- cb_long |> tail(20)
bind_rows(df1, df2)
#> # cubble:   key: id [1], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows

# relocate - dplyr_col_select, dplyr_col_select
cb_nested |> relocate(ts, .before = name)
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev ts                name              wmo_id
#>   <chr>       <dbl> <dbl> <dbl> <list>            <chr>              <dbl>
#> 1 ASN00086038  145. -37.7  78.4 <tibble [10 × 4]> essendon airport   95866
#> 2 ASN00086077  145. -38.0  12.1 <tibble [10 × 4]> moorabbin airport  94870
#> 3 ASN00086282  145. -37.7 113.  <tibble [10 × 4]> melbourne airport  94866
cb_nested |> face_temporal() |> relocate(tmin)
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>     tmin id          date        prcp  tmax
#>    <dbl> <chr>       <date>     <dbl> <dbl>
#>  1  11   ASN00086038 2020-01-01     0  26.8
#>  2  12.2 ASN00086038 2020-01-02     0  26.3
#>  3  12.7 ASN00086038 2020-01-03     0  34.5
#>  4  18.8 ASN00086038 2020-01-04     0  29.3
#>  5  12.5 ASN00086038 2020-01-05    18  16.1
#>  6  11.1 ASN00086038 2020-01-06   104  17.5
#>  7  12.1 ASN00086038 2020-01-07    14  20.7
#>  8  16.4 ASN00086038 2020-01-08     0  26.4
#>  9  17.4 ASN00086038 2020-01-09     0  33.1
#> 10  19.6 ASN00086038 2020-01-10     0  34  
#> # ℹ 20 more rows

# slice - all the slice_* uses dplyr::slice(), which uses dplyr_row_slice()
cb_nested |> slice_head(n = 2)
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.91, -37.98, 145.1, -37.73], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
cb_nested |> slice_tail(n = 2)
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 2 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
cb_nested |> slice_max(elev)
#> # cubble:   key: id [1], index: date, nested form
#> # spatial:  [144.83, -37.67, 144.83, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086282  145. -37.7  113. melbourne airport  94866 <tibble [10 × 4]>
cb_nested |> slice_min(elev)
#> # cubble:   key: id [1], index: date, nested form
#> # spatial:  [145.1, -37.98, 145.1, -37.98], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
cb_nested |> slice_sample(n = 2)
#> # cubble:   key: id [2], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 2 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>

# rowwise - rowwise.spatial_cubble_df, rowwise.temporal_cuble_df
cb_nested |> rowwise()
#> # cubble:   key: id [3], index: date, nested form, groups: rowwise
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
cb_long |> rowwise()
#> # cubble:   key: id [3], index: date, long form, groups: rowwise
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows

# group_by & ungroup -
(res <- cb_nested |> mutate(group1 = c(1, 1, 2)) |> group_by(group1))
#> # cubble:   key: id [3], index: date, nested form, groups: group1 [2]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts       group1
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>    <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble>      1
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble>      1
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble>      2
res |> ungroup()
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts       group1
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>    <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble>      1
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble>      1
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble>      2
(res2 <- res |> face_temporal())
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl], group1
#> #   [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows
res2 |> ungroup()
#> # cubble:   key: id [3], index: date, long form
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl], group1
#> #   [dbl]
#>    id          date        prcp  tmax  tmin
#>    <chr>       <date>     <dbl> <dbl> <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11  
#>  2 ASN00086038 2020-01-02     0  26.3  12.2
#>  3 ASN00086038 2020-01-03     0  34.5  12.7
#>  4 ASN00086038 2020-01-04     0  29.3  18.8
#>  5 ASN00086038 2020-01-05    18  16.1  12.5
#>  6 ASN00086038 2020-01-06   104  17.5  11.1
#>  7 ASN00086038 2020-01-07    14  20.7  12.1
#>  8 ASN00086038 2020-01-08     0  26.4  16.4
#>  9 ASN00086038 2020-01-09     0  33.1  17.4
#> 10 ASN00086038 2020-01-10     0  34    19.6
#> # ℹ 20 more rows
res2 |> mutate(first_5 = ifelse(lubridate::day(date) <= 5, 1, 6)) |>
  group_by(first_5)
#> # cubble:   key: id [3], index: date, long form, groups: first_5 [2]
#> # temporal: 2020-01-01 -- 2020-01-10 [1D], no gaps
#> # spatial:  long [dbl], lat [dbl], elev [dbl], name [chr], wmo_id [dbl], group1
#> #   [dbl]
#>    id          date        prcp  tmax  tmin first_5
#>    <chr>       <date>     <dbl> <dbl> <dbl>   <dbl>
#>  1 ASN00086038 2020-01-01     0  26.8  11         1
#>  2 ASN00086038 2020-01-02     0  26.3  12.2       1
#>  3 ASN00086038 2020-01-03     0  34.5  12.7       1
#>  4 ASN00086038 2020-01-04     0  29.3  18.8       1
#>  5 ASN00086038 2020-01-05    18  16.1  12.5       1
#>  6 ASN00086038 2020-01-06   104  17.5  11.1       6
#>  7 ASN00086038 2020-01-07    14  20.7  12.1       6
#>  8 ASN00086038 2020-01-08     0  26.4  16.4       6
#>  9 ASN00086038 2020-01-09     0  33.1  17.4       6
#> 10 ASN00086038 2020-01-10     0  34    19.6       6
#> # ℹ 20 more rows
```
