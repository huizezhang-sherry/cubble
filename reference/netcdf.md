# Functions to extract NetCDF dimension and variables

Functions to extract NetCDF dimension and variables

## Usage

``` r
extract_var(data, vars = NULL)

extract_longlat(data)

extract_time(data)
```

## Arguments

- data:

  a NetCDF file read in from
  [`ncdf4::nc_open()`](https://rdrr.io/pkg/ncdf4/man/nc_open.html)

- vars:

  variables to read, see the variables in your data with
  `names(data$var)`

## Value

extracted netcdf4 components
