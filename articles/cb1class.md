# 1. The cubble class

This article introduces the cubble class, demonstrates how to create a
cubble from various types of data, and shows how a cubble works with the
tsibble and sf classes. It ends with answers to common questions.

## The cubble object

The cubble class is an S3 class built on tibble that allows the
spatio-temporal data to be wrangled in two forms: a nested/spatial form
and a long/temporal form. It consists of two subclasses:

- a nested/ spatial cubble is represented by the class
  `c("spatial_cubble_df", "cubble_df")`
- a long/ temporal cubble is represented by the class
  `c("temporal_cubble_df", "cubble_df")`

In a nested cubble, spatial variables are organised as columns and
temporal variables are nested within a specialised `ts` column:

``` r

cb_nested
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
class(cb_nested)
#> [1] "spatial_cubble_df" "cubble_df"         "tbl_df"           
#> [4] "tbl"               "data.frame"
```

This toy dataset is a subset of a larger data `climate_aus` sourced from
the Global Historical Climatology Network Daily (GHCND). It records
three airport stations located in Melbourne, Australia and includes
spatial variables such as station ID, longitude, latitude, elevation,
station name, World Meteorology Organisation ID. The dataset contains
temporal variables including precipitation, maximum and minimum
temperature, which can be read from the cubble header.

In a long cubble, the temporal variables are expanded into the long
form, while the spatial variables are stored as a data attribute:

``` r

cb_long
#> # cubble:   key: id [3], index: date, long form
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
class(cb_long)
#> [1] "temporal_cubble_df" "cubble_df"          "tbl_df"            
#> [4] "tbl"                "data.frame"
```

The cubble header now shows the recorded temporal period (2020-01-01 to
2020-01-10), the interval (1 day), and there is no gaps in the data.

### The cubble attributes

A cubble object inherits the attributes from tibble (and its
subclasses): `class`, `row.names`, and `names`. Additionally, it has
three specialised attributes:

- `key`: the spatial identifier
- `index`: the temporal identifier
- `coords`: a pair of ordered coordinates associated with the location

Readers familiar with the `key` and `index` attributes from the
`tsibble` package will already know the two arguments. In cubble, the
`key` attribute identifies the row in the nested cubble, and when
combined with the `index` argument, it identifies the row in the long
cubble. Currently, cubble only supports one variable as the key, and the
accepted temporal classes for the index include the base R classes
`Date`, `POSIXlt`, `POSIXct` as well as tsibble’s
[`tsibble::yearmonth()`](https://tsibble.tidyverts.org/reference/year-month.html),
[`tsibble::yearweek()`](https://tsibble.tidyverts.org/reference/year-week.html),
and
[`tsibble::yearquarter()`](https://tsibble.tidyverts.org/reference/year-quarter.html)
classes.

The `coords` attribute represents an ordered pair of coordinates. It can
be either an unprojected pair of longitude and latitude or a projected
easting and northing value. The `sf` package is used under the hood to
calculate the bounding box, displayed in the header of a nested cubble,
and perform other spatial operations.

The long cubble has a special attribute called `spatial` to store the
spatial variables, which includes all the variables from the nested
cubble except for the `ts` column. Below we print the attributes
information for the previously shown `cb_nested` and `cb_long` objects:

``` r

attributes(cb_nested)
#> $class
#> [1] "spatial_cubble_df" "cubble_df"         "tbl_df"           
#> [4] "tbl"               "data.frame"       
#> 
#> $row.names
#> [1] 1 2 3
#> 
#> $names
#> [1] "id"     "long"   "lat"    "elev"   "name"   "wmo_id" "ts"    
#> 
#> $key
#> # A tibble: 3 × 2
#>   id                .rows
#>   <chr>       <list<int>>
#> 1 ASN00086038         [1]
#> 2 ASN00086077         [1]
#> 3 ASN00086282         [1]
#> 
#> $index
#> [1] "date"
#> 
#> $coords
#> [1] "long" "lat"
attributes(cb_long)
#> $key
#> # A tibble: 3 × 2
#>   id                .rows
#>   <chr>       <list<int>>
#> 1 ASN00086038        [10]
#> 2 ASN00086077        [10]
#> 3 ASN00086282        [10]
#> 
#> $index
#> [1] "date"
#> 
#> $coords
#> [1] "long" "lat" 
#> 
#> $spatial
#> # A tibble: 3 × 6
#>   id           long   lat  elev name              wmo_id
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866
#> 
#> $names
#> [1] "id"   "date" "prcp" "tmax" "tmin"
#> 
#> $row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30
#> 
#> $class
#> [1] "temporal_cubble_df" "cubble_df"          "tbl_df"            
#> [4] "tbl"                "data.frame"
```

The following shortcut functions are available to extract components
from the attributes:

- [`key_vars()`](https://tsibble.tidyverts.org/reference/key.html): the
  name of the key attribute as a string , i.e. `"id"`,
- [`key_data()`](https://tsibble.tidyverts.org/reference/key-data.html):
  the tibble object stored in the key attribute,
- [`key()`](https://tsibble.tidyverts.org/reference/key.html): the name
  of the key attribute as a symbol in a list, i.e. `[[1]] id`,
- [`index()`](https://huizezhang-sherry.github.io/cubble/reference/attr.md):
  the index attribute as a symbol, i.e. `date`,
- [`index_var()`](https://huizezhang-sherry.github.io/cubble/reference/attr.md):
  the index attribute as a string, i.e. `"date"`,
- [`coords()`](https://huizezhang-sherry.github.io/cubble/reference/attr.md):
  a character vector of length two representing the coordinate pairs,
  i.e. `"long" "lat"`, and
- [`spatial()`](https://huizezhang-sherry.github.io/cubble/reference/attr.md):
  the tibble object for the spatial variables.

## Creation and coercion

A cubble can be created from:

- separate spatial and temporal tables
- a `tibble` object
- NetCDF data
- an `stars` object
- an `sftime` object

### Create from separate spatial and temporal tables

In many cases, spatio-temporal data arrive in separate tables for
analysis. For example, in climate data, analysts may initially receive
station data containing geographic location information, recorded
variables and their recording periods. They can then query the temporal
variables using the stations of interest to obtain the relevant temporal
data. Alternatively, analyses may begin as purely spatial or temporal,
and analysts may obtain additional temporal or spatial data to expand
the result to spatio-temporal.

The function
[`make_cubble()`](https://huizezhang-sherry.github.io/cubble/reference/cubble-class.md)
composes a cubble object from a spatial table (`spatial`) and a temporal
table (`temporal`), along with the three attributes `key`, `index`, and
`coords` introduced above. The following code creates the nested
`cubble`:

``` r

make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
```

The spatial and temporal tables can also be sf and tsibble objects, in
which case some of the attributes can be omitted. See [Compatibility
with tsibble and sf](#compatibility-with-tsibble-and-sf) below.

### Coerce from foreign objects

#### The `tibble` objects

The dataset `climate_flat` combines the spatial data, `stations`, with
the temporal data, `meteo`, into a single tibble object. It can be
coerced into a cubble using:

``` r

climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts               
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>           
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble [10 × 4]>
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble [10 × 4]>
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble [10 × 4]>
```

#### The NetCDF data

In `R`, there are several packages available for wrangling NetCDF data,
including `ncdf4`, `RNetCDF`, and `tidync`. The code below converts a
NetCDF object of class ncdf4 into a cubble object:

``` r

path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
raw <- ncdf4::nc_open(path)
as_cubble(raw)
#> # cubble:   key: id [26565], index: time, nested form
#> # spatial:  [113, -53, 153, -12], Missing CRS!
#> # temporal: time [date], q [dbl], z [dbl]
#>       id  long   lat ts              
#>    <int> <dbl> <dbl> <list>          
#>  1     1  113    -12 <tibble [6 × 3]>
#>  2     2  113.   -12 <tibble [6 × 3]>
#>  3     3  114.   -12 <tibble [6 × 3]>
#>  4     4  114.   -12 <tibble [6 × 3]>
#>  5     5  114    -12 <tibble [6 × 3]>
#>  6     6  114.   -12 <tibble [6 × 3]>
#>  7     7  114.   -12 <tibble [6 × 3]>
#>  8     8  115.   -12 <tibble [6 × 3]>
#>  9     9  115    -12 <tibble [6 × 3]>
#> 10    10  115.   -12 <tibble [6 × 3]>
#> # ℹ 26,555 more rows
```

Sometimes, analysts may choose to read only a subset of the NetCDF data.
In such cases, the `vars`, `long_range` and `lat_range` arguments can be
used to subset the data based on the variable and the grid resolution:

``` r

as_cubble(raw, vars = "q",
          long_range = seq(-180, 180, 1), lat_range = seq(-90, 90, 1))
```

#### The `stars` objects

``` r

tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <- stars::read_stars(tif)
as_cubble(x, index = band)
#> # cubble:   key: id [122848], index: band, nested form
#> # spatial:  [288790.5, 9110743, 298708.5, 9120746.5], SIRGAS 2000 / UTM zone
#> #   25S
#> # temporal: band [int], L7_ETMs.tif [dbl]
#>          x        y    id ts              
#>      <dbl>    <dbl> <int> <list>          
#>  1 288791. 9120747.   352 <tibble [6 × 2]>
#>  2 288819. 9120747.   704 <tibble [6 × 2]>
#>  3 288848. 9120747.  1056 <tibble [6 × 2]>
#>  4 288876. 9120747.  1408 <tibble [6 × 2]>
#>  5 288905. 9120747.  1760 <tibble [6 × 2]>
#>  6 288933. 9120747.  2112 <tibble [6 × 2]>
#>  7 288962. 9120747.  2464 <tibble [6 × 2]>
#>  8 288990. 9120747.  2816 <tibble [6 × 2]>
#>  9 289019. 9120747.  3168 <tibble [6 × 2]>
#> 10 289047. 9120747.  3520 <tibble [6 × 2]>
#> # ℹ 122,838 more rows
```

When the `dimensions` object is too complex for the `cubble` package to
handle, a warning message will be generated.

#### The `sftime` objects

``` r

dt <- climate_flat |> 
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs("OGC:CRS84")) |> 
  sftime::st_as_sftime()
dt |> as_cubble(key = id, index = date)
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84 (CRS84)
#> # temporal: prcp [dbl], tmax [dbl], tmin [dbl], date [date]
#>   id           elev name   wmo_id            geometry  long   lat ts      
#>   <chr>       <dbl> <chr>   <dbl>         <POINT [°]> <dbl> <dbl> <list>  
#> 1 ASN00086038  78.4 essen…  95866 (144.9066 -37.7276)  145. -37.7 <tibble>
#> 2 ASN00086077  12.1 moora…  94870   (145.0964 -37.98)  145. -38.0 <tibble>
#> 3 ASN00086282 113.  melbo…  94866 (144.8321 -37.6655)  145. -37.7 <tibble>
```

## Compatibility with tsibble and sf

Analysts often have their preferred spatial or temporal data structure
that they prefer to use for spatio-temporal analysis. For example, the
`tbl_ts` class from the tsibble package (Wang et al. 2020) is commonly
used in time series forecasting and the sf class (Pebesma 2018) is
frequently used in spatial data science. In cubble, analysts have the
flexibility to combine these two structures together by allowing the
spatial component to be an sf object and the temporal component to also
be a tsibble object.

### Using a tsibble for the temporal component

The `key` and `index` arguments in a cubble object corresponds to the
tsibble counterparts and they can be safely omitted, if the temporal
component is a tsibble object, i.e. `meteo_ts` in the example below. The
tsibble class from the input will be carried over to the cubble object:

``` r

ts_nested <- make_cubble(
  spatial = stations, temporal = meteo_ts, coords = c(long, lat))
(ts_long <- face_temporal(ts_nested))
#> # cubble:   key: id [3], index: date, long form, [tsibble]
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
class(ts_long)
#> [1] "temporal_cubble_df" "cubble_df"          "tbl_ts"            
#> [4] "tbl_df"             "tbl"                "data.frame"
```

The long cubble shows `[tsibble]` in the header to indicate the object
also being in a `tbl_ts` class. Methods applies to the `tbl_ts` class
can also be applied to the temporal cubble objects, for example,
checking whether the data contain temporal gaps:

``` r

ts_long |> has_gaps()
#> # A tibble: 3 × 2
#>   id          .gaps
#>   <chr>       <lgl>
#> 1 ASN00086038 FALSE
#> 2 ASN00086077 FALSE
#> 3 ASN00086282 FALSE
```

An existing cubble object can promote its temporal component to a
tsibble object by applying
[`make_temporal_tsibble()`](https://huizezhang-sherry.github.io/cubble/reference/make_temporal_tsibble.md).
The promoted cubble object (`ts_long2`) will be the same as the one
created with a tsibble component initially (`ts_long`):

``` r

ts_long2 <- make_cubble(
  stations, meteo, 
  key = id, index = date, coords = c(long, lat)) |> 
  face_temporal() |> 
  make_temporal_tsibble() 
identical(ts_long2, ts_long)
#> [1] TRUE
```

### A spatial component with sf

Similarly, an sf object can be supplied as the spatial component to
create a cubble object, with the `coords` argument being omitted. This
opens up the possibility to represent fixed area with polygons or
multipolygons and the `coords` argument will be calculated as the
centroids of the (multi)polygons. The `[sf]` print in the cubble header
suggest an spatial component being also a sf object:

``` r

(sf_nested <- make_cubble(
  spatial = stations_sf, temporal = meteo, 
  key = id, index = date))
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           elev name   wmo_id  long   lat            geometry ts      
#>   <chr>       <dbl> <chr>   <dbl> <dbl> <dbl>         <POINT [°]> <list>  
#> 1 ASN00086038  78.4 essen…  95866  145. -37.7 (144.9066 -37.7276) <tibble>
#> 2 ASN00086077  12.1 moora…  94870  145. -38.0   (145.0964 -37.98) <tibble>
#> 3 ASN00086282 113.  melbo…  94866  145. -37.7 (144.8321 -37.6655) <tibble>
class(sf_nested)
#> [1] "spatial_cubble_df" "cubble_df"         "sf"               
#> [4] "tbl_df"            "tbl"               "data.frame"
```

Both components can be supplied at once. The class attributes from sf
and tsibble will be carried over to the nested and long cubble:

``` r

(res <- make_cubble(spatial = stations_sf, temporal = meteo_ts))
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           elev name   wmo_id  long   lat            geometry ts      
#>   <chr>       <dbl> <chr>   <dbl> <dbl> <dbl>         <POINT [°]> <list>  
#> 1 ASN00086038  78.4 essen…  95866  145. -37.7 (144.9066 -37.7276) <tbl_ts>
#> 2 ASN00086077  12.1 moora…  94870  145. -38.0   (145.0964 -37.98) <tbl_ts>
#> 3 ASN00086282 113.  melbo…  94866  145. -37.7 (144.8321 -37.6655) <tbl_ts>
class(res)
#> [1] "spatial_cubble_df" "cubble_df"         "sf"               
#> [4] "tbl_df"            "tbl"               "data.frame"
class(res$ts[[1]])
#> [1] "tbl_ts"     "tbl_df"     "tbl"        "data.frame"
```

The following code shows how to perform coordinate transformation with
`st_transform` on a cubble object:

``` r

sf_nested |> sf::st_transform(crs = "EPSG:3857")
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [16122635.62, -4576600.87, 16152057.36, -4532279.36], WGS 84 /
#> #   Pseudo-Mercator
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           elev name   wmo_id  long   lat            geometry ts      
#>   <chr>       <dbl> <chr>   <dbl> <dbl> <dbl>         <POINT [m]> <list>  
#> 1 ASN00086038  78.4 essen…  95866  145. -37.7 (16130929 -4541016) <tibble>
#> 2 ASN00086077  12.1 moora…  94870  145. -38.0 (16152057 -4576601) <tibble>
#> 3 ASN00086282 113.  melbo…  94866  145. -37.7 (16122636 -4532279) <tibble>
```

The counterpart to promote the spatial component in an existing cubble
to be an sf object is
[`make_spatial_sf()`](https://huizezhang-sherry.github.io/cubble/reference/make_spatial_sf.md):

``` r

(sf_nested2 <- make_cubble(
  stations, meteo, 
  key = id, index = date, coords = c(long, lat)) |> 
  make_spatial_sf())
#> # cubble:   key: id [3], index: date, nested form, [sf]
#> # spatial:  [144.83, -37.98, 145.1, -37.67], WGS 84 (CRS84)
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name   wmo_id ts                  geometry
#>   <chr>       <dbl> <dbl> <dbl> <chr>   <dbl> <list>           <POINT [°]>
#> 1 ASN00086038  145. -37.7  78.4 essen…  95866 <tibble> (144.9066 -37.7276)
#> 2 ASN00086077  145. -38.0  12.1 moora…  94870 <tibble>   (145.0964 -37.98)
#> 3 ASN00086282  145. -37.7 113.  melbo…  94866 <tibble> (144.8321 -37.6655)
```

## Common questions

### Why summarising of temporal data by station is not in temporal form?

Q: I’m trying to summarise the average maximum temperature by station.
I’m not sure why the summarise function is not returning a temporal
object.

``` r

climate_mel |> 
  face_temporal() |> 
  summarise(tmax_avg = mean(tmax, na.rm=TRUE))
#> # A tibble: 3 × 2
#>   id          tmax_avg
#>   <chr>          <dbl>
#> 1 ASN00086038     26.5
#> 2 ASN00086077     25.7
#> 3 ASN00086282     26.6
```

**A: This operation should be performed in the spatial form. The form to
use for an operation depends on the structure of the result. If the
result has each key in a row without temporal index, it should be
operated in the spatial form (example here):**

``` r

climate_mel |> 
  rowwise() |> 
  mutate(tmax_avg = mean(ts$tmax, na.rm=TRUE))
#> # cubble:   key: id [3], index: date, nested form
#> # spatial:  [144.83, -37.98, 145.1, -37.67], Missing CRS!
#> # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
#>   id           long   lat  elev name              wmo_id ts       tmax_avg
#>   <chr>       <dbl> <dbl> <dbl> <chr>              <dbl> <list>      <dbl>
#> 1 ASN00086038  145. -37.7  78.4 essendon airport   95866 <tibble>     26.5
#> 2 ASN00086077  145. -38.0  12.1 moorabbin airport  94870 <tibble>     25.7
#> 3 ASN00086282  145. -37.7 113.  melbourne airport  94866 <tibble>     26.6
```

The same treatment applies, if your spatial cubble is also an sf object.

## Reference

Pebesma, Edzer. 2018. “Simple Features for R: Standardized Support for
Spatial Vector Data.” *The R Journal* 10 (1): 439–46.
<https://doi.org/10.32614/RJ-2018-009>.

Wang, Earo, Dianne Cook, and Rob J Hyndman. 2020. “A New Tidy Data
Structure to Support Exploration and Modeling of Temporal Data.”
*Journal of Computational and Graphical Statistics* 29 (3): 466–78.
<https://doi.org/10.1080/10618600.2019.1695624>.
