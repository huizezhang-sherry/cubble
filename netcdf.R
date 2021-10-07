library(ncdf4)
library(magrittr)
library(lubridate)
nc <- nc_open(here::here("data/ipcc.nc"))
nc <- nc_open(here::here("data/2b.nc4"))
nc2 <- nc_open(here::here("data/feisty_gfdl-esm4_nobasd_ssp126_nat_default_bd90cm_global_monthly_2015_2100.nc"))
#data <- ncvar_get(nc, "pr")
data <- ncvar_get(nc, "atotuse")
long_raw <- ncvar_get(nc, "lon")
lat_raw <- ncvar_get(nc, "lat")
time_raw <- ncvar_get(nc, "time")
tunits <- ncatt_get(nc,"time","units")
time_origin <- as.Date(gsub("[^0-9|-]", "\\1", tunits$value))

time_new <- time_origin %m+%  months(time_raw)

names(nc$dim)



# check long lat matches with the dimension of the data

if (!length(long) %in% dim(data) | !length(lat) %in% dim(data)){
  abort("lat, long specified doesn't match with the data dimension.
        check `length(long)`, `length(lat)`, and `dim(data)`")
}

if (length(long) == dim(data)[1]){
  row_as_long <- TRUE
} else{
  row_as_long <- FALSE
}

if (length(dim(data)) == 3){
  time_length <- dim(data)[3]
  out <- map(1: time_length, clean_single)
}


one_layer <- data[]


out <- expand.grid(long = long, lat = lat) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(long_idx = which(long %in% long_raw),
                lat_idx = which(lat %in% lat_raw),
                id = dplyr::row_number()) %>%
  head(5) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(ts = list(tibble::tibble(
    time = time_new,
    val = as.vector(data[long_idx,lat_idx, ])))) %>%
  dplyr::select(-long_idx, -lat_idx)

out_cubble <- out %>%
  as_cubble(key = id, index = val, coords = c(long, lat))



