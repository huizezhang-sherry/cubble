test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


create_cubble <- function(data){
  data %>% as_cubble(key = id, index= date, coords = c(long, lat))
}

has_dup_idx <- climate_flat
has_dup_idx$date[[12]] <- has_dup_idx$date[[11]]
out <- has_dup_idx %>% create_cubble()


has_na_index <- climate_flat
has_dup_idx$date[[12]] <- NA
out <- has_dup_idx %>% create_cubble()


wrong_long <- climate_flat %>% mutate(long = ifelse(id == "ASN00086038", long + 100, long))
out <- wrong_long %>% create_cubble()
wrong_long %>% as_cubble(key = id, index= date, coords = c(lat, long))


a <- make_cubble(spatial = stations_sf, temporal = meteo_ts, key = id, index = date)
b <- a %>% face_temporal()
c <- b %>% face_spatial()


make_cubble(spatial = stations, temporal = meteo, key = id, index = date, coords = c(long, lat))

make_cubble(spatial = stations_sf, temporal = meteo, key = id, index = date)

make_cubble(spatial = stations, temporal = meteo_ts, coords = c(long, lat))

make_cubble(spatial = stations_sf, temporal = meteo_ts)
