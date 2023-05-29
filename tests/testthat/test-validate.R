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

a <- make_cubble(spatial = stations, temporal = meteo, key = id, index = date, coords = c(long, lat))
b <- a %>% face_temporal()
c <- b %>% face_spatial()


# dply_row_slice
res <- a %>% filter(elev > 40)
res <- b %>% filter(prcp > 0)
res <- b %>% filter(stringr::str_detect(id, "2"))

res <- a %>% mutate(elev2 = elev + 10)
res <- b %>% mutate(prcp2 = prcp + 10)


a <- make_cubble(spatial = stations_sf, temporal = meteo_ts)
b <- a %>% face_temporal()
c <- b %>% face_spatial()

# dply_row_slice
res <- a %>% filter(elev > 40) # fix from the sf side
res <- b %>% filter(prcp > 0)
res <- b %>% filter(stringr::str_detect(id, "2"))

res <- a %>% mutate(elev2 = elev + 10)
res <- b %>% mutate(prcp2 = prcp + 10)

# arrange
a %>% arrange(wmo_id) # fix from the sf side
b %>% arrange(prcp)

# select
a %>% select(-elev)
b %>% select(-prcp) # not working



# this doesn't work
# a <- climate_mel %>% dplyr::bind_cols(group = c(1, 1, 2))
a <- climate_mel
a$group = c(1,1, 2)


b <- a %>% face_temporal() %>% unfold(group)
c <- b %>% group_by(group)

# | verb | spatial_cubble_df | temporal_cubble_df|
# | arrange | dplyr_row_slice.spatial_cubble_df | arrange.temporal_cubble_df |
# | mutate | dplyr_row_slice.spatial_cubble_df | dplyr_row_slice.temporal_cubble_df|
# | filter | dplyr_row_slice.spatial_cubble_df | dplyr_row_slice.temporal_cubble_df|
# | select |
# | group_by | group_by.temporal_cubble_df | group_by.temporal_cubble_df |
# | summarise | summarise.temporal_cubble_df | summarise.temporal_cubble_df |


