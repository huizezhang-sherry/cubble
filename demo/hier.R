library(cubble)

# create an artificial cluster for stations
set.seed(1234)
cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat)) %>%
  mutate(cluster = sample(1:3, 1))

# switch the key from id to cluster
cb_cluster <- cb %>% switch_key(cluster)

# switch the key from cluster back to id
cb_id <- cb_cluster %>% switch_key(id)
