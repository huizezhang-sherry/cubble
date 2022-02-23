library(cubble)

# stretch
# basic
cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat))
tmp <- cb %>% stretch()

# stretch a hierarchical
cb_hier <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat)) %>%
  mutate(cluster = sample(1:3, 1)) %>%
  switch_key(cluster)
cb_hier %>% stretch()

# stretch a tsibble-cubble
cb_tsibble <- climate_flat %>%
  tsibble::as_tsibble(key = id, index = date) %>%
  as_cubble(coords = c(long, lat))
cb_tsibble %>% stretch()

# tamp
# basic
cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat)) %>%
  stretch()

cb %>% tamp()

cb %>%
  group_by(month = lubridate::month(date)) %>%
  tamp()

# tamp a hierarchical: Hierarchical with more than one key
set.seed(1234)
cb_hier <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat)) %>%
  mutate(cluster = sample(1:3, 1)) %>%
  switch_key(cluster) %>%
  stretch()

cb_hier %>% tamp()

# tamp a tsibble-cubble
cb_tsibble <- climate_flat %>%
  tsibble::as_tsibble(key = id, index = date) %>%
  as_cubble(coords = c(long, lat)) %>%
  stretch()
cb_tsibble %>% tamp()




