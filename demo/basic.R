library(cubble)

# basic stretch
cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat))
cb %>% stretch()

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
