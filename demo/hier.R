library(cubble)

cb <- climate_flat %>%
  as_cubble(key = id, index = date, coords = c(long, lat)) %>%
  mutate(cluster = sample(1:3, 1))


cb_hier <- cb %>% switch_key(cluster)

cb_hier %>% stretch()
