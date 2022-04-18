library(cubble)

# face_temporal
# basic
cb <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat))
tmp <- cb %>%  face_temporal()

# face_temporal a hierarchical
cb_hier <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  mutate(cluster = sample(1:3, 1)) %>% 
  switch_key(cluster)
cb_hier %>%  face_temporal()

# face_temporal a tsibble-cubble
cb_tsibble <- climate_flat %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  as_cubble(coords = c(long, lat))
cb_tsibble %>%  face_temporal()

####################################################################
####################################################################
# face_spatial
# basic
cb <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  face_temporal()

cb %>%  face_spatial()

cb %>% 
  group_by(month = lubridate::month(date)) %>% 
  face_spatial()

# face_spatial a hierarchical: Hierarchical with more than one key
set.seed(1234)
cb_hier <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  mutate(cluster = sample(1:3, 1)) %>% 
  switch_key(cluster) %>% 
  face_temporal()

cb_hier %>%  face_spatial()

# face_spatial a tsibble-cubble
cb_tsibble <- climate_flat %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  as_cubble(coords = c(long, lat)) %>% 
  face_temporal()
cb_tsibble %>%  face_spatial()

####################################################################
####################################################################
# unfold
cb <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  face_temporal()

# unfold long and lat
cb_mig <- cb %>%  unfold(long, lat)

# migration is not memorised by cubble:
# if you switch to the nested cubble and then switch back,
# long and lat will not be preserved
cb_mig %>%  face_spatial() %>%  face_temporal()
cb %>% 
  group_by(month = lubridate::month(date)) %>% 
  unfold(long, lat)

# face_spatial a hierarchical: Hierarchical with more than one key
set.seed(1234)
cb_hier <- climate_flat %>% 
  as_cubble(key = id, index = date, coords = c(long, lat)) %>% 
  mutate(cluster = sample(1:3, 1)) %>% 
  switch_key(cluster) %>% 
  face_temporal()
cb_hier %>%  unfold(long, lat)


# face_spatial a tsibble-cubble
cb_tsibble <- climate_flat %>% 
  tsibble::as_tsibble(key = id, index = date) %>% 
  as_cubble(coords = c(long, lat)) %>% 
  face_temporal()
cb_tsibble %>%  unfold(long, lat)


