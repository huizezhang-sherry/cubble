test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



#another example on spatial match
nsw_map <- ozmaps::abs_ste %>%
  filter(NAME == "New South Wales")

nsw <- climate_aus %>%
  # subset for New South Wales stations
  filter(between(as.numeric(stringr::str_sub(id, 7, 8)), 46, 75)) %>%
  mutate(automated = stringr::str_detect(name, "aws")) %>%
  face_temporal(ts) %>%
  filter(lubridate::month(date) == 1,
         lubridate::year(date) == 2020) %>%
  filter(!is.na(tmax)) %>%
  face_spatial() %>%
  rowwise() %>%
  filter(nrow(ts) == 31)

ggplot() +
  geom_sf(data = nsw_map, color = "grey", linetype = "dotted") +
  geom_point(data = nsw, aes(x = long, y = lat, color = automated)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("New Sourth Wales") +
  coord_sf(xlim = c(141, 154))

auto <- nsw %>%  filter(automated)
non_auto <- nsw %>%  filter(!automated)

matched <- match_spatial(auto, non_auto)
matched <- bind_rows(matched) %>% filter(group <= 10)

ggplot() +
  geom_sf(data = nsw_map) +
  geom_point(data = as_tibble(matched),
             aes(x = long, y = lat, color = automated)) +
  ggrepel::geom_label_repel(
    data = matched %>% filter(automated) %>% as_tibble(),
    aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("New South Wales") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_sf(xlim = c(141, 154))


ts <- matched %>%
  face_temporal() %>%
  unfold(group, automated)

ts %>%
  ggplot(aes(x = date, y = tmax, color = automated)) +
  geom_line() +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_labels = "%d") +
  theme_bw()
