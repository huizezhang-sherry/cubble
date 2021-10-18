## code to prepare `tzone_list` dataset goes here
tzone_list <- lutz::tz_list() %>%
  dplyr::group_by(utc_offset_h) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(tz_name, utc_offset_h)

usethis::use_data(tzone_list, internal = TRUE, overwrite = TRUE)
