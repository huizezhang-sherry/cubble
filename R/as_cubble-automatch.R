cubble_automatch <- function(spatial, temporal, key_nm, matched_tbl, only_spatial, only_temporal){
  # if has_unmatch, see if some can be auto-matched with
  spatial_v <- intersect(spatial[[key_nm]], only_spatial)
  temporal_v <- intersect(temporal[[key_nm]], only_temporal)
  t <- gsub("\\s\\(.+\\)", "", temporal_v)
  s <- gsub("\\s\\(.+\\)", "", spatial_v)
  t_idx <- grep(paste0(s, collapse = "|"), t)
  s_idx <- grep(paste0(t, collapse = "|"), s)


  if (length(t_idx) == 0 | length(s_idx) == 0){
    others <- list(temporal = temporal_v, spatial = spatial_v)
  } else{
    matched_tbl <- matched_tbl %>%
      rbind(
        tibble::tibble(
          spatial = sort(spatial_v[s_idx]),
          temporal = sort(temporal_v[t_idx])
          )
        )

    others <- list(temporal = temporal_v[-t_idx],
                   spatial = spatial_v[-s_idx])
  }

  return(list(
    paired = matched_tbl, others = others
  ))

}

