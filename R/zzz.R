.onLoad <- function(...) {
  s3_register("dplyr::dplyr_col_modify", "cubble_df")
  s3_register("dplyr::dplyr_row_slice", "spatial_cubble_df")
  s3_register("dplyr::dplyr_row_slice", "temporal_cubble_df")
  s3_register("dplyr::dplyr_reconstruct", "spatial_cubble_df")
  s3_register("dplyr::dplyr_reconstruct", "temporal_cubble_df")

  s3_register("dplyr::arrange", "temporal_cubble_df")
  s3_register("dplyr::select", "spatial_cubble_df")
  s3_register("dplyr::select", "temporal_cubble_df")
  s3_register("dplyr::group_by", "temporal_cubble_df")
  s3_register("dplyr::summarise", "spatial_cubble_df")
  s3_register("dplyr::summarise", "temporal_cubble_df")
  s3_register("dplyr::mutate", "spatial_cubble_df")
  s3_register("dplyr::filter", "spatial_cubble_df")
  s3_register("dplyr::arrange", "spatial_cubble_df")
  s3_register("dplyr::rowwise", "spatial_cubble_df")
  s3_register("dplyr::rowwise", "temporal_cubble_df")

  #s3_register("tsibble::index_by", "cubble_df")
  #s3_register("dplyr::ungroup", "cubble_df")

  # s3_register("dplyr::slice_head", "cubble_df")
  # s3_register("dplyr::slice_tail", "cubble_df")
  # s3_register("dplyr::slice_min", "cubble_df")
  # s3_register("dplyr::slice_max", "cubble_df")
  # s3_register("dplyr::slice_sample", "cubble_df")

  s3_register("base::print", "cubble_df")
  s3_register("tsibble::fill_gaps", "temporal_cubble_df")
  s3_register("tsibble::scan_gaps", "temporal_cubble_df")
  s3_register("tsibble::key", "cubble_df")
  s3_register("tsibble::key_data", "cubble_df")
  s3_register("tsibble::key_vars", "cubble_df")

}
