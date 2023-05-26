.onLoad <- function(...) {
  s3_register("dplyr::dplyr_col_modify", "cubble_df")
  s3_register("dplyr::dplyr_row_slice", "cubble_df")
  s3_register("dplyr::dplyr_reconstruct", "cubble_df")

  #s3_register("tsibble::index_by", "cubble_df")
  s3_register("dplyr::summarise", "cubble_df")
  s3_register("dplyr::ungroup", "cubble_df")
  s3_register("dplyr::group_by", "cubble_df")

  s3_register("dplyr::slice_head", "cubble_df")
  s3_register("dplyr::slice_tail", "cubble_df")
  s3_register("dplyr::slice_min", "cubble_df")
  s3_register("dplyr::slice_max", "cubble_df")
  s3_register("dplyr::slice_sample", "cubble_df")

  s3_register("dplyr::rename", "cubble_df")
  s3_register("base::print", "cubble_df")
  s3_register("tsibble::fill_gaps", "cubble_df")
  s3_register("tsibble::key_data", "cubble_df")
}
