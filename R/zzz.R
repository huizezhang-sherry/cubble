onLoad <- function(...) {
  s3_register("dplyr::dplyr_col_modify", "cubble_df")
  s3_register("dplyr::dplyr_row_slice", "cubble_df")
  s3_register("dplyr::dplyr_reconstruct", "cubble_df")
  s3_register("tsibble::as_tsibble", "cubble_df")
  s3_register("tsibble::index_by", "cubble_df")
  s3_register("dplyr::summarise", "cubble_df")
}
