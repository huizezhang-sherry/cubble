test_cubble <- function(df){
  if (!is_cubble(df)) abort(glue::glue("{dt} needs to be a cubble object!" ))
}


test_sheet_name <- function(df, sheet){
  if (!sheet %in% names(df)) abort(glue::glue("{sheet} must be one of the names of {df}"))
}


test_single_sheet <- function(sheet){
  if (!is_tsibble(sheet) & !is_tibble(sheet)) abort(glue::glue("{sheet_other} needs to be either a tsibble or a tibble"))
}


test_single_cb <- function(df){
  if (!is_single_cubble(df)) abort(glue::glue("{df} needs to be of class `single_cb`"))
}
