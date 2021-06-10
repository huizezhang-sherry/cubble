#' Choose which sheet to work on
#'
#' @param df a cubble object
#' @param sheet one of the sheet in the cubble object
#' @export
choose <- function(df, sheet){

  test_cubble(df)

  sheet <- as_string(ensym(sheet))
  test_sheet_name(df, sheet)

  sheet_chosen <- prep_chosen_sheet(df, sheet)
  sheet_other <- prep_other_sheet(df, sheet)

  by <- by_var(df)
  new_sheet_single(sheet_chosen, by = by, cb_other = sheet_other)

}

#' @keywords internal
prep_chosen_sheet <- function(df, sheet){
  df[[sheet]]
}

#' @keywords internal
prep_other_sheet <- function(df, sheet){
  other <- names(df)[names(df) %in% sheet]
  sheet_other <- df[[other]]

}

#' Create a single cubble object
#' @param single_sheet one of the sheet in a cubble object
#' @param by the joining variable
#' @param cb_other the other sheet in the cubble object
#' @export
new_sheet_single <- function(single_sheet, by, cb_other){
  if(is_tsibble(single_sheet)){
    new_tsibble(single_sheet, by = by, cb_other = cb_other, class = "single_cb")
  } else if (is_tibble(single_sheet)){
    new_tibble(single_sheet, by = by, cb_other = cb_other, class = "single_cb")
  }

}



# cubble_join <- function(single_cb){
#   test_single_cb(single_cb)
#
#
#
# }
