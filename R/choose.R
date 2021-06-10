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
  new_sheet_single(sheet_chosen, by = by, cb = df)

}

#' @keywords internal
prep_chosen_sheet <- function(df, sheet){
  df[[sheet]]
}

#' @keywords internal
prep_other_sheet <- function(df, sheet){
  other <- names(df)[!names(df) %in% sheet]
  df[[other]]

}
