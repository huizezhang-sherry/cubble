#' Join the result to the cubble object
#' @param single_cb a single_cb object
#'
#' @importFrom dplyr left_join select
#' @importFrom stats setNames
#' @export
cubble_join <- function(single_cb){
  test_single_cb(single_cb)

  relational <- cb(single_cb)
  out_names <- colnames(single_cb)
  all_names <- c(colnames(relational$main), colnames(relational$item))
  to_join_names <- out_names[!out_names %in% all_names]

  by <- by_var(single_cb)
  by <- by[by %in% colnames(single_cb)]

  index_var <- index(cb(single_cb)$main)

  if (as_string(index_var) %in% out_names){
    # go to main sheet
    to_join <- single_cb %>% dplyr::select(by, index_var, to_join_names)
    by_main <- by_var(single_cb)[by_var(single_cb) %in% colnames(relational$main)]
    main <- relational$main %>%
      dplyr::left_join(to_join, by = c(setNames(as_string(by), as_string(by_item)), as_string(index_var)))
    new_cubble(main = main, item = relational$item, by = by_var(relational))
  } else{
    # go to item sheet
    to_join <- single_cb %>% select(by, to_join_names)
    by_item <- by_var(single_cb)[by_var(single_cb) %in% colnames(relational$item)]
    item <- relational$item %>%
      dplyr::left_join(to_join, by = stats::setNames(as_string(by), as_string(by_item)))
    new_cubble(main = relational$main, item = item, by = by_var(relational))
  }


}
