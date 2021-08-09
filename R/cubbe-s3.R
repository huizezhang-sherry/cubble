#' Extract cubble attributes
#' @param group the spatio identifier
#' @param meta_data metadata to include in the attributes
#' @param form whether the long or wide form
#' @rdname data-structure
#' @export
cubble_df <- function(data, group, meta_data,  form) {
  new_cubble_df(data, group, meta_data, form = form)

}

#' @rdname data-structure
#' @export
new_cubble_df <- function(data, group, meta_data, form) {
  if (form == "list-col") {
    # this part will be simplified once we can create cubble from a subclass of rowwise_df
    nrow <- nrow(data)
    group_data <- tibble::as_tibble(data)[group]
    group_data <- tibble::new_tibble(vec_data(group_data), nrow = nrow)
    group_data$.rows <- new_list_of(as.list(seq_len(nrow)), ptype = integer())
  } else if (form == "long") {
    nrow <- nrow(meta_data)
    group_data <- dplyr:::compute_groups(data, group)
  }

  cls <- class(data)[!class(data) %in% c("cubble_df", "grouped_df", "rowwise_df")]
  if (form == "list-col"){
    class <- c("cubble_df", "rowwise_df", cls)
  } else if (form == "long"){
    class <- c("cubble_df", "grouped_df", cls)
  } else{
    abort("{form} meeds to be either long or list-col")
  }

  attr <- list(x = data,
               groups = group_data,
               meta = meta_data,
               form = form,
               class = class)
  tsibble_attr <- NULL

  if ("tbl_ts" %in% class(data)){

    # `key` attribute is not included since it is already there
    tsibble_attr_name <- c("index", "index2", "interval")
    tsibble_attr <- list(data %@% "index",
                         data %@% "index2",
                         data %@% "interval")
  }
  attr <- c(attr, tsibble_attr)

  rlang::exec("new_tibble", !!!attr)

}


#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {

  group <- group_vars(data)
  group_n <- map_dbl(group, ~length(unique(groups(data)[[.x]])))
  group_msg <- glue::glue_collapse(glue::glue("{group} [{group_n}]"), sep = ", ")
  meta_names <- meta(data) %>% names()
  type_sum <- map_chr(meta(data), pillar::type_sum)
  meta_msg <- glue::glue_collapse(glue::glue("{meta_names} [{type_sum}]"), sep = ", ")


  if(form(data) == "list-col"){
    item <- group_vars(data)[1]
    msg <- glue::glue("{item}-wise: list-column")
  } else if(form(data) == "long"){
    msg <- glue::glue("time-wise: long form")
  }

  if ("tbl_ts" %in% class(data)){
    msg <- glue::glue("{msg} [tsibble]")
  }

  c(
    "Cubble" = msg,
    "Group" = group_msg,
    "Meta" = meta_msg

  )
}


#' @rdname data-structure
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}


#' get attributes for a cubble object
#'
#' @param data an cubble object
#'
#' @export
#' @rdname attributes
form <- function(data){
  test_cubble(data)
  data %@% form
}

determine_form <- function(data){
  cls <- unlist(map(data, class))

  if ("list" %in% cls){
    "list-col"
  } else{
    "long"
  }
}

#' @export
#' @rdname attributes
meta <- function(data){
  test_cubble(data)
  data %@% meta
}

#' @export
#' @rdname attributes
group_vars <- function(data){
  groups <- groups(data)
  names <- names2(groups)
  names[names != ".rows"]
}

#' @export
#' @rdname attributes
groups <- function(data){
  data %@% groups
}
