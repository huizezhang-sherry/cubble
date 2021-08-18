#' Extract cubble attributes
#' @param ... a list object to create new cubble
#' @param data the object to be created or tested as cubble
#' @param group the spatio identifier
#' @param leaves metadata to include in the attributes
#' @param form whether the long or wide form
#' @rdname data-structure
#' @export
cubble <- function(..., group, leaves, form) {
  #browser()
  # data <- tibble::tibble(!!!list2(...))
  # group <- enquo(group)
  #leaves <- as_leaves(leaves)
  #new_cubble(data, !!group, form = form)

}

#' @rdname data-structure
#' @export
new_cubble <- function(data, group, leaves, form) {

  if (form == "nested") {
    # this part will be simplified once we can create cubble from a subclass of rowwise_df
    nrow <- nrow(data)
    group_data <- tibble::as_tibble(data)[group]
    group_data <- tibble::new_tibble(vec_data(group_data), nrow = nrow)
    group_data$.rows <- new_list_of(as.list(seq_len(nrow)), ptype = integer())
  } else if (form == "long") {
    nrow <- nrow(data)
    group_data <- dplyr:::compute_groups(data, group)
  }

  cls <- class(data)[!class(data) %in% c("cubble_df", "grouped_df", "rowwise_df")]
  if (form == "nested"){
    class <- c("cubble_df", "rowwise_df", cls)
  } else if (form == "long"){
    class <- c("cubble_df", "grouped_df", cls)
  } else{
    abort("{form} meeds to be either long or nested")
  }

  attr <- list(x = data,
               nrow = nrow,
               groups = group_data,
               leaves = leaves,
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


  if (form(data) == "nested"){
    variant <- leaves(data) %>% variant()
    var_names <- names(variant)
    var_type <- variant

  } else if (form(data) == "long"){
    invariant <- leaves(data) %>% invariant()
    var_names <- names(invariant)
    var_type <- invariant
  }
  leaves_msg <- glue::glue_collapse(glue::glue("{var_names} [{var_type}]"), sep = ", ")


  if(form(data) == "nested"){
    item <- group_vars(data)[1]
    msg <- glue::glue("{item}-wise: nested form")
  } else if(form(data) == "long"){
    msg <- glue::glue("time-wise: long form")
  }

  if ("tbl_ts" %in% class(data)){
    msg <- glue::glue("{msg} [tsibble]")
  }

  c(
    "Cubble" = msg,
    "Group" = group_msg,
    "Leaves" = leaves_msg

  )
}

#' @rdname data-structure
#' @export
is_cubble <- function(data){
  inherits(data, "cubble_df")
}

