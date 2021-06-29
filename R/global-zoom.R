#' Data structure
#' @param data the data to be converted into a cubble object
#' @param key the spatio identifier. key can be automatically detected for a cubble object
#' @param group the spatio identifier
#' @param meta_data metadata to include in the attributes
#' @param form whether the long or wide form
#' @examples
#' oz_global <- global(oz_climate, station)
#' oz_zoom <- oz_global %>% zoom()
#' back <- oz_zoom %>% global(station)
#' @export
#' @rdname data-structure
global <- function(data, key) {
  UseMethod("global")
}

#' @importFrom tsibble index
#' @export
global.cubble_df <- function(data, key) {

  test_cubble(data)

  key <- enquo(key)
  if (quo_is_missing(key)){
    key <- group_vars(data)
  }

  nest_var <- find_nest_var(data, !!key)
  meta_data <- meta(data)
  out <- tibble::as_tibble(data) %>%
    dplyr::left_join(tibble::as_tibble(meta_data)) %>%
    tidyr::nest(ts = c(!!!nest_var$nest_var)) %>%
    dplyr::rowwise()

  if ("tbl_ts" %in% class(data)){
    out <- out %>% mutate(ts = list(as_tsibble(.data$ts, index = tsibble::index(data))))
  }

  cubble_df(out, group = as_name(key), meta_data = meta_data, form = determine_form(out))
}

#' @export
global.tbl_df <- function(data, key) {
  key <- enquo(key)
  nest_var <- find_nest_var(data, !!key)

  out <- data %>%
    tidyr::nest(ts = c(!!!nest_var$nest_var)) %>%
    dplyr::rowwise()
  meta_data <- out[nest_var$non_varying_var]
  cubble_df(out, group = as_name(key), meta_data = meta_data, form = determine_form(out))
}


#' @rdname data-structure
#' @export
cubble_df <- function(data, group, meta_data,  form) {
  new_cubble_df(data, group, meta_data, form = form)

}

#' @rdname data-structure
#' @export
new_cubble_df <- function(data, group, meta_data, form) {
  if (form == "list-col") {
    nrow <- nrow(data)
    group_data <- tibble::as_tibble(data)[group]
    group_data <- tibble::new_tibble(vec_data(group_data), nrow = nrow)
    group_data$.rows <- new_list_of(as.list(seq_len(nrow)), ptype = integer())
  } else if (form == "long") {
    nrow <- nrow(meta_data)
    group_data <- dplyr:::compute_groups(data, union(map_chr(group, as_name), group_vars(data)))
  }

  if (form == "list-col"){
    class <- c("cubble_df", "rowwise_df", class(data))
  } else if (form == "long" & "grouped_df" %in% class(data)){
    class <- c("cubble_df", class(data))
    # cls <- class(data)
    # cls <- cls[!cls == "grouped_df"]
    # class <- c("cubble_df", "grouped_df", cls)
  } else if (form == "long" & !"grouped_df" %in% class(data)){
    class <- c("cubble_df", "grouped_df", class(data))
  } else{
    abort("{form} meeds to be either long or list-col")
  }


  if ("tbl_ts" %in% class(data)){

    attr <- attributes(data)
    attr_to_add <- attr[!names(attr) %in% c("names", "row.names", "class")]



    attr_basics <- list(x = data,
                        groups = group_data,
                        meta = meta_data,
                        form = form,
                        class = class)

    attr_all <- c(attr_basics, attr_to_add)
    attr_all[duplicated(names(attr_all))] <- NULL


    rlang::exec("new_tibble", !!!attr_all)

  } else{

    new_tibble(data, groups = group_data, meta = meta_data, form = form, class = class)
  }

}


#' @importFrom  tibble tbl_sum
#' @export
tbl_sum.cubble_df <- function(data) {

  if(form(data) == "list-col"){
    item <- group_vars(data)[1]
    msg <- glue::glue("{item}-wise: list-column")
  } else if(form(data) == "long"){
    msg <- glue::glue("tiem-wise: long form")
  }

  c(
    NextMethod(),
    "Cubble" = msg
  )
}

#' @export
#' @rdname data-structure
zoom <- function(data) {
  test_cubble(data)
  UseMethod("zoom")
}

#' @export
zoom.cubble_df <- function(data){

  col <- sym(names(data)[map_chr(data, class) == "list"])

  if (!is_list(eval_tidy(col, data))) {
    abort("The column to zoom need to be a list-column")
  }

  group_var <- sym(group_vars(data))
  meta_data <- meta(data)

  list_col <- data %>% dplyr::pull(!!col)

  if ("tbl_ts" %in% class(list_col[[1]])){
    data$ts <- map(data$ts, tibble::as_tibble)
    out <- data[vec_c(as_name(group_var), as_name(col))]
    out <- out %>%
      tidyr::unnest(!!col) %>%
      tsibble::as_tsibble(key = !!group_var)
  } else{
    out <- data %>%
      dplyr::select(!!group_var, !!col) %>%
      tidyr::unnest(!!col)
  }

  cubble_df(out, group = group_var, meta_data = meta_data, form = determine_form(out))
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
  groups <- data %@% groups
  names <- names2(groups)
  names[names != ".rows"]
}


