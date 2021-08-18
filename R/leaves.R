#' Create a leaf object
#' @param data the object to be created or tested as cubble
#' @param group the spatio identifier
#' @rdname leaves
#' @export
new_leaves <- function(data, group){

  data <- as_tibble(data)
  data <- data[map_lgl(as_tibble(data), ~all(class(.x) != "list") )]
  group <- enquo(group)
  all_vars <- find_invariant(data, !!group)
  invariant <- data %>% select(all_vars$invariant) %>% map_chr(pillar::type_sum)
  variant <- data %>% select(all_vars$variant) %>% map_chr(pillar::type_sum)
  leaves_data <- unique(data[names(invariant)])

  tibble::new_tibble(leaves_data, nrow = nrow(leaves_data),
                     group = as_name(group),
                     invariant = invariant, variant = variant,
                     class = "leaves")
}

#' @export
tbl_sum.leaves <- function(data){
  group <- groups(data)
  var_names <- names(variant(data))
  var_type <- variant(data)
  c("Leaves" = "invariant",
    "variant" = glue::glue_collapse(glue::glue("{var_names} [{var_type}]"), sep = ", "),
    NextMethod())
}

#' @export
#' @rdname leaves
is_leaves <- function(data){
  inherits(data, "leaves")
}

#' @export
#' @rdname leaves
invariant <- function(data){
  test_leaves(data)
  data %@% invariant
}

#' @export
#' @rdname leaves
variant <- function(data){
  test_leaves(data)
  data %@% variant
}
