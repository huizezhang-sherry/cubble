add_hier_idx <- function(data, group_key, flow = FALSE){

  group_key <- enquo(group_key)

  out <- data %>%
    switch_key(!!group_key) %>%
    mutate(.within = as.factor(dplyr::row_number()),
           .group = as.factor(dplyr::cur_group_id())) %>%
    strip_rowwise() %>%
    mutate(.id = as.factor(dplyr::row_number())) %>%
    switch_key(!!key_vars(data), back = TRUE)

  if (flow){
    # add flow index
  }

  out
}
