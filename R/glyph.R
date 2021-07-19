# reference to: https://github.com/ggobi/ggally/blob/master/R/gglyph.R
glyph_pixel <- function(data, x_major, y_major, var, value, gvar,
                        height = ggplot2::rel(0.7),
                        width = ggplot2::rel(0.7)){
  var_to_complete <- syms(c(gvar, var))
  x_major <- enquo(x_major)
  y_major <- enquo(y_major)
  data <- data %>% as_tibble()

  # display 3 pixels per row
  data %>%
    complete(!!!var_to_complete) %>%
    group_by(!!sym(gvar)) %>%
    mutate(minor_id = seq_len(n()),
           gid = interaction(!!x_major, !!y_major),
           gx = !!x_major + GGally::rescale11((minor_id-1) %% 3) * width / 2,
           gy = !!y_major + (minor_id-1) %/% 3) %>%
    ungroup()

}
