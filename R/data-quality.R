#' data quality plot
#'
#' @param data the data set with missing calculated
#' @param ... the variables to order by
#' @param order_op the operation on the variables for ordering, default to identity
#'
#' @examples
#' # add missing summaries
#' out <- climate_small %>%
#'          add_missing_prct(prcp:tmin) %>%
#'          add_missing_dscrb()
#'
#' # order by a single variable
#' out %>% quality_plot(prcp_missing)
#' out %>% quality_plot(tmax_missing)
#'
#' # order by mean or median of several variables
#' out %>% quality_plot(prcp_missing, tmax_missing, order_op = mean)
#' out %>% quality_plot(prcp_missing, tmax_missing, tmin_missing, order_op = median)
#' @export
#'
quality_plot <- function(data, ..., order_op = I){

  order_by <- ensyms(...)
  order_op <- enexpr(order_op)

  data_rank <- data %>%
    mutate(rank = eval(expr((!!order_op)(c(!!!order_by)))))

  longer_dscrb <- data_rank %>%
    tidyr::pivot_longer(cols = c(ends_with("dscrb")), names_to = "var", values_to = "dscrb")

  p1 <- longer_dscrb %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$var, y = forcats::fct_reorder(.data$station, rank), alpha = .data$dscrb)) +
    ggplot2::geom_tile() +
    ggplot2::coord_equal(ratio = 1) +
    ggplot2::ylab("station") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = -0.1))

  longer_missing <- data_rank %>%
    tidyr::pivot_longer(cols = c(ends_with("missing")), names_to = "var", values_to = "missing")

  p2 <- longer_missing %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$missing, y = forcats::fct_reorder(.data$station, rank), color = .data$var)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.length.y = ggplot2::unit(0, "cm")) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::ylab("")

  patchwork::wrap_plots(p1, p2) + patchwork::plot_layout(guides = "collect")
}
