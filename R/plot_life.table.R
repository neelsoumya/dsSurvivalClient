#' Plot Survival Curves
#'
#' This function plots the survival curves using ggplot2 and geom_step.
#' It takes a dataframe resulting from a survival analysis and an optional grouping variable for stratified plots.
#'
#' @param surv_df A dataframe from survival analysis, must contain 'time', 'surv', 'upper', 'lower' columns.
#' @param group_col (Optional) A character string specifying the column name in surv_df for stratified plots.
#'
#' @return A ggplot2 object representing the survival curves.
#' @export
#' @examples
#' df <- data.frame(time = c(1,2,3), surv = c(0.8, 0.6, 0.4), upper = c(0.9, 0.7, 0.5), lower = c(0.7, 0.5, 0.3), group = c('A', 'B', 'A'))
#' plot_survival_curves(df, 'group')
plot_life.table <- function(surv_df, group_col = NULL) {
  if(!is.null(group_col)) {
    plot <- ggplot2::ggplot(surv_df, ggplot2::aes(x = time, y = surv, color = .data[[group_col]])) +
      ggplot2::geom_step() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Time", y = "Survival probability", color = group_col) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 10), legend.text = ggplot2::element_text(size = 10))
  } else {
    plot <- ggplot2::ggplot(surv_df, ggplot2::aes(x = time, y = surv)) +
      ggplot2::geom_step() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Time", y = "Survival probability")
  }
  return(plot)
}

