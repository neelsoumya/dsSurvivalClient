#' Title
#'
#' @param survfit_object 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.life.table <- function(survfit_object, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call("life.tableDS", as.symbol(survfit_object))
  res <- DSI::datashield.aggregate(datasources, calltext)
  if(names(res[[1]][1]) == "life.table_no_strata"){
    df_list <- purrr::flatten(res)
    combined_df <- dplyr::bind_rows(df_list)
    filtered_df <- .compute_survival_rates(combined_df)
    plot_survival_curves(filtered_df)
  } else {
    df_list <- purrr::flatten(res)
    # First, create a list where each element is a list of data frames with the same name
    same_name_dfs <- split(df_list, names(df_list))
    # Now, combine each list of data frames into a single data frame
    combined_dfs <- purrr::map(same_name_dfs, dplyr::bind_rows)
    # Calcul surv
    combined_dfs_t <- lapply(1:length(combined_dfs), function(x){
      df <- combined_dfs[[x]]
      filtered_df <- .compute_survival_rates(df, names(combined_dfs)[x])
    })
    names(combined_dfs_t) <- names(combined_dfs)
    
    # Merge the data frames and add a 'strata' column
    merged_df <- dplyr::bind_rows(combined_dfs_t)
    plot_survival_curves(merged_df, "strata")
  }
}

#' Compute Survival Rates
#'
#' This function computes survival rates for each time period, as well as cumulative
#' survival rates, based on grouped data in the input dataframe.
#' It then enriches the input dataframe with these calculated values and other parameters.
#'
#' @param combined_df A dataframe with 'time', 'n.risk', and 'n.event' columns.
#'                    The data should be already grouped by time.
#' @param strata_value A numeric value to fill the 'strata' column. Defaults to 1.
#'
#' @return A dataframe with added columns 'survival_rate', 'surv', 'upper', 'lower',
#'         'strata', and 'n.censor'.
#' @examples
#' df <- data.frame(time = c(1,2,3), n.risk = c(5,10,15), n.event = c(1,2,3))
#' compute_survival_rates(df, strata_value = 2)
.compute_survival_rates <- function(combined_df, strata_value = 1) {
  result_df <- combined_df |>
    dplyr::group_by(time) |>
    dplyr::summarise(n.risk = sum(n.risk), n.event = sum(n.event))
  # Compute differences in 'n.risk' column
  diffs <- diff(result_df$n.risk)
  # Find the index of the first non-increasing value
  first_non_increasing <- which(diffs < 0)[1]
  # Remove the rows before that index
  filtered_df <- result_df[first_non_increasing:nrow(result_df), ]
  # Calculate survival rate for each time period
  filtered_df$survival_rate <- 1 - (filtered_df$n.event / filtered_df$n.risk)
  # Calculate cumulative survival rate
  filtered_df$surv <- cumprod(filtered_df$survival_rate)
  filtered_df$strata <- rep(strata_value, nrow(filtered_df))
  filtered_df <- rbind(data.frame(
    time = 0, 
    n.risk = 0, 
    n.event = 0, 
    survival_rate = 1, 
    surv = 1, 
    strata = strata_value
  ), filtered_df)
  return(filtered_df)
}

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
plot_survival_curves <- function(surv_df, group_col = NULL) {
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
