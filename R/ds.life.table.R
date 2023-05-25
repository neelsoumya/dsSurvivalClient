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
    filtered_df$upper <- filtered_df$surv
    filtered_df$lower <- filtered_df$surv
    filtered_df$strata <- rep(1, nrow(filtered_df))
    filtered_df$n.censor <- rep(1, nrow(filtered_df))
    
    survminer::ggsurvplot_df(filtered_df, color = "blue", censor.shape = "")
  } else {
    df_list <- purrr::flatten(res)
    # First, create a list where each element is a list of data frames with the same name
    same_name_dfs <- split(df_list, names(df_list))
    # Now, combine each list of data frames into a single data frame
    combined_dfs <- purrr::map(same_name_dfs, dplyr::bind_rows)
    # Calcul surv
    combined_dfs_t <- lapply(1:length(combined_dfs), function(x){
      df <- combined_dfs[[x]]
      result_df <- df |>
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
      filtered_df$upper <- filtered_df$surv
      filtered_df$lower <- filtered_df$surv
      filtered_df$strata <- rep(names(combined_dfs)[x], nrow(filtered_df))
      filtered_df$n.censor <- rep(1, nrow(filtered_df))
      return(filtered_df)
    })
    names(combined_dfs_t) <- names(combined_dfs)
    
    
    
    # Merge the data frames and add a 'strata' column
    merged_df <- dplyr::bind_rows(combined_dfs_t)
    # merged_df <- dplyr::bind_rows(combined_dfs, .id = "strata")
    survminer::ggsurvplot_df(merged_df, palette = "blue", linetype = c(1), censor.shape = "")
    
    
    
    browser()
  }
  
  # fit <- survival::survfit(survival::Surv(time, n.event) ~ 1, data = filtered_df)
  # survminer::ggsurvplot(fit)
  # summary(fit)
  # 
  # table1$group <- "group1"
  # table2$group <- "group2"
  
}