#' Clear data frame cache
#'
#' @return nothing
#'
#' @family data-frame-cache
#' @export
prep_purge_data_frame_cache <- function() {
  rm(list = ls(.dataframe_environment(), all.names = TRUE),
     envir = .dataframe_environment())
  util_message("All cached dataframes have been purged.")
}
