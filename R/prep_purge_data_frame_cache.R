#' Clear data frame cache
#'
#' @return nothing
#'
#' @family data-frame-cache
#' @export
prep_purge_data_frame_cache <- function() {
  util_prune_odm_cache()
  util_clean_condition_once_cache()
  rm(list = ls(.dataframe_environment(), all.names = TRUE),
     envir = .dataframe_environment())
  util_message("All cached dataframes have been purged.")
}
