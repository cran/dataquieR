#' `Roxygen`-Template for indicator functions
#'
#' @param resp_vars [variable] the names of the measurement variables, if
#'                             missing or `NULL`, all variables will be checked
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param meta_data_dataframe [data.frame] the data frame that contains the
#'                                          metadata for the data frame level
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`
#' @param segment_level [data.frame] alias for `meta_data_segment`
#' @return `invisible(NULL)`
#' @keywords internal
.template_function_indicator <-
  function(resp_vars, study_data, label_col, item_level, meta_data,
           meta_data_v2,
           meta_data_dataframe, meta_data_segment, dataframe_level,
           segment_level) { # nocov start
    util_error("nothing, just a template for Roxygen")
    invisible(NULL)
} # nocov end
