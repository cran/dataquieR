#' Internal function only existing for technical reasons, planned to be removed in
#' future releases
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description Please use instead the function [acc_mahalanobis()]
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type integer
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all variables of `resp_vars`
#' - The `Mahalanobis` distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The default to consider a value an `outlier` is to use the 0.975 quantile
#'    of a theoretical chi-square distribution with degrees of freedom
#'    equals to the number of variables used to calculate the
#'    `Mahalanobis` distance (`Mayrhofer and Filzmoser`, 2023)
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the names of the computed variable
#'                                    containing `Mahalanobis` distance ratio
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param label_col [variable attribute] the name of the column in the
#'                                       metadata containing the labels of
#'                                       the variables
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path or file name of the workbook like
#'                                 metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`
#' @param meta_data_cross_item [data.frame] -- Cross-item level metadata
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
#'
#'
#' @return a list with:
#'   - `SummaryData`: [data.frame] underlying the plot with user friendly caption
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2::ggplot2] Q-Q plot of squared `Mahalanobis`
#'                                        distances vs. a theoretical
#'                                        chi-squared distribution showing `outliers`.
#'   - `FlaggedStudyData` [data.frame] contains the original data frame of the
#'                                      variables used to calculate
#'                                      the squared `Mahalanobis` distances
#'                                      with an additional column indicating if
#'                                      for a group of variables if the
#'                                      observational unit is a
#'                                      multivariate `outlier`.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_path  scale_color_manual geom_point discrete_scale theme_minimal scale_alpha_manual
#'
#' @importFrom stats mahalanobis
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
acc_mahalanobis_ratio <- function(resp_vars = NULL,
                                  study_data,
                                  label_col = VAR_NAMES,
                                  item_level = "item_level",
                                  meta_data = item_level,
                                  meta_data_v2,
                                  meta_data_cross_item = "cross-item_level",
                                  cross_item_level,
                                  `cross-item_level`) {
  # preps -----------------------------------------------
  util_maybe_load_meta_data_v2()
  # Load cross-item_level metadata and normalize it ----
  # check if there is a cross item metadata and if it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(sprintf(
      "No cross-item level metadata %s found",
      sQuote(meta_data_cross_item)))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                       CHECK_LABEL = character(0))
  }

  # First normalize input for meta_data_cross_item from the user
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  #Check label_col
  if (missing(label_col)) {
    orig_label_col <- rlang::missing_arg()
  } else {
    orig_label_col <- force(label_col)
  }

  label_col <- attr(prep_get_labels("",
                                    item_level = meta_data,
                                    label_class = "SHORT",
                                    label_col = label_col),
                    "label_col")

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = FALSE) #Otherwise it already removes outliers (ratio values > 1)!

  #check for variable role in the metadata for the resp_var
  util_correct_variable_use(resp_vars,
                            need_type = DATA_TYPES$FLOAT,
                            need_scale = SCALE_LEVELS$RATIO,
                            need_computed_role = COMPUTED_VARIABLE_ROLES$MAHALANOBIS_RATIO
  )
  #Check resp_vars
  util_expect_scalar(
    arg_name = resp_vars,
    allow_more_than_one = FALSE,
    check_type = is.character
  )


  # select current variable from data ------------------------------------
  #Select CHECK_ID of the current variables group
  current_check_id <- util_map_labels(resp_vars, meta_data = meta_data, to = CHECK_ID, from = label_col)
  all_checkID_with_vars <- setNames(meta_data_cross_item$VARIABLE_LIST,
                                    nm = meta_data_cross_item$CHECK_ID)
  intermediate2 <- lapply(util_parse_assignments(all_checkID_with_vars,
                                                 multi_variate_text = TRUE),
                          lapply,
                          prep_get_labels,
                          label_col = VAR_NAMES,
                          force_label_col = "TRUE",
                          item_level = meta_data)
  intermediate3 <- lapply(intermediate2, unique)
  no_vars_per_check_ID <- vapply(lapply(intermediate3,
                                        function(vl) intersect(unlist(vl), meta_data$VAR_NAMES)), length,
                                 FUN.VALUE = integer(1))
 # vars_per_check_ID  <- lapply(intermediate2, names)

  current_no_vars <- as.numeric(no_vars_per_check_ID[current_check_id])
  current_vars <- all_checkID_with_vars[[current_check_id]]
  rm(all_checkID_with_vars, intermediate2, intermediate3, no_vars_per_check_ID)


  FlaggedStudyData <- ds1
  current_MD_ratio <- ds1[[resp_vars]]
  current_MD_ratio <-  current_MD_ratio[order(current_MD_ratio)]

  #current_MD_ratio <- current_MD_ratio[order(current_MD_ratio)]


  #Define the threshold ----
  threshold_defined <-meta_data_cross_item[meta_data_cross_item$CHECK_ID == current_check_id,
                         MAHALANOBIS_THRESHOLD]
  if (tolower(trimws(threshold_defined)) %in% c("true", "1", "t", "+")) {
    mahalanobis_threshold <- dataquieR.MAHALANOBIS_THRESHOLD_default
  } else {
    mahalanobis_threshold <- as.numeric(threshold_defined)
    util_expect_scalar(mahalanobis_threshold)
  }
  rm(threshold_defined)


  MD_outliers_threshold <- stats::qchisq(mahalanobis_threshold,
                                         df = current_no_vars)




  n_prior <- length(current_MD_ratio)
  current_MD_ratio_fin <- current_MD_ratio[!is.na(current_MD_ratio)]
  n_post <- length(current_MD_ratio_fin)

  if (n_post == 0) {
    util_error("No samples with complete cases in the variables %s. Aborting.",
               paste0(sQuote(vars), collapse = ", "),
               applicability_problem = FALSE)
  }

  if (n_post < n_prior) {
    util_message(paste0(
      "Due to missing values",
      " N=", n_prior - n_post,
      " observational units were excluded."
    ), applicability_problem = FALSE)
  }


  #Create plot
  res_MD <- util_create_mahalanobis_ggplot(MD_ratio = current_MD_ratio_fin,
                                           mahalanobis_threshold = mahalanobis_threshold,
                                           df = current_no_vars)
  p1 <- res_MD$plot_MD

  # new complete data with the column indicating the outliers
  FlaggedStudyData$MD_outliers <- NA
  FlaggedStudyData$MD_outliers <- ifelse(FlaggedStudyData[resp_vars] > 1, 1, 0)
  n_non_ol <- sum(FlaggedStudyData$MD_outliers == 0,na.rm = TRUE)
  n_devs <- sum(FlaggedStudyData$MD_outliers == 1,na.rm = TRUE)



  # create summary table
  st1 <- data.frame(Variables = resp_vars)
  st1$"MD_outliers (N)" <- n_devs
  st1$"MD_outliers (%)" <- round(n_devs/n_post*100,
                                 digits = 2)
  st1$"N" <- n_post
  st1$"observational_units_removed" <- n_prior - n_post
  st1$"mahalanobis_threshold" <- mahalanobis_threshold

  SummaryData <- st1
  SummaryTable <- st1
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (N)"] <- "NUM_ssc_mah" # add indicator to DQ_OBS to make grading rules work, here.
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (%)"] <- "PCT_ssc_mah"
  SummaryTable <- SummaryTable[, c("Variables", "NUM_ssc_mah", "PCT_ssc_mah")]



  #Add new attribute to the columns of SummaryData to define the datatype of each column
  attr(SummaryData$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SummaryData$`MD_outliers (N)`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryData$`MD_outliers (%)`, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(SummaryData$N, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryData$observational_units_removed, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryData$mahalanobis_threshold, DATA_TYPE) <- DATA_TYPES$FLOAT


  return(list(FlaggedStudyData = FlaggedStudyData,
              SummaryTable = SummaryTable,
              SummaryData = SummaryData,
              SummaryPlot = p1
         ))
}
