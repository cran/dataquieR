#' Compute Descriptive Statistics
#' @description
#'
#' generates a descriptive overview of the variables in `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the measurement variables
#' @param hard_limits_removal [logical] if TRUE values outside hard limits are
#'                                      removed from the data before calculating
#'                                      descriptive statistics.
#'                                      The default is FALSE
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame]
#'   - `SummaryData`: [data.frame]
#' @export
#'
#' @examples
#' \dontrun{
#' xx <- des_summary(study_data = "study_data", meta_data_v2 = "meta_data_v2")
#' xx$SummaryData
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'

des_summary <- function(resp_vars = NULL,
                        study_data,
                        label_col,
                        item_level = "item_level",
                        meta_data = item_level,
                        meta_data_v2,
                        hard_limits_removal =
                          getOption("dataquieR.des_summary_hard_lim_remove",
                                    dataquieR.des_summary_hard_lim_remove_default),
                        ...) {

  ##TODO: if a variable is present in the study data but not in the metadata is
  # not in the final descriptive statistics! It is already lost at line 37!

  # PREPARATION----
  # Metadata and study data set up ----
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_data_frame(study_data)
  try(util_expect_data_frame(meta_data), silent = TRUE)
  item_level <- meta_data <-
    util_amend_missing_metadata(study_data = study_data,
                                meta_data = meta_data,
                                level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
  if (util_is_try_error(
    erobj <- try(prep_prepare_dataframes(.replace_hard_limits = hard_limits_removal,
                                         .apply_factor_metadata = TRUE),
                 silent = TRUE))) {
    w <- "%s has problems: %s, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"),
                 dQuote(conditionMessage(util_condition_from_try_error(erobj))),
                 sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data,
                            .replace_hard_limits = hard_limits_removal,
                            .apply_factor_metadata = TRUE)
  }

  #ORDER of variables: reorder both metadata and study data with same order----
  if(!VARIABLE_ORDER %in% colnames(meta_data)){
    #VARIABLE_ORDER missing, add appearance order of variables in the study data
    mapping_order_of_vars <- setNames(seq_len(ncol(ds1)),
                                      nm = names(ds1))
  } else {
    #VARIABLE_ORDER present, so take that as order numbers
    meta_data$VARIABLE_ORDER <-
      suppressWarnings(as.numeric(meta_data$VARIABLE_ORDER))
    meta_data$VARIABLE_ORDER <- rank(meta_data$VARIABLE_ORDER,
                                     ties.method = "first",
                                     na.last = TRUE)
    mapping_order_of_vars <- setNames(meta_data$VARIABLE_ORDER,
                                      nm = meta_data[[label_col]])
  }
  # Order both study data and metadata variables
  ds1 <- ds1[, order(mapping_order_of_vars[colnames(ds1)]), drop = FALSE]
  meta_data <- meta_data[order(mapping_order_of_vars[meta_data[[label_col]]]), ,
                         drop = FALSE]

  # Define resp_vars and filter for them----
  if (length(resp_vars) == 0) {
    #in case of missing resp_vars use all variables in the study data
    suppress <- function(...) suppressWarnings(suppressMessages(...))
    resp_vars <- colnames(ds1)
  } else {
    # in case of resp_vars present, check them
    suppress <- eval
  }
  suppress(util_correct_variable_use(resp_vars,
                                     allow_more_than_one = TRUE,
                                     do_not_stop = TRUE,
                                     # I remove here the variables with
                                     # na in the scale level
                                     need_scale = "!na"))
  resp_vars <- intersect(colnames(ds1), resp_vars)


  # Select only resp_vars for both study data and metadata
  ds1_filtered <- ds1[, colnames(ds1) %in% resp_vars, drop = FALSE]
  md_filtered <- meta_data[meta_data[, label_col, drop = TRUE] %in%
                             resp_vars, ]
  md_filtered$nprog <- 1:nrow(md_filtered)

  #Define categorical variables----
  # nominal variables
  nominal_cols <- md_filtered$SCALE_LEVEL %in% c(SCALE_LEVELS$NOMINAL)
  my_nominal_ds <- ds1_filtered[, nominal_cols, drop = FALSE]
  my_nominal_meta <- md_filtered[nominal_cols, ,drop = FALSE]

  # ordinal variables
  ordinal_cols <- md_filtered$SCALE_LEVEL %in% c(SCALE_LEVELS$ORDINAL)
  my_ordinal_ds <- ds1_filtered[, ordinal_cols, drop = FALSE]
  my_ordinal_meta <- md_filtered[ordinal_cols, ,drop = FALSE]

  # nominal and ordinal variables
  categorical_cols <- md_filtered$SCALE_LEVEL %in% c(SCALE_LEVELS$NOMINAL,
                                                     SCALE_LEVELS$ORDINAL)
  my_categor_ds <- ds1_filtered[, categorical_cols, drop = FALSE]
  my_categor_meta <- md_filtered[categorical_cols, ,drop = FALSE]

  rm(nominal_cols, ordinal_cols, categorical_cols)


  #TODO: if we have a scale level ratio and interval, and datatype is not numeric is a problem
  # can it be remedied or not-- should be done is prep_prepare_dataframe??

  #Define continuous variables----
  ratio_interv_cols <- md_filtered$SCALE_LEVEL %in% c(SCALE_LEVELS$RATIO,
                                                      SCALE_LEVELS$INTERVAL)
  my_ratioint_ds <- ds1_filtered[, ratio_interv_cols, drop = FALSE]
  my_ratioint_meta <- md_filtered[ratio_interv_cols, ,drop = FALSE]

  ratioint_notime_cols <- md_filtered$SCALE_LEVEL %in%
    c(SCALE_LEVELS$RATIO,
      SCALE_LEVELS$INTERVAL) &
    !(md_filtered$DATA_TYPE %in% c(DATA_TYPES$DATETIME,
                                   DATA_TYPES$TIME))
  my_continuous_ds <- ds1_filtered[, ratioint_notime_cols, drop = FALSE]
  my_continuous_meta <- md_filtered[ratioint_notime_cols, ,drop = FALSE]


  datetime_cols <- md_filtered$SCALE_LEVEL %in%
    c(SCALE_LEVELS$INTERVAL) &
    md_filtered$DATA_TYPE %in% c(DATA_TYPES$DATETIME,
                                 DATA_TYPES$TIME)
  my_datetime_ds <- ds1_filtered[, datetime_cols, drop = FALSE]
  my_datetime_meta <- md_filtered[datetime_cols, ,drop = FALSE]


  #need also an extra subset for CV, Kurtosis, and Skewness
  num_cols <- md_filtered$SCALE_LEVEL %in%
    c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL) &
    md_filtered$DATA_TYPE %in%
    c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
  my_num_ds <- ds1_filtered[, num_cols, drop = FALSE]
  my_num_meta <- md_filtered[num_cols, ,drop = FALSE]

  rm(ratio_interv_cols, ratioint_notime_cols, datetime_cols, num_cols)

  # Create an overview table - Summary_table----
  #First all names/label columns----
  summary_table <- md_filtered[,
                               colnames(md_filtered) %in%
                                 label_col, drop = FALSE]

  # Create all_labels together and add to summary_data----
  summary_labels <- md_filtered[,
                                colnames(md_filtered) %in%
                                  c(VAR_NAMES,
                                    LABEL,
                                    label_col,
                                    LONG_LABEL,
                                    colnames(md_filtered)[
                                      startsWith(colnames(md_filtered),
                                                 paste0(LABEL, "_"))],
                                    colnames(md_filtered)[
                                      startsWith(colnames(md_filtered),
                                                 paste0(LONG_LABEL, "_"))]
                                  ), drop = FALSE]

  rev_summary_labels <- rev(summary_labels)
  rownames(rev_summary_labels) =  NULL
  names_tb <- names(rev_summary_labels)
  rev_summary_labels <- cbind(rev_summary_labels,
                              vapply(apply(rev_summary_labels, 1,
                                           unique,
                                           simplify = FALSE),
                                     paste,
                                     collapse = "<br />",
                                     FUN.VALUE = character(1)))
  names(rev_summary_labels) <- c(names_tb,"Variable_names")
  Variable_names <- setNames(nm = rev_summary_labels[[label_col]],
                             rev_summary_labels$Variable_names)
  rm(rev_summary_labels, names_tb, summary_labels)
  # add the new column to summary data, matching the label_col columns
  summary_table$Variable_names <- util_attach_attr(
    Variable_names[match(summary_table[[label_col]],
                         names(Variable_names))],
    plain_label = summary_table[[label_col]]
  )

  # Add data type, scale level, and study segment----
  summary_table <- cbind(summary_table,
                         md_filtered[,
                                     colnames(md_filtered) %in%
                                       c("SCALE_LEVEL",
                                         "DATA_TYPE",
                                         "STUDY_SEGMENT")])

  ### Mean ----
  # Mean ratio and interval variables - no datetime
  vec1 <- unlist(lapply(my_continuous_ds,
                        mean, na.rm = TRUE))
  summary_table$mean_contin <- vec1[match(summary_table[[label_col]],
                                          names(vec1))]
  rm(vec1)

  #create empty ones in case no continuous data are present and round
  if (!"mean_contin" %in% colnames(summary_table)) {
    summary_table$mean_contin <- NA
  } else {
    summary_table$mean_contin[!is.na(summary_table$mean_contin)] <-
      format(util_round_to_decimal_places(
        summary_table$mean_contin[!is.na(summary_table$mean_contin)]))
    summary_table$mean_contin <- trimws(summary_table$mean_contin)
  }

  # Mean datetime variables
  vec1 <- unlist(lapply(my_datetime_ds,
                        function(x) {
                          mean_value <- mean(x, na.rm = TRUE)
                          r <- format(mean_value, usetz = TRUE)
                          return(r)
                        }
  ))
  summary_table$mean_datetime <- vec1[match(summary_table[[label_col]],
                                            names(vec1))]
  rm(vec1)

  #create empty ones in case no continuous data are present
  if (!"mean_datetime" %in% colnames(summary_table)) {
    summary_table$mean_datetime <- NA
  }

  #Combine columns Mean
  existing_columns <- intersect(colnames(summary_table),
                                c("mean_contin", "mean_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$Mean <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content, subject = "Mean")
  } else {
    summary_table$Mean <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)


  ### Median----
  # Median of ordinal variables
  vec1 <- unlist(lapply(my_ordinal_ds,
                        util_des_functions_env$util_compute_median_cat))
  summary_table$median_cat <- vec1[match(summary_table[[label_col]],
                                         names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"median_cat" %in% colnames(summary_table)) {
    summary_table$median_cat <- NA
  }

  # Median of ratio and interval - no datetime
  vec1 <- unlist(lapply(my_continuous_ds,
                        median, na.rm = TRUE))
  summary_table$median_contin <- vec1[match(summary_table[[label_col]],
                                            names(vec1))]
  rm(vec1)
  #create empty one in case not created and round
  if (!"median_contin" %in% colnames(summary_table)) {
    summary_table$median_contin <- NA
  } else{
    summary_table$median_contin[!is.na(summary_table$median_contin)] <-
      format(util_round_to_decimal_places(
        summary_table$median_contin[!is.na(summary_table$median_contin)]))
    summary_table$median_contin <- trimws(summary_table$median_contin)
  }

  # Median datetime
  vec1 <- unlist(lapply(my_datetime_ds,
                        function(x){
                          m <- median(x, na.rm = TRUE)
                          format(m, usetz = TRUE)
                        }
  ))
  summary_table$median_datetime <-
    vec1[match(summary_table[[label_col]],names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"median_datetime" %in% colnames(summary_table)) {
    summary_table$median_datetime <- NA
  }


  # Merge the 3 median values in one column
  existing_columns <- intersect(colnames(summary_table),
                                c("median_cat",
                                  "median_contin",
                                  "median_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$Median <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "Median")
  } else {
    summary_table$Median <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### Mode----
  # Mode of categorical variables
  vec1 <- unlist(lapply(my_categor_ds,
                        util_des_functions_env$util_compute_mode_cat))
  summary_table$mode_cat <- vec1[match(summary_table[[label_col]],
                                       names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"mode_cat" %in% colnames(summary_table)) {
    summary_table$mode_cat <- NA
  }

  # Mode of ratio and interval - no datetime
  vec1 <- unlist(lapply(my_continuous_ds,
                        util_des_functions_env$util_compute_mode_contin))
  summary_table$mode_contin <- vec1[match(summary_table[[label_col]],
                                          names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"mode_contin" %in% colnames(summary_table)) {
    summary_table$mode_contin <- NA
  }

  # Mode of datetime vars
  vec1 <- unlist(lapply(my_datetime_ds,
                        util_des_functions_env$util_compute_mode_datetime))
  summary_table$mode_datetime <- vec1[match(summary_table[[label_col]],
                                            names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"mode_datetime" %in% colnames(summary_table)) {
    summary_table$mode_datetime <- NA
  }

  #Combine columns Mode
  existing_columns <- intersect(colnames(summary_table),
                                c("mode_cat",
                                  "mode_contin",
                                  "mode_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$Mode <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "Mode")
  } else {
    summary_table$Mode <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### SD----
  # SD ratio and interval variables - no datetime
  vec1 <- unlist(lapply(my_continuous_ds,
                        sd, na.rm = TRUE))
  summary_table$SD_contin <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
  rm(vec1)
  #create empty one in case not created and round
  if (!"SD_contin" %in% colnames(summary_table)) {
    summary_table$SD_contin <- NA
  } else {
    summary_table$SD_contin[!is.na(summary_table$SD_contin)] <-
      format(util_round_to_decimal_places(
        summary_table$SD_contin[!is.na(summary_table$SD_contin)]))
    summary_table$SD_contin <- trimws(summary_table$SD_contin)
  }

  # SD datetime variables
  vec1 <-
    unlist(lapply(my_datetime_ds,
                  function(x) {
                    time_in_sec <- as.difftime(sd(x, na.rm = TRUE),
                                               units = 'secs')
                    #total_seconds <- lubridate::as.duration(
                    #  as.numeric(time_in_sec, units = "secs"))
                    #formatted_time <- util_des_functions_env$util_format_duration_human(start = total_seconds)
                    formatted_time <-
                      util_des_functions_env$util_compute_difftime_auto(time_in_sec)
                    format(formatted_time,
                           usetz = TRUE)
                  }))
  summary_table$SD_datetime <- vec1[match(summary_table[[label_col]],
                                          names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"SD_datetime" %in% colnames(summary_table)) {
    summary_table$SD_datetime <- NA
  }

  #Combine columns SD
  existing_columns <- intersect(colnames(summary_table),
                                c("SD_contin",
                                  "SD_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$SD <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "SD")
  } else {
    summary_table$SD <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### IQR----
  # IQR ordinal variables
  vec1 <- lapply(my_ordinal_ds,
                 util_des_functions_env$util_compute_IQR_ord)
  #actual range
  IQR <- vapply(vec1, `[[`, "IQR",
                FUN.VALUE = numeric(1))
  # range + Q1 and Q3
  IQR_q1_q3 <- vapply(vec1, `[[`, "IQR_q1_q3",
                      FUN.VALUE = character(1))
  rm(vec1)
  #Add both to summary_table
  summary_table$IQR_ord <- IQR[match(summary_table[[label_col]],
                                     names(IQR))]
  summary_table$IQR_ord_q1_q3 <- IQR_q1_q3[match(summary_table[[label_col]],
                                                 names(IQR_q1_q3))]
  #create empty one in case not created
  if (!"IQR_ord" %in% colnames(summary_table)) {
    summary_table$IQR_ord <- NA
  } else {
    summary_table$IQR_ord <- format(summary_table$IQR_ord)
  }
  if (!"IQR_ord_q1_q3" %in% colnames(summary_table)) {
    summary_table$IQR_ord_q1_q3 <- NA
  }

  # IQR ratio and interval - no datetime
  vec1 <- lapply(my_continuous_ds,
                 util_des_functions_env$util_compute_IQR_contin)
  #actual range
  IQR <- vapply(vec1, `[[`, "IQR", FUN.VALUE = numeric(1))
  # range + Q1 and Q3
  IQR_q1_q3 <- vapply(vec1, `[[`, "IQR_q1_q3", FUN.VALUE = character(1))
  rm(vec1)
  #Add both to summary_table
  summary_table$IQR_cont <- IQR[match(summary_table[[label_col]],
                                      names(IQR))]
  summary_table$IQR_cont_q1_q3 <- IQR_q1_q3[match(summary_table[[label_col]],
                                                  names(IQR_q1_q3))]
  #create empty one in case not created
  if (!"IQR_cont" %in% colnames(summary_table)) {
    summary_table$IQR_cont <- NA
  } else {
    summary_table$IQR_cont[!is.na(summary_table$IQR_cont)] <-
      format(util_round_to_decimal_places(
        summary_table$IQR_cont[!is.na(summary_table$IQR_cont)]))
  }
  if (!"IQR_cont_q1_q3" %in% colnames(summary_table)) {
    summary_table$IQR_cont_q1_q3 <- NA
  }

  # IQR datetime variables
  vec1 <- lapply(my_datetime_ds,
                 util_des_functions_env$util_compute_IQR_datetime)
  #actual range
  IQR <- vapply(vec1, `[[`, "IQR", FUN.VALUE = character(1))
  # range + Q1 and Q3
  IQR_q1_q3 <- vapply(vec1, `[[`, "IQR_q1_q3", FUN.VALUE = character(1))
  rm(vec1)
  #Add both to summary_table
  summary_table$IQR_datetime <- IQR[match(summary_table[[label_col]],
                                          names(IQR))]
  summary_table$IQR_datetime_q1_q3 <-
    IQR_q1_q3[match(summary_table[[label_col]],
                    names(IQR_q1_q3))]
  #create empty one in case not created
  if (!"IQR_datetime" %in% colnames(summary_table)) {
    summary_table$IQR_datetime <- NA
  }
  if (!"IQR_datetime_q1_q3" %in% colnames(summary_table)) {
    summary_table$IQR_datetime_q1_q3 <- NA
  }

  # Create a column `IQR (Quartiles)`with all IQR values
  # (IQR_ord_q1_q3, IQR_cont_q1_q3, IQR_datetime_q1_q3)
  #Combine columns SD
  existing_columns <- intersect(colnames(summary_table),
                                c("IQR_ord_q1_q3",
                                  "IQR_cont_q1_q3",
                                  "IQR_datetime_q1_q3"))
  if(length(existing_columns) > 0) {
    summary_table$`IQR (Quartiles)` <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "IQR (Quartiles)")
  } else {
    summary_table$`IQR (Quartiles)` <- NA
  }

  # identify columns NOT to remove
  existing_columns <- intersect(colnames(summary_table),
                                c("IQR_ord_q1_q3",
                                  "IQR_cont_q1_q3",
                                  "IQR_datetime_q1_q3",
                                  "IQR_ord",
                                  "IQR_cont",
                                  "IQR_datetime"))
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### MAD----
  # MAD ratio and interval variables - no datetime
  vec1 <- unlist(lapply(my_continuous_ds,
                        mad, na.rm = TRUE))

  summary_table$MAD_contin <- vec1[match(summary_table[[label_col]],
                                         names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"MAD_contin" %in% colnames(summary_table)) {
    summary_table$MAD_contin <- NA
  } else {
    summary_table$MAD_contin[!is.na(summary_table$MAD_contin)] <-
      format(util_round_to_decimal_places(
        summary_table$MAD_contin[!is.na(summary_table$MAD_contin)]))
    summary_table$MAD_contin <- trimws(summary_table$MAD_contin)
  }

  # MAD datetime variables
  vec1 <-
    unlist(lapply(my_datetime_ds,
                  function(x) {
                    time_in_sec <- as.difftime(mad(x, na.rm = TRUE),
                                               units = 'secs')
                    # total_seconds <- lubridate::as.duration(
                    #   as.numeric(time_in_sec, units = "secs"))

                    #formatted_time <-
                    #  util_des_functions_env$util_format_duration_human(start = total_seconds)

                    formatted_time <-
                      util_des_functions_env$util_compute_difftime_auto(time_in_sec)
                    format(formatted_time,
                           usetz = TRUE)
                  }))
  summary_table$MAD_datetime <- vec1[match(summary_table[[label_col]],
                                           names(vec1))]
  rm(vec1)

  #create empty one in case not created
  if (!"MAD_datetime" %in% colnames(summary_table)) {
    summary_table$MAD_datetime <- NA
  }

  # merge MAD values
  existing_columns <- intersect(colnames(summary_table),
                                c("MAD_contin",
                                  "MAD_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$MAD <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "MAD")
  } else {
    summary_table$MAD <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### Range Min max----
  # Min max ordinal variables
  #min
  vec1 <- unlist(lapply(my_ordinal_ds,
                        min, na.rm = TRUE))
  if (!is.null(vec1)) {
    summary_table$min_ord <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
    rm(vec1)
  }
  #max
  vec1 <- unlist(lapply(my_ordinal_ds,
                        max, na.rm = TRUE))
  if (!is.null(vec1)) {
    summary_table$max_ord <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
    rm(vec1)
  }
  summary_table$min_max_ord <- NA
  result <- !is.na(summary_table$min_ord) & !is.na(summary_table$max_ord)
  # create the min_max_ord vars when results are present
  summary_table$min_max_ord[result] <-
    paste0("(",
           summary_table$min_ord[result], " - ",
           summary_table$max_ord[result], ")")

  #create empty one in case not created
  if (!"min_ord" %in% colnames(summary_table)) {
    summary_table$min_ord <- NA
  }
  if (!"max_ord" %in% colnames(summary_table)) {
    summary_table$max_ord <- NA
  }
  if (!"min_max_ord" %in% colnames(summary_table)) {
    summary_table$min_max_ord <- NA
  }


  # Min max ratio and interval variables - no datetime
  #min
  vec1 <- unlist(lapply(my_continuous_ds,
                        min, na.rm = TRUE))
  if (!is.null(vec1)) {
    summary_table$min_contin <- vec1[match(summary_table[[label_col]],
                                           names(vec1))]
    rm(vec1)
  }
  #max
  vec1 <- unlist(lapply(my_continuous_ds,
                        max, na.rm = TRUE))
  if (!is.null(vec1)) {
    summary_table$max_contin <- vec1[match(summary_table[[label_col]],
                                           names(vec1))]
    rm(vec1)
  }
  # string with min and max
  #create empty one in case not created
  if (!"min_contin" %in% colnames(summary_table)) {
    summary_table$min_contin <- NA
  }
  if (!"max_contin" %in% colnames(summary_table)) {
    summary_table$max_contin <- NA
  }
  summary_table$range_temp <- summary_table$max_contin-summary_table$min_contin
  result <- !is.na(summary_table$min_contin) & !is.na(summary_table$max_contin)
  summary_table$min_max_contin <- NA
  summary_table$min_max_contin[result] <-
    paste0( summary_table$range_temp[result],
            " (",
            summary_table$min_contin[result], " - ",
            summary_table$max_contin[result], ")")
  #create empty one in case not created
  if (!"min_max_contin" %in% colnames(summary_table)) {
    summary_table$min_max_contin <- NA
  }
  summary_table$range_temp <- NULL


  # Min max datetime variables
  # min
  vec1 <- unlist(lapply(my_datetime_ds,
                        function(x){
                          m <- min(x, na.rm = TRUE)
                          format(m, usetz = TRUE)
                        }))

  if (!is.null(vec1)) {
    summary_table$min_datetime <- vec1[match(summary_table[[label_col]],
                                             names(vec1))]
    rm(vec1)
  }
  #max
  vec1 <- unlist(lapply(my_datetime_ds,
                        function(x){
                          m <- max(x, na.rm = TRUE)
                          format(m, usetz = TRUE)
                        }
  ))
  if (!is.null(vec1)) {
    summary_table$max_datetime <- vec1[match(summary_table[[label_col]],
                                             names(vec1))]
    rm(vec1)
  }
  # string with min and max
  vec1 <-
    unlist(lapply(my_datetime_ds,
                  function(x){
                    max <- max(x, na.rm = TRUE)
                    min <- min(x, na.rm = TRUE)
                    if (inherits(x, "hms")) {
                      max <- hms::as_hms(max)
                      min <- hms::as_hms(min)
                    }
                    # iqr_interval <- lubridate::interval(min, max)
                    # Convert to a period human-readable
                    # formatted_time <- lubridate::as.period(iqr_interval)
                    # formatted_time <-gsub("(^| )0[ymdHMS]", "", formatted_time)
                    # print(formatted_time)
                    time_in_sec <- as.difftime(max - min,
                                               units = 'secs')
                    formatted_time <-
                      util_des_functions_env$util_format_duration_human(
                        start = min,
                        end = max)
                    paste0(formatted_time,
                           " (",
                           format(min, usetz = TRUE), " - ",
                           format(max, usetz = TRUE), ")")
                  }))
  if (!is.null(vec1)) {
    summary_table$min_max_datetime <- vec1[match(summary_table[[label_col]],
                                                 names(vec1))]
    rm(vec1)
  }
  #create empty one in case not created
  if (!"min_datetime" %in% colnames(summary_table)) {
    summary_table$min_datetime <- NA
  }
  if (!"max_datetime" %in% colnames(summary_table)) {
    summary_table$max_datetime <- NA
  }
  if (!"min_max_datetime" %in% colnames(summary_table)) {
    summary_table$min_max_datetime <- NA
  }
  #merge min-max values
  existing_columns <- intersect(colnames(summary_table),
                                c("min_max_ord",
                                  "min_max_contin",
                                  "min_max_datetime"))
  if(length(existing_columns) > 0) {
    summary_table$`Range (Min - Max)` <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "Range (Min - Max)")
  } else {
    summary_table$`Range (Min - Max)` <- NA
  }
  # identify columns NOT to remove
  existing_columns <- intersect(colnames(summary_table),
                                c("min_max_ord",
                                  "min_max_contin",
                                  "min_max_datetime",
                                  "min_ord",
                                  "max_ord",
                                  "min_contin",
                                  "max_contin",
                                  "min_datetime",
                                  "max_datetime"))
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  ### CV ----
  # CV numeric variables - only continuous that are integer or float
  vec1 <- unlist(lapply(my_num_ds,
                        function(x){
                          sd(x, na.rm = TRUE) /
                            mean(x, na.rm = TRUE) * 100
                        }))
  summary_table$CV <- vec1[match(summary_table[[label_col]],
                                 names(vec1))]
  rm(vec1)

  #create empty one in case not created
  if (!"CV" %in% colnames(summary_table)) {
    summary_table$CV <- NA
  } else {
    summary_table$CV[!is.na(summary_table$CV)] <-
      format(util_round_to_decimal_places(
        summary_table$CV[!is.na(summary_table$CV)]))
    summary_table$CV <- trimws(summary_table$CV)
  }
  # replace NAs with ""
  no_result <- is.na(summary_table$CV)
  # replace NA with ""
  summary_table$CV[no_result] <- ""



  #### Skewness value ----
  # Skewness - only continuous that are integer or float
  vec1 <- unlist(lapply(my_num_ds,
                        util_des_functions_env$util_compute_skewness))
  summary_table$skewness_value <- vec1[match(summary_table[[label_col]],
                                             names(vec1))]
  rm(vec1)
  #round results
  if (!"skewness_value" %in% colnames(summary_table)) {
    summary_table$skewness_value <- ""
  } else {
    no_result <- is.na(summary_table$skewness_value)
    summary_table$skewness_value[!no_result] <-
      format(util_round_to_decimal_places(summary_table$skewness_value[!no_result]))
    summary_table$skewness_value <- trimws(summary_table$skewness_value)
    # replace NA with ""
    summary_table$skewness_value[no_result] <- ""
  }


  #### SE Skewness ----
  # SE Skewness  - only continuous that are integer or float
  vec1 <- unlist(lapply(my_num_ds,
                        util_des_functions_env$util_compute_SE_skewness))
  summary_table$SE_skewness <- vec1[match(summary_table[[label_col]],
                                          names(vec1))]
  rm(vec1)
  #round results
  if (!"SE_skewness" %in% colnames(summary_table)) {
    summary_table$SE_skewness <- ""
  } else {
    no_result <- is.na(summary_table$SE_skewness)
    summary_table$SE_skewness[!no_result] <-
      format(util_round_to_decimal_places(summary_table$SE_skewness[!no_result]))
    summary_table$SE_skewness <- trimws(summary_table$SE_skewness)
    # replace NA with ""
    summary_table$SE_skewness[no_result] <- ""
  }


  ### Skewness----
  # merge skewness and replace NA with ""
  if ("skewness_value" %in% colnames(summary_table) &&
      !all(summary_table$skewness_value == "")) {
    #merge skewness
    no_result <- is.na(summary_table$skewness_value) |  summary_table$skewness_value==""
    summary_table$Skewness[!no_result] <-
      paste0(summary_table$skewness_value[!no_result], " (",
             summary_table$SE_skewness[!no_result], ")")
    # replace NA with "" and deleted not anymore needed cols
    summary_table$Skewness[no_result] <- ""
  }

  if (!"Skewness" %in% colnames(summary_table)) {
    summary_table$Skewness <- NA
  }



  ### Kurtosis ----
  # Kurtosis  - only continuous that are integer or float
  vec1 <- unlist(lapply(my_num_ds,
                        util_des_functions_env$util_compute_kurtosis))
  summary_table$Kurtosis <- vec1[match(summary_table[[label_col]],
                                       names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"Kurtosis" %in% colnames(summary_table)) {
    summary_table$Kurtosis <- NA
  } else {
    summary_table$Kurtosis[!is.na(summary_table$Kurtosis)] <-
      format(util_round_to_decimal_places(
        summary_table$Kurtosis[!is.na(summary_table$Kurtosis)]))
    summary_table$Kurtosis <- trimws(summary_table$Kurtosis)
  }

  # replace Na with ""
  if ("Kurtosis" %in% colnames(summary_table) &&
      !all(is.na(summary_table$Kurtosis))) {
    no_result  <- is.na(summary_table$Kurtosis)
    summary_table$Kurtosis[no_result] <- ""
  }



  ### Frequency table - only categorical-----
  # Obtain a table for ordinal and nominal variables - my_categor_ds
  #### 1. No.unique_values(included NA) ----
  vec1 <- unlist(lapply(my_categor_ds,
                        function(x){
                          length(unique(x))
                        }
  ))
  summary_table$No_categories_incl_NA <-
    vec1[match(summary_table[[label_col]],
               names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"No_categories_incl_NA" %in% colnames(summary_table)) {
    summary_table$No_categories_incl_NA <- NA
  }


  #### 2. Levels with N----
  vec1 <- unlist(lapply(my_categor_ds,
                        util_des_functions_env$util_compute_frequency_table))
  summary_table$Level_freq <- vec1[match(summary_table[[label_col]],
                                         names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"Level_freq" %in% colnames(summary_table)) {
    summary_table$Level_freq <- NA
  }

  ### Valid----
  # Valid N
  vec1 <- unlist(lapply(ds1_filtered,
                        function(x) sum(is.finite(x))
  ))
  summary_table$valid_N <- vec1[match(summary_table[[label_col]],
                                      names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"valid_N" %in% colnames(summary_table)) {
    summary_table$valid_N <- NA
  }

  # Valid %
  vec1 <- unlist(lapply(ds1_filtered,
                        function(x) {
                          perc_result <- sum(is.finite(x)) / length(x) * 100
                          return(perc_result)
                        }
  ))
  summary_table$valid_PCT <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"valid_PCT" %in% colnames(summary_table)) {
    summary_table$valid_PCT <- NA
  } else{
    summary_table$valid_PCT <-
      format(util_round_to_decimal_places(summary_table$valid_PCT,
                                          digits = 2))
    summary_table$valid_PCT <- trimws(summary_table$valid_PCT)
  }

  # merge valid N and %
  summary_table$Valid <-
    paste0(summary_table$valid_N, " (",
           summary_table$valid_PCT,
           "%)")

  ### Missing ----
  # Missing N
  vec1 <- unlist(lapply(ds1_filtered,
                        function(x) sum(util_empty(x))
  ))
  summary_table$missing_N <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"missing_N" %in% colnames(summary_table)) {
    summary_table$missing_N <- NA
  }

  # Missing %
  vec1 <- unlist(lapply(ds1_filtered,
                        function(x) {
                          perc_result <- (sum(util_empty(x)) / length(x)) * 100
                          return(perc_result)
                        }
  ))
  summary_table$missing_PCT <- vec1[match(summary_table[[label_col]],
                                          names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"missing_PCT" %in% colnames(summary_table)) {
    summary_table$missing_PCT <- NA
  } else {
    summary_table$missing_PCT <- format(
      util_round_to_decimal_places(summary_table$missing_PCT, digits = 2))
    summary_table$missing_PCT <- trimws(summary_table$missing_PCT)
  }

  #merge Missing N and %
  summary_table$Missing <-
    paste0(summary_table$missing_N, " (",
           summary_table$missing_PCT ,
           "%)")

  ### Graph -----
  # Graph nominal and ordinal variables
  vec1 <- unlist(lapply(my_categor_ds,
                        util_des_functions_env$util_compute_graph_cat))

  summary_table$graph_cat <- vec1[match(summary_table[[label_col]],
                                        names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"graph_cat" %in% colnames(summary_table)) {
    summary_table$graph_cat <- NA
  }

  # Graph DATETIME variables
  vec1 <- unlist(lapply(my_datetime_ds,
                        util_des_functions_env$util_compute_graph_datetime))
  summary_table$graph_datetime <- vec1[match(summary_table[[label_col]],
                                             names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"graph_datetime" %in% colnames(summary_table)) {
    summary_table$graph_datetime <- NA
  }

  # Graph integer and float variables
  vec1 <- unlist(lapply(my_num_ds,
                        util_des_functions_env$util_compute_graph_cont))
  summary_table$graph_cont <- vec1[match(summary_table[[label_col]],
                                         names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"graph_cont" %in% colnames(summary_table)) {
    summary_table$graph_cont <- NA
  }

  #Combine graphs
  existing_columns <- intersect(colnames(summary_table),
                                c("graph_cat",
                                  "graph_datetime",
                                  "graph_cont"))
  if(length(existing_columns) > 0) {
    summary_table$Graph <-
      apply(summary_table[, existing_columns, drop = FALSE], 1,
            util_des_functions_env$util_combine_cols_content,
            subject = "Graph")
  } else {
    summary_table$Graph <- NA
  }

  # identify columns NOT to remove
  cols_to_keep <- setdiff(colnames(summary_table), existing_columns)
  # remove the extra columns the data frame
  summary_table <- summary_table[, cols_to_keep]
  rm(cols_to_keep, existing_columns)



  # Additional column to summary_table to make it compatible with previous version
  # Type----
  vec_Type <- setNames(nm = summary_table[[label_col]],
                       paste0(summary_table$SCALE_LEVEL,
                              ", ", summary_table$DATA_TYPE))
  summary_table$Type <- vec_Type[match(summary_table[[label_col]],
                                       names(vec_Type))]


  # Create a summary_data starting from summary_table ----
  summary_data <- summary_table

  ### Remove unused columns----
  summary_data <- summary_data[, !(colnames(summary_data) %in%
                                     c(SCALE_LEVEL,DATA_TYPE ))]

  ### Create the plot of the frequency table----
  # only categorical
  vec1 <-
    unlist(lapply(my_categor_ds,
                  util_des_functions_env$util_compute_graph_frequency_table))
  summary_data$"Frequency table" <- vec1[match(summary_data[[label_col]],
                                               names(vec1))]
  rm(vec1)
  #create empty one in case not created
  if (!"Frequency table" %in% colnames(summary_data)) {
    summary_data$`Frequency table` <- NA
  }

  # Clean summary data table----
  col_vector <- c("Variable_names",
                  "Type",
                  "STUDY_SEGMENT",
                  "Mean",
                  "SD",
                  "Median",
                  "Mode",
                  "IQR (Quartiles)",
                  "MAD",
                  "Range (Min - Max)",
                  "CV",
                  "Skewness",
                  "Kurtosis",
                  "No_categories_incl_NA",
                  "Frequency table",
                  "Valid",
                  "Missing",
                  "Graph")

  summary_data <- summary_data[, colnames(summary_data) %in%
                                 col_vector]

  valid_cols_in_vec <- intersect(col_vector, names(summary_data))
  summary_data <- summary_data[, valid_cols_in_vec, drop = FALSE]

  ### correct names and replace NA with ""
  # correct names summary_table
  names(summary_table)[names(summary_table) == label_col] <- "Variables"
  # correct names summary_data
  names(summary_data)[names(summary_data) == "Variable_names"] <- "Variables"
  names(summary_data)[names(summary_data) == "Skewness"] <- "Skewness (SE)"
  names(summary_data)[names(summary_data) == "No_categories_incl_NA"] <- "No. categories (incl.NAs)"
  # replace NA in summary_data with ""
  summary_data[is.na(summary_data)] <- ""



  return(list(SummaryData = util_attach_attr(summary_data,
                                             is_html_escaped = TRUE),
              SummaryTable = util_attach_attr(summary_table,
                                              is_html_escaped = TRUE)))
}

