#' Calculate and plot `Mahalanobis` distances
#'
#' @description
#' A standard tool to calculate `Mahalanobis` distance.
#' In this approach the squared `Mahalanobis` distance is calculated for ordinal
#' variables (treated as continuous) to identify inattentive responses.
#' It calculates the distance for each observational unit from the sample mean.
#' The greater the distance, the atypical the responses.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type integer
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all variables from `variable_group`
#' - The `Mahalanobis` distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The default to consider a value an `outlier` is to use the 0.975 quantile
#'    of a theoretical chi-square distribution with degrees of freedom
#'    equals to the number of variables used to calculate the
#'    `Mahalanobis` distance (`Mayrhofer and Filzmoser`, 2023)
#'
#' @inheritParams .template_function_indicator
#'
#' @param variable_group [variable list] the names of the variables used to
#'                                        calculate the `Mahalanobis` distance
#' @param study_data [data.frame] the data frame that contains the measurements
#'
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_cross_item [data.frame] -- Cross-item level metadata
#' @param label_col [variable attribute] the name of the column in the
#'                                       metadata containing the labels of
#'                                       the variables
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
#' @param meta_data_v2 [character] path or file name of the workbook like
#'                                 metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`
#' @param mahalanobis_threshold [numeric] the confidence level to use to define
#'                                        `outliers`, if not stated it is by default
#'                                        0.975.
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryData`: [data.frame] underlying the plot with speaking column labels
#'   - `SummaryPlot`: [ggplot2::ggplot2] Q-Q plot of squared `Mahalanobis`
#'                                        distances vs. a theoretical
#'                                        chi-squared distribution showing `outliers`.
#'   - `FlaggedStudyData`: [data.frame] contains the original data frame of the
#'                                      variables used to calculate
#'                                      the squared `Mahalanobis` distances
#'                                      with the additional column,
#'                                      containing the squared
#'                                       `Mahalanobis` distance, and a column
#'                                       called `MD_outliers`, that contains
#'                                       1 if the observational unit is considered
#'                                       a multivariate `outlier`.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_path  scale_color_manual geom_point discrete_scale theme_minimal scale_alpha_manual
#'
#' @importFrom stats mahalanobis
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
acc_mahalanobis <- function(variable_group = NULL,
                            study_data,
                            item_level = "item_level",
                            meta_data = item_level,
                            meta_data_cross_item = "cross-item_level",
                            label_col = VAR_NAMES,
                            meta_data_v2,
                            cross_item_level,
                            `cross-item_level`,
                            mahalanobis_threshold =
                              suppressWarnings(
                                as.numeric(
                                  getOption("dataquieR.MAHALANOBIS_THRESHOLD",
                                            dataquieR.MAHALANOBIS_THRESHOLD_default)))) {
  #Preps------
  if(.called_in_pipeline) {
    util_error(m = "This function is not meant to run in the pipeline",
               intrinsic_applicability_problem = TRUE
    )
  }

  util_maybe_load_meta_data_v2()

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
  # Load cross-item_level metadata and normalize it ----
  # check if there is a cross item metadata and if it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(sprintf(
      "No cross-item level metadata %s found",
      dQuote(meta_data_cross_item)))
    meta_data_cross_item <- data.frame(VARIABLE_LIST = character(0),
                                       CHECK_LABEL = character(0))
  }

  # First normalize input for meta_data_cross_item from the user
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)


  #Define variable groups
  vars <- NULL
  if(!is.null(variable_group)) {
    util_correct_variable_use("variable_group",
                              allow_more_than_one = TRUE,
                              allow_any_obs_na = TRUE,
                              need_type = "integer | float",
                              need_scale = "interval | ratio | ordinal" #ES: should I leave only ordinal?
    )

    if (length(variable_group) == 1) {
      util_error("Need at least two variables for Mahalanobis distance.",
                 applicability_problem = TRUE)
    }

    vars <- setNames(list(variable_group[!is.na(variable_group)]), "variable_group")



    if (length(mahalanobis_threshold) != 1 ||
        !is.numeric(mahalanobis_threshold) ||
        !is.finite(mahalanobis_threshold)) {
      util_message(
        c("The mahalanobis_threshold argument is not correct",
          "default (%g) is used."),
        dataquieR.MAHALANOBIS_THRESHOLD_default,
        applicability_problem = TRUE)
      mahalanobis_threshold <- dataquieR.MAHALANOBIS_THRESHOLD_default
    }

    mahalanobis_threshold <- setNames(list(mahalanobis_threshold), "variable_group")
  } else {

    given_mahal_cols <- intersect(c("MAHALANOBIS_THRESHOLD", "MAHALANOBIS_RATIO"),
                                  colnames(meta_data_cross_item))
    if (!all(util_empty(as.vector(meta_data_cross_item[, given_mahal_cols,
                                                       FALSE])))) {
      #reduce the cross-item_level content to only the rows that contains mahal. info
      cur_cross <-
        meta_data_cross_item[!util_empty(meta_data_cross_item[[MAHALANOBIS_THRESHOLD]]),
                             , FALSE]
      if (nrow(cur_cross) == 0) {
        vars <- NULL
      } else {
        #Select only the column of interest
        cur_cross <- cur_cross[, intersect(c(VARIABLE_LIST,
                                             CHECK_ID,
                                             CHECK_LABEL,
                                             DATA_PREPARATION,
                                             MAHALANOBIS_THRESHOLD,
                                             MAHALANOBIS_RATIO),
                                           colnames(cur_cross)
        ),
        FALSE]

        cur_cross$MAHALANOBIS_THRESHOLD <- ifelse(
          tolower(as.character(cur_cross$MAHALANOBIS_THRESHOLD)) %in%
            c("true", "1", "t", "+") |
            is.na(suppressWarnings(as.numeric(
              as.character(cur_cross$MAHALANOBIS_THRESHOLD)))),
          dataquieR.MAHALANOBIS_THRESHOLD_default,
          as.numeric(as.character(cur_cross$MAHALANOBIS_THRESHOLD))
        )


        vars <- setNames(lapply(cur_cross$VARIABLE_LIST, function(a) {
          unname(unlist(util_parse_assignments(a)))
        }), cur_cross$CHECK_LABEL)

        mahalanobis_threshold <- as.list(setNames(
          as.numeric(cur_cross$MAHALANOBIS_THRESHOLD),
          cur_cross$CHECK_LABEL
        ))
      }
    }
  }

  #vars is a list
  if (is.null(vars)) {
    util_error(util_error("No variables provided to calculate Mahalanobis distance",
                          applicability_problem = TRUE))
  }

  plot_list <- lapply(setNames(nm = names(vars), vars), function(rv) {
    #Filter to only keep the columns in the variable_group
    ds1_group <- ds1[, rv, drop = FALSE]
    check_label_rv <- names(vars)[sapply(vars, function(x) identical(x, rv))]
    MD_res <- util_generate_mahalanobis_dist(ds1_group,
                                             rv,
                                             check_label_rv)

    ds1_group <- MD_res$x_with_MD
    degree_freedom <- MD_res$df
    rm(MD_res)
    ds1plot <- ds1_group[rowSums(is.na(ds1_group[, rv, drop = FALSE])) == 0, ,
                         drop = FALSE]

    # Threshold to identify multivariate outliers (argument mahalanobis_threshold)
    # is by default 0.975 (Mayrhofer and Filzmoser, 2023) or set by the user
    MD_outliers_threshold <- unname(stats::qchisq(mahalanobis_threshold[[check_label_rv]],
                                                  df = degree_freedom))

    #TODO: Check! This differs from careless function mahad that calculates MD even if there are missing values.

    #create a column with outliers
    ds1plot[[paste0("MD_outliers_", check_label_rv)]] <- NA
    ds1plot[[paste0("MD_outliers_", check_label_rv)]] <-
      ifelse(ds1plot[[paste0("MD_", check_label_rv)]] > MD_outliers_threshold, 1, 0)
    n_non_ol <- sum(ds1plot[[paste0("MD_outliers_", check_label_rv)]] == 0)
    n_devs <- sum(ds1plot[[paste0("MD_outliers_", check_label_rv)]] == 1)


    # Q-Q plot ----
    p_df <- length(ds1plot[[paste0("MD_", check_label_rv)]])
    probabilities <- stats::ppoints(p_df)

    theoretical_quantiles <- stats::qchisq(probabilities,
                                           df = degree_freedom) #fixed, df is not the no. rows, but it is the no. columns
    ds1plot <- ds1plot[order(ds1plot[[paste0("MD_", check_label_rv)]]), ]
    ds1plot$MD_ratio <- ds1plot[[paste0("MD_", check_label_rv)]]/MD_outliers_threshold


    #Utility function to create the plot
    res_MD <- util_create_mahalanobis_ggplot(MD_ratio = ds1plot$MD_ratio,
                                             mahalanobis_threshold = mahalanobis_threshold[[check_label_rv]],
                                             df = degree_freedom)
    p1 <- res_MD$plot_MD

    return(p1)


  })
  ################

  sumdat <- do.call(rbind.data.frame,
                    lapply(setNames(nm = names(vars), vars), function(rv) {
                      #Filter to only keep the columns in the variable_group
                      ds1_group <- ds1[, rv, drop = FALSE]
                      check_label_rv <- names(vars)[sapply(vars,
                                                           function(x) identical(x, rv))]
                      MD_res <- util_generate_mahalanobis_dist(ds1_group,
                                                               rv,
                                                               check_label_rv)
                      ds1_group <- MD_res$x_with_MD
                      degree_freedom <- MD_res$df
                      rm(MD_res)


                      # Threshold to identify multivariate outliers (argument mahalanobis_threshold)
                      # is by default 0.975 (Mayrhofer and Filzmoser, 2023) or set by the user
                      MD_outliers_threshold <-
                        unname(stats::qchisq(mahalanobis_threshold[[check_label_rv]],
                                             df = degree_freedom))

                      #TODO: Check! This differs from careless function mahad that calculates MD even if there are missing values.
                      #create a column with outliers
                      ds1_group[[paste0("MD_outliers_", check_label_rv)]] <- NA
                      ds1_group[[paste0("MD_outliers_", check_label_rv)]] <-
                        ifelse(ds1_group[[paste0("MD_", check_label_rv)]] > MD_outliers_threshold, 1, 0)
                      n_non_ol <- sum(ds1_group[[paste0("MD_outliers_",
                                                        check_label_rv)]] == 0,
                                      na.rm = TRUE)
                      n_devs <- sum(ds1_group[[paste0("MD_outliers_",
                                                      check_label_rv)]] == 1,
                                    na.rm = TRUE)
                      nas_obs_units <- sum(rowSums(is.na(ds1_group)) > 0)
                      nrows_completecases <- nrow(ds1_group)-nas_obs_units
                      # create summary table
                      st1 <- data.frame(Variables = check_label_rv)
                      st1$"MD_outliers (N)" <- n_devs
                      st1$"MD_outliers (%)" <- round(n_devs/nrows_completecases*100,
                                                     digits = 2)
                      st1$"N" <- nrows_completecases
                      st1$"observational_units_removed" <- nrow(ds1_group) -
                        nrows_completecases
                      st1$"mahalanobis_threshold" <-
                        mahalanobis_threshold[[check_label_rv]]
                      SummaryData <- st1
                    }))

  SummaryTable <- sumdat
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (N)"] <- "NUM_ssc_mah"
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (%)"] <- "PCT_ssc_mah"


  FlaggedStudyData <- lapply(setNames(nm = names(vars), vars), function(rv) {
    #Filter to only keep the columns in the variable_group
    ds1_group <- ds1[, rv, drop = FALSE]
    check_label_rv <- names(vars)[sapply(vars, function(x) identical(x, rv))]
    MD_res <- util_generate_mahalanobis_dist(ds1_group,
                                             rv,
                                             check_label_rv)
    ds1_group <- MD_res$x_with_MD
    degree_freedom <- MD_res$df
    rm(MD_res)

    # Threshold to identify multivariate outliers (argument mahalanobis_threshold)
    # is by default 0.975 (Mayrhofer and Filzmoser, 2023) or set by the user
    MD_outliers_threshold <- unname(stats::qchisq(mahalanobis_threshold[[check_label_rv]],
                                                  df = degree_freedom))
    #create a column with outliers
    ds1_group[[paste0("MD_outliers_", check_label_rv)]] <- NA
    ds1_group[[paste0("MD_outliers_", check_label_rv)]] <-
      ifelse(ds1_group[[paste0("MD_", check_label_rv)]] > MD_outliers_threshold, 1, 0)
    ds1_group$row_n <- c(1:nrow(ds1_group))

    return(ds1_group)
  })

  if (length(FlaggedStudyData) == 1) {
    print("The list has exactly one element.")
    FlaggedStudyData_all <- FlaggedStudyData[[1]]
    FlaggedStudyData_all <-
      FlaggedStudyData_all[, names(FlaggedStudyData_all) != "row_n"]

  } else {
    row_counts <- sapply(FlaggedStudyData, nrow)
    if (length(unique(row_counts)) != 1) {
      util_error("Internal error, sorry: The original data frame should have the same number of rows. Please report")
    }
    FlaggedStudyData_all <-  Reduce(function(x, y) {
      extra_cols <- c("row_n", setdiff(colnames(y), colnames(x)))
      merge(x, y[, extra_cols], by = "row_n", all = TRUE)
    }, FlaggedStudyData)


    FlaggedStudyData_all <-
      FlaggedStudyData_all[, names(FlaggedStudyData_all) != "row_n"]

  }

  return(list(SummaryTable = SummaryTable,
              SummaryData = sumdat,
              SummaryPlotList = plot_list,
              FlaggedStudyData = FlaggedStudyData_all
  ))
}
