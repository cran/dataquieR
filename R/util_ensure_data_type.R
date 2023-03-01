#' Ensure matching data types
#'
#' Utility function to convert selected variables in the study data to match the
#' data types given in the metadata. If such a conversion is not possible, the
#' study data remains unchanged.
#'
#' @param variables [variable list] the names of the variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return the transformed (if necessary and possible) study data
#'
#' @examples
#' \dontrun{
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
#'   environment())
#' load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
#'   environment())
#' study_data$v00000 <- as.character(study_data$v00000)
#' sd1 <-
#'   util_ensure_data_type(
#'     c("CENTER_0", "AGE_0", "v00000", "v003"),
#'     study_data = study_data,
#'     meta_data = meta_data,
#'     label_col = "LABEL"
#'   )
#' all.equal(study_data, sd1)
#' study_data$v00013 <- as.numeric(study_data$v00013)
#' Sys.setenv(TZ = 'CET')
#' sd2 <-
#'   util_ensure_data_type(
#'     c("CENTER_0", "AGE_0", "v00000", "EXAM_DT_0"),
#'     study_data = study_data,
#'     meta_data = meta_data,
#'     label_col = "LABEL"
#'    )
#' all.equal(study_data, sd2)
#' all.equal(sd1$v00013, sd2$v00013)
#' }
util_ensure_data_type <- function(variables, study_data, meta_data, label_col) {
  if (is.null(label_col)) label_col <- "LABEL"
  util_expect_data_frame(study_data)
  util_expect_data_frame(meta_data)
  util_expect_scalar(variables,
                     allow_more_than_one = TRUE,
                     allow_na = TRUE,
                     check_type = is.character)
  variables <- variables[which(!util_empty(variables))]
  # try to consider all relevant variables, in case that 'variables' contains
  # a mix of LABEL, VAR_NAMES, LONG_LABEL etc.
  possible_vars <- unique(c(meta_data[[VAR_NAMES]],
                            meta_data[[LABEL]],
                            meta_data[[LONG_LABEL]],
                            meta_data[[label_col]]))
  variables <- variables[which(variables %in% possible_vars)]
  # consider only those variables that can be matched
  # map non-matching variables to VAR_NAMES (column names of study_data)
  if (!all(variables %in% colnames(study_data))) {
    non_matching_ind <- which(!(variables %in% colnames(study_data)))
    non_matching_vars <- variables[non_matching_ind]
    map_res <- lapply(setNames(nm = c("LABEL", "LONG_LABEL", label_col)),
                      function(oc) {
      unname(
        util_map_labels(
          non_matching_vars,
          warn_ambiguous = FALSE,
          meta_data = meta_data,
          to = "VAR_NAMES",
          from = oc,
          ifnotfound = NA_character_
        )
      )
    })
    map_res <- as.data.frame(map_res)
    variables[non_matching_ind] <- vapply(
      1:nrow(map_res),
      FUN.VALUE = character(1),
      FUN = function(i) {
        unique(na.omit(as.character(map_res[i, ])))[1]
      }
    )
  }
  # in case that some variables are contained several times by accident
  variables <- unique(variables)

  if (length(variables) > 0) {
    check_type <- as.numeric(util_compare_meta_with_study(
      sdf = study_data[, variables, drop = FALSE],
      mdf = meta_data,
      label_col = "VAR_NAMES",
      check_convertible = TRUE,
      threshold_value = 0))

    if (any(check_type == 0)) {
      util_error(c("Data type transformation of %s",
                   "would introduce additional NAs."),
                   paste(dQuote(variables[check_type == 0]), collapse = ", "),
                   applicability_problem = TRUE)
    }

    if (any(check_type == 2)) {
      vars_to_transform <- variables[which(check_type == 2)]
      expected_type <- setNames(util_find_var_by_meta(
        resp_vars = vars_to_transform,
        target = "DATA_TYPE",
        meta_data = meta_data),
        nm = vars_to_transform)
      converts <- setNames(
        list(
          as.integer,
          as.numeric,
          as.character,
          lubridate::as_datetime
        ),
        nm = c(
          DATA_TYPES$INTEGER,
          DATA_TYPES$FLOAT,
          DATA_TYPES$STRING,
          DATA_TYPES$DATETIME
        )
      )
      transf_data <- lapply(setNames(nm = vars_to_transform), function(rv) {
        rv_transf_list <- lapply(study_data[[rv]],
                                 converts[[expected_type[[rv]]]])
          rv2 <- unlist(rv_transf_list)
          attributes(rv2) <- attributes(rv_transf_list[[1]])
          if (!identical(which(is.na(study_data[[rv]])),
                         which(is.na(rv2)))) {
            util_warning(paste("Data type transformation of", rv,
                               "introduced",
                               length(which(is.na(rv2))) -
                                 length(which(is.na(study_data[[rv]]))),
                               "additional NAs."),
                         applicability_problem = TRUE)
          }
          return(rv2)
        })
      study_data[, vars_to_transform] <- transf_data
    }
  }
  return(study_data)
}
