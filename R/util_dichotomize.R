#' Utility function to dichotomize variables
#'
#' This function uses the metadata attributes `RECODE_CASES` and/or
#' `RECODE_CONTROL` to dichotomize the data. 'Cases' will be recoded to 1,
#' 'controls' to 0. The recoding can be specified by an interval (for metric
#' variables) or by a list of categories separated by the 'SPLIT_CHAR'. Recoding
#' will be used for data quality checks that include a regression model.
#'
#' @param study_data study data without jump/missing codes as specified in the
#'                   code conventions
#' @param meta_data metadata as specified in the code conventions
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#'
#' @family data_management
#' @concept data_management
#' @noRd
util_dichotomize <- function(study_data,
                             meta_data,
                             label_col = VAR_NAMES) {

  # if (!isTRUE(attr(study_data, "Codes_to_NA"))) {
  #   util_error("Expect to have missings already replaced.")
  # }
  # would be a nice check, but the attribute gets lost when subsetting ds1,
  # e.g. to select certain columns and rows

  vrcnt <- 0
  list_out <- list()

  for (var in colnames(study_data)) {
    recode_cases <- meta_data[meta_data[[label_col]] == var, RECODE_CASES]
    recode_control <- meta_data[meta_data[[label_col]] == var, RECODE_CONTROL]
    case_vec <- NULL
    contr_vec <- NULL
    val_lab_applied <- is.factor(study_data[[var]])
    val_lab <- util_parse_assignments(
      meta_data[[VALUE_LABELS]][meta_data[[label_col]] == var])
    # TODO STS: What needs to be done in case we have a VALUE_LABEL_TABLE
    # instead of VALUE_LABELS here?

    if ((!is.null(recode_cases) && !util_empty(recode_cases)) ||
        (!is.null(recode_control) && !util_empty(recode_control))) {

      # step 1: preprocess metadata information
      if (!is.null(recode_cases) && !util_empty(recode_cases)) {
        case_vec <- trimws(strsplit(recode_cases, SPLIT_CHAR, fixed = TRUE)[[1]])
        case_vec <- case_vec[!util_empty(case_vec)]
        if (length(case_vec) > 0 & !val_lab_applied) {
          # The definition might use the interval notation:
          case_int <- suppressWarnings(lapply(case_vec, util_parse_interval))
          is_interval <- vapply(case_int, FUN.VALUE = logical(1),
                                function(int) { inherits(int, "interval") })
          case_int <- case_int[is_interval]
          if (length(case_int) > 0) {
            case_vec <- case_int
          }
        } else if (length(case_vec) > 0 & val_lab_applied) {
          if (all(case_vec %in% names(val_lab))) {
            # If the levels are given as integers, we need to transform them to
            # value labels.
            case_vec <- unlist(lapply(case_vec, function(cc) {
              val_lab[[as.character(cc)]]
            }))
          }
        }
      }
      if (!is.null(recode_control) && !util_empty(recode_control)) { # same as for cases above
        contr_vec <- trimws(strsplit(recode_control, SPLIT_CHAR, fixed = TRUE)[[1]])
        contr_vec <- contr_vec[!util_empty(contr_vec)]
        if (length(contr_vec) > 0 & !val_lab_applied) {
          contr_int <- suppressWarnings(lapply(contr_vec, util_parse_interval))
          is_interval <- vapply(contr_int, FUN.VALUE = logical(1),
                                function(int) { inherits(int, "interval") })
          contr_int <- contr_int[is_interval]
          if (length(contr_int) > 0) {
            contr_vec <- contr_int
          }
        } else if (length(contr_vec) > 0 & val_lab_applied) {
          if (all(contr_vec %in% names(val_lab))) {
            contr_vec <- unlist(lapply(contr_vec, function(cc) {
              val_lab[[as.character(cc)]]
            }))
          }
        }
      }

      # step 2: apply recoding
      if (length(case_vec) == 0 & length(contr_vec) == 0) {
        util_warning(paste("Variable", var, "could not be recoded."))
      } else {
        col <- study_data[[var]]
        na <- is.na(col)
        if (length(case_vec) > 0) {
          if (is.list(case_vec)) {
            case_ind <- apply(as.data.frame(
              lapply(case_vec, function(ii) { redcap_env$`in`(col, ii) })),
              MARGIN = 1, FUN = any)
          } else {
            case_ind <- col %in% case_vec
          }
          if (length(contr_vec) > 0) {
            if (is.list(contr_vec)) {
              contr_ind <- apply(as.data.frame(
                lapply(contr_vec, function(ii) { redcap_env$`in`(col, ii) })),
                MARGIN = 1, FUN = any)
            } else {
              contr_ind <- col %in% contr_vec
            }
          } else {
            contr_ind <- !case_ind
          }
        } else {
          if (is.list(contr_vec)) {
            contr_ind <- apply(as.data.frame(
              lapply(contr_vec, function(ii) { redcap_env$`in`(col, ii) })),
              MARGIN = 1, FUN = any)
          } else {
            contr_ind <- col %in% contr_vec
          }
          case_ind <- !contr_ind
        }

        if (length(c(which(contr_ind), which(case_ind))) == 0) {
          util_warning(paste("Variable", var, "could not be recoded."))
        } else if (length(intersect(which(contr_ind), which(case_ind))) > 0) {
          util_warning(paste("Variable", var, "could not be recoded.",
                             "Cases and controls overlap."))
        } else {
          vrcnt <- vrcnt + 1
          col <- NA
          col[case_ind] <- 1
          col[contr_ind] <- 0
          col[na] <- NA
          study_data[, var] <- col
          list_out[[var]] <- paste(
            paste("Cases (1):",
                  ifelse(length(case_vec) == 0,
                         "any other value",
                         paste(unlist(lapply(case_vec, function(cc) {
                           if (is.list(cc)) {
                             paste0(ifelse(cc$inc_l, "[", "("),
                                    cc$low, ";", cc$upp, ifelse(cc$inc_u, "]", ")"))
                             } else {
                               cc
                             }})), collapse = " | "))),
            paste("Control (0):",
                  ifelse(length(contr_vec) == 0,
                         "any other value",
                         paste(unlist(lapply(contr_vec, function(cc) {
                           if (is.list(cc)) {
                             paste0(ifelse(cc$inc_l, "[", "("),
                                    cc$low, ";", cc$upp, ifelse(cc$inc_u, "]", ")"))
                           } else {
                             cc
                           }})), collapse = " | "))),
            sep = ". ")
        }
      }
    }
  }

  if (vrcnt > 0) {
    util_message(sprintf("Recoded %d variable(s).", vrcnt))
  }

  attr(study_data, "Dichotomization") <- list_out
  return(util_attach_attr(study_data))
}
