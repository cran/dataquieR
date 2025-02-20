#' Detects variable levels not specified in metadata
#'
#' @description
#' For each categorical variable, value lists should be defined in the metadata.
#'  This implementation will examine, if all observed levels in the study data
#'  are valid.
#'
#' [Indicator]
#'
#' @details
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific VALUE_LABELS as supplied in the
#'    metadata.
#'  - Identification of measurements not corresponding to the expected
#'    categories. Therefore two output data frames are generated:
#'    - on the level of observation to flag each undefined category, and
#'    - a summary table for each variable.
#'  - Values not corresponding to defined categories are removed in a data frame
#'    of modified study data
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                           from 0-100.
#'
#' @return a list with:
#' - `SummaryData`: data frame summarizing inadmissible categories with the
#' columns:
#'   - `Variables`: variable name/label
#'   - `OBSERVED_CATEGORIES`: the categories observed in the study data
#'   - `DEFINED_CATEGORIES`: the categories defined in the metadata
#'   - `NON_MATCHING`: the categories observed but not defined
#'   - `NON_MATCHING_N`: the number of observations with categories not defined
#'   - `NON_MATCHING_N_PER_CATEGORY`: the number of observations for each of the
#'                unexpected categories
#' - `SummaryTable`: data frame for the `dataquieR` pipeline reporting the number
#'                   and percentage of inadmissible categorical values
#' - `ModifiedStudyData`: study data having inadmissible categories removed
#' - `FlaggedStudyData`: study data having cases with inadmissible categories
#' flagged
#'
#' @importFrom utils capture.output
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_inadmissible_categorical.html
#' )
con_inadmissible_categorical <- function(resp_vars = NULL,
                                         study_data,
                                         label_col,
                                         item_level = "item_level",
                                         threshold_value = 0,
                                         meta_data = item_level,
                                         meta_data_v2) {

  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  # detect, if called as "con_inadmissible_vocabulary"
  voc_mode <-
    rlang::call_name(rlang::caller_call(4)) == "con_inadmissible_vocabulary"


  as_numeric_no_warn <- function(...) {
    suppressWarnings(as.numeric(...))
  }

  # map metadata to study data
  prep_prepare_dataframes()

  util_stop_if_not(
`Internal error, sorry, please report. VALUE_LABELS after prep_prepare_dataframes` =
       is.null(meta_data[[VALUE_LABELS]]))

  if (voc_mode) {
    meta_data[[VALUE_LABEL_TABLE]] <- NULL
    meta_data[[VALUE_LABEL_TABLE]] <- meta_data[[STANDARDIZED_VOCABULARY_TABLE]]
  }


  if (!VALUE_LABEL_TABLE %in% colnames(meta_data)) {
    if (voc_mode) {
      util_error(paste0("Function con_inadmissible_vocabulary requires",
                        "the metadata column STANDARDIZED_VOCABULARY_TABLE."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)

    } else {
      util_error(paste0("Function con_inadmissible_categorical requires",
                        "the metadata column VALUE_LABELS or VALUE_LABEL_TABLE."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)

    }
  }

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            allow_any_obs_na = TRUE,
                            need_type = "integer | string", # TODO: What about datetime as categorical variable, if there are, e.g., only three timepoints?
                            need_scale = "nominal | ordinal"
  )

  if (length(resp_vars) == 0) {
    # Select all variables with VALUE_LABEL_TABLE (if any), if resp_vars were not specified.
    if (all(util_empty(meta_data[[VALUE_LABEL_TABLE]]))) {
      if (voc_mode) {
        util_error(c("No variables with defined",
                     "STANDARDIZED_VOCABULARY_TABLE"),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      } else {
        util_error(c("No variables with defined",
                     "VALUE_LABELS or VALUE_LABEL_TABLE."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      }
    } else {
      if (voc_mode) {
        util_message(c("All variables with STANDARDIZED_VOCABULARY_TABLE",
                       "in the metadata are used,",
                       "if SCALE_LEVEL is nominal or ordinal."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      } else {
        util_message(c("All variables with VALUE_LABELS or",
                       "VALUE_LABEL_TABLE in the metadata are used,",
                       "if SCALE_LEVEL is nominal or ordinal."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      }
      resp_vars <-
        intersect(meta_data[[label_col]][!(
          util_empty(meta_data[[VALUE_LABEL_TABLE]])) &
            meta_data[[SCALE_LEVEL]] %in% c(
              SCALE_LEVELS$NOMINAL, SCALE_LEVELS$ORDINAL)],
                  colnames(ds1))
    }
  } else {
    # Check for specified resp_vars whether VALUE_LABEL_TABLE have been defined at all.
    if (all(util_empty(
      meta_data[[VALUE_LABEL_TABLE]][meta_data[[label_col]] %in% resp_vars]))) {
      if (voc_mode) {
        util_error("No variables with defined STANDARDIZED_VOCABULARY_TABLE.",
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      } else {
        util_error("No variables with defined VALUE_LABELS or VALUE_LABEL_TABLE.",
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      }
    }
    # check whether VALUE_LABEL_TABLE are missing for some of the specified resp_vars
    rvs <- meta_data[[label_col]][!(
      util_empty(meta_data[[VALUE_LABEL_TABLE]])) &
                                    meta_data[[label_col]] %in% resp_vars]
    if (length(rvs) < length(resp_vars)) {
      if (voc_mode) {
        util_message(paste0("The variables ", resp_vars[!(resp_vars %in% rvs)],
                          " have no defined",
                          "STANDARDIZED_VOCABULARY_TABLE."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      } else {
        util_message(paste0("The variables ", resp_vars[!(resp_vars %in% rvs)],
                            " have no defined",
                            "VALUE_LABELS or VALUE_LABEL_TABLE."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      }
    }
    resp_vars <- rvs
  }

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            allow_any_obs_na = TRUE,
                            need_type = "integer | string", # TODO: What about datetime as categorical variable, if there are, e.g., only three timepoints?
                            need_scale = "nominal | ordinal"
  )

  if (length(resp_vars) == 0) {
    util_error("No categorical variables with value lists / vocabulary found.",
               applicability_problem = TRUE)
  }

  util_expect_scalar(threshold_value,
                     check_type = util_is_numeric_in(min = 0, max = 100))

  #util_expect_scalar(use_value_labels, check_type = is.logical)

  # Calculations ---------------------------------------------------------------

  sumdf1 <- data.frame(Variables = resp_vars)

  # Which categories were used in the study data?
  get_obs_cats <- function(x) {
    as.character(sort(unique(x[!is.na(x)])))
  }
  sumdf1$OBSERVED_CATEGORIES <- lapply(ds1[, resp_vars, drop = FALSE],
                                       get_obs_cats)

  voc_names <- setNames(
    util_map_labels(
      resp_vars,
      meta_data = meta_data,
      to = VALUE_LABEL_TABLE,
      from = label_col,
      ifnotfound = NA
    ),
    nm = resp_vars
  )

  # Which categories were defined in metadata?
  def_cats <- lapply(voc_names, function(vlt) {
        vl <- NA_character_
        if (!is.na(vlt)) {
          codes <- try(prep_get_data_frame(vlt), silent = TRUE)
          if (util_is_try_error(codes)) {
            util_warning(codes,
                         title = "Could not fetch value_labels/vocabulary %s, ignoring:\n",
                         dQuote(vlt))
          } else {
            # TODO: Handle | in [[1]] or [[2]]
            if (voc_mode && ncol(codes) == 1) {
              vl <- codes
              colnames(vl) <- CODE_VALUE
            } else if (voc_mode && ncol(codes) == 2) {
              vl <- codes
              colnames(vl) <- c(CODE_VALUE, CODE_LABEL)
            } else if (voc_mode) {
              url <-
                subset(util_get_voc_tab(), paste0("<", get("voc"), ">") == vlt,
                       "url", drop = TRUE)
              util_warning(c("For the standardized vocabulary table %s,",
                             "there are %d columns from the table %s defined.",
                             "There should be 1 or 2 (2nd one would be the",
                             "labels).",
                             "Ignoring this table."),
                           dQuote(vlt),
                           ncol(codes),
                           dQuote(url))
            } else {
              vl <- codes
            }
          }
        }

    if (is.data.frame(vl)) {
      if (CODE_VALUE %in% colnames(vl)) {
        if (CODE_LABEL %in% colnames(vl)) {
          return(setNames(as.character(vl[[CODE_VALUE]]), nm = vl[[CODE_LABEL]]))
        } else {
          return(as.character(vl[[CODE_VALUE]]))
        }
      } else {
        return(NA_character_)
      }
    } else {
      return(NA_character_)
    }
  })

  sumdf1$DEFINED_CATEGORIES <- def_cats


  # Which categories were used in the study data but not defined in the metadata?
  which_not <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) !(sumdf1$OBSERVED_CATEGORIES[[x]] %in%
                    sumdf1$DEFINED_CATEGORIES[[x]])
  )
  sumdf1$NON_MATCHING <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) paste0(sumdf1$OBSERVED_CATEGORIES[[x]][which_not[[x]]])
  )
  # If there are no unexpected categories, the entry should be NA (instead of "character(0)").
  sumdf1$NON_MATCHING <- lapply(
    sumdf1$NON_MATCHING,
    function(x) if (identical(x, character(0))) NA_character_ else x
  )

  # Count the number of non-matching observations in total and per unexpected category.
  sumdf1$NON_MATCHING_N <- NA
  sumdf1$NON_MATCHING_N_PER_CATEGORY <- NA
  # msdf = modified study data
  msdf <- ds1

  for (i in seq_len(nrow(sumdf1))) {
    if (!is.na(sumdf1$NON_MATCHING[i])) {
      if (all(!is.na(as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i]))))) {
        sumdf1$NON_MATCHING_N[i] <- sum(ds1[[paste(sumdf1$Variables[i])]] %in%
                                          as_numeric_no_warn(unlist(
                                            sumdf1$NON_MATCHING[i])),
          na.rm = TRUE
        )
        n_per_cat <- table(ds1[[sumdf1$Variables[i]]][which(
          ds1[[sumdf1$Variables[i]]] %in%
            as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i])))])
        n_per_cat <- n_per_cat[order(n_per_cat, decreasing = TRUE)]
        sumdf1$NON_MATCHING_N_PER_CATEGORY[i] <- paste(
          paste0(n_per_cat, " (", dQuote(names(n_per_cat)), ")"),
          collapse = ", ")
        msdf[[paste(sumdf1$Variables[i])]][
          msdf[[paste(sumdf1$Variables[i])]] %in%
            as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i]))] <- NA
        ds1[[paste0(sumdf1$Variables[i], "_IAV")]] <-
          ifelse(ds1[[paste(sumdf1$Variables[i])]] %in%
                   as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i])), 1, 0)
      } else {
        sumdf1$NON_MATCHING_N[i] <- sum(ds1[[paste(sumdf1$Variables[i])]] %in%
                                          unlist(sumdf1$NON_MATCHING[i]),
          na.rm = TRUE
        )
        n_per_cat <- table(ds1[[sumdf1$Variables[i]]][which(
          ds1[[sumdf1$Variables[i]]] %in% unlist(sumdf1$NON_MATCHING[i]))])
        sumdf1$NON_MATCHING_N_PER_CATEGORY[i] <- paste(
          paste0(n_per_cat, " (", dQuote(names(n_per_cat)), ")"),
          collapse = ", ")
        msdf[[paste(sumdf1$Variables[i])]][
          msdf[[paste(sumdf1$Variables[i])]] %in%
            as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i]))] <- NA
        ds1[[paste0(sumdf1$Variables[i], "_IAV")]] <-
          ifelse(ds1[[paste(sumdf1$Variables[i])]] %in%
                   as_numeric_no_warn(unlist(sumdf1$NON_MATCHING[i])), 1, 0)
      }
    }
  }

  # any IAVs?
  checkIAV <- names(ds1)[grep("_IAV", names(ds1))]
  if (length(checkIAV) > 0) {
    if (!.called_in_pipeline) util_message(paste0("The following variable(s): ",
                        paste0(checkIAV, collapse = ", "),
                        " flag(s) inadmissible values."),
                 applicability_problem = FALSE)
  }

  # attribute
  attr(msdf, "rmIAVcat") <- TRUE

  # if (use_value_labels) {
  #   sumdf1$DEFINED_CATEGORIES <- lapply(sumdf1[["DEFINED_CATEGORIES"]], function(x) {
  #     if (all(!is.na(as_numeric_no_warn(x)))) {
  #       x <- paste0(x, " (", dQuote(names(x)), ")")
  #     }
  #     x
  #   })
  # }

  # handle lists
  sumdf1[["OBSERVED_CATEGORIES"]] <-
    as.vector(unlist(lapply(sumdf1[["OBSERVED_CATEGORIES"]],
                            paste, collapse = ", ")))

  if (voc_mode) {
    sumdf1[["DEFINED_CATEGORIES"]] <- voc_names[sumdf1[["Variables"]]]
  } else {
    sumdf1[["DEFINED_CATEGORIES"]] <-
      as.vector(unlist(lapply(sumdf1[["DEFINED_CATEGORIES"]],
                              paste, collapse = ", ")))
  }

  sumdf1[["NON_MATCHING"]] <-
    as.vector(unlist(lapply(sumdf1[["NON_MATCHING"]],
                            paste, collapse = ", ")))
  sumdf1$NON_MATCHING <-
    ifelse(sumdf1$NON_MATCHING == "NA", "", sumdf1$NON_MATCHING)
  sumdf1$NON_MATCHING_N <-
    ifelse(is.na(sumdf1$NON_MATCHING_N), 0, sumdf1$NON_MATCHING_N)
  sumdf1$NON_MATCHING_N_PER_CATEGORY <-
    ifelse(is.na(sumdf1$NON_MATCHING_N_PER_CATEGORY), 0,
           sumdf1$NON_MATCHING_N_PER_CATEGORY)
  sumdf1[["GRADING"]] <-
    ifelse(sumdf1$NON_MATCHING_N > threshold_value / 100 * nrow(ds1), 1, 0)

  sumdf2 <- sumdf1[, c("Variables", "NON_MATCHING_N")]

  if (voc_mode) {
    colnames(sumdf2)[2] <- "NUM_con_rvv_icat" # FIXME: use correct indicator metrics
  } else {
    colnames(sumdf2)[2] <- "NUM_con_rvv_icat"
  }
  sumdf2$PCT_con_rvv_icat <- round(sumdf2$NUM_con_rvv_icat / nrow(ds1) * 100, 1)
  sumdf2$GRADING <- sumdf1$GRADING
  sumdf2$FLG_con_rvv_icat <- ifelse(sumdf2$GRADING == 1, TRUE, FALSE)


  #remove GRADING from sumdf1 that is going to be the SummaryData
  sumdf1$GRADING <- NULL

  # to add descriptions in the hover text of the headers of the table
  text_to_display <- util_get_hovertext("[con_inadmissible_categorical_hover]")

  attr(sumdf1, "description") <- text_to_display

  # TODO EK: Use util_pretty_vector_string for SummaryData to avoid very long strings
  # Examples:
  # util_pretty_vector_string(letters, n_max = 30)
  # util_pretty_vector_string(letters, n_max = 3)

  return(list(SummaryData = sumdf1,
              SummaryTable = sumdf2,
              ModifiedStudyData = msdf,
              FlaggedStudyData = ds1))
}

#' Detects variable levels not specified in standardized vocabulary
#'
#' @description
#' For each categorical variable, value lists should be defined in the metadata.
#'  This implementation will examine, if all observed levels in the study data
#'  are valid.
#'
#' [Indicator]
#'
#' @details
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific VALUE_LABELS as supplied in the
#'    metadata.
#'  - Identification of measurements not corresponding to the expected
#'    categories. Therefore two output data frames are generated:
#'    - on the level of observation to flag each undefined category, and
#'    - a summary table for each variable.
#'  - Values not corresponding to defined categories are removed in a data frame
#'    of modified study data
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                           from 0-100.
#'
#' @return a list with:
#' - `SummaryData`: data frame summarizing inadmissible categories with the
#' columns:
#'   - `Variables`: variable name/label
#'   - `OBSERVED_CATEGORIES`: the categories observed in the study data
#'   - `DEFINED_CATEGORIES`: the categories defined in the metadata
#'   - `NON_MATCHING`: the categories observed but not defined
#'   - `NON_MATCHING_N`: the number of observations with categories not defined
#'   - `NON_MATCHING_N_PER_CATEGORY`: the number of observations for each of the
#'                unexpected categories
#'   - `GRADING`: indicator TRUE/FALSE if inadmissible categorical values were
#'     observed (more than indicated by the `threshold_value`)
#' - `SummaryTable`: data frame for the `dataquieR` pipeline reporting the number
#'                   and percentage of inadmissible categorical values
#' - `ModifiedStudyData`: study data having inadmissible categories removed
#' - `FlaggedStudyData`: study data having cases with inadmissible categories
#' flagged
#'
#' @importFrom utils capture.output
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_inadmissible_categorical.html
#' )
#' @examples
#' \dontrun{
#' sdt <- data.frame(DIAG = c("B050", "B051", "B052", "B999"),
#'                   MED0 = c("S01XA28", "N07XX18", "ABC", NA), stringsAsFactors = FALSE)
#' mdt <- tibble::tribble(
#' ~ VAR_NAMES, ~ DATA_TYPE, ~ STANDARDIZED_VOCABULARY_TABLE, ~ SCALE_LEVEL, ~ LABEL,
#' "DIAG", "string", "<ICD10>", "nominal", "Diagnosis",
#' "MED0", "string", "<ATC>", "nominal", "Medication"
#' )
#' con_inadmissible_vocabulary(NULL, sdt, mdt, label_col = LABEL)
#' prep_load_workbook_like_file("meta_data_v2")
#' il <- prep_get_data_frame("item_level")
#' il$STANDARDIZED_VOCABULARY_TABLE[[11]] <- "<ICD10GM>"
#' il$DATA_TYPE[[11]] <- DATA_TYPES$INTEGER
#' il$SCALE_LEVEL[[11]] <- SCALE_LEVELS$NOMINAL
#' prep_add_data_frames(item_level = il)
#' r <- dq_report2("study_data", dimensions = "con")
#' r <- dq_report2("study_data", dimensions = "con",
#'      advanced_options = list(dataquieR.non_disclosure = TRUE))
#' r
#' }
con_inadmissible_vocabulary <- con_inadmissible_categorical

# TODO: allow gsub before checking, because of this:
# > head(prep_get_data_frame("<ICD10>")[[1]])
# [1] "A00"  "A00"  "A00"  "A000" "A001" "A009"
# > head(prep_get_data_frame("<ICD10GM>")[[1]])
# [1] "A00.0" "A00.1" "A00.9" "A01.-" "A01.0" "A01.1"

