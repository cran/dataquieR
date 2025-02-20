#' Determine missing and/or superfluous data elements
#'
#' Depends on [dataquieR.ELEMENT_MISSMATCH_CHECKTYPE] option,
#' see there
#'
#' [Indicator]
#' @inheritParams .template_function_indicator
#' @param check_type [enum] none | exact | subset_u | subset_m. See
#'                               [dataquieR.ELEMENT_MISSMATCH_CHECKTYPE]
#'
#' @return [list] with names lots:
#'   - `DataframeData`: data frame with the unexpected elements check results.
#'   - `DataframeTable`: [data.frame] table with all errors, used for the data quality report:
#'                       - `PCT_int_sts_element`: Percentage of element
#'                                                mismatches
#'                       - `NUM_int_sts_element`: Number of element
#'                                                mismatches
#'                       - `resp_vars`: affected element names
#'
#' @export
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("~/tmp/df_level_test.xlsx")
#' meta_data_dataframe <- "dataframe_level"
#' meta_data <- "item_level"
#' }
int_sts_element_dataframe <- function(item_level = "item_level",
                                      meta_data_dataframe = "dataframe_level",
                                      meta_data = item_level,
                                      meta_data_v2,
                                      check_type =
                                        getOption(
                                          "dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
                                          dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default),
                                      dataframe_level) { # TODO: Add MISSING-column as in segment
  # Preps and checks ----

  util_maybe_load_meta_data_v2()

  # checks and fixes the function arguments
  util_ck_arg_aliases()

  meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
  util_expect_data_frame(meta_data_dataframe, col_names = DF_CODE)

  util_expect_data_frame(meta_data, col_names = DATAFRAMES)
  prep_check_meta_names(meta_data = meta_data,
                        level = REQUIRED)


  util_match_arg(check_type,
                 choices = c(
                   "none",
                   "exact",
                   "subset_u",
                   "subset_m"
                 ))

  DataframeData <- data.frame(DF_NAME = meta_data_dataframe[[DF_NAME]],
                              NUM_int_sts_element = rep(NA_integer_,
                                                        nrow(meta_data_dataframe
                                                        )),
                              PCT_int_sts_element = rep(NA_real_,
                                                        nrow(meta_data_dataframe
                                                        )),
                              resp_vars = rep(NA_character_,
                                              nrow(meta_data_dataframe))
  )
  if (check_type != "none") {
    # dfr_names <- list2env(
    #   setNames(as.list(meta_data_dataframe[[DF_NAME]]),
    #            nm = meta_data_dataframe[[DF_CODE]])
    # )

    dfr_codes <- list2env(
      setNames(as.list(meta_data_dataframe[[DF_CODE]]),
               nm = meta_data_dataframe[[DF_NAME]])
    )

    meta_data_dataframe <-
      meta_data_dataframe[!util_empty(meta_data_dataframe[[DF_CODE]]), , FALSE]
    parsed_dataframes_col <- lapply(setNames(
      meta_data[[DATAFRAMES]],
      nm = meta_data[[VAR_NAMES]]
    ), util_parse_assignments,
    multi_variate_text = TRUE
    )

    # switch names and values
    # usable_df_names <- intersect(prep_list_dataframes(),
    #           meta_data_dataframe[[DF_NAME]])

    DataframeData <- data.frame(DF_NAME = meta_data_dataframe[[DF_NAME]],
                                NUM_int_sts_element = rep(NA_integer_,
                                                          nrow(meta_data_dataframe
                                                          )),
                                PCT_int_sts_element = rep(NA_real_,
                                                          nrow(meta_data_dataframe
                                                          )),
                                resp_vars = rep(NA_character_,
                                                nrow(meta_data_dataframe))
    )


    DataframeData$resp_vars <-
      vapply(meta_data_dataframe[[DF_NAME]], function(df) {
        cur_code <- dfr_codes[[df]]

        from_item_level <- meta_data[, VAR_NAMES]
        from_df <- colnames(prep_get_data_frame(df,
                                                column_names_only = TRUE))
        from_all <- union(from_item_level, from_df)

        not_in_df <- from_all[(!(from_all %in% from_df))]
        not_in_il <- from_all[(!(from_all %in% from_item_level))]

        r <- character(0)
        if (length(not_in_df) > 0) {
          r["not_in_df"] <- paste("{",
                                  util_pretty_vector_string(not_in_df, n_max = 5),
                                  "} \u2209 sd")
        }
        if (length(not_in_il) > 0) {
          r["not_in_il"] <- paste("{",
                                  util_pretty_vector_string(not_in_il, n_max = 5),
                                  "} \u2209 md")
        }

        if (check_type == "subset_u") {
          r <- r["not_in_il"]
        }
        if (check_type == "subset_m") {
          r <- r["not_in_df"]
        }
        return(paste(r, collapse = " \u2227 "))
        # optional: add the dataframes to the cache?
        # cur_df <- prep_get_data_frame(df,
        #                     keep_types = TRUE)
        # prep_add_data_frames(data_frame_list = setNames(
        #   list(cur_df),
        #   nm = get(DF_CODE, envir = dfr_names)
        # ))
      }, FUN.VALUE = character(1))

    DataframeData$NUM_int_sts_element <-
      vapply(meta_data_dataframe[[DF_NAME]], function(df) {
        cur_code <- dfr_codes[[df]]

        from_item_level <- meta_data[, VAR_NAMES]
        from_df <- colnames(prep_get_data_frame(df,
                                                column_names_only = TRUE))
        from_all <- union(from_item_level, from_df)

        not_in_df <- sum(!(from_all %in% from_df))
        not_in_il <- sum(!(from_all %in% from_item_level))

        if (check_type == "exact") {
          return(not_in_df + not_in_il)
        }
        if (check_type == "subset_u") {
          return(not_in_il)
        }
        if (check_type == "subset_m") {
          return(not_in_df)
        }
        # optional: add the dataframes to the cache?
        # cur_df <- prep_get_data_frame(df,
        #                     keep_types = TRUE)
        # prep_add_data_frames(data_frame_list = setNames(
        #   list(cur_df),
        #   nm = get(DF_CODE, envir = dfr_names)
        # ))
      }, FUN.VALUE = integer(1))

    DataframeData$PCT_int_sts_element <-
      vapply(meta_data_dataframe[[DF_NAME]], function(df) {
        cur_code <- dfr_codes[[df]]

        from_item_level <- meta_data[, VAR_NAMES]
        from_df <- colnames(prep_get_data_frame(df,
                                                column_names_only = TRUE))
        from_all <- union(from_item_level, from_df)

        not_in_df <- sum(!(from_all %in% from_df))
        not_in_il <- sum(!(from_all %in% from_item_level))

        if (check_type == "exact") {
          return(100 * (not_in_df + not_in_il) / length(from_all))
        }
        if (check_type == "subset_u") {
          return(100 * not_in_il / length(from_df))
        }
        if (check_type == "subset_m") {
          return(100 * not_in_df / length(from_item_level))
        }
      }, FUN.VALUE = numeric(1))
  }

  DataframeTable <- DataframeData[, c(
    "DF_NAME",
    "NUM_int_sts_element" ,
    "PCT_int_sts_element"
  ), FALSE]
  DataframeData$PCT_int_sts_element <-
    round(DataframeData$PCT_int_sts_element, 2)
  colnames(DataframeData) <-
    util_translate_indicator_metrics(colnames(DataframeData),
                                     ignore_unknown = TRUE)
  colnames(DataframeData)[colnames(DataframeData) == "resp_vars"] <-
    "Affected Elements"
  return(list(
    DataframeTable = DataframeTable,
    DataframeData = DataframeData
  ))

}
