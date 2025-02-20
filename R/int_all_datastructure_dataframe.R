#' Wrapper function to check for studies data structure
#'
#' @description
#' This function tests for unexpected elements and records, as well as duplicated identifiers and content.
#' The unexpected element record check can be conducted by providing the number of expected records or
#' an additional table with the expected records.
#' It is possible to conduct the checks by study segments or to consider only selected
#' segments.
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `DataframeTable`: data frame with selected check results, used for the data quality report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' out_dataframe <- int_all_datastructure_dataframe(
#'   meta_data_dataframe = "meta_data_dataframe",
#'   meta_data = "ship_meta"
#' )
#' md0 <- prep_get_data_frame("ship_meta")
#' md0
#' md0$VAR_NAMES
#' md0$VAR_NAMES[[1]] <- "Id" # is this missmatch reported -- is the data frame
#'                            # also reported, if nothing is wrong with it
#' out_dataframe <- int_all_datastructure_dataframe(
#'   meta_data_dataframe = "meta_data_dataframe",
#'   meta_data = md0
#' )
#'
#' # This is the "normal" procedure for inside pipeline
#' # but outside this function  checktype is exact by default
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")
#' lapply(setNames(nm = prep_get_data_frame("meta_data_dataframe")$DF_NAME),
#'   int_sts_element_dataframe, meta_data = md0)
#' md0$VAR_NAMES[[1]] <-
#'   "id" # is this missmatch reported -- is the data frame also reported,
#'        # if nothing is wrong with it
#' lapply(setNames(nm = prep_get_data_frame("meta_data_dataframe")$DF_NAME),
#'   int_sts_element_dataframe, meta_data = md0)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' }
#'
int_all_datastructure_dataframe <- function(meta_data_dataframe =
                                              "dataframe_level",
                                            item_level = "item_level",
                                            meta_data = item_level,
                                            meta_data_v2,
                                            dataframe_level) {

  # Preps and checks ----

  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
  util_expect_data_frame(meta_data)
  prep_check_meta_names(meta_data = meta_data,
                        level = REQUIRED)

  study_data_list <- lapply(
    setNames(nm = meta_data_dataframe[[DF_NAME]]),
    function(dfn) {
      r <- NULL
      try(r <- prep_get_data_frame(dfn), silent = TRUE)
      if (!is.data.frame(r)) {
        util_warning("Could not load/find data frame %s. Trying it without.",
                     dQuote(dfn))
      }
      r
    }
  )

  study_data_list <-
    study_data_list[!vapply(
      study_data_list, is.null, FUN.VALUE = logical(1))]

  df_name_ok <- meta_data_dataframe[[DF_NAME]] %in% names(study_data_list)

  if (!all(df_name_ok)) {
    util_warning("Losing %d data frame(s), because they could not be loaded",
                 sum(!df_name_ok)
                 )
  }

  meta_data_dataframe <- meta_data_dataframe[df_name_ok, , FALSE]

  id_vars_list <- lapply(setNames(meta_data_dataframe[[DF_ID_VARS]],
                                  nm = meta_data_dataframe[[DF_NAME]]),
                         util_parse_assignments,
                         multi_variate_text = TRUE )

  id_vars_list_vector <- lapply(id_vars_list, unlist, recursive = TRUE)


  # 1. Unexpected data element count ----

  # subset metadata with entries
  meta_data_element_count_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_ELEMENT_COUNT]]), ,
    drop = FALSE
  ]

  unexp_element_count_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    unexp_element_count_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE),
                                                   int_unexp_elements(
      identifier_name_list = meta_data_element_count_1[[DF_NAME]],
      data_element_count = meta_data_element_count_1[[DF_ELEMENT_COUNT]]
    ))
  })

  # 2. Unexpected data element set ----

  unexp_element_set_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    # subset_m cases are not reported here, since they are not usually a problem, if variable-level metadata is a large file for many data frames with diverse variable sets in, each.

    meta_data[[STUDY_SEGMENT]] <- NULL
    unexp_element_set_out <-withr::with_options(list(
      dataquieR.testdebug = TRUE), int_sts_element_dataframe(item_level = meta_data,
                                                       meta_data_dataframe =
                                                         meta_data_dataframe
                                                        )$DataframeTable)

    unexp_element_set_out$GRADING <-
      ifelse(unexp_element_set_out$NUM_int_sts_element == 0, 0, 1)
  })

  # 3. Unexpected data record count ----

  # subset metadata with entries
  meta_data_record_count_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_RECORD_COUNT]]), ,
      drop = FALSE
    ]

  unexp_records_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    unexp_records_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE), int_unexp_records_dataframe(
      identifier_name_list = meta_data_record_count_1[[DF_NAME]],
      data_record_count = meta_data_record_count_1[[DF_RECORD_COUNT]]
    ))
  })

  # 4. Unexpected data record set ----

  # subset metadata with entries
  meta_data_record_set_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_RECORD_CHECK]]), ,
      drop = FALSE
    ]

#  meta_data_record_set_1

  unexp_records_id_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    unexp_records_id_out <- util_int_unexp_records_set_dataframe(
      id_vars_list = id_vars_list_vector[meta_data_record_set_1[[DF_NAME]]],
      identifier_name_list = meta_data_record_set_1[[DF_NAME]],
      valid_id_table_list = meta_data_record_set_1[[DF_ID_REF_TABLE]],
      meta_data_record_check_list = meta_data_record_set_1[[DF_RECORD_CHECK]]
    )
  })


  # 5. Duplicates: ids ----

  meta_data_dup_ids_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_UNIQUE_ID]]) &
        !util_empty(meta_data_dataframe[[DF_ID_VARS]]), ,
      drop = FALSE
  ]

  duplicate_ids_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    duplicate_ids_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE), int_duplicate_ids(
      level = "dataframe",
      identifier_name_list = meta_data_dup_ids_1[[DF_NAME]],
      id_vars_list = id_vars_list_vector[meta_data_dup_ids_1[[DF_NAME]]],
      repetitions = meta_data_dup_ids_1[[DF_UNIQUE_ID]]))
    })

  # 6. Duplicates: content ----

  meta_data_dup_rows_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_UNIQUE_ROWS]]), ,
      drop = FALSE
    ]

  meta_data_dup_rows_1 <-
    meta_data_dup_rows_1[
      trimws(tolower(meta_data_dup_rows_1[[DF_UNIQUE_ROWS]])) ==
        "no_id" |
        !util_is_na_0_empty_or_false(meta_data_dup_rows_1[[DF_UNIQUE_ROWS]]), ]

  duplicates_rows_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    duplicates_rows_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE), int_duplicate_content(
      level = "dataframe",
      identifier_name_list = meta_data_dup_rows_1[[DF_NAME]],
      id_vars_list = id_vars_list_vector[meta_data_dup_rows_1[[DF_NAME]]],
      unique_rows = setNames(meta_data_dup_rows_1[[DF_UNIQUE_ROWS]],
                             nm = meta_data_dup_rows_1[[DF_NAME]])
    ))
  })

  # Output ----

  result <- list(
    int_sts_countel = unexp_element_count_out$DataframeTable,
    int_sts_element = unexp_element_set_out,
    int_sts_countre = unexp_records_out$DataframeTable,
    int_sts_setrc = unexp_records_id_out$DataframeTable,
    int_sts_dupl_ids = duplicate_ids_out$DataframeTable,
    int_sts_dupl_row = duplicates_rows_out$DataframeTable
  )

  unexp_element_set_outData <- unexp_element_set_out
  unexp_element_set_outData$`Data frame` <-
    unexp_element_set_outData[[DF_NAME]]
  unexp_element_set_outData[[DF_NAME]] <- NULL
  # TODO: Further map names here, later only from the technical table?
  cn <- colnames(unexp_element_set_outData)
  if (length(cn) > 0) {
    cn_pref <- vapply(strsplit(cn, "_"), `[[`, 1, FUN.VALUE = character(1))
    cn_suff <- vapply(lapply(strsplit(cn, "_"), tail, -1), paste,
                      collapse = "_", FUN.VALUE = character(1))
    cn_pref <-
      util_recode(
        cn_pref,
        util_get_concept_info("abbreviationMetrics"),
        "Abbreviation",
        "Metrics",
        NA_character_
      )

    cn_suff <-
      util_recode(
        cn_suff,
        util_get_concept_info("dqi"),
        "abbreviation",
        "Name",
        NA_character_
      )


    colnames(unexp_element_set_outData)[!is.na(cn_pref) & !is.na(cn_suff)] <-
      paste0(cn_suff, ": ", cn_pref)[!is.na(cn_pref) & !is.na(cn_suff)]
  }
  resultData <- list(
    int_sts_countel = unexp_element_count_out$DataframeData,
    int_sts_element = unexp_element_set_outData,
    int_sts_countre = unexp_records_out$DataframeData,
    int_sts_setrc = unexp_records_id_out$DataframeData,
    int_sts_dupl_ids = duplicate_ids_out$DataframeData,
    int_sts_dupl_row = duplicates_rows_out$DataframeData
  )

  for (n in names(resultData)) {
    rownames(resultData[[n]]) <- NULL
  }

  names(resultData) <-
    util_recode(
      names(resultData),
      util_get_concept_info("dqi"),
      "abbreviation",
      "Name",
      names(resultData)
    )

  DataframeTable <- util_merge_data_frame_list(result, "DF_NAME")
  cn <- colnames(DataframeTable)
  if (length(cn) > 0) {
    cn[startsWith(cn, "GRADING.")] <- gsub("^GRADING\\.", "GRADING_",
                                       cn[startsWith(cn, "GRADING.")])
    colnames(DataframeTable) <- cn
  }
  # DataframeData <- util_merge_data_frame_list(resultData, "Data frame")

  DataframeData_1 <- util_make_data_slot_from_table_slot(DataframeTable)
  if ("resp_vars" %in% colnames(DataframeTable)) {
    DataframeData_1$`Unexp. Variables` <- DataframeTable$resp_vars
  }

  DataframeData <- data.frame(Dataframe = DataframeTable[[DF_NAME]])
  #Replace separate columns with just one column containing N and %
    #Only if content is present, merge columns
  if(!is.null(DataframeData_1$`Unexpected data element count (Number)`)){
    DataframeData$`Unexpected data element count N (%)` <- util_paste0_with_na(DataframeData_1$`Unexpected data element count (Number)`, " (",
                                                   DataframeData_1$`Unexpected data element count (Percentage (0 to 100))`, ")")
   DataframeData$`Unexpected data element count (Grading)`<- DataframeData_1$`Unexpected data element count (Grading)`
  }

  if(!is.null(DataframeData_1$`Unexp. Variables`)){
    DataframeData$`Unexp. Variables`<- DataframeData_1$`Unexp. Variables`
  }

  if(!is.null(DataframeData_1$`Unexpected data element set (Number)`)){
    DataframeData$`Unexpected data element set N (%)` <- util_paste0_with_na(DataframeData_1$`Unexpected data element set (Number)`, " (",
                                                   DataframeData_1$`Unexpected data element set (Percentage (0 to 100))`, ")")
    DataframeData$`Unexpected data element set (Grading)`<- DataframeData_1$`Unexpected data element set (Grading)`
  }

  if(!is.null(DataframeData_1$`Unexpected data record count (Number)`)){
    DataframeData$`Unexpected data record count N (%)` <- util_paste0_with_na(DataframeData_1$`Unexpected data record count (Number)`, " (",
                                                   DataframeData_1$`Unexpected data record count (Percentage (0 to 100))`, ")")
    DataframeData$`Unexpected data record count (Grading)`<- DataframeData_1$`Unexpected data record count (Grading)`
  }

  if(!is.null(DataframeData_1$`Unexpected data record set (Number)`)){
    DataframeData$`Unexpected data record set N (%)` <- util_paste0_with_na(DataframeData_1$`Unexpected data record set (Number)`, " (",
                                                 DataframeData_1$`Unexpected data record set (Percentage (0 to 100))`, ")")
    DataframeData$`Unexpected data record set (Grading)`<- DataframeData_1$`Unexpected data record set (Grading)`
  }

  if(!is.null(DataframeData_1$`Duplicates (Number)`)){
    DataframeData$`Duplicates N (%)`<- util_paste0_with_na(DataframeData_1$`Duplicates (Number)`," (",
                                                    DataframeData_1$`Duplicates (Percentage (0 to 100))`, ")")
    DataframeData$`Duplicates (Grading)`<- DataframeData_1$`Duplicates (Grading)`
  }

  rm(DataframeData_1)


  return(list(
    DataframeTable = DataframeTable,
    DataframeData = DataframeData,
    DataframeDataList = resultData
  ))
}
# FIXME: In INT, we see errors for partiaql reports, because study_data was split by segments, so variables are missing -- maybe meta_data was split but not study  data?
