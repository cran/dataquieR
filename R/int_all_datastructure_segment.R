#' Wrapper function to check for segment data structure
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
#' @param meta_data_segment [data.frame] the data frame that contains the metadata for the segment level, mandatory
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#'
#' @return a [list] with
#'   - `SegmentTable`: data frame with selected check results, used for the data quality report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' out_segment <- int_all_datastructure_segment(
#'   meta_data_segment = "meta_data_segment",
#'   study_data = "ship",
#'   meta_data = "ship_meta"
#' )
#'
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Ex"))
#'
#' out_segment <- int_all_datastructure_segment(
#'   meta_data_segment = "meta_data_segment",
#'   study_data = study_data,
#'   meta_data = meta_data
#' )
#' }
int_all_datastructure_segment <- function(meta_data_segment = "segment_level",
                                          study_data,
                                          meta_data = "item_level") {

  # Preps and checks ----
  # map metadata to study data
  prep_prepare_dataframes(.allow_empty = TRUE)
  if (!(STUDY_SEGMENT %in% colnames(meta_data))) {
    meta_data[[STUDY_SEGMENT]] <- "ALL" # TODO: Warn
  }
  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)

  id_vars_list <- lapply(setNames(meta_data_segment[[SEGMENT_ID_VARS]], # TODO: use the constants everywhere: meta_data_segment[[SEGMENT_ID_VARS]], not ...$SEGMENT_ID_VARS
                                  nm = meta_data_segment[[STUDY_SEGMENT]]),
    util_parse_assignments,
    multi_variate_text = TRUE
  )
  id_vars_list_vector <- lapply(id_vars_list, unlist, recursive = TRUE)

  id_vars_list_vector <- lapply(id_vars_list_vector,
                                util_map_labels,
                                meta_data = meta_data,
                                to = label_col)

  # 1. Unexpected data record count ----

  # subset metadata with entries
  meta_data_record_count_0 <-
    meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_RECORD_COUNT]]), ,
      drop = FALSE]

  unexp_records_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    unexp_records_out <- int_unexp_records_segment(
      study_segment = meta_data_record_count_0[[STUDY_SEGMENT]],
      data_record_count = meta_data_record_count_0[[SEGMENT_RECORD_COUNT]],
      study_data, meta_data)
    })


  # 2. Unexpected data record set ----
  # subset metadata with entries
  meta_data_record_set_1 <-
    meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_RECORD_CHECK]]), ,
    drop = FALSE
  ]

  unexp_records_id_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    unexp_records_id_out <- int_unexp_records_set(
      level = "segment",
      id_vars_list =
        id_vars_list_vector[meta_data_record_set_1[[STUDY_SEGMENT]]],
      identifier_name_list = meta_data_record_set_1[[STUDY_SEGMENT]],
      valid_id_table_list = meta_data_record_set_1[[SEGMENT_ID_REF_TABLE]],
      meta_data_record_check_list =
        meta_data_record_set_1[[SEGMENT_RECORD_CHECK]],
      study_data = study_data,
      meta_data = meta_data)
  })


  # 3. Duplicates: ids ----
  meta_data_dup_ids_1 <-
    meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_ID_VARS]]), ,
                      drop = FALSE
    ]

  duplicate_ids_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {

    duplicate_ids_out <- int_duplicate_ids(
      level = "segment",
      id_vars_list = id_vars_list_vector[meta_data_dup_ids_1[[STUDY_SEGMENT]]],
      study_segment = meta_data_dup_ids_1[[STUDY_SEGMENT]],
      study_data = study_data,
      meta_data = meta_data
    )})


  # 4. Duplicates: content ----
  meta_data_dup_rows_1 <-
    meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_UNIQUE_ROWS]]), ,
    drop = FALSE
  ]

  meta_data_dup_rows_1 <-
    meta_data_dup_rows_1[meta_data_dup_rows_1[[SEGMENT_UNIQUE_ROWS]], ,
    drop = FALSE
  ]

  duplicate_rows_out <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    duplicate_rows_out <- int_duplicate_content(
      level = "segment",
      study_segment = meta_data_dup_rows_1[[STUDY_SEGMENT]],
      study_data = study_data,
      meta_data = meta_data
    )
  })

  # X. Unexpected data element set ----

  # less worse, but only subset_m possible
  check_type_old <- options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_m") # "subset_u") # subset_m, subset_u, exact ,none
  on.exit(options(check_type_old))

  out_int_sts_element <- NULL # TODO: capture errors form the next all and put them to the matrices
  try(silent = TRUE, {
    out_int_sts_element <-
      int_sts_element_segment(study_data = study_data,
                              meta_data = meta_data)$SegmentTable

    out_int_sts_element$GRADING <-
      ifelse(out_int_sts_element$NUM_int_sts_element == 0, 0, 1)
    rownames(out_int_sts_element) <- NULL
  })

  # Output ----
  result <- list(
    int_sts_countre = unexp_records_out$SegmentTable,
    int_sts_setrc = unexp_records_id_out$SegmentTable,
    int_sts_dupl_ids = duplicate_ids_out$SegmentTable,
    int_sts_dupl_content = duplicate_rows_out$SegmentTable,
    int_sts_element = out_int_sts_element
  )

  result <- result[vapply(result, FUN.VALUE = logical(1), # TODO: Why remove empty seg-dfs, and why not in int_all_datastructure_dataframe?!
                          FUN = function(df) {
                            !!prod(dim(df))
                          })]

  out_int_sts_elementData <- out_int_sts_element
  # unexp_element_set_outData$`Data frame` <- TODO: here not needed: Both are currently Segment, why not STUDY_SEGMENT
  #   unexp_element_set_outData[[DF_NAME]]
  # unexp_element_set_outData[[DF_NAME]] <- NULL
  # TODO: Further map names here, later only from the technical table?
  cn <- colnames(out_int_sts_elementData)
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


  colnames(out_int_sts_elementData)[!is.na(cn_pref) & !is.na(cn_suff)] <-
    paste0(cn_suff, ": ", cn_pref)[!is.na(cn_pref) & !is.na(cn_suff)]

  resultData <- list(
    int_sts_countre = unexp_records_out$SegmentData,
    int_sts_setrc = unexp_records_id_out$SegmentData,
    int_sts_dupl_ids = duplicate_ids_out$SegmentData,
    int_sts_dupl_content = duplicate_rows_out$SegmentData,
    int_sts_element = out_int_sts_elementData
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

  SegmentTable <- util_merge_data_frame_list(result, "Segment") # TODO: why Segment and not STUDY_SEGMENT?!
  cn <- colnames(SegmentTable)
  cn[startsWith(cn, "GRADING.")] <- gsub("^GRADING\\.", "GRADING_",
                                     cn[startsWith(cn, "GRADING.")])
  colnames(SegmentTable) <- cn

  # SegmntData <- util_merge_data_frame_list(resultData, "Segment")



  SegmentData1 <- util_make_data_slot_from_table_slot(SegmentTable)
  #Create the df
  SegmentData<- data.frame(Segment = SegmentData1$Segment)
  #Only if content is present, merge columns
  if(!is.null(SegmentData1$`Unexpected data record count (Number)`)){
    SegmentData$`Unexpected data record count N (%)` <- paste0(SegmentData1$`Unexpected data record count (Number)`, " (",
                                                               SegmentData1$`Unexpected data record count (Percentage (0 to 100))`, ")")
  }

  SegmentData$`Unexpected data record count (Grading)`<- SegmentData1$`Unexpected data record count (Grading)`

  if(!is.null(SegmentData1$`Unexpected data record set (Number)`)){
    SegmentData$`Unexpected data record set N (%)` <- paste0(SegmentData1$`Unexpected data record set (Number)`, " (",
                                                 SegmentData1$`Unexpected data record set (Percentage (0 to 100))`, ")")
  }
  SegmentData$`Unexpected data record set (Grading)`<- SegmentData1$`Unexpected data record set (Grading)`


   if(!is.null(SegmentData1$`Duplicates (Number)`)){
    SegmentData$`Duplicates N (%)` <- paste0(SegmentData1$`Duplicates (Number)`, " (",
                                               SegmentData1$`Duplicates (Percentage (0 to 100))`, ")")
  }
  SegmentData$`Duplicates (Grading)`<- SegmentData1$`Duplicates (Grading)`


   if(!is.null(SegmentData1$`Unexpected data element set (Number)`)){
    SegmentData$`Unexpected data element set N (%)` <- paste0(SegmentData1$`Unexpected data element set (Number)`, " (",
                                                  SegmentData1$`Unexpected data element set (Percentage (0 to 100))`, ")")
  }
  SegmentData$`Unexpected data element set (Grading)`<- SegmentData1$`Unexpected data element set (Grading)`

  rm(SegmentData1)

  return(list(
    SegmentTable = SegmentTable,
    SegmentData = SegmentData,
    SegmentDataList = resultData
  ))
}
