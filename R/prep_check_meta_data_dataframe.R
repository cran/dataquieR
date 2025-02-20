#' Verify and normalize metadata on data frame level
#'
#' if possible, mismatching data types are converted (`"true"` becomes `TRUE`)
#'
#' missing columns are added, filled with `NA`, if this is valid, i.e., n.a.
#' for `DF_NAME` as the key column
#'
#' @param meta_data_dataframe [data.frame] data frame or path/url of a metadata
#'                                       sheet for the data frame level
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`
#' @return standardized metadata sheet as data frame
#' @export
#'
#' @examples
#' \dontrun{
#' mds <- prep_check_meta_data_dataframe("ship_meta_dataframe|dataframe_level") # also converts
#' print(mds)
#' prep_check_meta_data_dataframe(mds)
#' mds1 <- mds
#' mds1$DF_RECORD_COUNT <- NULL
#' print(prep_check_meta_data_dataframe(mds1)) # fixes the missing column by NAs
#' mds1 <- mds
#' mds1$DF_UNIQUE_ROWS[[2]] <- "xxx" # not convertible
#' # print(prep_check_meta_data_dataframe(mds1)) # fail
#' mds1 <- mds
#' mds1$DF_UNIQUE_ID[[2]] <- 12
#' # print(prep_check_meta_data_dataframe(mds1)) # fail
#' }
prep_check_meta_data_dataframe <- function(meta_data_dataframe =
                                             "dataframe_level",
                                           meta_data_v2,
                                           dataframe_level) {
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_data_frame(meta_data_dataframe)

  if (!(DF_ELEMENT_COUNT %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_ELEMENT_COUNT]] <- rep(NA_integer_, nrow(meta_data_dataframe))
  }
  if (!(DF_RECORD_COUNT %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_RECORD_COUNT]] <- rep(NA_integer_, nrow(meta_data_dataframe))
  }
  if (!(DF_ID_REF_TABLE %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_ID_REF_TABLE]] <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_RECORD_CHECK %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_RECORD_CHECK]] <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_ID_VARS %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_ID_VARS]] <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_UNIQUE_ID %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_UNIQUE_ID]] <- rep(NA, nrow(meta_data_dataframe))
  }
  if (!(DF_UNIQUE_ROWS %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_UNIQUE_ROWS]] <- rep(NA, nrow(meta_data_dataframe))
  }
  if (!(DF_CODE %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe[[DF_CODE]] <- rep(NA, nrow(meta_data_dataframe))
  }

  r <- util_expect_data_frame(
    custom_errors = list(
      DF_ID_VARS = "Column DF_ID_VARS must be character",
      DF_NAME = "Column DF_NAME must be character",
      DF_CODE = "Column DF_CODE must be character",
      DF_RECORD_COUNT = "Column DF_RECORD_COUNT must be integer",
      DF_ELEMENT_COUNT = "Column DF_ELEMENT_COUNT must be integer",
      DF_ID_REF_TABLE = "Column DF_ID_REF_TABLE must be character",
      DF_RECORD_CHECK = "Column DF_RECORD_CHECK must be subset, superset or exact",
      DF_UNIQUE_ROWS = "Column DF_UNIQUE_ROWS must be true, false, or no_id",
      DF_UNIQUE_ID = "Column DF_UNIQUE_ID must be integer"
    ),
    meta_data_dataframe,
    list(
      DF_ID_VARS = is.character, # TODO: Write utility functions for converting data types with proper warnings
      DF_NAME = is.character,
      DF_CODE = is.character,
      DF_RECORD_COUNT = util_all_is_integer,
      DF_ELEMENT_COUNT = util_all_is_integer,
      DF_ID_REF_TABLE = is.character,
      DF_RECORD_CHECK = function(x) {
        all(util_empty(x) | x %in% c("superset", "subset", "exact"))
      },
      DF_UNIQUE_ROWS = function(x) {
        all(util_empty(x) | tolower(trimws(x)) %in%
              c("f", "t", "true", "false", "no_id"))
      },
      DF_UNIQUE_ID = util_all_is_integer
    ), # TODO: We have DF_ID_REF_TABLE but DF_ID_TABLE. Could you add a todo for this?
    list(
      DF_ID_VARS = as.character,
      DF_NAME = as.character,
      DF_CODE = as.character,
      DF_RECORD_COUNT = as.integer,
      DF_ELEMENT_COUNT = as.integer,
      DF_ID_REF_TABLE = as.character,
      DF_RECORD_CHECK = function(x) {
        r <-
          factor(tolower(trimws(as.character(x))),
                 levels = c(c("superset", "subset", "exact")))
        levels(r)[as.numeric(r)]
      },
      DF_UNIQUE_ROWS = as.character,
      DF_UNIQUE_ID = as.integer
    )
  )

  no_key <- util_empty(r[[DF_NAME]])
  sum_no_key <- sum(no_key)

  if (sum_no_key) {
    util_message("Removing %d rows from %s, because %s is empty.",
                 sum_no_key,
                 dQuote("meta_data_dataframe"),
                 sQuote(DF_NAME),
                 applicability_problem = TRUE
                 )
  }

  r <- r[!no_key, , drop = FALSE]

  # Check each DF_CODE and DF_NAME not used more than once
  if (any(duplicated(r[[DF_NAME]], incomparables = NA))) {
    util_error("Found duplicated dataframes in dataframe level metadata: %s",
               util_pretty_vector_string(
                 sQuote(r[[DF_NAME]][
                   duplicated(r[[DF_NAME]], incomparables = NA)])
               ),
               applicability_problem = TRUE)
  }

  if (any(duplicated(r[[DF_CODE]], incomparables = NA))) {
    util_error(
      "Found duplicated dataframe codes in dataframe level metadata: %s",
      util_pretty_vector_string(
        sQuote(r[[DF_CODE]][
          duplicated(r[[DF_CODE]], incomparables = NA)])
      ),
      applicability_problem = TRUE)
  }

  r
}
