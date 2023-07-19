#' Verify and normalize metadata on data frame level
#'
#' if possible, mismatching data types are converted (`"true"` becomes `TRUE`)
#'
#' missing columns are added, filled with `NA`, if this is valid, i.e., n.a.
#' for `DF_NAME` as the key column
#'
#' @param meta_data_dataframe [data.frame] data frame or path/url of a metadata
#'                                       sheet for the data frame level
#'
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
#' mds1$DF_UNIQUE_ID[[2]] <- 12 # not yet supported
#' # print(prep_check_meta_data_dataframe(mds1)) # fail
#' }
prep_check_meta_data_dataframe <- function(meta_data_dataframe =
                                             "dataframe_level") {
  util_expect_data_frame(meta_data_dataframe)

  if (!(DF_RECORD_COUNT %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe$DF_RECORD_COUNT <- rep(NA_integer_, nrow(meta_data_dataframe))
  }
  if (!(DF_ID_REF_TABLE %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe$DF_ID_REF_TABLE <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_RECORD_CHECK %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe$DF_RECORD_CHECK <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_ID_VARS %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe$DF_ID_VARS <- rep(NA_character_, nrow(meta_data_dataframe))
  }
  if (!(DF_UNIQUE_ROWS %in% colnames(meta_data_dataframe))) {
    meta_data_dataframe$DF_RECORD_CHECK <- rep(NA, nrow(meta_data_dataframe))
  }

  r <- util_expect_data_frame(
    meta_data_dataframe,
    list(
      DF_ID_VARS = is.character, # TODO: Write utility functions for converting data types with proper warnings
      DF_NAME = is.character,
      DF_RECORD_COUNT = util_all_is_integer,
      DF_ID_REF_TABLE = is.character,
      DF_RECORD_CHECK = function(x) {
        all(util_empty(x) | x %in% c("subset", "exact"))
      },
      DF_UNIQUE_ROWS = is.logical,
      DF_UNIQUE_ID = util_all_is_integer
    ), # TODO: We have DF_ID_REF_TABLE but DF_ID_TABLE. Could you add a todo for this?
    list(
      DF_ID_VARS = as.character,
      DF_NAME = as.character,
      DF_RECORD_COUNT = as.integer,
      DF_ID_REF_TABLE = as.character,
      DF_RECORD_CHECK = function(x) {
        r <-
          factor(tolower(trimws(as.character(x))),
                 levels = c(c("subset", "exact")))
        levels(r)[as.numeric(r)]
      },
      DF_UNIQUE_ROWS = as.logical,
      DF_UNIQUE_ID = as.integer
    )
  )

  if (any(r$DF_UNIQUE_ID != 1, na.rm = TRUE)) {
    util_error("%s is not yet supported in %s",
               dQuote("DF_UNIQUE_ID"),
               sQuote("meta_data_dataframe"))
  }

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

  r
}
