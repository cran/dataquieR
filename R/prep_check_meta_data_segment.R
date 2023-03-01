#' Verify and normalize meta data on segment level
#'
#' if possible, mismatching data types are converted (`"true"` becomes `TRUE`)
#'
#' missing columns are added, filled with `NA`, if this is valid, i.e., n.a.
#' for `STUDY_SEGMENT` as the key column
#'
#' @param meta_data_segment [data.frame] data frame or path/url of a metadata
#'                                       sheet for the segment level
#'
#' @return standardized metadata sheet as data frame
#' @export
#'
#' @examples
#' \dontrun{
#' mds <- prep_check_meta_data_segment("ship_meta_v2|segment_level") # also converts
#' print(mds)
#' prep_check_meta_data_segment(mds)
#' mds1 <- mds
#' mds1$SEGMENT_RECORD_COUNT <- NULL
#' print(prep_check_meta_data_segment(mds1)) # fixes the missing column by NAs
#' mds1 <- mds
#' mds1$SEGMENT_UNIQUE_ROWS[[2]] <- "xxx" # not convertible
#' # print(prep_check_meta_data_segment(mds1)) # fail
#' }
prep_check_meta_data_segment <- function(meta_data_segment = "segment_level") {
  util_expect_data_frame(meta_data_segment)

  if (!(SEGMENT_RECORD_COUNT %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_RECORD_COUNT <- NA_integer_
  }
  if (!(SEGMENT_ID_TABLE %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_ID_TABLE <- NA_character_
  }
  if (!(SEGMENT_RECORD_CHECK %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_RECORD_CHECK <- NA_character_
  }
  if (!(SEGMENT_ID_VARS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_ID_VARS <- NA_character_
  }
  if (!(SEGMENT_PART_VARS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_PART_VARS <- NA_character_
  }
  if (!(SEGMENT_UNIQUE_ROWS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_RECORD_CHECK <- NA
  }

  r <- util_expect_data_frame(
    meta_data_segment,
    list(
      SEGMENT_ID_VARS = is.character,
      SEGMENT_PART_VARS = is.character,
      STUDY_SEGMENT = is.character,
      SEGMENT_RECORD_COUNT = util_all_is_integer,
      SEGMENT_ID_TABLE = is.character,
      SEGMENT_RECORD_CHECK = function(x) {
        all(util_empty(x) | x %in% c("subset", "exact"))
      },
      SEGMENT_UNIQUE_ROWS = is.logical
    ),
    list(
      SEGMENT_ID_VARS = as.character, # TODO: Write utility functions for converting data types with proper warnings
      SEGMENT_PART_VARS = as.character,
      STUDY_SEGMENT = as.character,
      SEGMENT_RECORD_COUNT = as.integer,
      SEGMENT_ID_TABLE = as.character,
      SEGMENT_RECORD_CHECK = function(x) {
        r <-
          factor(tolower(trimws(as.character(x))),
                 levels = c(c("subset", "exact")))
        levels(r)[as.numeric(r)]
      },
      SEGMENT_UNIQUE_ROWS = as.logical
    )
  )

  no_key <- util_empty(r[[STUDY_SEGMENT]])
  sum_no_key <- sum(no_key)

  if (sum_no_key) {
    util_message("Removing %d rows from %s, because %s is empty.",
                 sum_no_key,
                 dQuote("meta_data_segment"),
                 sQuote(STUDY_SEGMENT)
    )
  }

  r <- r[!no_key, , drop = FALSE]

  r
}
