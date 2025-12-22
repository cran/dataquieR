#' Verify and normalize metadata on segment level
#'
#' if possible, mismatching data types are converted (`"true"` becomes `TRUE`)
#'
#' missing columns are added, filled with `NA`, if this is valid, i.e., n.a.
#' for `STUDY_SEGMENT` as the key column
#'
#' @param meta_data_segment [data.frame] data frame or path/url of a metadata
#'                                       sheet for the segment level
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param segment_level [data.frame] alias for `meta_data_segment`
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
prep_check_meta_data_segment <- function(meta_data_segment = "segment_level",
                                         meta_data_v2,
                                         segment_level
                                         ) {

  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_data_frame(meta_data_segment)

  if (!(SEGMENT_RECORD_COUNT %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_RECORD_COUNT <- rep(NA_integer_,
                                                  nrow(meta_data_segment))
  }
  if (!(SEGMENT_ID_REF_TABLE %in% colnames(meta_data_segment))) {
    if ("SEGMENT_ID_TABLE" %in% colnames(meta_data_segment)) {
      util_message("Did not find %s in metadata, but %s, renaming it...",
                   dQuote("SEGMENT_ID_REF_TABLE"),
                   dQuote("SEGMENT_ID_TABLE"),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = FALSE)
      colnames(meta_data_segment)[colnames(meta_data_segment) ==
                                    "SEGMENT_ID_TABLE"] <-
        SEGMENT_ID_REF_TABLE
    } else {
      meta_data_segment$SEGMENT_ID_REF_TABLE <- rep(NA_character_,
                                                    nrow(meta_data_segment))
    }
  }
  if (!(SEGMENT_RECORD_CHECK %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_RECORD_CHECK <- rep(NA_character_,
                                                  nrow(meta_data_segment))
  }
  if (!(SEGMENT_ID_VARS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_ID_VARS <- rep(NA_character_,
                                             nrow(meta_data_segment))
  }
  if (!(SEGMENT_PART_VARS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_PART_VARS <- rep(NA_character_,
                                               nrow(meta_data_segment))
  }
  if (!(SEGMENT_UNIQUE_ROWS %in% colnames(meta_data_segment))) {
    meta_data_segment$SEGMENT_UNIQUE_ROWS <- rep(NA,
                                                 nrow(meta_data_segment))
  }
  if (!(SEGMENT_UNIQUE_ID %in% colnames(meta_data_segment))) {
    meta_data_segment[[SEGMENT_UNIQUE_ID]] <- rep(1,
                                                  nrow(meta_data_segment))
  }

  r <- util_expect_data_frame(
    custom_errors = list(
      SEGMENT_ID_VARS = "Column DF_ID_VARS must be character",
      SEGMENT_PART_VARS = "Column SEGMENT_PART_VARS must be character",
      STUDY_SEGMENT = "Column STUDY_SEGMENT must be character",
      SEGMENT_RECORD_COUNT = "Column SEGMENT_RECORD_COUNT must be integer",
      SEGMENT_ID_REF_TABLE = "Column SEGMENT_ID_REF_TABLE must be character",
      SEGMENT_RECORD_CHECK = "Column SEGMENT_RECORD_CHECK must be subset, superset or exact",
      SEGMENT_UNIQUE_ROWS = "Column SEGMENT_UNIQUE_ROWS must be true, false, or no_id",
      SEGMENT_UNIQUE_ID = "Column SEGMENT_UNIQUE_ID must be integer"
    ),
    meta_data_segment,
    list(
      SEGMENT_ID_VARS = is.character,
      SEGMENT_PART_VARS = is.character,
      STUDY_SEGMENT = is.character,
      SEGMENT_RECORD_COUNT = util_all_is_integer,
      SEGMENT_ID_REF_TABLE = is.character,
      SEGMENT_RECORD_CHECK = function(x) {
        all(util_empty(x) | x %in% c("superset", "subset", "exact"))
      },
      SEGMENT_UNIQUE_ROWS = function(x) {
        all(util_empty(x) | tolower(trimws(x)) %in%
              c("f", "t", "true", "false", "no_id"))
      },
      SEGMENT_UNIQUE_ID = util_all_is_integer
    ),
    list(
      SEGMENT_ID_VARS = as.character, # TODO: Write utility functions for converting data types with proper warnings
      SEGMENT_PART_VARS = as.character,
      STUDY_SEGMENT = as.character,
      SEGMENT_RECORD_COUNT = as.integer,
      SEGMENT_ID_REF_TABLE = as.character,
      SEGMENT_RECORD_CHECK = function(x) {
        r <-
          factor(tolower(trimws(as.character(x))),
                 levels = c("superset", "subset", "exact"))
        levels(r)[as.numeric(r)]
      },
      SEGMENT_UNIQUE_ROWS = as.character,
      SEGMENT_UNIQUE_ID = as.integer
    )
  )

  no_key <- util_empty(r[[STUDY_SEGMENT]])
  sum_no_key <- sum(no_key)

  if (sum_no_key) {
    util_message("Removing %d rows from %s, because %s is empty.",
                 sum_no_key,
                 dQuote("meta_data_segment"),
                 sQuote(STUDY_SEGMENT),
                 applicability_problem = TRUE
    )
  }

  r <- r[!no_key, , drop = FALSE]

  r
}
