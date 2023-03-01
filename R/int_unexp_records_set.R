#' Check for unexpected data record set
#'
#' @description
#' This function tests that the identifiers match a provided record set. It is possible to
#' check for unexpected data record sets by study segments or to consider only selected
#' segments.
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param ... Depending on `level`, passed to either
#'            [util_int_unexp_records_set_segment] or
#'            [util_int_unexp_records_set_dataframe]
#'
#' @return a [list]. Depending on `level`, see
#'   [util_int_unexp_records_set_segment] or
#'   [util_int_unexp_records_set_dataframe] for a description of the outputs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- readRDS(system.file("extdata", "ship.RDS",
#'   package = "dataquieR"
#' ))
#' meta_data <- readRDS(system.file("extdata", "ship_meta.RDS",
#'   package = "dataquieR"
#' ))
#' md1_segment <- readRDS(system.file("extdata", "meta_data_segment.RDS",
#'   package = "dataquieR"
#' ))
#' ids_segment <- readRDS(system.file("extdata", "meta_data_ids_segment.RDS",
#'   package = "dataquieR"
#' ))
#'
#' # TODO: update examples
#' int_unexp_records_set(
#'   level = "segment",
#'   identifier_name_list = c("INTERVIEW", "LABORATORY"),
#'   valid_id_table_list = ids_segment,
#'   meta_data_record_check = md1_segment[,
#'     c("STUDY_SEGMENT", "SEGMENT_RECORD_CHECK")],
#'   study_data = study_data,
#'   meta_data = meta_data,
#'   meta_data_level = md1_segment
#' )
#' }
int_unexp_records_set <- function(level = c("dataframe", "segment"),
                                  ...) {

  level <- util_match_arg(level)
  cl <- sys.call()
  fname <- paste("util", util_deparse1(cl[[1]]), level, sep = "_")
  cl2 <- do.call("call",
                 list(fname, level = level, ...))
  eval(cl2)

}
