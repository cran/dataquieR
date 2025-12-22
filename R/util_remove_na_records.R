#' remove all records, that have at least one `NA` in any of the given variables
#'
#' @param study_data the study data frame
#' @param vars the variables being checked for `NA`s
#'
#' @return modified study_data data frame
#'
#' @examples
#' \dontrun{
#' dta <- iris
#' dim(util_remove_na_records(dta))
#' dta$Species[4:6] <- NA
#' dim(util_remove_na_records(dta))
#' dim(util_remove_na_records(dta, c("Sepal.Length", "Petal.Length")))
#' }
#'
#'
#' @family data_management
#' @concept missing
#' @noRd
util_remove_na_records <- function(study_data, vars = colnames(study_data)) {
  obs_wo_na <- rowSums(is.na(study_data[, vars, FALSE])) == 0
  if (sum(!obs_wo_na) > 0) {
    util_message(
      c("Removing %d observations because of NAs in some of",
        "the following columns: %s"),
      sum(!obs_wo_na),
      paste0(dQuote(vars), collapse = ", ")
    )
  }
  r <- study_data[obs_wo_na, , FALSE]
  attributes(r)[.ds1_attribute_names] <-
    attributes(study_data)[.ds1_attribute_names]
  r
}
