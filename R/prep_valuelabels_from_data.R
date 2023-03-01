#' Get value labels from data
#'
#' Detects factors and converts them to compatible metadata/study data.
#'
#' @param resp_vars [variable] names of the variables to fetch the value labels
#'                             from the data
#' @param study_data [data.frame] the data frame that contains the measurements
#'
#' @return a [list] with:
#'   - `VALUE_LABELS`: vector of value labels and modified study data
#'   - `ModifiedStudyData`: study data with factors as integers
#'
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' dataquieR::prep_datatype_from_data(iris)
#' }
prep_valuelabels_from_data <-
  function(resp_vars = colnames(study_data), study_data) {
  if (!missing(resp_vars) && is.data.frame(resp_vars) && missing(study_data)) {
    study_data <- resp_vars
    resp_vars <- colnames(study_data)
  } else if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame in the argument %s",
               dQuote("study_data"))
  }

  util_expect_data_frame(study_data)

  if (requireNamespace("tibble", quietly = TRUE)) {
    if (tibble::is_tibble(study_data)) {
      study_data <- as.data.frame(study_data)
    }
  } else if (inherits(study_data, "tbl_df")) { # nocov start
    util_warning(
      c(
        "%s looks like a tibble. However, the package %s seems not to be",
        "available, which is quite strange.",
        "I cannot convert the tibble to a data.frame therefore.",
        "Tibbles do not always work like base R data.frames (see %s), so",
        "this can cause errors,",
        "because %s expects %s in base R data.frames, not in tibbles."
      ),
      dQuote("study_data"),
      dQuote("tibble"),
      dQuote("https://r4ds.had.co.nz/tibbles.html#tibbles-vs.data.frame"),
      dQuote("dataquieR"),
      dQuote("study_data"),
      applicability_problem = FALSE
    )
  } # nocov end


  if ((length(study_data) == 0) || !is.character(resp_vars)) {
    util_error(
      "%s should be missing or give variable names referring the study_data.",
      dQuote("resp_vars"), applicability_problem = TRUE)
  }

  if (!(all(resp_vars %in% colnames(study_data)))) {
    util_warning(c(
      "The following %s are missing from the %s.",
      "Won't return a type for them: %s"),
      dQuote("resp_vars"),
      dQuote("study_data"),
      sQuote(resp_vars[!(resp_vars %in% colnames(study_data))]),
      applicability_problem = TRUE
    )
  }

  factor_vars <-
    colnames(study_data)[vapply(study_data, is.factor, FUN.VALUE = logical(1))]

  factor_resp_vars <- intersect(factor_vars, resp_vars)

  valuelabels <- vapply(setNames(nm = resp_vars),
                        FUN.VALUE = character(1),
                  function(variable) {
      if (variable %in% factor_resp_vars) {
        fctr <- factor(levels = levels(study_data[[variable]]),
                       x = levels(study_data[[variable]]))

        paste(as.integer(fctr), "=", levels(fctr)[as.integer(fctr)],
              collapse = sprintf(" %s ", SPLIT_CHAR))
      } else {
        NA_character_
      }
  })

  modifiedstudydata <- study_data
  modifiedstudydata[, factor_resp_vars] <-
    lapply(study_data[, factor_resp_vars, FALSE], as.integer)

  return(list(
    VALUE_LABELS = valuelabels,
    ModifiedStudyData = modifiedstudydata
  ))
}
