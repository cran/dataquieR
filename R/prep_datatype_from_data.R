#' Get data types from data
#'
#' @param resp_vars [variable] names of the variables to fetch the data type
#'                             from the data
#' @param study_data [data.frame] the data frame that contains the measurements
#'
#' @return vector of data types
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' dataquieR::prep_datatype_from_data(cars)
prep_datatype_from_data <-
  function(resp_vars = colnames(study_data), study_data) {
  if (!missing(resp_vars) && is.data.frame(resp_vars) && missing(study_data)) {
    study_data <- resp_vars
    resp_vars <- colnames(study_data)
  } else if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame in the argument %s",
               dQuote("study_data"))
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    if (tibble::is_tibble(study_data)) {
      study_data <- as.data.frame(study_data)
    }
  } else if (inherits(study_data, "tbl_df")) {
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
  }


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

  types <- vapply(setNames(nm = resp_vars), FUN.VALUE = character(1),
                  function(variable) {
    if (variable %in% colnames(study_data)) {
      cl <- class(study_data[[variable]])[1]
      if (cl == "numeric") {
        if (all(util_is_integer(study_data[[variable]]))) {
          cl <- "integer"
        }
      }
      r <- DATA_TYPES_OF_R_TYPE[[cl]]
      if (length(r) == 0) {
        r <- DATA_TYPES$STRING
      }

      if (length(r) > 1) {
        r <- r[[1]] # nocov
        # this should never happen, since DATA_TYPES_OF_R_TYPE points to
        # scalars only
      }
      r
    } else {
      NA_character_
    }
  })
  types
}
