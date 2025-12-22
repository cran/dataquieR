#' Get data types from data
#'
#' @param resp_vars [variable] names of the variables to fetch the data type
#'                             from the data
#' @param study_data [data.frame] the data frame that contains the measurements
#'                                Hint: Only data frames supported, no URL
#'                                or file names.
#' @param .dont_cast_off_cols [logical] internal use, only
#' @param guess_character [logical] guess a data type for character columns
#'                                  based on the values
#'
#' @return vector of data types
#' @export
#' @importFrom stats setNames
#' @seealso [prep_robust_guess_data_type()]
#'
#' @examples
#' \dontrun{
#' dataquieR::prep_datatype_from_data(cars)
#' }
prep_datatype_from_data <-
  function(resp_vars = colnames(study_data), study_data,
           .dont_cast_off_cols = FALSE,
           guess_character = getOption("dataquieR.guess_character",
                                       default =
                                         dataquieR.guess_character_default
           )) {
  util_expect_scalar(guess_character, check_type = is.logical)
  if (!missing(resp_vars) && is.data.frame(resp_vars) && missing(study_data)) {
    study_data <- resp_vars
    resp_vars <- colnames(study_data)
  } else if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame in the argument %s",
               dQuote("study_data"))
  }

  study_data <- util_cast_off(study_data, "study_data", .dont_cast_off_cols)

  if (ncol(study_data) == 0) {
    return(character(0))
  }

  if (!is.character(resp_vars)) {
    util_error(
      "%s should be missing or give variable names referring the study_data.",
      dQuote("resp_vars"), applicability_problem = TRUE)
  }

  if (!(all(resp_vars %in% colnames(study_data)))) {
    util_message(c(
      "The following %s are missing from the %s.",
      "Won't return a type for them: %s"),
      dQuote("resp_vars"),
      dQuote("study_data"),
      sQuote(resp_vars[!(resp_vars %in% colnames(study_data))]),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  types <- vapply(setNames(nm = resp_vars), FUN.VALUE = character(1),
                  function(variable) {
    if (variable %in% colnames(study_data)) {
      r <- prep_dq_data_type_of(study_data[[variable]],
                                guess_character = guess_character)
      if (length(r) == 0) {
        r <- DATA_TYPES$STRING
      }
      r
    } else {
      NA_character_
    }
  })
  types
}
