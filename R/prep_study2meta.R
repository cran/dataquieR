#' Guess a metadata data frame from study data.
#'
#' Guess a minimum metadata data frame from study data. Minimum required variable
#' attributes are:
#'
#' ```{r}
#' dataquieR:::util_get_var_att_names_of_level(VARATT_REQUIRE_LEVELS$REQUIRED)
#' ````
#'
#' The function also tries to detect missing codes.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param level [enum] levels to provide (see also [VARATT_REQUIRE_LEVELS])
#' @param cumulative [logical] include attributes of all levels up to level
#' @param convert_factors [logical] convert the
#'
#' @return a meta_data data frame
#' @export
#' @importFrom stats setNames
#'
prep_study2meta <- function(study_data, level = c(
                              VARATT_REQUIRE_LEVELS$REQUIRED,
                              VARATT_REQUIRE_LEVELS$RECOMMENDED
                            ),
                            cumulative = TRUE,
                            convert_factors = FALSE) {
  util_expect_data_frame(study_data)
  if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame")
  }

  if (length(convert_factors) != 1 || !is.logical(convert_factors)) {
    util_error("Argument %s must be logical(1)", dQuote("convert_factors"))
  }

  study_data <- util_cast_off(study_data, "study_data")

  util_get_var_att_names_of_level(VARATT_REQUIRE_LEVELS$REQUIRED)

  var_names <- colnames(study_data)
  var_labels <- var_names # TODO: maybe do something nicer here, e.g., SNAKE_CASE -> Snake Case, or so.

  if (length(var_names) == 0) {
    util_error("No study variables found -- cannot proceed.")
  }

  datatypes <- prep_datatype_from_data(resp_vars = var_names, study_data =
                                         study_data)

  missing_list <-
    mapply(function(x, dt) {
      if (dt %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        r <- paste(unique(
          sort(suppressWarnings(as.numeric(x[util_looks_like_missing(x)])))),
          collapse = SPLIT_CHAR)
      } else {
        r <- ""
      }
      if (length(r) == 1 && util_empty(r))
        r <- SPLIT_CHAR # missing on purpose
      r
    }, study_data, datatypes)

  if (convert_factors) {
    valuelabels <- prep_valuelabels_from_data(resp_vars = var_names,
                                              study_data = study_data)
  } else {
    valuelabels <- list()
    valuelabels[[VALUE_LABELS]] <- NA_character_
  }

  if (convert_factors) {
    res <- prep_create_meta(
      VAR_NAMES = var_names,
      LABEL = var_labels,
      LONG_LABEL = var_labels,
      DATA_TYPE = datatypes,
      VALUE_LABELS = valuelabels[[VALUE_LABELS]],
      MISSING_LIST = missing_list,
      JUMP_LIST = NA_character_
    )
  } else {
    res <- prep_create_meta(
      VAR_NAMES = var_names,
      LABEL = var_labels,
      LONG_LABEL = var_labels,
      DATA_TYPE = datatypes,
      MISSING_LIST = missing_list,
      JUMP_LIST = NA_character_
    )
  }

  generated_atts <- util_get_var_att_names_of_level(level,
                                                    cumulative = cumulative)
  missing_atts <- setdiff(generated_atts, colnames(res))
  if (length(missing_atts) > 0) {
    empty_cols <-
      do.call(data.frame, c(
        list(stringsAsFactors = FALSE),
        lapply(setNames(nm = missing_atts), function(x) {
          rep(NA_character_, nrow(res))
        })
      ))

    res <- cbind.data.frame(res, empty_cols)
  }

  if (MISSING_LIST_TABLE %in% generated_atts)
    generated_atts <- union(generated_atts, c(MISSING_LIST, JUMP_LIST))

  missing_atts <- setdiff(generated_atts, colnames(res))

  res <- res[, intersect(generated_atts, colnames(res))]

  if (length(missing_atts)) {
    util_error(
      c("Internal error. The function prep_study2meta should return a minimum",
      "metadata data frame, but the attributes %s are missing."),
      paste0(dQuote(
        missing_atts
      ), collapse = ", ")
    )
  }

  if (convert_factors) {
    res <- list(
      MetaData = res,
      ModifiedStudyData = valuelabels[["ModifiedStudyData"]]
    )
  }

  res
}
