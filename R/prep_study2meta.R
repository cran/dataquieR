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
#' @param convert_factors [logical] convert factor columns to coded integers.
#'                                  if selected, then also the study data will
#'                                  be updated and returned.
#' @param guess_missing_codes [logical] try to guess missing codes from the data
#'
#' @return a meta_data data frame or a list with study data and metadata, if
#'         `convert_factors == TRUE`.
#' @export
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' dataquieR::prep_study2meta(Orange, convert_factors = FALSE)
#' }
prep_study2meta <- function(study_data, level = c(
                              VARATT_REQUIRE_LEVELS$REQUIRED,
                              VARATT_REQUIRE_LEVELS$RECOMMENDED
                            ),
                            cumulative = TRUE,
                            convert_factors = FALSE,
                            guess_missing_codes =
                              getOption("dataquieR.guess_missing_codes",
                                        dataquieR.guess_missing_codes_default)) {
  withr::local_options(list(dataquieR.fix_column_type_on_read = TRUE))
  with_dataframe_environment(
    util_expect_data_frame(study_data, keep_types = TRUE) # prep_robust_guess_data_type
  )
  if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame")
  }

  if (length(convert_factors) != 1 || !is.logical(convert_factors)) {
    util_error("Argument %s must be logical(1)", dQuote("convert_factors"))
  }

  util_expect_scalar(guess_missing_codes, check_type = is.logical,
                     error_message = sprintf("%s needs to be one logical value",
                                             sQuote("guess_missing_codes")))

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
    mapply(function(x, dt, cn) {
      if (dt %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        mcs <-
          unique(sort(suppressWarnings(as.numeric(x[
            util_looks_like_missing(x)]))))
        r <- paste(mcs, collapse = SPLIT_CHAR)
        if (length(mcs) > 0)
          util_message(
            "For %s, maybe, the following values are missing codes: %s",
            sQuote(cn),
            util_pretty_vector_string(mcs, n_max = 5)
          )
      } else {
        r <- ""
      }
      if (length(r) == 1 && util_empty(r))
        r <- SPLIT_CHAR # missing on purpose
      r
    }, study_data, datatypes, colnames(study_data))

  if (!guess_missing_codes) {
    missing_list <- SPLIT_CHAR
  }

  if (convert_factors) { # TODO: work with VALUE_LABEL_TABLE
    valuelabels <- prep_valuelabels_from_data(resp_vars = var_names,
                                              study_data = study_data)
  } else {
    valuelabels <- list()
    valuelabels[[VALUE_LABELS]] <- vapply(
      var_names,
      FUN.VALUE = character(1),
      function(v) {
        if (is.factor(study_data[[v]])) {
          lvs <- levels(study_data[[v]])
          if (is.ordered(study_data[[v]])) {
            split_char <- "<"
          } else {
            split_char <- SPLIT_CHAR
          }
          paste(lvs, collapse = sprintf(" %s ", split_char))
          # lvs[as.integer(study_data[[v]])]
        } else {
          NA_character_
        }
      }
    )
  }

  res <- prep_create_meta(
    VAR_NAMES = var_names,
    LABEL = var_labels,
    LONG_LABEL = var_labels,
    DATA_TYPE = datatypes,
    VALUE_LABELS = valuelabels[[VALUE_LABELS]],
    MISSING_LIST = missing_list,
    HARD_LIMITS = NA_character_,
    JUMP_LIST = NA_character_
  )

  generated_atts <- util_get_var_att_names_of_level(level,
                                                    cumulative = cumulative)

  if (SCALE_LEVEL %in% generated_atts) {
    .md <- res
    .md[[JUMP_LIST]] <- SPLIT_CHAR
    with_scale_level <-
      prep_scalelevel_from_data_and_metadata(#resp_vars = var_names,
                                             study_data = study_data,
                                             meta_data = .md,
                                             label_col = VAR_NAMES)

      res[[SCALE_LEVEL]] <- setNames(
        with_scale_level[[SCALE_LEVEL]], nm = with_scale_level[[VAR_NAMES]])[
          res[[VAR_NAMES]]]
  }

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
