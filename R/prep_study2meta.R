#' Guess a meta data frame from study data.
#'
#' Guess a minimum meta data frame from study data. Minimum required variable
#' attributes are:
#'
#' ```{r}
#' dataquieR:::util_get_var_att_names_of_level(VARATT_REQUIRE_LEVELS$REQUIRED)
#' ````
#'
#' The function also tries to detect missing codes.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param level [enum] level to provide (see also [VARATT_REQUIRE_LEVELS])
#'
#' @return a meta_data data frame
#' @export
#' @importFrom stats setNames
#'
prep_study2meta <- function(study_data, level = c(
                              VARATT_REQUIRE_LEVELS$REQUIRED,
                              VARATT_REQUIRE_LEVELS$OPTIONAL
                            )) {
  if (missing(study_data) || !is.data.frame(study_data)) {
    util_error("Need study data as a data frame")
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    if (tibble::is_tibble(study_data)) {
      study_data <- as.data.frame(study_data)
    }
  } else if (inherits(study_data, "tbl_df")) {
    util_warning(
      paste(
        "%s looks like a tibble. However, the package %s seems not to be",
        "available, which is quite strange.",
        "I cannot convert the tibble to a data.frame therefore.",
        "Tibbles do not always work like base R data.frames (see %s), so this",
        "can cause errors,",
        "because %s expects %s in base R data.frames, not in tibbles."
      ),
      dQuote("study_data"),
      dQuote("tibble"),
      dQuote("https://r4ds.had.co.nz/tibbles.html#tibbles-vs.data.frame"),
      dQuote("dataquieR"),
      dQuote("study_data")
    )
  }

  util_get_var_att_names_of_level(VARATT_REQUIRE_LEVELS$REQUIRED)

  var_names <- colnames(study_data)

  if (length(var_names) == 0) {
    util_error("No study variables found -- cannot proceed.")
  }

  datatypes <- prep_datatype_from_data(resp_vars = var_names, study_data =
                                         study_data)

  missing_list <-
    mapply(function(x, dt) {
      if (dt %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)) {
        paste(unique(
          sort(suppressWarnings(as.numeric(x[util_looks_like_missing(x)])))),
          collapse = SPLIT_CHAR)
      } else {
        ""
      }
    }, study_data, datatypes)

  res <- prep_create_meta(
    VAR_NAMES = var_names,
    DATA_TYPE = datatypes,
    MISSING_LIST = missing_list
  )


  generated_atts <- util_get_var_att_names_of_level(level)
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

  if (!all(generated_atts %in% colnames(res))) {
    util_error(
      c("Internal error. The function prep_study2meta should return a mininum",
      "meta data frame, but the attributes %s are missing."),
      paste0(dQuote(
        generated_atts[!(generated_atts %in% colnames(res))]
      ), collapse = ", ")
    )
  }

  res
}
