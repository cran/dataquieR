#' Data frame leaves `haven`
#'
#' if `df` is/contains a `haven` `labelled` or `tibble` object, convert it to
#' a base R data frame
#'
#' @param df [data.frame] may have or contain non-standard classes
#' @param symb [character] name of the data frame for error messages
#' @param .dont_cast_off_cols [logical] internal use, only.
#'
#' @return [data.frame] having all known special things removed
#'
#' @keywords internal
util_cast_off <- function(df, symb, .dont_cast_off_cols = FALSE) {
  if (missing(symb)) {
    symb <- as.character(substitute(df))
  }
  orig <- df
  if (requireNamespace("tibble", quietly = TRUE)) { # TODO: data.table, what else, is missing
    if (tibble::is_tibble(df)) {
      df <- as.data.frame(df)
    }
  } else { # nocov start
    if (inherits(df, "tbl_df")) {
      util_warning(
        paste(
          "%s looks like a tibble. However, the package %s seems not",
          "to be available, which is quite strange.",
          "I cannot convert the tibble to a data.frame therefore.",
          "Tibbles do not always work like base R data.frames (see %s),",
          "so this can cause errors,",
          "because %s expects %s in base R data.frames, not in tibbles."
        ),
        dQuote(symb),
        dQuote("tibble"),
        dQuote("https://r4ds.had.co.nz/tibbles.html#tibbles-vs.data.frame"),
        dQuote("dataquieR"),
        dQuote(symb),
        applicability_problem = FALSE
      )
    }
  } # nocov end
  # drop all stuff like haven labels, labelled labels
  # df[] <- lapply(df, function(cl) {
  #   mostattributes(cl) <- NULL
  #   cl
  #   # if (is.factor(cl)) {
  #   #   mostattributes(cl) <- NULL
  #   #   cl
  #   # } else if (lubridate::is.timepoint(cl)) {
  #   #   # lubridate::as_datetime(cl)
  #   #   mostattributes(cl) <- NULL
  #   #   cl
  #   # } else {
  #   #   mostattributes(cl) <- NULL
  #   #   cl
  #   # }
  # })


  if (!.dont_cast_off_cols && !!ncol(df)) {
    li <- as.list(df)

    dtypes <- prep_datatype_from_data(colnames(df), df,
                                      .dont_cast_off_cols = TRUE)

    li <- mapply(cl = li, dt = dtypes, FUN = function(cl, dt) {
      if (is.factor(cl)) {
        mostattributes(cl) <- NULL
        return(cl)
      } else {
        return(util_data_type_conversion(cl, type = dt))
      }
    }, SIMPLIFY = FALSE)

    df <- do.call(data.frame,
                  c(list(
                        check.names = FALSE,
                        stringsAsFactors = FALSE
                      ), li)
    )

  }

  known_atts <- .ds1_attribute_names

  for (att in known_atts) attr(df, att) <- attr(orig, att)

  df
}
