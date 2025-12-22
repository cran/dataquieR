#' Create a caption from an alias name of a `dq_report2` result
#'
#' @param alias alias name
#' @param long return result based on `menu_title_report`,
#'             `matrix_column_title_report` otherwise
#'
#' @return caption
#'
#' @seealso `util_html_table`
#' @family reporting_functions
#' @concept html
#' @noRd
util_alias2caption <- function(alias, long = FALSE) {

  util_expect_scalar(long, check_type = is.logical)

  if (length(alias) != 1 ||
      !is.character(alias) ||
      is.na(alias)) {
    return("Unkown Alias")
  }

  fname <- util_map_by_largest_prefix(
    alias,
    haystack = names(.manual$titles))

  if (is.na(fname)) {
    fname <- alias
  }

  if (long) {
    ftitle <-
      util_map_labels(fname,
                      util_get_concept_info("implementations"),
                      to = "menu_title_report",
                      from = "function_R",
                      ifnotfound = NA_character_)
    if (util_empty(ftitle)) {
      ftitle <-
        util_map_labels(fname,
                        util_get_concept_info("implementations"),
                        to = "Implementationform",
                        from = "function_R",
                        ifnotfound = NA_character_)
    }
  } else {
    ftitle <-
      util_map_labels(fname,
                      util_get_concept_info("implementations"),
                      to = "matrix_column_title_report",
                      from = "function_R",
                      ifnotfound = NA_character_)
    if (util_empty(ftitle)) {
      ftitle <-
        util_map_labels(fname,
                        util_get_concept_info("implementations"),
                        to = "dq_report2_short_title",
                        from = "function_R",
                        ifnotfound = NA_character_)
    }
  }

  if (is.na(ftitle)) {
    ftitle <-
      r <- .manual$titles[[fname]];
    if (length(r) != 1 || is.na(r)) ftitle <- r <- alias;
  }

  if (startsWith(alias, fname)) {
    suffix <- substr(alias, nchar(fname) + 1 + 1, nchar(alias)) # name + "_" (first +1), start is the next character (second +1)
  } else {
    suffix <- alias
  }

  # acronyms <-
  #   util_map_labels(fnames,
  #                   util_get_concept_info("implementations"),
  #                   to = "dq_report2_short_title",
  #                   from = "function_R",
  #                   ifnotfound = util_abbreviate(fnames))

  suffix <- gsub("_", " ", suffix)
  if (!util_empty(suffix))
    suffix <- paste0(" ", suffix)

  r  <- paste0(ftitle, suffix)

  names(r) <- alias

  r

}
