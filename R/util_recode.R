#' Map a vector of values based on an assignment table
#'
#' @param values [vector] the vector
#' @param mapping_table [data.frame] a table with the mapping table
#' @param from [character] the name of the column with the "old values"
#' @param to [character] the name of the column with the "new values"
#' @param default [character] either one character or on character per value,
#'                            being used, if an entry from `values` is not
#'                            in the `from` column in `mapping_table
#'
#' @return the mapped values
#'
#' @seealso [dplyr::recode]
#'
#' @importFrom dplyr recode
#'
#' @family mapping
#' @concept process
#' @noRd
util_recode <- function(values, mapping_table, from, to, default = NULL) {
  util_expect_scalar(values,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     allow_na = TRUE)
  util_expect_scalar(from, check_type = is.character)
  util_expect_scalar(to, check_type = is.character)
  if (length(default) == 1) {
    util_expect_scalar(default,
                       allow_na = TRUE, check_type = is.character)
  } else {
    util_expect_scalar(default,
                       allow_na = TRUE,
                       allow_null = TRUE,
                       allow_more_than_one = TRUE,
                       min_length = length(values),
                       max_length = length(values))
  }
  util_expect_data_frame(mapping_table, c(from, to))
  mapping_list <- setNames(mapping_table[[to]], nm = mapping_table[[from]])
  mapping_list <- c(list(.x = values),
                    mapping_list,
                    list(.default = default))
  do.call(recode, mapping_list)
}
