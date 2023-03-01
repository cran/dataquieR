#' Map based on largest common prefix
#'
#' @param needle [character]`(1)` item to search
#' @param haystack [character] items to find the entry sharing the largest
#'                             prefix with `needle`
#' @param split_char [character]`(1)` to split entries to atomic words (like
#' letters, if "" or `snake_elements`, if "_")
#'
#' @return [character]`(1)` with the fitting function name or [NA_character_]
#' @examples
#' \dontrun{ # internal function
#' util_map_by_largest_prefix(
#'   "acc_distributions_loc_ecdf_observer_time",
#'   names(dataquieR:::.manual$titles)
#' )
#' util_map_by_largest_prefix(
#'   "acc_distributions_loc_observer_time",
#'   names(dataquieR:::.manual$titles)
#' )
#' util_map_by_largest_prefix(
#'   "acc_distributions_loc_ecdf",
#'   names(dataquieR:::.manual$titles)
#' )
#' util_map_by_largest_prefix(
#'   "acc_distributions_loc",
#'   names(dataquieR:::.manual$titles)
#' )
#' }
util_map_by_largest_prefix <- function(
    needle,
    haystack,
    split_char = "_"
    ) {
  util_expect_scalar(needle, check_type = is.character)
  util_expect_scalar(haystack,
                     check_type = is.character,
                     allow_more_than_one = TRUE)
  if (is.null(names(haystack))) {
    haystack <- setNames(nm = haystack)
  }
  needle0 <- needle
  needle <- strsplit(needle, split_char, fixed = TRUE)[[1]]
  haystack <- strsplit(haystack, split_char, fixed = TRUE)
  lneedle <- length(needle)

  found <- FALSE
  i <- length(needle)
  while (!found) {
    needle <- needle[seq_len(i)]
    found <- any(vapply(haystack, FUN.VALUE = logical(1), function(straw) {
      length(straw) == i && all(straw == needle)
    }))
    i <- i - 1
    if (i < 0) {
      return(NA_character_)
    }
  }
  paste0(needle, collapse = split_char)
}
