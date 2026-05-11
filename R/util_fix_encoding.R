#' Fix encoding errors
#'
#' @param x [character] a vector, possibly with encoding errors
#'
#' @returns `x`, but with all invalidly encoded characters deleted
#' @noRd
util_fix_encoding <- function(x) {
  orig <- x
  from <- Encoding(x)
  from <- setdiff(from, "unknown")
  from <- head(names(which.max(table(from))), 1)
  if (length(from) != 1) {
    from <- "UTF-8"
  }
  x <- iconv(x, from, "UTF-8", sub = '')
  Encoding(x) <- "UTF-8"
  if (any((is.na(x) != is.na(orig)) | x != orig, na.rm = TRUE)) {
    in_pipe <- FALSE # careful, runs early
    tmp <- try(.called_in_pipeline2(), silent = TRUE)
    if (!util_is_try_error(tmp)) in_pipe <- isTRUE(tmp)
    if (!in_pipe && exists(".called_in_pipeline", .dq2_globs,
                           mode = "logical")) {
      in_pipe <- get(".called_in_pipeline", .dq2_globs, mode = "logical")
    }
    if (length(in_pipe) != 1 || is.na(in_pipe)) in_pipe <- FALSE
    if (!in_pipe)
      util_warning("Found encoding errors, deleted invalid characters")
  }
  return(x)
}

#' Fix encoding errors
#'
#' @param r [data.frame] possibly with encoding errors
#'
#' @returns `r`, but with all invalidly encoded characters deleted
#' @noRd
util_fix_encoding_cols <- function(r) {
  if (nrow(r) > 0) {
    chars <- vapply(r, is.character, FUN.VALUE = logical(1))
    if (any(chars))
      r[, chars] <-
        lapply(r[, chars, drop = FALSE], util_fix_encoding)
  }
  return(r)
}
