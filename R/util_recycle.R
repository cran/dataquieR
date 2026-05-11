#' Recycle vectors to a common size
#'
#' Recycles multiple vectors to a common target size, similar in spirit to
#' `vctrs::vec_recycle_common()`, but limited to plain vector-like inputs.
#'
#' Recycling is only allowed if each input has length `1` or if its length
#' divides the target size without remainder.
#'
#' @param ... Vectors to recycle.
#' @param .size Optional target size. If omitted, the maximum input length is
#'   used.
#'
#' @return A list containing the recycled inputs in the original order.
#'
#' @noRd
util_recycle <- function(..., .size = NULL) {
  xs <- list(...)

  if (!length(xs)) {
    return(list())
  }

  is_vector_like <- vapply(
    xs,
    function(x) is.atomic(x) || is.list(x) || is.expression(x),
    logical(1L)
  )

  if (!all(is_vector_like)) {
    bad <- which(!is_vector_like)[1L]
    util_error(
      "Argument %s is of unsupported class %s",
      bad,
      sQuote(paste(class(xs[[bad]]), collapse = ", "))
    )
  }

  lens <- lengths(xs)

  if (is.null(.size)) {
    .size <- max(lens)
  } else {
    if (!is.numeric(.size) || length(.size) != 1L || is.na(.size) ||
        .size < 0 || .size != as.integer(.size)) {
      util_error(
        "Argument %s must be a single non-negative integer",
        sQuote(".size")
      )
    }
    .size <- as.integer(.size)
  }

  bad_len <- which(lens != 1L & lens != 0L & (.size %% lens != 0L))
  if (length(bad_len)) {
    i <- bad_len[1L]
    util_error(
      "Can't recycle argument %s of length %s to size %s",
      i,
      lens[[i]],
      .size
    )
  }

  lapply(xs, function(x) rep(x, length.out = .size))
}
