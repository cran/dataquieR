#' Return hex code colors from color names or `STATAReporter` syntax
#'
#' @param colors the colors, e.g.,"255 0 0" or "red" or "#ff0000"
#'
#' @return character vector with colors using `HTML` hexadecimal encoding, e..g,
#'         "#ff0000" for "red"
#' @keywords internal
util_col2rgb <- function(colors) {
  with_space <- grepl(" ", colors, fixed = TRUE)
  res <- colors
  rgbmatrix <- try(col2rgb(colors[!with_space], alpha = TRUE), silent = TRUE)
  if (inherits(rgbmatrix, "try-error")) {
    is_color <- vapply(FUN.VALUE = logical(1), colors[!with_space],
                          function(cl) {
                                  !inherits(try(col2rgb(cl, alpha = TRUE),
                                                silent = TRUE), "try-error")
                          })
    util_warning("No known colors: %s, replaced by black.",
                 util_pretty_vector_string(colors[!with_space][!is_color]),
                 applicability_problem = TRUE)
    colors[!with_space][!is_color] <- "black"
    rgbmatrix <- try(col2rgb(colors[!with_space], alpha = TRUE), silent = TRUE)
    util_stop_if_not(
      "Internal error 0x34556345345. Should not happen after cleaning above." =
        !inherits(rgbmatrix, "try-error"))
  }
  res[!with_space] <- apply(rgbmatrix, 2, function(cl) {
    paste0("#", paste0(vapply(cl, sprintf, fmt = "%02x",
                              FUN.VALUE = character(1)), collapse = ""))
  })
  ws <- suppressWarnings(lapply(strsplit(res[with_space],
                                                      "\\s"), as.numeric))
  invalid <- !vapply(ws, length, FUN.VALUE = integer(1)) %in% c(3, 4) |
      vapply(ws, function(triple) any(is.na(triple)),
                 FUN.VALUE = logical(1))
  if (any(invalid)) {
    util_warning("No known colors: %s, replaced by black.",
                 util_pretty_vector_string(colors[with_space][invalid]),
                 applicability_problem = TRUE)
    ws[invalid] <- list(c(0, 0, 0))
  }
  ws <- lapply(ws, sprintf, fmt = "%02x")
  ws <- vapply(ws, paste0, collapse = "",
                            FUN.VALUE = character(1))
  ws <- paste0("#", ws)
  res[with_space] <- ws
  unlist(res)
}
