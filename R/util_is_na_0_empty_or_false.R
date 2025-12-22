#' Detect falsish values
#'
#' @param x a value/vector of values
#'
#' @return vector of logical values:
#'    `TRUE`, wherever x is somehow empty
#'
#' @family missing_functions
#' @concept robustness
#' @noRd
util_is_na_0_empty_or_false <- function(x) { # TODO: generic?
  cs <- paste0("util_is_na_0_empty_or_false.", rlang::hash(x))
  if (exists(cs, envir = .study_data_cache)) { # TODO: maybe RAM intensive?
    return(get(cs, envir = .study_data_cache))
  }
  if (inherits(x, "hms")) { # maybe more general: !is.vector(x)
    x <- util_as_character(x)
  }
  y <- x
  class(y) <- "logical"
  attributes(y) <- NULL
  y[] <- FALSE
  y[is.na(x)] <- TRUE
  y[(trimws(x) %in% c("false", "FALSE", "F", "f", "-", ""))] <- TRUE
  idx <- !suppressWarnings(as.numeric(x))
  idx[is.na(idx)] <- FALSE
  y[idx] <- TRUE
  y <- as.logical(y)
  assign(cs, y, envir = .study_data_cache)
  return(y)
}

