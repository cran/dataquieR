#' Normalize paths, even if the tail does not exist yet
#'
#' Similar to [base::normalizePath()], but also resolves paths where a suffix
#' of the path does not yet exist. The longest existing prefix is normalized via
#' [base::normalizePath()], while the remaining non-existing tail is normalized
#' syntactically by resolving `.` and `..`.
#'
#' @param path A character vector of paths.
#' @param winslash The separator to use on Windows, passed through to
#'   [base::normalizePath()].
#' @param mustWork Logical scalar. If `TRUE`, an error is thrown when the final
#'   path does not exist. If `NA`, a warning is emitted. If `FALSE`, no error is
#'   thrown.
#'
#' @return A character vector of normalized paths.
#'
#' @noRd
util_normalize_path <- function(path, winslash = "/", mustWork = FALSE) {
  if (!is.character(path)) {
    util_error("`path` must be a character vector.")
  }
  if (length(winslash) != 1L || is.na(winslash)) {
    util_error("`winslash` must be a non-missing character scalar.")
  }
  if (length(mustWork) != 1L || !is.logical(mustWork)) {
    util_error("`mustWork` must be a logical scalar.")
  }

  is_abs_path <- function(x) {
    grepl("^(/|~|[A-Za-z]:[/\\\\]|[/\\\\]{2})", x)
  }

  path_dirname <- function(x) {
    x2 <- gsub("[/\\\\]+$", "", x)
    if (!nzchar(x2)) {
      return(x)
    }
    dirname(x2)
  }

  split_path_components <- function(x) {
    parts <- strsplit(x, "[/\\\\]+", perl = TRUE)[[1L]]
    parts[nzchar(parts)]
  }

  normalize_tail <- function(x) {
    parts <- split_path_components(x)
    if (!length(parts)) {
      return("")
    }

    stack <- character()

    for (part in parts) {
      if (identical(part, ".") || !nzchar(part)) {
        next
      }
      if (identical(part, "..")) {
        if (length(stack)) {
          stack <- stack[-length(stack)]
        }
        next
      }
      stack <- c(stack, part)
    }

    paste(stack, collapse = "/")
  }

  normalize_one <- function(p) {
    if (is.na(p)) {
      return(base::normalizePath(
        path = p,
        winslash = winslash,
        mustWork = mustWork
      ))
    }

    p_exp <- path.expand(p)
    p_abs <- if (is_abs_path(p_exp)) p_exp else file.path(getwd(), p_exp)

    p_abs <- gsub("[/\\\\]+", .Platform$file.sep, p_abs)

    if (file.exists(p_abs)) {
      return(base::normalizePath(
        path = p_abs,
        winslash = winslash,
        mustWork = mustWork
      ))
    }

    prefix <- p_abs
    repeat {
      parent <- path_dirname(prefix)
      if (file.exists(prefix) || identical(parent, prefix)) {
        break
      }
      prefix <- parent
    }

    prefix_norm <- base::normalizePath(
      path = prefix,
      winslash = winslash,
      mustWork = FALSE
    )

    prefix_cmp <- gsub("[/\\\\]+", "/", prefix)
    path_cmp <- gsub("[/\\\\]+", "/", p_abs)

    rest <- substr(path_cmp, nchar(prefix_cmp) + 1L, nchar(path_cmp))
    rest <- sub("^[/\\\\]+", "", rest)
    rest <- normalize_tail(rest)

    out <- if (nzchar(rest)) {
      file.path(prefix_norm, rest)
    } else {
      prefix_norm
    }

    if (.Platform$OS.type == "windows") {
      if (identical(winslash, "/")) {
        out <- gsub("\\\\", "/", out)
      } else if (identical(winslash, "\\")) {
        out <- gsub("/", "\\\\", out)
      }
    }

    exists_out <- file.exists(out)

    if (isTRUE(mustWork) && !exists_out) {
      util_error("path[%d]=\"%s\": No such file or directory", 1L, p)
    }
    if (is.na(mustWork) && !exists_out) {
      warning(
        sprintf("path[%d]=\"%s\": No such file or directory", 1L, p),
        call. = FALSE
      )
    }

    out
  }

  out <- vapply(path, normalize_one, character(1L), USE.NAMES = FALSE)

  names(out) <- names(path)
  out
}
