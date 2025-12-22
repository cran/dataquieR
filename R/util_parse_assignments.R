#' Utility function to parse assignments
#'
#' This function parses labels & level assignments in the format
#' `1 = male | 2 = female`. The function also handles `m = male | f = female`,
#' but this would not match the metadata concept. The split-character can
#' be given, if not the default from [SPLIT_CHAR] is to be used, but this
#' would also violate the metadata concept.
#'
#' @param text Text to be parsed
#' @param split_char Character separating assignments, may be a vector, then
#'                   all will be tried and the the most likely matching one will
#'                   be returned as attribute `split_char` of the result.
#' @param multi_variate_text don't paste text but parse element-wise
#' @param split_on_any_split_char split on any split `split_char`, if > 1 given.
#'
#' @return the parsed assignments as a named list
#'
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' md <- prep_get_data_frame("meta_data")
#' vl <- md$VALUE_LABELS
#' vl[[50]] <- "low<medium < high"
#' a <- util_parse_assignments(vl, split_char = c(SPLIT_CHAR, "<"),
#'   multi_variate_text = TRUE)
#' b <- util_parse_assignments(vl, split_char = c(SPLIT_CHAR, "<"),
#'   split_on_any_split_char = TRUE, multi_variate_text = TRUE)
#' is_ordered <- vapply(a, attr, "split_char", FUN.VALUE = character(1)) == "<"
#' md$VALUE_LABELS[[50]] <- "low<medium < high"
#' md$VALUE_LABELS[[51]] <- "1 = low< 2=medium < 3=high"
#' md$VALUE_LABELS[[49]] <- "2 = medium< 1=low < 3=high" # counter intuitive
#' with_sl <- prep_scalelevel_from_data_and_metadata(study_data = "study_data",
#'   meta_data = md)
#' View(with_sl[, union(SCALE_LEVEL, colnames(with_sl))])
#' }
#'
#' @family parser_functions
#' @concept metadata_management
#' @noRd
util_parse_assignments <- function(text, split_char = SPLIT_CHAR,
                                   multi_variate_text = FALSE, # Dont change default here, many calls of this function rely on a non-list-result
                                   split_on_any_split_char = FALSE
                                   ) {
  if (!multi_variate_text) {
    if (all(util_empty(text))) {
      text <- NA_character_
    } else {
      if (is.list(text)) {
        text <- unlist(text, recursive = TRUE, use.names = FALSE)
      }
      text <- paste0(text, collapse = "\n")
    }
  }
  use_regexp <- FALSE
  if (length(split_char) > 1) {
    if (!split_on_any_split_char) {
      r <- lapply(X = setNames(nm = split_char),
                  FUN = util_parse_assignments,
                  text = text,
                  multi_variate_text = TRUE)
      rl <- lapply(r, vapply, length, FUN.VALUE = integer(1))
      rl <- vapply(rl, identity, FUN.VALUE = integer(length(text)))
      if (is.null(dim(rl))) {
        rl <- matrix(rl,
                     ncol = length(split_char),
                     dimnames = list(as.character(seq_len(length(text))),
                                     names(rl)))
      }
      rlwm <- apply(rl, 1, which.max, simplify = FALSE)
      atts <- vapply(rlwm, names,
                     FUN.VALUE = character(1))
      res <- mapply(SIMPLIFY = FALSE,
                    wm = rlwm, i = seq_along(rlwm), function(wm, i) {
        util_attach_attr(r[[wm]][[i]], split_char = atts[[i]])
      })
      return(res)
    } else {
      use_regexp <- TRUE
    }
  }
  res <- lapply(text, function(x) {
    if (is.list(x)) {
      x <- paste(unlist(x, recursive = TRUE, use.names = FALSE),
                 collapse = "\n")
    }
    if (use_regexp) {
      split <- sprintf("[%s]", paste0(gsub("[", "\\[", fixed = TRUE,
                                           gsub("]", "\\]", fixed = TRUE,
                                                split_char)), collapse = ""))
      if (all(util_empty(gsub(split, "", perl = TRUE, x)))) {
        return(setNames(list(), nm = character(0)))
      }
      assignments <- base::strsplit(x = as.character(x),
                                    split = split, perl = TRUE)[[1]]
    } else {
      if (all(util_empty(gsub(split_char, "", fixed = TRUE, x)))) {
        return(setNames(list(), nm = character(0)))
      }
      assignments <- base::strsplit(x = as.character(x),
                                    split = split_char, fixed = TRUE)[[1]]
    }

    keys <- trimws(gsub(pattern = "(?ms)\\s*=\\s*.*$", replacement = "",
                        x = assignments, perl = TRUE))
    values <- trimws(gsub(pattern = "(?ms)^.*?\\s*=\\s*", replacement = "\\1",
                          x = assignments, perl = TRUE))
    as.list(setNames(values, keys))
  })
  res <- lapply(res,
                function(r) {
                  if (!length(r)) {
                    return(r)
                  } else if (
                    all(startsWith(as.character(unlist(r)), "'") &
                        endsWith(as.character(unlist(r)), "'"))) {
                    r <- substr(r, 2, nchar(r))
                    r <- substr(r, 1, nchar(r) - 1)
                  } else                   if (
                    all(startsWith(as.character(unlist(r)), '"') &
                        endsWith(as.character(unlist(r)), '"'))) {
                    r <- substr(r, 2, nchar(r))
                    r <- substr(r, 1, nchar(r) - 1)
                  }
                  r
                })
  if (getOption("dataquieR.VALUE_LABELS_htmlescaped",
                dataquieR.VALUE_LABELS_htmlescaped_default)) {
    util_ensure_suggested(
      "textutils",
      "use the option(dataquieR.VALUE_LABELS_htmlescaped = TRUE) ")
    res[] <- lapply(res, lapply, textutils::HTMLdecode)
  }
  if (!multi_variate_text) {
    if (length(res) != 1) { # nocov start
      util_error(c("univariate use of util_parse_assignments returned %d",
                   "results. Sorry, this should not happen, internal error."),
                 length(res))
    } # nocov end
    return(res[[1]])
  } else {
    return(res)
  }
}
