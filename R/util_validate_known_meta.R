#' Utility function verifying syntax of known metadata columns
#'
#' This function goes through metadata columns, `dataquieR` supports
#' and verifies for these, that they follow its metadata conventions.
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @return invisible(NULL)
util_validate_known_meta <- function(meta_data) {
  if (!VAR_NAMES %in% colnames(meta_data)) { # avoid errors in checks, if
    # not a complete metadata frame is being checked.
    meta_data[[VAR_NAMES]] <- paste0("v", seq_len(nrow(meta_data)))
  }
  env <- new.env(environment())
  if (any(grepl("^(HARD|SOFT|DETECTION)_LIMITS$", colnames(meta_data)))) {
    env$error <- character(0)
    suppressWarnings(
      l <- withCallingHandlers(
        util_interpret_limits(mdata = meta_data),
        warning = function(w) env$error <- c(env$error, conditionMessage(w)),
        error = function(e) env$error <- c(env$error, conditionMessage(e))
      )
    )
    newlimcols <- grep("INCL_([^_]*)_LIMIT_(UP|LOW)", colnames(l), value = TRUE)
    newlimcols <- gsub("^INCL_", "", newlimcols)
    newlimcols <- unique(gsub("_LIMIT_(LOW|UP)$", "", newlimcols))
    for (limc in newlimcols) {
      wrong_interval_order <- l[, sprintf("%s_LIMIT_LOW", limc), FALSE] >
        l[, sprintf("%s_LIMIT_UP", limc), FALSE]
      wrong_interval_order[is.na(wrong_interval_order)] <- FALSE
      if (any(wrong_interval_order)) {
        env$error <- c(env$error,
                   sprintf("Found %s with lower limit > upper limit: %s",
                           limc,
                           unique(unlist(l[wrong_interval_order,
                                    sprintf("%s_LIMITS", limc),
                                    TRUE]))))
      }
    }
    env$error <- unique(env$error)
    if (length(env$error) > 0) {
      util_warning("Invalid limits detected: %s",
                   paste0(env$error, collapse = "\n"),
                   applicability_problem = TRUE)
    }
  }
  if (VALUE_LABELS %in% colnames(meta_data)) {
    env$error <- character(0)
    for (vl in meta_data[[VALUE_LABELS]]) {
      if (!util_empty(vl)) {
        if (!grepl(SPLIT_CHAR, vl, fixed = TRUE)) {
          env$error <- c(env$error,
                     sprintf(
                       "Suspicious value labels (only 1 level) detected: %s",
                       dQuote(vl)))
        }
      }
    }
    env$error <- unique(env$error)
    if (length(env$error) > 0)
      util_warning(env$error, applicability_problem = TRUE)
  }
  if (any(c(MISSING_LIST, JUMP_LIST) %in% colnames(meta_data))) {
    env$error <- character(0)
    for (l in intersect(c(MISSING_LIST, JUMP_LIST), colnames(meta_data))) {
      vl <- meta_data[[l]]
      notnum <- !grepl(sprintf(
        "^[%s0-9\\s\\-\\.]*$",
        SPLIT_CHAR), perl = TRUE, vl)
      if (any(!util_empty(vl) & notnum)) {
        env$error <- c(env$error,
                   sprintf(
                     "Suspicious %s: not numeric: %s",
                     sQuote(l),
                     paste(dQuote(unique(sort(
                       as.character(vl[!util_empty(vl) & notnum])))),
                       collapse = ", ")))
      } else {
        vlc <- as.character(vl)
        if (any(is.na(vlc) != is.na(vl))) { # nocov start
          util_warning(c("This should never be displayed,",
                         "so you've found a bug in dataquieR.",
                         "Please report it as bug in",
                         "%s to %s"),
                       dQuote("util_validate_known_meta"),
                       dQuote(utils::packageDescription("dataquieR")[["BugReports"]]),
                       applicability_problem = FALSE)
          env$error <- c(env$error,
            sprintf("Found missing or jump code not being a character: %s",
                       dQuote(
                         deparse(head(vl[is.na(vlc) != is.na(vl)], 1),
                                 nlines = 1)))
          )
        } # nocov end
        vl <- vlc
        lists <- strsplit(vl,
                 split = sprintf("\\s*[%s]\\s*", SPLIT_CHAR),
                 perl = TRUE,
                )
        dups <-
          vapply(lists, FUN.VALUE = logical(1), function(x) any(duplicated(x)))
        if (any(dups)) {
          dup_lists <- unique(unlist(
            lapply(lists[dups], paste, collapse = sprintf(" %s ", SPLIT_CHAR))))
          util_warning(
            "Duplicates in %s: %s. Maybe another missing code is not listed?",
                       dQuote(l),
                       paste0(sQuote(dup_lists), collapse = ", "),
            applicability_problem = TRUE)
        }
      }
    }
    env$error <- unique(env$error)
    if (length(env$error) > 0)
      util_warning(env$error, applicability_problem = TRUE)
  }
  invisible(NULL)
}
