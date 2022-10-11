#' Support function to create [data.frame]s of metadata
#'
#' @description
#' Create a meta data frame and map names.
#' Generally, this function only creates a [data.frame], but using
#' this constructor instead of calling
#' `data.frame(..., stringsAsFactors = FALSE)`, it becomes possible, to adapt
#' the metadata [data.frame] in later developments, e.g. if we decide to use
#' classes for the metadata, or if certain standard names of variable attributes
#' change. Also, a validity check is possible to implement here.
#'
#' @details
#' For now, this calls [data.frame], but it already renames variable attributes,
#' if they have a different name assigned in [WELL_KNOWN_META_VARIABLE_NAMES],
#' e.g. `WELL_KNOWN_META_VARIABLE_NAMES$RECODE` maps to `recode` in lower case.
#'
#' NB: `dataquieR` exports all names from WELL_KNOWN_META_VARIABLE_NAME as
#' symbols, so `RECODE` also contains `"recode"`.
#'
#' @param ... named column vectors, names will be mapped using
#'            [WELL_KNOWN_META_VARIABLE_NAMES],
#'            if included in [WELL_KNOWN_META_VARIABLE_NAMES]
#'            can also be a data frame, then its column names will be mapped
#'            using [WELL_KNOWN_META_VARIABLE_NAMES]
#' @param stringsAsFactors [logical] if the argument is a list of vectors, a
#'                         data frame will be
#'                         created. In this case, `stringsAsFactors` controls,
#'                         whether characters will be auto-converted to Factors,
#'                         which defaults here always to false independent from
#'                         the [default.stringsAsFactors].
#' @param level [enum] level of requirement (see also [VARATT_REQUIRE_LEVELS])
#'                     set to `NULL`, if not a complete metadata frame is
#'                     created.
#' @param character.only [logical] a logical indicating whether level can be
#'                                 assumed to be character strings.
#'
#' @seealso [WELL_KNOWN_META_VARIABLE_NAMES]
#' @return a data frame with:
#'   - meta data attribute names mapped and
#'   - meta data checked using [prep_check_meta_names] and do some more
#'     verification about conventions, such as check for valid intervals
#'     in limits)
#' @export
#' @importFrom stats setNames
prep_create_meta <- function(..., stringsAsFactors = FALSE,
                             level, character.only = FALSE) {
  if (length(list(...)) == 1 && is.data.frame(list(...)[[1]])) {
    metas <- list(...)[[1]]
    cn <- colnames(metas)
  } else {
    args <- list(...)
    if (any(duplicated(names(args)))) {
      util_error("Found duplicated meta columns: %s",
                 paste0(dQuote(unique(names(args)[duplicated(names(args))])),
                        collapse = ", "),
                 applicability_problem = TRUE)
    }

    args_lens <- vapply(args, length, integer(1))

    null_args <- args_lens == 0

    if (any(null_args)) {
      util_warning(c("The following variable attributes are",
                   "NULL, will ignore these: %s"),
                   paste0(dQuote(names(null_args[null_args])),
                          collapse = ", "),
                   applicability_problem = TRUE)
      args <- args[!null_args]
      args_lens <- vapply(args, length, integer(1))
    }

    max_len <- max(args_lens)

    if (any(max_len %% args_lens != 0)) {
      util_error(c("The given variable attributes have different lengths and",
                   "cannot be equalized by repeating the shorter ones to",
                   "create a data frame with as %d rows (the longest",
                   "variable attribute vector provided). R would say:",
                   "arguments imply differing number of rows: %s"),
                   max_len,
                   paste0(sort(unique(args_lens)), collapse = ", "))
    }

    metas <- do.call(data.frame, c(args, stringsAsFactors = stringsAsFactors))
    cn <- names(args)
  }
  if (ncol(metas) > 0) {
    cn <-  mget(
      x = cn,
      envir = as.environment(WELL_KNOWN_META_VARIABLE_NAMES),
      mode = "character",
      ifnotfound = setNames(nm = colnames(metas)),
      inherits = FALSE
    )
    colnames(metas) <- cn
  }
  if (missing(level)) {
    level <- VARATT_REQUIRE_LEVELS$REQUIRED
  } else {
    if (character.only) {
      level <- as.character(level)
    } else {
      level <- as.character(substitute(expr = level))
    }
    if (length(level) > 0) {
      level <- try(match.arg(level, choices = c(names(VARATT_REQUIRE_LEVELS),
                                                unlist(VARATT_REQUIRE_LEVELS)),
                             several.ok = FALSE), silent = TRUE)
      if (inherits(level, "try-error")) {
        util_error(
          "Error regarding argument %s: %s",
          dQuote("level"),
          conditionMessage(attr(level, "condition")),
          applicability_problem = TRUE
        )
      }
    }
  }
  # level <- VARATT_REQUIRE_LEVELS[[level]]

  prep_check_meta_names(metas, level = level, character.only = TRUE)
  util_validate_known_meta(metas)
  return(metas)
}
