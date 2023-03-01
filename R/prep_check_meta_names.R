#' Checks the validity of meta data w.r.t. the provided column names
#'
#' @description
#' This function verifies, if a data frame complies to meta data conventions and
#' provides a given richness of meta information as specified by `level`.
#'
#' @details
#'
#' Note, that only the given level is checked despite, levels are somehow
#' hierarchical.
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param level [enum] level of requirement (see also [VARATT_REQUIRE_LEVELS]).
#'                     set to `NULL` to deactivate the check of richness.
#' @param character.only [logical] a logical indicating whether level can be
#'                                 assumed to be character strings.
#'
#' @return a logical with:
#'   - invisible(TRUE). In case of problems with the meta data, a condition is
#'      raised (`stop()`).
#' @export
#'
#' @examples
#' \dontrun{
#' prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
#'                       MISSING_LIST = 3))
#'
#' prep_check_meta_names(
#'   data.frame(
#'     VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
#'     LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
#'     JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
#'     GROUP_VAR_OBSERVER = "GROUP_VAR_OBSERVER",
#'     GROUP_VAR_DEVICE = "GROUP_VAR_DEVICE",
#'     TIME_VAR = "TIME_VAR",
#'     PART_VAR = "PART_VAR",
#'     STUDY_SEGMENT = "STUDY_SEGMENT",
#'     LOCATION_RANGE = "LOCATION_RANGE",
#'     LOCATION_METRIC = "LOCATION_METRIC",
#'     PROPORTION_RANGE = "PROPORTION_RANGE",
#'     MISSING_LIST_TABLE = "MISSING_LIST_TABLE",
#'     CO_VARS = "CO_VARS",
#'     LONG_LABEL = "LONG_LABEL"
#'   ),
#'   RECOMMENDED
#' )
#'
#' prep_check_meta_names(
#'   data.frame(
#'     VAR_NAMES = 1, DATA_TYPE = 2, MISSING_LIST = 3,
#'     LABEL = "LABEL", VALUE_LABELS = "VALUE_LABELS",
#'     JUMP_LIST = "JUMP_LIST", HARD_LIMITS = "HARD_LIMITS",
#'     GROUP_VAR_OBSERVER = "GROUP_VAR_OBSERVER",
#'     GROUP_VAR_DEVICE = "GROUP_VAR_DEVICE",
#'     TIME_VAR = "TIME_VAR",
#'     PART_VAR = "PART_VAR",
#'     STUDY_SEGMENT = "STUDY_SEGMENT",
#'     LOCATION_RANGE = "LOCATION_RANGE",
#'     LOCATION_METRIC = "LOCATION_METRIC",
#'     PROPORTION_RANGE = "PROPORTION_RANGE",
#'     DETECTION_LIMITS = "DETECTION_LIMITS", SOFT_LIMITS = "SOFT_LIMITS",
#'     CONTRADICTIONS = "CONTRADICTIONS", DISTRIBUTION = "DISTRIBUTION",
#'     DECIMALS = "DECIMALS", VARIABLE_ROLE = "VARIABLE_ROLE",
#'     DATA_ENTRY_TYPE = "DATA_ENTRY_TYPE",
#'     CO_VARS = "CO_VARS",
#'     VARIABLE_ORDER = "VARIABLE_ORDER", LONG_LABEL =
#'       "LONG_LABEL", recode = "recode",
#'       MISSING_LIST_TABLE = "MISSING_LIST_TABLE"
#'   ),
#'   OPTIONAL
#' )
#'
#' # Next one will fail
#' try(
#'   prep_check_meta_names(data.frame(VAR_NAMES = 1, DATA_TYPE = 2,
#'     MISSING_LIST = 3), TECHNICAL)
#' )
#' }
prep_check_meta_names <- function(meta_data = "item_level", level,
                                  character.only = FALSE) {
  util_expect_data_frame(meta_data)
  if (missing(level)) {
    level <- VARATT_REQUIRE_LEVELS$REQUIRED
  } else {
    if (character.only) {
      level <- as.character(level)
    } else {
      lv <- substitute(expr = level)
      if (length(lv) == 3 &&
          as.character(lv[[1]]) == "$" &&
          as.character(lv[[2]]) =="VARATT_REQUIRE_LEVELS")
        util_error(
          c("%s is called either with %s",
            "set to %s or with names from %s without quotes."),
          sQuote("prep_check_meta_names"),
          sQuote("character.only"),
          dQuote(TRUE),
          dQuote('VARATT_REQUIRE_LEVELS')
        )
      level <- as.character(lv)
    }
    if (length(level) > 0) {
      level <- try(match.arg(level, choices = c(names(VARATT_REQUIRE_LEVELS),
                                                unlist(VARATT_REQUIRE_LEVELS)),
                             several.ok = FALSE), silent = TRUE)
      if (inherits(level, "try-error")) {
        util_error(
          "Error regarding argument %s: %s",
          dQuote("level"),
          conditionMessage(attr(level, "condition"))
        )
      }
      if (level %in% names(VARATT_REQUIRE_LEVELS)) {
        level <- VARATT_REQUIRE_LEVELS[[level]]
      }
    }
  }
  env <- new.env(environment())
  env$res <- try(stop("Internal error"), silent = TRUE)
  try(
    withCallingHandlers({
        if (!is.data.frame(meta_data)) {
          util_error("meta data is not a data frame at all")
        }
        required_atts <- util_get_var_att_names_of_level(level)
        if (!all(required_atts %in% colnames(meta_data))) {
          util_error(
            c("Missing %s: Not all variable attributes of requirement",
              "level %s (%s) are available in the meta data (%s)."),
            paste0(dQuote(required_atts[!(required_atts %in%
                                            colnames(meta_data))]),
                   collapse = ", "),
            dQuote(level),
            paste0(dQuote(required_atts), collapse = ", "),
            paste0(dQuote(colnames(meta_data)), collapse = ", "),
            applicability_problem = TRUE
          )
        }
        if (!all(colnames(meta_data) %in% required_atts)) {
          suspicious_names <-
            colnames(meta_data)[!(colnames(meta_data) %in% required_atts)]

          normLev.fnc <- function(a, b, ...) {
            # https://stackoverflow.com/q/10140923
            drop(adist(a, b, ...) / nchar(attr(adist(a, b, counts = TRUE, ...),
                                               "trafos")))
          }

          fuzzy_match <- vapply(
            # and try to guess, what the user wanted to put there
            suspicious_names,
            function(v) {
              distances <- normLev.fnc(trimws(v), trimws(required_atts),
                                       ignore.case = TRUE, fixed = TRUE)
              if (any(distances < 0.3)) {
                required_atts[which.min(distances)]
              } else {
                NA_character_
              }
            },
            ""
          )
          fuzzy_match <- fuzzy_match[!is.na(fuzzy_match)]
          if (length(fuzzy_match) > 0) {
            util_warning(c("Found the following addtional metadata columns,",
                           "which look like typos of defined names: %s"),
                         paste(dQuote(names(fuzzy_match)), dQuote(fuzzy_match),
                               sep = " -> ", collapse = ", "),
                         applicability_problem = TRUE)
          }
        }
        env$res <- TRUE
      },
      error = function(cond) {
        env$res <- cond
      }
    ),
    silent = TRUE
  )
  if (inherits(env$res, "condition")) {
    env$res[["call"]] <- sys.call()
    util_error(env$res)
  }
  invisible(env$res)
}

