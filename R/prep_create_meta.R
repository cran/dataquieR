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
    metas <- data.frame(..., stringsAsFactors = stringsAsFactors)
    cn <- names(list(...))
  }
  cn <-  mget(
    x = cn,
    envir = as.environment(WELL_KNOWN_META_VARIABLE_NAMES),
    mode = "character",
    ifnotfound = setNames(nm = colnames(metas)),
    inherits = FALSE
  )
  if (any(duplicated(cn))) {
    util_error("Found duplicated meta columns: %s",
               paste0(dQuote(unique(cn[duplicated(cn)])),
               collapse = ", "))
  }
  colnames(metas) <- cn
  if (missing(level) || !is.null(level))
    prep_check_meta_names(metas, level = level, character.only = character.only)
  util_validate_known_meta(metas)
  return(metas)
}
