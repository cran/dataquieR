#' Get variable attributes of a certain provision level
#'
#' This function returns all variable attribute names of a certain
#' metadata provision level or of more than one level.
#'
#' @param level level(s) of requirement
#' @param cumulative include all names from more basic levels
#'
#' @return all matching variable attribute names
#'
#' @family metadata_management
#' @concept metadata_management
#' @noRd


util_get_var_att_names_of_level <- function(level, cumulative = TRUE) {
  if (length(level) > 0) {
    level <- match.arg(level, choices = unlist(VARATT_REQUIRE_LEVELS),
                       several.ok = TRUE)
  } else {
    return(character(0))
  }
  if (cumulative) {
    max_level <- max(which(VARATT_REQUIRE_LEVELS_ORDER %in% level))
    level <- head(VARATT_REQUIRE_LEVELS_ORDER, max_level)
  }
  r <- vapply(WELL_KNOWN_META_VARIABLE_NAMES, function(att_name) {
    if (attr(att_name, REQUIREMENT_ATT) %in% level) {
      att_name
    } else {
      NA_character_
    }
  }, character(1))
  r[!is.na(r)]
}
