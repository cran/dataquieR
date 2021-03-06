#' Get variable attributes of a certain provision level
#'
#' This function returns all variable attribute names of a certain
#' meta data provision level or of more than one level.
#'
#' @param level level(s) of requirement
#'
#' @return all matching variable attribute names
#'
util_get_var_att_names_of_level <- function(level) {
  level <- match.arg(level, choices = unlist(VARATT_REQUIRE_LEVELS),
                     several.ok = TRUE)
  r <- vapply(WELL_KNOWN_META_VARIABLE_NAMES, function(att_name) {
    if (attr(att_name, REQUIREMENT_ATT) %in% level) {
      att_name
    } else {
      NA_character_
    }
  }, character(1))
  r[!is.na(r)]
}
