#' Get a translation
#'
#' @param keys [character] translation keys
#' @param ns [character] translation namespace
#' @param lang [character] language to translate to
#'
#' @return [character] translations
#'
#' @family string_functions
#' @concept reporting
#' @noRd
util_translate <- function(
    keys,
    ns = "general",
    lang = getOption("dataquieR.lang", dataquieR.lang_default)) {
  translations <-
    util_get_concept_info("translations")
  vapply(keys, function(key) {
    match <-
      translations$namespace == ns &
        translations$key == key &
        translations$lang == lang
    if (!any(match)) {
      match <-
        translations$namespace == ns &
        translations$key == key &
        translations$lang == ""
      # if (match) {
      #   # missing translation
      # }
    }
    if (sum(match) > 1) {
      util_error(
        "Internal error, sorry, please report: >1 translation for %s:%s:%s",
        sQuote(key),
        sQuote(ns),
        sQuote(lang))
    } else if (sum(match) == 1) {
      return(translations$translation[match])
    } else {
      return(key)
    }
  }, FUN.VALUE = character(1))
}
