#' Get a translation
#'
#' @param keys [character] translation keys
#' @param ns [character] translation namespace
#' @param lang [character] language to translate to
#' @param as_this_translation `dataquieR_translated` use this as template and
#'                            translate the keys as this one.
#' @param reverse [logical] translate backwards
#'
#' @return [character] translations
#'
#' @family string_functions
#' @concept reporting
#' @noRd
util_translate <- function(
    keys,
    ns = "general",
    lang = getOption("dataquieR.lang", dataquieR.lang_default),
    as_this_translation = NULL,
    reverse = FALSE) {

  if (!missing(as_this_translation)) {
    util_stop_if_not(inherits(as_this_translation, "dataquieR_translated"))
    if (!missing(ns) || !missing(lang)) {
      util_error(
        c("%s can called with %s and/or %s *or* with %s, but not both.",
        "this is an internal error, sorry. please report."),
        sQuote("util_translate"),
        sQuote("ns"), sQuote("lang"), sQuote("as_this_translation"))
    }
    ns <- attr(as_this_translation, "ns")
    lang <- attr(as_this_translation, "lang")
  }

  # Hint: the language may differ at rendering time from the language at computation time.
  translations <-
    util_get_concept_info("translations")
  values <- vapply(keys, function(key) {
    if (reverse) {
      match <-
        translations$namespace == ns &
        translations$translation == key &
        translations$lang == lang
      if (!any(match)) {
        match <-
          translations$namespace == ns &
          translations$translation == key &
          translations$lang == ""
        # if (match) {
        #   # missing translation
        # }
      }
    } else {
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
    }
    if (sum(match) > 1) {
      util_error(
        "Internal error, sorry, please report: >1 translation for %s:%s:%s",
        sQuote(key),
        sQuote(ns),
        sQuote(lang))
    } else if (sum(match) == 1) {
      if (reverse) {
        return(translations$key[match])
      } else {
        return(translations$translation[match])
      }
    } else {
      return(key)
    }
  }, FUN.VALUE = character(1))
  structure(
    values,
    names = keys,
    ns = ns,
    lang = lang,
    class = "dataquieR_translated"
  )
}

#' `print` implementation for the class `dataquieR_translated`
#'
#' `dataquieR`'s translated texts featuring access to the language keys, still.
#'
#' @param x `dataquieR_translated` object to print
#' @param ... passed to base::print
#'
#' @seealso base::print
#'
#' @return as print
#' @export
print.dataquieR_translated <- function(x, ...) {
  y <- x
  class(y) <- "character"
  attr(y, "names") <- NULL
  attr(y, "ns") <- NULL
  attr(y, "lang") <- NULL
  print(y, ...)
  invisible(x)
}

#' `as.character` implementation for the class `dataquieR_translated`
#'
#' `dataquieR`'s translated texts featuring access to the language keys, still.
#'
#' @param x `dataquieR_translated` object to print
#' @param ... passed to base::as.character
#'
#' @seealso base::as.character
#'
#' @return character with only the translated entries
#' @export
as.character.dataquieR_translated <- function(x, ...) {
  class(x) <- "character"
  attr(x, "names") <- NULL
  attr(x, "ns") <- NULL
  attr(x, "lang") <- NULL
  x
}

#' `names` implementation for the class `dataquieR_translated`
#'
#' `dataquieR`'s translated texts featuring access to the language keys, still.
#' this function returns the language keys.
#'
#' only `setNames(nm = x)` is allowed for convenience. Any other assignment
#' would mean to change the language keys, so this is not allowed.
#'
#' @param x `dataquieR_translated` object
#' @param value the names to assign
#'
#' @seealso base::as.character
#'
#' @return names of the underlying character vector
#' @export
`names<-.dataquieR_translated` <- function(x, value) {
  if (identical(x, value)) { # enable setNames(nm = colnames(x)), if colnames(x) are translated
    x <- as.character(unclass(x))
    attr(x, "names") <- value
    return(x)
  }
  util_error("You cannot change the language keys of an %s object",
             sQuote("dataquieR_translated"))
}

#' Detect if an object is a `dataquieR_translated` object
#'
#' @param x the object to test
#'
#' @returns `TRUE`, if `x` is a `dataquieR_translated` object.
#' @export
prep_is_translated <- function(x) {
  inherits(x, "dataquieR_translated")
}

`util_translated_colnames<-` <- function(x, value) {
  util_stop_if_not(is.data.frame(x) || is.matrix(x))
  util_stop_if_not(inherits(value, "dataquieR_translated"))
  util_stop_if_not(!inherits(colnames(x), "dataquieR_translated"))
  if (is.data.frame(x)) {
    attr(x, "names") <-
      value
  } else if (is.matrix(x)) {
    attr(x, "dimnames")[[2]] <-
      value
  }
  x
}

util_untranslated_colnames <- function(x) {
  util_stop_if_not(inherits(cn <- colnames(x), "dataquieR_translated"))
  attr(cn, "names")
}

#' JSON representation for translated dataquieR objects
#'
#' @param x A `dataquieR_translated` object.
#' @param ... Passed through to `jsonlite::toJSON()` internals.
#' @return A JSON-ready representation.
#' @noRd
asJSON.dataquieR_translated <- function(x, ...) {
  if (util_ensure_suggested("jsonlite", err = FALSE)) {
   jsonlite::toJSON(unname(as.character(unclass(x))))
  } else {
    util_error(
      "Should not be reached in asJSON, internal error, sorry, please report.")
  }
}

util_with_orig_names <- function(x) {
  if (inherits(attr(x, "names"), "dataquieR_translated")) {
    names(x) <- names(attr(x, "names"))
  }
  x
}

translation_version <- as.numeric_version(attr(readRDS(system.file("translations.rds",
                                                      package = packageName())),
                                  "version"))
