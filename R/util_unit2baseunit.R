#' Detect base unit from composite units
#'
#' @param unit [character] a unit
#' @param warn_ambiguities [logical] warn about all ambiguous units
#' @param unique [logical] choose the more `SI`-like unit in case of ambiguities
#'
#' @return [character] all possible or the preferable (unique set `TRUE`)
#'                     base units. Can be `character(0)`, if unit is invalid
#'                     or uniqueness was requested, but even using precedence
#'                     rules of `SI`-closeness do not help selecting the most
#'                     suitable unit.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' util_unit2baseunit("%")
#' util_unit2baseunit("d%")
#'
#' # Invalid unit
#' util_unit2baseunit("aa%")
#' util_unit2baseunit("aa%", unique = FALSE)
#'
#' util_unit2baseunit("a%")
#'
#' # Invalid unit
#' util_unit2baseunit("e%")
#' util_unit2baseunit("e%", unique = FALSE)
#'
#' util_unit2baseunit("E%")
#' util_unit2baseunit("Eg")
#'
#' # Invalid unit
#' util_unit2baseunit("E")
#' util_unit2baseunit("E", unique = FALSE)
#'
#' util_unit2baseunit("EC")
#' util_unit2baseunit("EK")
#' util_unit2baseunit("µg")
#' util_unit2baseunit("mg")
#' util_unit2baseunit("°C")
#' util_unit2baseunit("k°C")
#' util_unit2baseunit("kK")
#' util_unit2baseunit("nK")
#'
#' # Ambiguous units, if used with unique = FALSE
#' util_unit2baseunit("kg")
#' util_unit2baseunit("cd")
#' util_unit2baseunit("Pa")
#' util_unit2baseunit("kat")
#' util_unit2baseunit("min")
#'
#' # atto atom units or astronomical units, both in state "accepted"
#' util_unit2baseunit("au")
#' util_unit2baseunit("au", unique = FALSE)
#'
#' # astronomical units or micro are, both in state "accepted"
#' util_unit2baseunit("ua")
#' util_unit2baseunit("ua", unique = FALSE)
#'
#' util_unit2baseunit("kt")
#'
#' # parts per trillion or pico US_liquid_pint, both in state "common",
#' # but in this case, plain count units will be preferred
#' util_unit2baseunit("ppt")
#' util_unit2baseunit("ppt", unique = FALSE)
#'
#' util_unit2baseunit("ft")
#' util_unit2baseunit("yd")
#' util_unit2baseunit("pt")
#'
#' # actually the same, but both only common, and to my knowledge not-so-common
#' # gram-force vs. kilogram-force (kilo pond)
#' util_unit2baseunit("kgf")
#' util_unit2baseunit("kgf", unique = FALSE)
#'
#' util_unit2baseunit("at")
#' util_unit2baseunit("ph")
#' util_unit2baseunit("nt")
#' }
util_unit2baseunit <- function(unit,
                               warn_ambiguities = !exists("warn_ambiguities",
                                                         .unit2baseunitenv),
                               unique = TRUE) {
  if (warn_ambiguities) {
    assign("warn_ambiguities", FALSE, .unit2baseunitenv)
    problems <- vapply(lapply(setNames(nm = UNITS), util_unit2baseunit,
                              warn_ambiguities = FALSE, unique = FALSE),
           length, FUN.VALUE = integer(1)) != 1
    if (any(problems)) {
      util_message(
        "Found ambiguous units (could also be prefix + some other unit): %s",
        paste0(dQuote(names(problems[problems])), collapse = ", "))
    }
  }
  # all_units <-
  #   apply(expand.grid(UNIT_PREFIXES, UNITS), 1, paste0, collapse = "")
  myUNIT_PREFIXES <- c(UNIT_PREFIXES, "")
  mp <- lapply(setNames(nm = UNITS),
         FUN = function(suf, pre) {
             paste0(pre, suf)
         },
         myUNIT_PREFIXES)
  base_unit <- vapply(mp, function(block) {
    any(block == unit)
  }, FUN.VALUE = logical(1))
  base_unit <- which(base_unit)
  r <- names(mp)[base_unit]
  allowed_set <- c("base", "derived", "accepted", "common")
  orig_r <- r
  while (unique && length(r) > 1 && length(allowed_set) > 0) {
    orig_r <- r
    r <- r[UNIT_SOURCES[r] %in% allowed_set]
    allowed_set <- head(allowed_set, -1)
  }
  if (unique && length(r) == 0 &&
      length(intersect(orig_r, UNIT_IS_COUNT)) == 1) {
    r <- intersect(orig_r, UNIT_IS_COUNT)
  }
  r
}
.unit2baseunitenv <- new.env(parent = emptyenv())
