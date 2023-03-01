#' Abbreviate snake_case function names to shortened `CamelCase`
#'
#' @param x a vector of indicator function names
#'
#' @return abbreviations
util_abbreviate <- function(x) {
  util_expect_scalar(x, allow_more_than_one = TRUE, allow_null = TRUE,
                     allow_na = TRUE, check_type = is.character)
  r <- vapply(strsplit(x, "_", fixed = TRUE), function(xx) {
    prefix <- head(xx, 1)
    known_prefix <- c("acc", "com", "con", "int")
    if (any(prefix %in% known_prefix)) {
      prefix[prefix %in% known_prefix] <- c(
        "acc" = "a",
        "com" = "m",
        "con" = "c",
        "int" = "i"
      )[[prefix[prefix %in% known_prefix]]]
    }
    suffix <- gsub("^(.)(..).*$", "\\U\\1\\E\\2", tail(xx, -1), perl = TRUE)
    paste0(prefix, paste(suffix, collapse = ""), collapse = "")
  },  FUN.VALUE = character(1))
  newly_ambig <- duplicated(x) != duplicated(r)
  while (any(newly_ambig)) {
    r[newly_ambig] <- paste0(r[newly_ambig], "\u00b0")
    newly_ambig <- duplicated(x) != duplicated(r)
  }
  r
}
