#' Get the dataquieR `DATA_TYPE` of `x`
#'
#' @param x object to define the dataquieR data type of
#' @param guess_character [logical] guess a data type for character columns
#'                                  based on the values
#'
#' @return the dataquieR data type as listed in `DATA_TYPES`
#' @seealso [`DATA_TYPES_OF_R_TYPE`]
#' @export
prep_dq_data_type_of <- function(x, guess_character =
                                    getOption("dataquieR.guess_character",
                                              default =
                                              dataquieR.guess_character_default
                                            )) {
  util_expect_scalar(guess_character, check_type = is.logical)
  if (is.list(x) && all(vapply(x, inherits, "hms", FUN.VALUE = logical(1)))) {
    return(tolower(DATA_TYPES_OF_R_TYPE$hms))
  }
  if (is.list(x) && all(vapply(x, inherits, "times", FUN.VALUE = logical(1)))) {
    return(tolower(DATA_TYPES_OF_R_TYPE$times))
  }
  r <- head(unique(intersect(names(DATA_TYPES_OF_R_TYPE), class(x))), 1)

  if (length(r) != 1) {
    NULL
  } else {
    if (r %in% c("numeric", "float", "double")) {
      if (all(util_is_integer(x))) {
        r <- "integer"
      }
    } else if (guess_character && r == "character") {
      r <- switch (readr::guess_parser(x, guess_integer = TRUE),
        integer = "integer",
        character = "character",
        double = "double",
        number = "numeric",
        time = "hms",
        date = "POSIXct",
        datetime = "POSIXct",
        logical = "logical",
        "character" # fallback
      )
      # readr::guess_parser(as.character(1:10), guess_integer = TRUE) # integer
      # readr::guess_parser("asdf", guess_integer = TRUE) # character
      # readr::guess_parser("1.4", guess_integer = TRUE) # double, number
      # readr::guess_parser("12:00:00", guess_integer = TRUE) # time
      # readr::guess_parser("2007-08-07", guess_integer = TRUE) # date
      # readr::guess_parser("2007-08-07 01:01:01", guess_integer = TRUE) # datetime
      # readr::guess_parser("true", guess_integer = TRUE) # logical
      # factor or guess are obviously never returned.
    }

    tolower(DATA_TYPES_OF_R_TYPE[[r]])
  }
}
