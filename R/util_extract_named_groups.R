#' Extract named capture groups from String
#'
#' created with support from `ChatGPT` in April 2025
#'
#' @param pattern [character] regular expression with named capture groups,
#'                            e.g. `""^\\[(?<row>\\d+), +(?<col>\\d+)\\]:.*""`
#' @param text_vector [character] haystack
#'
#' @returns [data.frame] with matches
#'
#' @seealso [grep()]
#'
#' @noRd
util_extract_named_groups <- function(pattern, text_vector) {
  m <- regexpr(pattern, text_vector, perl = TRUE)

  starts <- attr(m, "capture.start")
  lengths <- attr(m, "capture.length")
  names <- attr(m, "capture.names")

  if (is.null(starts)) {
    # Kein Match oder keine Gruppen
    return(data.frame())
  }

  # Sicherstellen, dass es sich um Matrizen handelt
  if (is.null(dim(starts))) {
    starts <- matrix(starts, nrow = 1)
    lengths <- matrix(lengths, nrow = 1)
  }

  # Ergebnisliste: je Element ein benannter Vektor der Gruppenwerte
  result <- lapply(seq_along(text_vector), function(i) {
    s_row <- starts[i, ]
    l_row <- lengths[i, ]
    grp_vals <- mapply(function(start, len) {
      if (start != -1) substr(text_vector[i], start, start + len - 1) else NA_character_
    }, s_row, l_row, SIMPLIFY = TRUE)
    setNames(as.list(grp_vals), names)
  })

  # In Data Frame umwandeln
  result_df <- do.call(rbind.data.frame, result)
  rownames(result_df) <- NULL
  return(result_df)}
