#' Creates a Link to our Website
#'
#' i.e., to a vignette on the website
#'
#' @param fkt_name [character] function name to generate a link for
#'
#' @return [character] the link
#'
#' @noRd
util_online_ref <- function(fkt_name) {

  # convert function names to website names
  fkt_name <- strsplit(fkt_name, "_", fixed = TRUE)[[1]]
  fkt_name_impl <-  c("VIN", head(fkt_name, 1), "impl", tail(fkt_name, -1))
  fkt_name <- c("VIN", fkt_name)
  fkt_name <- paste(fkt_name, collapse = "_")
  fkt_name_impl <- paste(fkt_name_impl, collapse = "_")

  fkt_name <-
    sprintf("https://dataquality.qihs.uni-greifswald.de/%s.html", fkt_name)

  fkt_name_impl <-
    sprintf("https://dataquality.qihs.uni-greifswald.de/%s.html", fkt_name_impl)


  if (identical(
    try(attr(
      base::curlGetHeaders(fkt_name), "status"), silent = TRUE), 200L)) {
    return(fkt_name)
  }

  return(fkt_name_impl)
}
