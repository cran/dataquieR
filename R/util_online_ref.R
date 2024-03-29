#' Creates a Link to our Website
#'
#' i.e., to a vignette on the website
#'
#' @param fkt_name [character] function name to generate a link for
#'
#' @return [character] the link
#'
#' @keywords internal
util_online_ref <- function(fkt_name) {

  # convert function names to website names
  fkt_name <- strsplit(fkt_name, "_", fixed = TRUE)[[1]]
  fkt_name <- c("VIN", head(fkt_name, 1), "impl", tail(fkt_name, -1))
  fkt_name <- paste(fkt_name, collapse = "_")

  sprintf("https://dataquality.qihs.uni-greifswald.de/%s.html", fkt_name)
}
