#' Can we really be sure to run `RStudio`
#'
#' `Jetbrain's` `Idea` and so on fake to be `RStudio` by having `RStudio` in
#' `.Platform$GUI`.
#'
#' @return `TRUE`, if really sure to be `RStudio`, `FALSE`, otherwise.
#'
#' @keywords internal
util_really_rstudio <- function() {

  is_shiny <- suppressWarnings(util_ensure_suggested("shiny", err = FALSE)) &&
    (!is.null(shiny::getDefaultReactiveDomain()))

  is_rstudio <-
    !is_shiny &&
    suppressWarnings(util_ensure_suggested("rstudioapi", err = FALSE)) &&
    rstudioapi::isAvailable()

  is_not_jetbrains <- identical(Sys.getenv("RSTUDIO"), "1") &&
    !exists(".jetbrains", globalenv(), mode = "environment")

  is_rstudio && is_not_jetbrains

}
