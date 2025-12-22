#' Read additional concept tables
#'
#' @param filename RDS-file name without extension to read from
#' @param ... passed to [subset]
#'
#' @return a data frame
#'
#' @family concept_functions
#' @concept reporting
#' @noRd

util_get_concept_info <- function(filename, ...) {
  if (exists(filename, .concept_chache, mode = "list")) {
    dfr <- get(filename, .concept_chache, mode = "list")
  } else {
    f <- system.file(paste0(filename, ".rds"), package = "dataquieR")
    if (0 != file.access(f, 4)) {
      util_error("Cannot read file %s. Internal error.", dQuote(filename))
    }
    dfr <- readRDS(f)
    assign(filename, dfr, .concept_chache)
  }
  cl <- sys.call()
  cl[[1]] <- as.symbol("subset")
  cl[[2]] <- NULL
  cl$x <- dfr
  r <- try(eval(cl,
       envir = parent.frame(),
       enclos = environment()), silent = TRUE)
  if (util_is_try_error(r)) {
    util_warning(r)
    r <- NULL
  }
  r
}
.concept_chache <- new.env(parent = emptyenv())
