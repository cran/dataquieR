#' Read additional concept tables
#'
#' @param filename RDS-file name without extension to read from
#' @param ... passed to [subset]
#'
#' @return a data frame
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
  subset(dfr, ...)
}
.concept_chache <- new.env(parent = emptyenv())
