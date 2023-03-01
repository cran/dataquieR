util_remove_dataquieR_result_class <- function(x) { # TODO: The [[ for dataquieR_result should not work this way, a wrapper class would be needed, instead. Then, this funcion can be abandondd
  remove_class <- function(x) {
    if (!is.null(x)) {
      if (is.list(x) && !inherits(x, "patchwork")) {
        x[] <- lapply(x, remove_class)
      }
      newclass <- setdiff(class(x), "dataquieR_result")
      if (length(newclass)) {
        class(x) <- newclass
      } else {
        x <- unclass(x)
      }
      attr(x, "error") <- NULL
      attr(x, "warning") <- NULL
      attr(x, "message") <- NULL
    }
    x
  }
  remove_class(x)
}
