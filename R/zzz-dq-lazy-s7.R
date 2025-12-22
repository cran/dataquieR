.dq_lazy_state <- new.env(parent = emptyenv())

.dq_lazy_state$s7_ready <- FALSE
.dq_lazy_state$s7_class <- NULL

dq_lazy_unwrap <- function(x) {
  if (!inherits(x, "S7_object")) return(x)
  payload <- tryCatch(x@payload, error = function(e) NULL)
  if (!is.null(payload) && inherits(payload, "dq_lazy_ggplot")) return(payload)
  x
}
