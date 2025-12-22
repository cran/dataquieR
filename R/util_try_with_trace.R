util_try_with_trace <- function(expr, silent = FALSE) {
  force(silent)
  tryCatch(
    expr,
    error = function(cnd) {
      # Attach an rlang backtrace to ANY error (base or rlang)
      cnd <- rlang::cnd_entrace(cnd)

      msg <- conditionMessage(cnd)

      if (!silent) {
        # Print a compact message; keep the heavy trace in the attribute
        message("Error: ", msg)
      }

      # Return a classic try-error *character* vector with a "condition" attribute
      structure(
        msg,
        class = "try-error",
        condition = cnd
      )
    }
  )
}
