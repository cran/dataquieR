#' @title Check disjunctness of *_vars argument sets
#' @description
#' Checks that vectors supplied via arguments ending in `_vars` are pairwise
#' disjoint. If called with `...`, only the supplied arguments are checked
#' (legacy behavior). If called without `...`, the caller function is
#' introspected: all its `*_vars` arguments are resolved to their effective
#' values (passed, modified in the caller, or defaults) and checked.
#'
#' @param ... Optional named vectors to check. If provided, only these are
#'   considered.
#'
#' @return `invisible(NULL)`. Raises an error via `util_error()` on overlap.
#'
#' @noRd
#' @family utilities
util_disjunct_var_sets <- function(...) {
  dots <- rlang::dots_list(...)
  if (length(dots) > 0L) {
    arg_list <- dots
    arg_names <- names(arg_list)
    if (is.null(arg_names) || any(arg_names == "")) {
      arg_names <- vapply(
        substitute(list(...))[-1],
        rlang::as_label,
        character(1)
      )
    }
    names(arg_list) <- arg_names
  } else {
    caller_fn   <- rlang::caller_fn()
    caller_env  <- rlang::caller_env()
    caller_call <- rlang::caller_call()
    if (is.null(caller_fn) || is.null(caller_call)) return(invisible(NULL))

    formals_list <- formals(caller_fn)
    var_args <- grep("_vars$", names(formals_list), value = TRUE)
    if (length(var_args) == 0L) return(invisible(NULL))

    matched_call <- rlang::call_match(caller_call, caller_fn)
    matched_args <- rlang::call_args(matched_call)

    resolve_one <- function(nm) {
      if (rlang::env_has(caller_env, nm, inherit = TRUE)) {
        return(get(nm, envir = caller_env, inherits = TRUE))
      }
      if (nm %in% names(matched_args)) {
        return(rlang::eval_bare(matched_args[[nm]], env = caller_env))
      }
      def <- formals_list[[nm]]
      if (!rlang::is_missing(def)) {
        return(rlang::eval_bare(def, env = caller_env))
      }
      NULL
    }

    vals <- lapply(var_args, resolve_one)
    names(vals) <- var_args
    keep <- !vapply(vals, function(x) is.null(x) || length(x) == 0L,
                    logical(1))
    if (!any(keep)) return(invisible(NULL))
    arg_list <- vals[keep]
  }

  arg_names <- names(arg_list)
  pairs <- utils::combn(seq_along(arg_list), 2L, simplify = FALSE)

  overlaps <- lapply(pairs, function(ix) {
    ov <- intersect(arg_list[[ix[1]]], arg_list[[ix[2]]])
    if (length(ov) > 0L) {
      list(args = c(arg_names[[ix[1]]], arg_names[[ix[2]]]),
           values = ov)
    } else {
      NULL
    }
  })
  overlaps <- overlaps[!vapply(overlaps, is.null, logical(1))]

  if (length(overlaps) > 0L) {
    problematic_args <- unique(unlist(lapply(overlaps, `[[`, "args")))
    problematic_vals <- unique(unlist(lapply(overlaps, `[[`, "values")))
    util_error(
      c("Overlap in the arguments %s. %s cannot be parts of more than one ",
        "argument."),
      util_pretty_vector_string(problematic_args),
      util_pretty_vector_string(problematic_vals, n_max = 5),
      applicability_problem = TRUE
    )
  }

  invisible(NULL)
}
