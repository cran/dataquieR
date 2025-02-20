# dataquieR_resultset2 <- function(...) {
#   this <- list(...)
#   # dataquieR_resultset_verify2(this) # TODO: Implement me!!
#   class(this) <- dataquieR_resultset_class2
#   this
# }
dataquieR_resultset_class2 <- "dataquieR_resultset2"

#' Get namespace for attributes
#'
#' @param my_storr_object the `storr` object
#'
#' @return the namespace name
#' @keywords internal
util_get_storr_att_namespace <- function(my_storr_object) {
  r <- my_storr_object$default_namespace
  r <- paste0(r, ".attributes")
  r
}

#' Get namespace specifically for summary attributes for speed-up
#'
#' @param my_storr_object the `storr` object
#'
#' @return the namespace name
#' @keywords internal
util_get_storr_summ_namespace <- function(my_storr_object) {
  r <- my_storr_object$default_namespace
  r <- paste0(r, ".summary")
  r
}

#' Get a single result from a `dataquieR 2` report
#'
#' @param x the report
#' @param el the index
#'
#' @return the `dataquieR` result object
#' @export
`[[.dataquieR_resultset2` <- function(x, el) {

  my_storr_object <- util_get_storr_object_from_report(x)

  if (is.null(my_storr_object)) {
    # default
    return(NextMethod())
  }

  stopifnot(inherits(my_storr_object, "storr"))

  r_names <- my_storr_object$get("names", namespace =
                                   util_get_storr_att_namespace(my_storr_object))

  if (is.numeric(el) && suppressWarnings(as.integer(el) == as.numeric(el)) &&
      el > 0 && el <= length(r_names)) {
    return(my_storr_object$get(r_names[[el]]))
  } else if (el %in% r_names) {
    return(my_storr_object$get(el))
  } else {
    util_error("element not found")
  }
}

# # IDEA: attr, attr<-, attrbiutes, ..., cave wrt my_storr_object, which needs to be found by the primitive, always, postponed, since obviously impossible
# Solution, so far: keep the attributes in all backends (this will not be kept in sync automatically, but it should still work)
# does not really work, we need to handle attributes separately
# trace ("attr", edit =  function (...)
# {
#   args <- list(...)
#   x <- NULL
#   if (length(args) > 0) {
#     x <- args[[1]]
#   }
#   if (inherits(x, "dataquieR_resultset2")) {
#     return(42)
#   }
#   .prim <- .Primitive("attr")
#   .prim(...)
# }

# attr.dataquieR_resultset2 <- function(x, which, exact = FALSE) {
#
#     y <- unclass(x)
#     my_storr_object <- attr(y, "my_storr_object")
#
#     if (is.null(my_storr_object)) {
#       # default
#       return(NextMethod())
#     }
#
#     stopifnot(inherits(my_storr_object, "storr"))
#
#     my_storr_object$get(which, namespace = util_get_storr_att_namespace(my_storr_object))
# }


#' Set a single result from a `dataquieR 2` report
#'
#' @param x the report
#' @param el the index
#' @param value the single result
#'
#' @return the `dataquieR` result object
#' @export
`[[<-.dataquieR_resultset2` <- function(x, el, value) { # TODO: verify class of value

  my_storr_object <- util_get_storr_object_from_report(x)

  if (is.null(my_storr_object)) {
    # default
    return(NextMethod())
  }

  stopifnot(inherits(my_storr_object, "storr"))

  r_names <- my_storr_object$get("names", namespace =
                                   util_get_storr_att_namespace(my_storr_object))

  if (is.numeric(el) && suppressWarnings(as.integer(el) == as.numeric(el)) &&
      el > 0 && el <= length(r_names)) {
    my_storr_object$set(r_names[[el]], value = value)
    return(x)
  } else if (el %in% r_names) {
    my_storr_object$set(el, value = value)
    return(x)
  } else {
    util_error("element not found, extending reports not yet supported")
  }
}

#' Write to a report
#'
#' Overwriting of elements only list-wise supported
#'
#' @param x a `dataquieR_resultset2
#' @param ... if this contains only one entry and this entry is not named
#'            or its name is `els`, then, the report will be accessed in
#'            list mode.
#' @param value new value to write
#'
#' @return nothing, stops
#' @export
`[<-.dataquieR_resultset2` <- function(x, ..., value) {
  if (nargs() > 3) {
    util_error("You cannot write subsets of a dataquieR report, yet.")
  }
  if (nargs() == 3 && !identical(rlang::missing_arg(),
                                 rlang::call_args(sys.call())[[2]])) {
    args <- list(...)
  } else {
    args <- list()
  }
  if (identical(names(args), "els") ||
      (length(args) == 1 && is.null(names(args))) ||
      (length(args) == 0)) {
    # list mode
    return(`.access_dq_rs2<-`(x, seq_along(names(x)), value))
  } else {
    util_error("You cannot write subsets of a dataquieR report, yet.")
  }
}

#' Access single results from a [dataquieR_resultset2] report
#' @aliases cash-.dataquieR_resultset2
#' @inherit [[.dataquieR_resultset2
#' @export
`$.dataquieR_resultset2` <- `[[.dataquieR_resultset2`

#' Write single results from a [dataquieR_resultset2] report
#' @aliases cash-set-.dataquieR_resultset2.Rd
#' @inherit [[<-.dataquieR_resultset2
#' @export
`$<-.dataquieR_resultset2` <- `[[<-.dataquieR_resultset2`

#' inefficient way to convert a report to a list. try [prep_set_backend()]
#'
#' @param x [dataquieR_resultset2]
#' @param ... no used
#'
#' @return [list]
#' @export
as.list.dataquieR_resultset2 <- function(x, ...) {
  my_storr_object <- util_get_storr_object_from_report(x)

  if (is.null(my_storr_object)) {
    # default
    return(NextMethod())
  } else {
    util_warning("as.list is inefficient for dataquieR_resultset2 objects",
            immediate. = TRUE)
    # print(rlang::trace_back())
    lapply(x, identity)
  }
}

#' Class [`r dataquieR_resultset_class2`].
#'
#' @seealso [dq_report2]
#' @aliases .dataquieR_resultset2
#' @importFrom methods new
dataquieR_resultset2 <- methods::setClass("dataquieR_resultset2")

suppressMessages({
  methods::setMethod('vapply', signature(X="dataquieR_resultset2"),
            function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
              vapply(setNames(nm = names(X)), function(nm) {
                FUN(X[[nm]], ...)
              }, FUN.VALUE = FUN.VALUE, USE.NAMES = USE.NAMES)
            })

  methods::setMethod('lapply', signature(X="dataquieR_resultset2"),
            function(X, FUN, ...) {
              lapply(setNames(nm = names(X)), function(nm) {
                FUN(X[[nm]], ...)
              })
            })

  methods::setMethod('sapply', signature(X="dataquieR_resultset2"),
            function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
              sapply(setNames(nm = names(X)), function(nm) {
                FUN(X[[nm]], ...)
              }, simplify = simplify, USE.NAMES = USE.NAMES)
            })
})

#' Change the back-end of a report
#'
#' with this function, you can move a report from/to a `storr` storage.
#'
#' @param r [dataquieR_resultset2] the report
#' @param storr_factory `storr` the `storr` storage or `NULL`, to move
#'                                the report fully back into the RAM.
#' @param amend [logical] if there is already data in.`storr_factory`,
#'                        use it anyways -- unsupported, so far!
#'
#' @return [dataquieR_resultset2] but now with the desired back-end
#' @export
prep_set_backend <- function(r, storr_factory = NULL, amend = FALSE) {
  util_expect_scalar(amend, check_type = is.logical)
  stopifnot(inherits(r, "dataquieR_resultset2"))
  if (is.null(storr_factory)) {
    my_storr_object <- util_get_storr_object_from_report(r)
    stopifnot(inherits(my_storr_object, "storr"))
    atts_r_nm <- my_storr_object$list(namespace =
                                        util_get_storr_att_namespace(my_storr_object))
    atts_r <- setNames(my_storr_object$mget(atts_r_nm,
                                            namespace =
                                              util_get_storr_att_namespace(my_storr_object)),
                       atts_r_nm
    )
    r_nm <- atts_r[["names"]] # my_storr_object$list()
    r <- setNames(my_storr_object$mget(r_nm), r_nm);
    attr(r, "my_storr_object") <- NULL; # my_storr_object # never ever restore this
    attributes(r) <- atts_r
  } else {

    my_storr_object <- util_storr_object(storr_factory)

    stopifnot(inherits(my_storr_object, "storr"))

    if (!is.null(my_storr_object)) {

      if (!is.null(my_storr_object) && (
        length(my_storr_object$list()) > 0 ||
        length(my_storr_object$list(
          util_get_storr_att_namespace(my_storr_object))) > 0 ||
        length(my_storr_object$list(
          util_get_storr_summ_namespace(my_storr_object))) > 0
      )) {
        if (amend) {
          util_message(c("Your storr-object is not empty, but %s was set %s,",
                         "so I'll amend the storage object. This is unsupported,",
                         "yet, so expect strange behavior."),
                       dQuote("amend"), sQuote(TRUE))
        } else {
          util_error(c("Your storr-object is not empty, and %s was set %s,",
                       "so I won't amend the storage object, which would",
                       "still be unsupported, so could cause strange behavior.",
                       "We strongly recommend to use clear storr objects (or",
                       "at least the default namespace (%s in your case)",
                       "and its sister namespaces (the default namespace suffixed",
                       "with %s and %s, should be empty. In case of %s, just",
                       "delete the folder that backs the storr."),
                     dQuote("amend"),
                     sQuote(FALSE),
                     sQuote(my_storr_object$default_namespace),
                     sQuote(".attributes"),
                     sQuote(".summary"),
                     sQuote("driver_rds")
          )
        }
      }


      my_storr_object <- util_fix_storr_object(my_storr_object)
      atts_r <- attributes(r)
      atts_r[["my_storr_object"]] <- NULL # dont save this ever
      my_storr_object$mset(key = names(atts_r), value = atts_r, namespace =
                             util_get_storr_att_namespace(my_storr_object))

      my_storr_object$mset(key = names(r), value = r)

      my_storr_object$mset(
        key = names(r),
        value = lapply(r, attr, "r_summary"),
        namespace = util_get_storr_summ_namespace(my_storr_object))


      r[] <- lapply(r, function(x) NA)
      attr(r, "my_storr_object") <- my_storr_object
      class(r) <- "dataquieR_resultset2"
    }
  }
  r
}

#' Load a report from a back-end
#'
#' @param namespace the namespace to read the report's results from
#' @param storr_factory a function returning a `storr` object holding the report
#' @param db_dir [character] path to the directory for the back-end, if
#'                           a `storr_rds` or `storr_torr` is used.
#'
#' @return [dataquieR_resultset2] the report
#' @export
#' @seealso [prep_create_storr_factory()]
#' @examples
#' \dontrun{
#' r <- dataquieR::dq_report2("study_data", meta_data_v2 = "meta_data_v2",
#'                            dimensions = NULL)
#' storr_factory <- prep_create_storr_factory()
#' r_storr <- prep_set_backend(r, storr_factory)
#' r_restorr <- prep_set_backend(r_storr, NULL)
#' r_loaded <- prep_load_report_from_backend(storr_factory)
#' }
prep_load_report_from_backend <- function(
    namespace = "objects",
    db_dir,
    storr_factory = prep_create_storr_factory(namespace = namespace,
                                               db_dir = db_dir)) {

  my_storr_object <- util_storr_object(storr_factory)

  if (is.null(my_storr_object)) {
    util_error("You did not pass a valid storr factory in the argument %s",
               sQuote("storr_factory"))
  }

  atts_r_nm <- my_storr_object$list(namespace =
                                      util_get_storr_att_namespace(my_storr_object))
  atts_r <- setNames(my_storr_object$mget(atts_r_nm, namespace =
                                            util_get_storr_att_namespace(my_storr_object)),
                     atts_r_nm
  )
  r_nm <- atts_r[["names"]] # my_storr_object$list()
  r <- lapply(r_nm, function(x) NULL)
  attributes(r) <- atts_r
  attr(r, "my_storr_object") <- my_storr_object
  class(r) <- "dataquieR_resultset2"
  r
}

#' Get a subset of a `dataquieR` `dq_report2` report
#'
#' @param x the report
#'
#' @param row the variable names, must be unique
#' @param col the function-call-names, must be unique
#' @param res the result slot, must be unique
#' @param drop drop, if length is 1
#' @param els used, if in list-mode with named argument
#'
#' @return a list with results, depending on `drop` and the number of results,
#'         the list may contain all requested results in sub-lists. The order
#'         of the results follows the order of the row/column/result-names given
#'
#' @export
`[.dataquieR_resultset2` <- function(x, row, col, res, drop = FALSE,
                                     els = row) {
  util_stop_if_not(inherits(x, "dataquieR_resultset2"))

  if (identical(rlang::call_args_names(sys.call()), c("", "")) ||
      identical(rlang::call_args_names(sys.call()), c("", "els"))) {
    # list mode
    return(.access_dq_rs2(x, els))
  }

  cn <- attr(x, "cn")
  rn <- attr(x, "rn")

  if (missing(col)) {
    col_matches <- rep(TRUE, length(attr(x, "names")))
  } else {
    if (!is.vector(col)) {
      util_error("column coordinate %s is not a vector/scalar", sQuote(util_deparse1(
        substitute(col))))
    }
    util_stop_if_not(!any(duplicated(col)))
    col_matches <- cn %in% col
  }

  if (missing(row)) {
    row_matches <- rep(TRUE, length(attr(x, "names")))
  } else {
    util_stop_if_not(!any(duplicated(row)))
    # row_matches <- rn %in% c(row, "[ALL]") # TODO: Ensure, that only one match is found (int_ should return [all] but nothing else, com_, eg, vv)
    row_matches <- rn %in% row
  }

  matches <- row_matches & col_matches

  r <- .access_dq_rs2(x, matches)
  rcn <- cn[matches]
  rrn <- rn[matches]
  if (!missing(col)) {
    first_order <- ordered(rcn, col)
  } else {
    first_order <- seq_along(rcn)
  }
  if (!missing(row)) {
    second_order <- ordered(rrn, row)
  } else {
    second_order <- seq_along(rrn)
  }
  r <- r[order(first_order, second_order)]

  if (!missing(res) && length(res)) {
    util_stop_if_not(!any(duplicated(res)))
    errors <- lapply(r, attr, "error")
    errors <- vapply(errors, length, FUN.VALUE = integer(1)) > 0
    r[!errors] <- lapply(r[!errors], `[`, res, drop = drop)
    # if (drop) r[!errors] <- lapply(r[!errors], `[[`, 1)
    # if (drop && length(r) == 1) { r <- r[[1]] }
    want_combine <- TRUE
  } else {
    want_combine <- FALSE
  }

  if (want_combine) {
    r <- r[!vapply(r, function(rs) {
      all(vapply(rs, is.null, FUN.VALUE = logical(1)))
    }, FUN.VALUE = logical(1))]
    if (length(r) > 0 &&
        !all(vapply(r, FUN.VALUE = logical(1),
                    function(x) {
                      all(vapply(x,
                                 function(y) {
                                   length(y) == 0
                                 }, FUN.VALUE = logical(1)))
                    }))) {
      if (!util_is_try_error(try(r <- util_combine_res(r), silent = TRUE))) {
        if (length(r) > 1) {
          for (i in seq_along(r)) {
            class(r[[i]]) <- unique(c("master_result",
                                      "dataquieR_result",
                                      class(r[[i]])))
          }
          # class(r) <- unique(c("dataquieR_result", class(r)))
        } else {
          r <- util_dataquieR_result(r[[1]])
        }
      }
    } else {
      class(r) <- union(c("dataquieR_NULL", "dataquieR_result"), class(r))
    }
  }

  if (length(r) == 1 && drop) {
    r <- r[[1]]
  }

  return(r)

}

#' Access elements from a `dataquieR_resultset2`
#'
#' does so, but similar to `[` for lists.
#'
#' @param x the `dataquieR_resultset2`
#' @param els the selector (character, number or logical)
#'
#' @return the sub-list of `x`
#' @keywords internal
.access_dq_rs2 <- function(x, els) {
  if (is.character(els)) {
    lapply(setNames(nm = els), function(el) x[[el]])
  } else if (is.numeric(els)) {
    lapply(setNames(nm = names(x)[els]), function(el) x[[el]])
  } else if (is.logical(els)) {
    Recall(x, which(els))
  } else {
    util_error(
      c("Access to report can use numbers, logical vectors or names as index,",
        "but not %s"), util_pretty_vector_string(class(els)))
  }
}

#' Write elements from a `dataquieR_resultset2`
#'
#' does so, but similar to `[` for lists.
#'
#' @param x the `dataquieR_resultset2`
#' @param els the selector (character, number or logical)
#' @param value `dataquieR_result` to write
#'
#' @return the modified `x`
#' @keywords internal
`.access_dq_rs2<-` <- function(x, els, value) {
  if (is.logical(els)) {
    els <- which(els)
  }
  if (is.character(els)) {
    values <- rep(value, length.out = length(els))
    for (el in seq_along(els)) {
      x[[els[[el]]]] <- values[[el]]
    }
  } else if (is.numeric(els)) {
    values <- rep(value, length.out = length(els))
    for (el in seq_along(els)) {
      x[[els[[el]]]] <- values[[el]]
    }
  } else {
    util_error(
      c("Access to report can use numbers, logical vectors or names as index,",
        "but not %s"), util_pretty_vector_string(class(els)))
  }
  return(x)
}
