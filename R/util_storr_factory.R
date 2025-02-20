#' Create a `storr` object with a `storr_factory` attribute
#'
#' also does basic validity checks
#'
#' @param my_storr_object a `storr`-object
#' @param my_storr_factory a function creating the/a `storr_object`
#'
#' @return `storr`-object with the factory attribute and (hopefully) valid.
#' @keywords internal
util_storr_factory <- function(my_storr_object, my_storr_factory) {
  if (!missing(my_storr_object) && is.null(my_storr_object)) {
    return(NULL)
  }
  if (!missing(my_storr_object) && missing(my_storr_factory)) {
    my_storr_factory <- attr(my_storr_object, "storr_factory")
  }
  if ((missing(my_storr_object) || is.null(my_storr_object)) &&
      (missing(my_storr_factory) || is.null(my_storr_factory))) {
    return(NULL)
  }
  if (!is.function(my_storr_factory) ||
      length(formals(my_storr_factory)) != 0) {
    # util_error("No storr factory")
    my_storr_factory <- function() {
      return(my_storr_object)
    }
  }
  if (missing(my_storr_object)) {
    my_storr_object <- my_storr_factory()
  }
  if (!inherits(my_storr_object, "storr")) {
    util_error("storr factory should return a storr object")
  }
  if (util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
    try(my_storr_object$driver$reconnect(), silent = TRUE)
  }
  if (util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
    my_storr_object <- my_storr_factory()
  }
  if (!inherits(my_storr_object, "storr")) {
    util_error("storr factory should return a storr object")
  }
  if (util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
    util_error("storr object not working")
  }
  attr(my_storr_object, "storr_factory") <- my_storr_factory
  if (!identical(try(my_storr_object$driver$type(), silent = TRUE),
            "rds")) {
    rlang::warn(
      "storr classes other than RDS not yet supported, expect errors.",
      .frequency = "regularly", .frequency_id = rlang::hash(my_storr_factory))
  }
  my_storr_object
}

#' Create a `storr`-object using the factory
#'
#' also performs checks.
#'
#' @param my_storr_factory a function returning a `storr` object
#'
#' @return a `storr` object
#' @keywords internal
util_storr_object <- function(my_storr_factory = function() {
  storr::storr_environment()
}) {
  util_storr_factory(my_storr_factory = my_storr_factory)
}

#' Get the `storr` object backing a report
#'
#' @param r the dataquieR_resultset2 / report
#'
#' @return the `storr` object holding the results or `NULL`, if the report
#'         lives in the memory, only
#' @keywords internal
util_get_storr_object_from_report <- function(r) {
  my_storr_object <- attr(r, "my_storr_object")
  storr_factory <- attr(my_storr_object, "storr_factory")
  util_storr_factory(my_storr_object, storr_factory)
}

#' Fix a `storr` object, if it features the factory-attribute
#'
#' @seealso [util_storr_factory()]
#'
#' @param my_storr_object a `storr`-object
#'
#' @return a (hopefully) working `storr_object`
#' @keywords internal
util_fix_storr_object <- function(my_storr_object) {
  storr_factory <- attr(my_storr_object, "storr_factory")
  util_storr_factory(my_storr_object, storr_factory)
}


# Example for thor:
# unlink("/tmp/thor", recursive = TRUE)
# r_rds <- dq_report2("study_data", meta_data_v2 = "meta_data_v2", dimensions = NULL, storr_factory = function(){thor::storr_thor(thor::mdb_env("/tmp/thor", mapsize =  2 * (2 ^ 10)^4))}, amend = T)


#' Create a factory function for `storr` objects for backing
#' a [dataquieR_resultset2]
#'
#' @param db_dir [character] path to the directory for the back-end, if
#'                           one is created on the fly.
#' @param namespace [character] namespace for the report, so that one back-end
#'                              can back several reports
#'
#' the returned function will try to create a `storr` object using a temporary
#' folder or the folder in `db_dir`, if specified. The database will either
#' be the `storr_rds`.
#'
#' @return `storr` object or `NULL`, if package `storr` is not available
#' @export
prep_create_storr_factory <- function(db_dir = tempfile(),
                                      namespace = "objects") {
  if (missing(db_dir)) {
    withr::defer_parent({
      if (dir.exists(db_dir) && startsWith(db_dir, tempdir())) {
        try(unlink(db_dir, recursive = TRUE, force = TRUE), silent = TRUE)
      }
    })
  }
  force(db_dir)
  force(namespace)
  function() {
    if (util_ensure_suggested("storr", err = FALSE)) { # TODO: store somewhere the full constructor call to reproduce.
      storr_factory <- function() {
        storr::storr_rds(db_dir, default_namespace =
                           namespace)
      }
      my_storr_object <- storr_factory()
      attr(my_storr_object, "storr_factory") <- storr_factory
    } else {
      my_storr_object <- NULL
    }
    my_storr_object
  }
}

