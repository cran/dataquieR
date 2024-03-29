#' Copy default dependencies to the report's lib directory
#'
#' @param dir report directory
#' @param pages all pages to write
#' @param ... additional `htmltools::htmlDependency` objects to be added to all
#'            pages, also
#'
#' @return `invisible(NULL)`
#'
#' @family reporting_functions
#' @concept process
#' @keywords internal
util_copy_all_deps <- function(dir, pages, ...) {

  libdir <- file.path(dir, "lib")

  withCallingHandlers({
    rendered_pages <- lapply(pages, htmltools::renderTags)
  },
  warning = function(cond) { # suppress a waning caused by ggplotly for barplots
    if (startsWith(conditionMessage(cond),
                   "'bar' objects don't have these attributes: 'mode'")) {
      invokeRestart("muffleWarning")
    }
  })

  deps <- c(lapply(rendered_pages, `[[`, "dependencies"),
            GLOBAL_ = list(list(...)))

  deps_cnt <- lapply(deps, length)

  pos_in_file <- lapply(deps_cnt, seq_len)

  deps <- unlist(deps, recursive = FALSE)

  deps_info <- data.frame(index = seq_len(length(deps)),
                          file_name =
                            unlist(lapply(names(deps_cnt), function(nm) rep(nm, deps_cnt[[nm]]))),
                          pos_in_file = unlist(pos_in_file))
  deps_info$version <- vapply(deps, `[[`, "version", FUN.VALUE = character(1))
  deps_info$name <- vapply(deps, `[[`, "name", FUN.VALUE = character(1))
  deps_info$take <- FALSE

  # Omit older version, if libraries are duplicated
  deps_info <- split(deps_info, deps_info$name)

  deps_info <- lapply(deps_info, function(x) {
    r <- x[order(numeric_version(x$version),
                 decreasing = TRUE,
                 na.last = TRUE), , FALSE]
    r[1, "take"] <- TRUE
    r
  })

  deps_info <- do.call(rbind, deps_info)

  # overall order of dependencies

  n_deps <- length(unique(deps_info$name))

  # find dep, which is either not in a file or at first position

  order_of_deps <- unique(unname(unlist(lapply(setNames(nm = seq_len(n_deps)), function(pos) {
    lapply(setNames(nm = unique(deps_info$name)), function(nm) {
      r <- deps_info[deps_info$name == nm & deps_info$pos_in_file > pos, ,
                     FALSE]
      if (nrow(r)) {
        NULL
      } else {
        nm
      }
    })
  }))))


  index <- deps_info[deps_info$take, "index", TRUE]

  deps <- deps[index]

  names(deps) <- vapply(deps, `[[`, "name", FUN.VALUE = character(1))

  deps <- deps[order_of_deps]

  deps <-
    lapply(deps,
           htmltools::copyDependencyToDir,
           outputDir = libdir,
           mustWork = TRUE) # no external http-dependencies allowed (mustWork)

  deps <-
    lapply(deps,
           htmltools::makeDependencyRelative,
           basepath = dir,
           mustWork = TRUE) # no external http-dependencies allowed (mustWork)

  list(deps = htmltools::renderDependencies(deps, "file"), # TODO: What, if two versions of the same lib are used (in acc_acc_distributions_observer, we have an old jQuery injected by plotly)
       rendered_pages = rendered_pages)
}
