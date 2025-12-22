# Makes the manual accessible at run-time
#' Holds parts of the manual at run-time
#' @noRd
..manual <- new.env(parent = emptyenv())

# Makes Indicator .//. Descriptor information
# from the manual accessible at run-time
#' Holds Indicator .// Descriptor assignments from the manual at run-time
#' @noRd
..indicator_or_descriptor <- new.env(parent = emptyenv())

#' @name Indicator
#' @title Indicator Function
#' @description
#' A function that returns some value that correlates with the magnitude of
#' a certain class of data quality problems. Typically, in `dataquieR`, such
#' functions return a `SummaryTable` that features columns with names, that
#' start with a short abbreviation that describes the specific semantics of
#' the value (e.g., `PCT` for a percentage or `COR` for a correlation) and
#' the public name of the indicator according to the data quality concept
#' `DQ_OBS`, e.g., `com_qum_nonresp` for item-non-response-rate. A name could
#' therefore be `PCT_com_qum_nonresp`.
#'
#' The object `Indicator` only contains the name used internally to tag
#' such functions.
#'
#' @docType data
#' @keywords internal
#' @seealso [Descriptor]
Indicator <- "Indicator"

#' @name Descriptor
#' @title Descriptor Function
#' @description
#' A function that returns some figure or table to assess data quality, but it
#' does not return a value correlating with the magnitude of a data quality
#' problem. It's the opposite of an [Indicator].
#'
#' The object `Descriptor` only contains the name used internally to tag
#' such functions.
#'
#' @docType data
#' @keywords internal
#' @seealso [Indicator]
Descriptor <- "Descriptor"


#' being called by the active binding function for .manual
#'
#' @param rebuild rebuild the cache
#' @param target file for `..manual`
#' @param target2 file for `..indicator_or_descriptor`
#' @param man_hash internal use: hash-sum over the manual to prevent rebuild
#'                 if not changed.
#'
#' @family reporting_functions
#' @concept system
#' @noRd
util_load_manual <- function(rebuild = FALSE,
                             target = "inst/manual.RData",
                             target2 = "inst/indicator_or_descriptor.RData",
                             man_hash = "") {
  if (!rebuild) {
    .manual_file <-
      system.file("manual.RData", package = "dataquieR")
    .indicator_or_descriptor_file <-
      system.file("indicator_or_descriptor.RData", package = "dataquieR")
    if (file.exists(.manual_file) &&
        file.exists(.indicator_or_descriptor_file)) {
      if ((!util_is_try_error(try(load(.manual_file, envir = ..manual),
                                 silent = TRUE))) &&
           !util_is_try_error(try(load(.indicator_or_descriptor_file, envir =
                                       ..indicator_or_descriptor),
                               silent = TRUE)))

        return()
    }
  }
  is_dev_package <- function() FALSE
  if (suppressWarnings(util_ensure_suggested("pkgload", err = FALSE,
                                             goal =
                                             "provide help on report sections during development",
                                             and_import = "is_dev_package"))) {
    dev_package <- is_dev_package(utils::packageName())
  } else {
    dev_package <- FALSE
  }
  ind_fkts <- setNames(nm =
             ls(envir = parent.env(environment()),
                pattern = "^(des|int|com|con|acc)_"))
  if (FALSE && dev_package) {
    ..manual$rd_objects <- lapply(ind_fkts,
                                 function(fn) eval(call("?", as.symbol(fn))))
  } else {
    if (suppressWarnings(util_ensure_suggested("Rdpack", err = FALSE,
                                               goal =
                                               "provide help on report sections",
    ))) {
      ..manual$rd_objects <- lapply(ind_fkts,
                                   function(f) {
                                     o <- Rdpack::Rdo_fetch(f,
                                                          utils::packageName(),
                                                            installed = FALSE)
                                     list(#path = attr(o, "Rdfile"),
                                          doc = o)
                                   })
    } else {
      ..manual$rd_objects <- lapply(ind_fkts,
                                   function(f) list(
                                     # path = "",
                                     doc = NULL))
    }
  }


  # docs <- lapply(..manual$rd_objects, function(rdo) {
  #   if (!!length(rdo) &&
  #       "path" %in% names(rdo) &&
  #       length(rdo[["path"]]) == 1 &&
  #       is.character(rdo[["path"]]) &&
  #       file.exists(rdo[["path"]])) {
  #       doc <- tools::parse_Rd(rdo[["path"]])
  #     list(
  #       doc = doc,
  #       rd_as_latex = rd_as_latex
  #     )
  #   } else {
  #     list(
  #       doc = NULL,
  #       rd_as_latex = NULL
  #     )
  #   }
  # })
  docs <- ..manual$rd_objects

  for (function_name in names(docs)) {
    assign(function_name,
           NA,
           envir = ..indicator_or_descriptor)
    doc <- docs[[function_name]]$doc

    is_descriptor <- FALSE
    is_indicator <- FALSE

    if (!is.null(doc)) {
      find_rd_tag_link <- function(el, parent = NULL) {
        if (!is.null(attr(el, "Rd_tag"))) {
          if (identical(attr(el, "Rd_tag"), "TEXT") &&
              identical(attr(parent, "Rd_tag"), "\\link") &&
              length(el) == 1 &&
              !is.na(el) &&
              is.character(el)) {
            if (el %in% c("Indicator", "Descriptor")) {
              return(el)
            }
          }
        }
        if (!identical(el, parent)) {
          return(lapply(el, find_rd_tag_link, parent = el))
        } else {
          return(NULL)
        }
      }
      ind_or_desc <- unlist(find_rd_tag_link(doc))
      is_indicator <- "Indicator" %in% ind_or_desc
      is_descriptor <- "Descriptor" %in% ind_or_desc
    }

    if (is_indicator && is_descriptor) {
      util_warning(
        c("Internal error, sorry. Please report: For function %s,",
          "the information, if it is an",
          "Indicator or a Descriptor is set to both in the",
          "Roxygen2 comments. Will classify it as a %s, for now."),
        sQuote(function_name),
        sQuote(Descriptor)
      )
      assign(function_name,
             FALSE,
             envir = ..indicator_or_descriptor)
    } else if (!is_indicator && !is_descriptor) {
      if (length(doc) > 0) {
        util_warning(
          c("Internal error, sorry. Please report: For function %s,",
            "the information, if it is an",
            "Indicator or a Descriptor is not correctly available in the",
            "Roxygen2 comments. Will classify it as a %s, for now."),
          sQuote(function_name),
          sQuote(Descriptor)
        )
      }
      assign(function_name,
             FALSE,
             envir = ..indicator_or_descriptor)
    } else {
      assign(function_name,
             is_indicator,
             envir = ..indicator_or_descriptor)
    }
  }

  ..manual$descriptions <- lapply(setNames(nm = names(docs)),
                                  function(function_name) {
    doc <- docs[[function_name]]$doc
    if (!is.null(doc)) {
      descr <- paste(unlist(doc[lapply(doc, attr, "Rd_tag") == "\\description"]),
                     collapse = "")
      descr <- trimws(gsub("\\s+", " ", descr))
    } else {
      descr <- ""
    }
    descr
  })

  ..manual$titles <- lapply(setNames(nm = names(docs)),
                            function(function_name) {
      doc <- docs[[function_name]]$doc
      if (!is.null(doc)) {
        title <- paste(unlist(doc[lapply(doc, attr, "Rd_tag") == "\\title"]),
                       collapse = "")
        title <- trimws(gsub("\\s+", " ", title))
      } else {
        title <- ""
      }
      title
  })
  if (rebuild) {
    ..manual$.man_hash <- man_hash
    save(list = ls(envir = ..manual), envir = ..manual, file = target,
         compress = "xz")
    ..indicator_or_descriptor$.man_hash <- man_hash
    save(list = ls(envir = ..indicator_or_descriptor),
         envir = ..indicator_or_descriptor, file = target2,
         compress = "xz")
  }
}
