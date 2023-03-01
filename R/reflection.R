# Makes the manual accessible at run-time
#' Holds parts of the manual at run-time
..manual <- new.env(parent = emptyenv())

#' being called by the active binding function for .manual
util_load_manual <- function() {
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
                pattern = "^(int|com|con|acc)"))
  if (dev_package) {
    ..manual$rd_objects <- lapply(ind_fkts,
                                 function(fn) eval(call("?", as.symbol(fn))))
  } else {
    if (suppressWarnings(util_ensure_suggested("Rdpack", err = FALSE,
                                               goal =
                                               "provide help on report sections",
    ))) {
      ..manual$rd_objects <- lapply(ind_fkts,
                                   function(f) {
                                     list(path = attr(Rdpack::Rdo_fetch(f,
                                                            utils::packageName(),
                                                            installed = FALSE),
                                          "Rdfile"))
                                   })
    } else {
      ..manual$rd_objects <- lapply(ind_fkts,
                                   function(f) list(path = ""))
    }
  }
  ..manual$descriptions <- lapply(..manual$rd_objects, function(rdo) {
    if (!!length(rdo) && "path" %in% names(rdo) &&
        length(rdo[["path"]]) == 1 && is.character(rdo[["path"]])) {
      if (file.exists(rdo[["path"]])) {
        doc <- tools::parse_Rd(rdo[["path"]])
        descr <- paste(unlist(doc[lapply(doc, attr, "Rd_tag") == "\\description"]),
                       collapse = "")
        descr <- trimws(gsub("\\s+", " ", descr))
      } else {
        descr <- ""
      }
      descr
      # htmltools::HTML(capture.output(tools::Rd2HTML(rdo$path)))
      # html <- paste0(capture.output(tools::Rd2HTML(rdo$path)),
      #                collapse = "\n")
      # xml2::read_html(html)
    }
  })

  ..manual$titles <- lapply(..manual$rd_objects, function(rdo) {
    if (!!length(rdo) && "path" %in% names(rdo) &&
        length(rdo[["path"]]) == 1 && is.character(rdo[["path"]])) {
      if (file.exists(rdo[["path"]])) {
        doc <- tools::parse_Rd(rdo[["path"]])
        title <- paste(unlist(doc[lapply(doc, attr, "Rd_tag") == "\\title"]),
                       collapse = "")
        title <- trimws(gsub("\\s+", " ", title))
      } else {
        title <- ""
      }
      title
      # htmltools::HTML(capture.output(tools::Rd2HTML(rdo$path)))
      # html <- paste0(capture.output(tools::Rd2HTML(rdo$path)),
      #                collapse = "\n")
      # xml2::read_html(html)
    }
  })
  # ..manual$rd_objects <- lapply(..manual$rd_objects, function(rdo) {
  #   htmltools::HTML(capture.output(tools::Rd2HTML(rdo$path)))
  # })
}
