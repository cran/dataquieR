#' Extract report summary from reports
#'
#' @param r [dq_report2] a [dq_report2] report
#' @param ... not used
#'
#' @return [list] with two slots `Data` and `Table` with [data.frame]s
#'                featuring all metrics columns
#'                from the report `r`, the [STUDY_SEGMENT] and the [VAR_NAMES].
#'                In case of `Data`, the columns are formatted nicely but still
#'                with the standardized column names -- use
#'                `util_translate_indicator_metrics()` to rename them nicely. In
#'                case of `Table`, just as they are.
#' @family summary_functions
#' @seealso [prep_combine_report_summaries()]
#' @export
prep_extract_summary.dataquieR_resultset2 <- function(
    r,
    ...
    ) {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_extract_summary.dataquieR_resultset2()",
                              "summary()")
  }
  util_stop_if_not(
    "Can only be called for dq_report2 objects of class dataquieR_resultset2" =
      inherits(r, "dataquieR_resultset2"))
  sts <-
    lapply(setNames(nm = rownames(r)), function(v) {
      all_cll <- lapply(setNames(nm = colnames(r)), function(cll) {
        st <- r[v, cll, res = "SummaryTable", drop = TRUE] # TODO: SegmentTable, ...
        if (is.data.frame(st)) {
          st <- util_extract_indicator_metrics(st)
          if (nrow(st) == 1 && ncol(st) > 0) {
            return(st)
          }
        }
        NULL
      })
      all_cll[vapply(all_cll, is.null, FUN.VALUE = logical(1))] <- NULL
      all_cll
    })
  sts[vapply(sts, length, FUN.VALUE = integer(1)) == 0] <- NULL
  sts <- lapply(sts, function(st) {
    st <- lapply(names(st), function(stnm) {
      colnames(st[[stnm]]) <-
        paste(stnm, colnames(st[[stnm]]), sep = ".")
      st[[stnm]]
    })
    r <- do.call(cbind, st)
    r
  })
  res <- util_rbind(data_frames_list = sts)


  if (STUDY_SEGMENT %in% colnames(attr(r, "meta_data"))) {
    res[[STUDY_SEGMENT]] <-
      prep_map_labels(
        rownames(res),
        meta_data = attr(r, "meta_data"),
        from = attr(r, "label_col"),
        to = STUDY_SEGMENT)
    if (length(res[[STUDY_SEGMENT]]) == 0)
      res[[STUDY_SEGMENT]] <- character(0)
  } else {
    res[[STUDY_SEGMENT]] <- rep("Study", nrow(res))
  }

  if (length(rownames(res)) == 0) {
    res[[VAR_NAMES]] <-
      character(0)
  } else {
    res[[VAR_NAMES]] <-
      prep_map_labels(
        rownames(res),
        meta_data = attr(r, "meta_data"),
        from = attr(r, "label_col"),
        to = VAR_NAMES)
  }

  res_raw <- res

  counts <- vapply(colnames(res), FUN.VALUE = logical(1),
                   FUN = function(x) {
                     util_stop_if_not(length(x) == 1)
                     x <- sub("^[^\\.]+\\.", "", x)
                     nm <- strsplit(x, "_", fixed = TRUE)[[1]]
                     if (length(nm) >= 2) {
                       identical(nm[[1]], "NUM")
                     } else {
                       FALSE
                     }
                   })
  res_raw[, counts] <- lapply(res[, counts, FALSE], as.numeric)
  res[, counts] <- lapply(lapply(res[, counts, FALSE], as.numeric),
                          scales::number, accuracy = 1)

  percentages <- vapply(colnames(res), FUN.VALUE = logical(1),
                        FUN = function(x) {
                          util_stop_if_not(length(x) == 1)
                          x <- sub("^[^\\.]+\\.", "", x)
                          nm <- strsplit(x, "_", fixed = TRUE)[[1]]
                          if (length(nm) >= 2) {
                            identical(nm[[1]], "PCT")
                          } else {
                            FALSE
                          }
                        })
  res_raw[, percentages] <- lapply(res_raw[, percentages, FALSE], as.numeric)
  res[, percentages] <- lapply(lapply(res[, percentages, FALSE], as.numeric),
                               scales::percent, , scale = 1, accuracy = 0.01)

  flags <- vapply(colnames(res), FUN.VALUE = logical(1),
                  FUN = function(x) {
                    util_stop_if_not(length(x) == 1)
                    x <- sub("^[^\\.]+\\.", "", x)
                    nm <- strsplit(x, "_", fixed = TRUE)[[1]]
                    if (length(nm) >= 2) {
                      identical(nm[[1]], "FLG")
                    } else {
                      FALSE
                    }
                  })
  res_raw[, flags] <- lapply(res_raw[, flags, FALSE], as.logical)
  res[, flags] <- lapply(lapply(res[, flags, FALSE], as.logical),
                         ifelse, "T", "F")

  res <- res[, sort(colnames(res))]
  res_raw <- res_raw[, sort(colnames(res_raw))]

  r <- list(Data = res, Table = res_raw, meta_data = attr(r, "meta_data"))
  class(r) <- "dq_report2_summary"
  r
}
