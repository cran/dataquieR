#' Extract report summary from reports
#'
#' @param r [dataquieR_result] a result from a[dq_report2] report
#' @param ... not used
#'
#' @return [list] with two slots `Data` and `Table` with [data.frame]s
#'                featuring all metrics columns
#'                from the report `r`, the [STUDY_SEGMENT] and the [VAR_NAMES].
#'                In case of `Data`, the columns are formatted nicely but still
#'                with the standardized column names -- use
#'                [util_translate_indicator_metrics()] to rename them nicely. In
#'                case of `Table`, just as they are.
#' @family summary_functions
#' @seealso [prep_combine_report_summaries()]
#' @export
prep_extract_summary.dataquieR_result <- function(
    r,
    ...
    ) {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_extract_summary.dataquieR_result()")
  }
  # TODO: Tidy up, and remove all left-overs from the copy from prep_extract_summary.dataquieR_resultset2
  util_stop_if_not(
    "Can only be called for results objects from a dq_report2 of class dataquieR_result" =
      inherits(r, "dataquieR_result"))

  lb <- attr(attr(r, "call"), "entity_name") # this exists for long

  try(
    rule_set <- attr(attr(r, "call"), GRADING_RULESET),
    silent = TRUE)
  if (is.null(rule_set)) rule_set <- 0

  try(
    v0 <- attr(attr(r, "call"), VAR_NAMES),
  silent = TRUE)
  if (is.null(v0)) v0 <- lb

  try(
    label_col <- attr(attr(r,"call"), "label_col"),
    silent = TRUE)
  if (is.null(label_col)) label_col <- LABEL

  sts <-
    lapply(setNames(nm = v0), function(v) {
      all_cll <- lapply(setNames(nm = attr(r, "cn")), function(cll) {
        st <- r[["SummaryTable"]] # TODO: SegmentTable, ...
        if (is.data.frame(st)) {
          st <- util_extract_indicator_metrics(st)
          if (nrow(st) == 1 && ncol(st) > 0) return(st)
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

  sseg <- attr(attr(r,"call"), STUDY_SEGMENT)

  if (is.null(sseg)) sseg <- "Study"

  res[[STUDY_SEGMENT]] <- rep(sseg, nrow(res))
  res[[VAR_NAMES]] <- rep(v0, nrow(res))
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

  meta_data <- data.frame(
    VAR_NAMES = v0,
    GRADING_RULESET = rule_set,
    STUDY_SEGMENT = sseg
  )

  meta_data[[label_col]] <- lb

  r <- list(Data = res, Table = res_raw, meta_data = meta_data)
  class(r) <- "dq_report2_summary"
  r
}
