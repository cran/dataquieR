#' Determine missing and/or superfluous data elements
#'
#' Depends on `dataquieR.RECORD_MISSMATCH_CHECKTYPE` option,
#' see there -- # TODO: Rename this option and find out, how to document and link
#' it here using `Roxygen`.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @return [list] with names lots:
#'   - `DataframeData`: data frame with the unexpected elements check results.
#'   - `DataframeTable`: [data.frame] table with all errors, used for the data quality report:
#'                       - `MISSING`: `meta_data` or `study_data`: where is
#'                                    the element missing
#'                       - `PCT_int_sts_element`: Percentage of element
#'                                                mismatches
#'                       - `NUM_int_sts_element`: Number of element
#'                                                mismatches
#'                       - `resp_vars`: affected element names
#'
#' @export
int_sts_element_dataframe <- function(study_data,
                                      meta_data = "item_level") { # TODO: by far too complicated.
  # TODO: also differntly implemetned from int_sts_element_segment -- other loop level!!
  util_expect_data_frame(meta_data, c(VAR_NAMES))
  if (STUDY_SEGMENT %in% colnames(meta_data) && any(meta_data$STUDY_SEGMENT == "ALL")) {
    util_warning("No segment should be named %s, renaming it.",
                 dQuote("ALL"))
    meta_data$STUDY_SEGMENT[meta_data$STUDY_SEGMENT == "ALL"] <-
      "RENAMED SEGMENT: ALL"
  }

  if (!(STUDY_SEGMENT %in% colnames(meta_data))) {
    meta_data$STUDY_SEGMENT <- "ALL"
  }

  Q <- substr(dQuote(""), 1, 1)
  E <- substr(dQuote(""), 2, 2)
  e <- new.env(parent = emptyenv())
  e$conds <- list()
  register_cond <- function(cond) {
    if (identical(
      attr(cond, "integrity_indicator"), "int_sts_element")) {
      e$conds <- c(e$conds, list(cond))
    }
  }

  .orig_meta <- meta_data

  suppressMessages(
    suppressWarnings(
      withCallingHandlers(
        prep_prepare_dataframes(.label_col = VAR_NAMES, .allow_empty = TRUE),
        message = register_cond,
        warning = register_cond
      )
    )
  )

  meta_data <- .orig_meta

  msgs <- vapply(e$conds, inherits, "simpleMessage", FUN.VALUE = logical(1))
  wrns <- vapply(e$conds, inherits, "warning", FUN.VALUE = logical(1))

  pct <- data.frame(stringsAsFactors = FALSE,
             Segment = rep("ALL", sum(wrns)), # TODO: If segment-wiese, how to tell the segemnt of an uexpected study variable
             DESCRIPTION = vapply(e$conds[wrns],
                                  conditionMessage,
                                  FUN.VALUE = character(1)),
             MISSING = ifelse(grepl("of the study data", vapply(e$conds[wrns],
                                                                conditionMessage,
                                                                FUN.VALUE = character(1)), fixed = TRUE),
                              "metadata",
                              "study data"
                              ),
             PCT_int_sts_element = as.numeric(gsub("^.*?([0-9,.]+)%.*$", "\\1",
                                                   vapply(e$conds[wrns],
               conditionMessage,
               FUN.VALUE = character(1)))))

  abs <- data.frame(stringsAsFactors = FALSE,
                    Segment = rep("ALL", sum(msgs)), # TODO: If segment-wiese, how to tell the segemnt of an uexpected study variable
                    DESCRIPTION = vapply(e$conds[msgs],
                                         conditionMessage,
                                         FUN.VALUE = character(1)),
                    MISSING = ifelse(grepl("Did not find any meta data", vapply(e$conds[msgs],
                                                                                conditionMessage,
                                                                                FUN.VALUE = character(1)), fixed = TRUE),
                                     "metadata",
                                     "study data"
                    ),
                    NUM_int_sts_element = vapply(gsub(sprintf("[^%s]", Q), "", vapply(e$conds[msgs],
                                                                                                             conditionMessage,
                                                                                                             FUN.VALUE = character(1))), nchar, FUN.VALUE = integer(1))
  )

  r <- merge(pct, abs, by = c("Segment", "MISSING"), suffixes = c("p", "a"))

  r$DESCRIPTION <- paste(r$DESCRIPTIONp, r$DESCRIPTIONa)
  r$DESCRIPTIONa <- NULL
  r$DESCRIPTIONp <- NULL

  vars <- util_extract_matches(
    r$DESCRIPTION,
    paste0(Q, ".+?", E)
  )

  vars <- lapply(
    vars,
    gsub,
    pattern = paste0("^", Q, collapse = ""),
    replacement = ""
  )

  vars <- lapply(
    vars,
    gsub,
    pattern = paste0(E, "$", collapse = ""),
    replacement = ""
  )

  rr <- lapply(seq_len(nrow(r)), function(x) r[x, , FALSE])

  segsizes <- util_table_of_vct(meta_data$STUDY_SEGMENT) # TODO: Use [[STUDY_SEGMENT]]
  segsizes <- setNames(segsizes$Freq, nm = segsizes$Var1)

  rrr <- mapply(rr, vars, SIMPLIFY = FALSE, FUN = function(a, v) {
    x <- merge(a, v, by = NULL)
    x$Segment <- prep_map_labels(x = v,
                                  meta_data = meta_data,
                                  to = STUDY_SEGMENT,
                                  ifnotfound = NA_character_)

    x$Segment <- ifelse(is.na(x$Segment), "ALL", x$Segment)
    x$resp_vars <- x$y
    x$y <- NULL

    SPLIT_STRING <- paste0(" ", SPLIT_CHAR, " ")

    if (all(x$Segment != "ALL")) {
      xl <- split(x, x$Segment)
      xl <- lapply(xl, function(x) {
        x$NUM_int_sts_element <- nrow(x)
        x$PCT_int_sts_element <- nrow(x) / segsizes[unique(x$Segment)]
        x$resp_vars <- paste(sort(unique(x$resp_vars)),
                             collapse = SPLIT_STRING)
        x[!duplicated(x), , FALSE]
      })
      x <- do.call(rbind.data.frame, c(xl, list(
        make.row.names = FALSE,
        stringsAsFactors = FALSE)))
    } else {
      if (any(x$Segment != "ALL")) {
        util_error(c("Unexpected error for segments: found a mixture of",
                   "segements. Internal error in dataquieR, please report."))
      }
      x <- data.frame(stringsAsFactors = FALSE,
                      Segment = "ALL",
                      MISSING = paste(unique(x$MISSING), collapse = SPLIT_STRING),
                      DESCRIPTION = paste(unique(x$DESCRIPTION),
                                          collapse = SPLIT_STRING),
                      NUM_int_sts_element = nrow(x),
                      PCT_int_sts_element = max(x$PCT_int_sts_element),
                      resp_vars = paste(sort(unique(x$resp_vars)),
                                        collapse = SPLIT_STRING)
      )
    }
    x
  })

  DataframeTable <- do.call(rbind.data.frame,
                          c(rrr, list(
                            make.row.names = FALSE,
                            stringsAsFactors = FALSE)))
  DataframeTable$DESCRIPTION <- NULL

  if (!prod(dim(DataframeTable))) {
    DataframeTable <- data.frame(
      # Segment = character(0),
      MISSING = character(0),
      PCT_int_sts_element = numeric(0),
      NUM_int_sts_element = integer(0),
      resp_vars = character(0))
  }

  DataframeTable$Segment <- NULL

  DataframeData <- DataframeTable
  names(DataframeData) <- c("Missing",
                            "Number of unexpected elements",
                            "Percentage of unexpected elements",
                            "Response variables")

  list(DataframeData = DataframeData,
       DataframeTable = DataframeTable)

}
