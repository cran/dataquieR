#' Checks for element set
#'
#' Depends on `dataquieR.ELEMENT_MISSMATCH_CHECKTYPE` option,
#' see there -- # TODO: Rind out, how to document and link
#' it here using `Roxygen`.
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the unexpected elements check results.
#'                       - `Segment`: name of the corresponding segment,
#'                                    if applicable, `ALL` otherwise
#'   - `SegmentTable`: data frame with the unexpected elements check results, used for the data quality report.
#'                       - `Segment`: name of the corresponding segment,
#'                                    if applicable, `ALL` otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Ex"))
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Intro"))
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(VAR_NAMES = c("speed", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Intro"))
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' }
int_sts_element_segment <- function(study_data,
                                    item_level = "item_level",
                                    label_col,
                                    meta_data = item_level,
                                    meta_data_v2) {
  util_maybe_load_meta_data_v2()
  # prevent error that we have item_level and meta_data
  # if prep_prepare_dataframes was not called but both objects
  # are not missing any more, when prep_prepare_dataframes is called below.
  invisible(prep_prepare_dataframes(.label_col = VAR_NAMES,
                                    .allow_empty = TRUE))
  meta_data <- prep_meta_data_v1_to_item_level_meta_data(
    meta_data = meta_data, verbose = FALSE, label_col = VAR_NAMES)
  util_expect_data_frame(meta_data, c(VAR_NAMES))
  if (!STUDY_SEGMENT %in% colnames(meta_data)) {
    meta_data[[STUDY_SEGMENT]] <- "<NO SEGMENT>"
  }
  if (any(meta_data$STUDY_SEGMENT == "ALL")) { # TODO: Use [[STUDY_SEGMENT]]
    util_message("No segment should be named %s, renaming it.",
                 dQuote("ALL"), applicability_problem = TRUE)
    meta_data$STUDY_SEGMENT[meta_data$STUDY_SEGMENT == "ALL"] <-
      "RENAMED SEGMENT: ALL"
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

  util_purge_study_data_cache()

  suppressMessages(
    suppressWarnings(
      withCallingHandlers(
        without_pipeline(
          prep_prepare_dataframes(.study_data = study_data,
                                  .meta_data = meta_data,
                                  .label_col = VAR_NAMES, .allow_empty = TRUE,
                                  .internal = TRUE)
        ),
        message = register_cond,
        warning = register_cond
      )
    )
  )

  msgs <- vapply(e$conds, inherits, "message", FUN.VALUE = logical(1))
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
             PCT_int_sts_element = round(as.numeric(gsub("^.*?([0-9,.]+)%.*$", "\\1",
                                                   vapply(e$conds[wrns],
               conditionMessage,
               FUN.VALUE = character(1)))), 2))

  abs <- data.frame(stringsAsFactors = FALSE,
                    Segment = rep("ALL", sum(msgs)), # TODO: If segment-wiese, how to tell the segemnt of an uexpected study variable
                    DESCRIPTION = vapply(e$conds[msgs],
                                         conditionMessage,
                                         FUN.VALUE = character(1)),
                    MISSING = ifelse(grepl("Did not find any metadata", vapply(e$conds[msgs],
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

  segsizes <- util_table_of_vct(
    meta_data[meta_data[[VAR_NAMES]] %in% colnames(study_data),
              STUDY_SEGMENT])
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

    if (a$MISSING == "study data") {
      # prep_get_labels....... for known variables missing from the study data
      rvs <- prep_get_labels(
            resp_vars = x$resp_vars,
            item_level = meta_data,
            label_col = LABEL,
            label_class = "SHORT",
            resp_vars_are_var_names_only = TRUE
        )
    } else {
      rvs <- x$resp_vars
    }
    rvs <- sort(rvs)
    if (length(names(rvs)) == 0) {
      rvs <- prep_deparse_assignments(mode = "string_codes",
                                      rvs)
    } else {
      rvs <- prep_deparse_assignments(mode = "string_codes",
                                      names(rvs),
                                      rvs)
    }

    if (all(x$Segment != "ALL")) {
      xl <- split(x, x$Segment)
      xl <- lapply(xl, function(x) {
        x$NUM_int_sts_element <- nrow(x)
        x$PCT_int_sts_element <- nrow(x) / segsizes[unique(x$Segment)]
        x$resp_vars <- rvs
        x[!duplicated(x), , FALSE]
      })
      x <- do.call(rbind.data.frame, c(xl, list(
        make.row.names = FALSE,
        stringsAsFactors = FALSE)))
    } else {
      if (any(x$Segment != "ALL")) {
        util_error(c("Unexpected error for Segments: found a mixture of",
                   "segments. Internal error in dataquieR, please report."))
      }
      x <- data.frame(stringsAsFactors = FALSE,
                      Segment = "ALL",
                      MISSING = paste(unique(x$MISSING), collapse = ", "),
                      DESCRIPTION = paste(unique(x$DESCRIPTION),
                                          collapse = ", "),
                      NUM_int_sts_element = nrow(x),
                      PCT_int_sts_element = max(x$PCT_int_sts_element),
                      resp_vars = rvs
      )
    }
    x
  })

  SegmentTable <- do.call(rbind.data.frame,
                          c(rrr, list(
                            make.row.names = FALSE,
                            stringsAsFactors = FALSE)))

  SegmentTable$DESCRIPTION <- NULL

  # segments <- unique(names(segsizes), "ALL")

  segments <- setdiff(meta_data$STUDY_SEGMENT, SegmentTable$Segment)

  # add all segments that did not have a problem ----
  complement <- data.frame(Segment = segments,
                           MISSING = rep(NA_character_, length(segments)),
                           PCT_int_sts_element = rep(0, length(segments)),
                           NUM_int_sts_element = rep(0, length(segments)),
                           resp_vars = rep(NA_character_, length(segments)))

  SegmentTable <- rbind(complement, SegmentTable)

  SegmentData <- SegmentTable
  names(SegmentData) <- c("Segment",
                          "Missing",
                          "Percentage of unexpected elements",
                          "Number of unexpected elements",
                          "Response variables")

  list(SegmentData = SegmentData,
       SegmentTable = SegmentTable)

}
