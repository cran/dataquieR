#' Encoding Errors
#'
#' @description
#' Detects errors in the character encoding of string variables
#'
#' [Indicator]
#'
#' @details
#' Strings are stored based on
#' [code tables](https://en.wikipedia.org/wiki/Character_encoding), nowadays,
#' typically as
#' [UTF-8](https://en.wikipedia.org/wiki/UTF-8). However, other code systems
#' are still in use, so, sometimes, strings from different systems are mixed
#' in the data. This indicator checks for such problems and returns the count
#' of entries per variable, that do not match the reference coding system, which
#' is estimated from the study data (addition of metadata field is planned).
#'
#' If not specified in the metadata (columns `ENCODING` in item- or data-frame-
#' level, the encoding is guessed from the data). Otherwise, it may be any
#' supported encoding as returned by `iconvlist()`.
#'
#' @inheritParams .template_function_indicator
#' @param ref_encs reference encodings (names are `resp_vars`)
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] with information on such problems
#'   - `SummaryData`: [data.frame] human readable version of `SummaryTable`
#'   - `FlaggedStudyData`: [data.frame] tells for each entry in study data if
#'                         its encoding is OK. has the same dimensions as
#'                         `study_data`

#' @export
int_encoding_errors <- function(resp_vars = NULL,
                                study_data,
                                label_col,
                                meta_data_dataframe =
                                  "dataframe_level",
                                item_level = "item_level",
                                ref_encs,
                                meta_data = item_level,
                                meta_data_v2,
                                dataframe_level) {


  util_maybe_load_meta_data_v2()
  util_first_arg_study_data_or_resp_vars()
  util_ck_arg_aliases()

  meta_data_dataframe <-
    try(prep_check_meta_data_dataframe(meta_data_dataframe), silent = TRUE)

  if (util_is_try_error(meta_data_dataframe)) {
    meta_data_dataframe <- data.frame()
  }

  util_expect_data_frame(study_data, keep_types =  TRUE)

  if (missing(label_col)) {
    label_col <- VAR_NAMES
  }

  if (!missing(ref_encs)) {
    util_expect_scalar(ref_encs, allow_more_than_one = TRUE)
  }

  meta_data <- try(util_expect_data_frame(meta_data), silent = TRUE)

  if (util_is_try_error(meta_data) || nrow(meta_data) == 0) {
    meta_data <- data.frame(
      VAR_NAMES = colnames(study_data),
      DATA_TYPE = prep_datatype_from_data(resp_vars = colnames(study_data),
                                          study_data =
                                            study_data)
    )
    meta_data[[label_col]] <- meta_data[[VAR_NAMES]]
  }

  util_expect_data_frame(meta_data, col_names = c(
    VAR_NAMES,
    label_col
  ))

  if (missing(resp_vars) || is.null(resp_vars)) {
    resp_vars <- colnames(study_data)
  }

  util_expect_scalar(resp_vars,
                     allow_more_than_one = TRUE,
                     error_message =
                       "Need either no resp_vars or valid variable names")

  rv <- util_find_var_by_meta(resp_vars,
                              meta_data = meta_data,
                              target = VAR_NAMES,
                              ifnotfound = NA,
                              label_col = label_col);
  rv <- rv[!is.na(rv)]
  study_data <- study_data[, rv, FALSE]
  resp_vars <- prep_get_labels(rv,
                               label_col = label_col,
                               item_level = meta_data,
                               label_class = "LONG",
                               resp_vars_are_var_names_only = TRUE)
  ds1_label_col <- attr(resp_vars, "label_col")
  colnames(study_data) <- resp_vars
  ds1 <- study_data

  # correct variable use?
  util_correct_variable_use(resp_vars,
                            allow_more_than_one = TRUE,
                            allow_null          = TRUE,
                            allow_any_obs_na    = TRUE # not using need_type allow running w/o metadata
  )
  resp_vars_character <- names(which(vapply(study_data,
                                is.character,
                                FUN.VALUE = logical(1))))

  if (!any(resp_vars %in% resp_vars_character)) {
    util_error(c("No %s are of type character, cannot work on",
                   "the non-character variables."),
                 sQuote("resp_vars"),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
  } else if (!all(resp_vars %in% resp_vars_character)) {
    util_message(c("Not all %s are of type character, leave out",
                   "the non-character variables."),
                 sQuote("resp_vars"),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
  }
  resp_vars <- intersect(resp_vars,
            resp_vars_character)
  if (missing(ref_encs)) {
    # ref_encs <- vapply(res, attr, "ref_enc", FUN.VALUE = character(1))
    ref_encs <- util_get_encoding(study_data = ds1,
                                  label_col = ds1_label_col,
                                  meta_data = meta_data,
                                  meta_data_dataframe = meta_data_dataframe)
  }
  res <- util_verify_encoding(study_data[,
                                         resp_vars,
                                         FALSE], ref_encs = ref_encs)
  cols_enc <- attr(res, "cols_enc")
  enc_guessed <- vapply(setNames(nm = names(cols_enc)), function(nm) {
    x <- cols_enc[[nm]]
    ref_enc <- "---~unkonwn~----"
    try(
      ref_enc <- ref_encs[[nm]],
      silent = TRUE
    )
    x <- x[order(x[["confidence"]], decreasing = TRUE), , FALSE]
    x[x[["encoding"]] == ref_enc, "encoding"] <- paste("(*)", ref_enc)
    prep_deparse_assignments(x[["encoding"]],
                             x[["confidence"]],
                             mode = "string_codes")
  }, FUN.VALUE = character(1))
  num_prolems_per_var <-
    vapply(res, length, FUN.VALUE = integer(1))
  num_prolems_per_var[setdiff(resp_vars, names(num_prolems_per_var))] <- 0
  num_prolems_per_var <- num_prolems_per_var[resp_vars]
  pct_prolems_per_var <- 100 * num_prolems_per_var / nrow(study_data) # TODO: or remove NAs for denominator?
  SummaryTable <- data.frame(check.names = FALSE,
    Variables = resp_vars, # resp_vars_lcol[resp_vars],
    `Defined Encoding` = ref_encs[resp_vars],
    `Guessed Encoding` = enc_guessed[resp_vars],
    PCT_int_uenc = pct_prolems_per_var,
    NUM_int_uenc = num_prolems_per_var
  )
  # TODO: Fix this after we have an indicator for this in DQ_OBS
  SummaryData <- SummaryTable[, 1:3]
  SummaryData$`Mismatches` <- paste0(
    num_prolems_per_var, " (",
    util_round_to_decimal_places(pct_prolems_per_var), "%)"
  )
  FlaggedStudyData <- study_data
  FlaggedStudyData[] <- FALSE
  FlaggedStudyData[] <- lapply(names(FlaggedStudyData), function(cn) {
    r <- FlaggedStudyData[[cn]]
    if (cn %in% names(res)) {
      r[as.integer(res[[cn]])] <- TRUE
    }
    r
  })

  resp_vars_lcol <- prep_map_labels(SummaryData$Variables,
                                    ifnotfound = SummaryData$Variables,
                                    item_level = meta_data,
                                    to = label_col,
                                    from = ds1_label_col)

  SummaryData$Variables <- util_attach_attr(SummaryData$Variables,
                                            plain_label =
                                              resp_vars_lcol[resp_vars])

  rownames(SummaryTable) <- NULL # html table would display them
  rownames(SummaryData) <- NULL
  rownames(FlaggedStudyData) <- NULL

  list(
    SummaryTable = SummaryTable,
    SummaryData = SummaryData,
    FlaggedStudyData = FlaggedStudyData
  )
}

# Encoding(nako_WS_fil$ws_note) <- "UTF-8"
# nako_WS_fil$ws_note<- iconv(nako_WS_fil$ws_note, "UTF-8", "UTF-8", sub = '')
