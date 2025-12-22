#' Compute Indicators for Qualified Item Missingness
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. Report the
#'                                     number of observations expected using
#'                                     the old `PART_VAR` concept. See
#'                                     [com_item_missingness] for an
#'                                     explanation.
#'
#' @return A [list] with:
#'   - `SummaryTable`: [data.frame] containing data quality checks for
#'                     "Non-response rate" (`PCT_com_qum_nonresp`) and
#'                     "Refusal rate" (`PCT_com_qum_refusal`) for each response
#'                     variable in `resp_vars`.
#'   - `SummaryData`: a [data.frame] containing data quality checks for
#'                    “Non-response rate” and "Refusal rate"
#'                    for a report
#'
#' @export
com_qualified_item_missingness <-    function(resp_vars,
                                              study_data,
                                              label_col = NULL,
                                              item_level = "item_level",
                                              expected_observations = c("HIERARCHY",
                                                                        "ALL",
                                                                        "SEGMENT"),
                                              meta_data = item_level,
                                              meta_data_v2) {
  # TODO: Allow also here dates values as missing codes
  # TODO (for the example): Add also missing table assignments to the synthetic cases with PART_VAR working following the old implementation

  # Preparation of the input ----

  util_maybe_load_meta_data_v2()

  prep_prepare_dataframes(.replace_missings = FALSE)

  expected_observations <- util_match_arg(expected_observations)

  if (missing(resp_vars) || length(resp_vars) == 0) {
    resp_vars <- meta_data[[label_col]]
  }

  util_correct_variable_use(resp_vars,
                            allow_more_than_one = TRUE,
                            allow_all_obs_na = TRUE,
                            allow_any_obs_na = TRUE)

  # allowed AAPOR-States
  AAPOR_STATES <- c("I", "P", "PL", "R", "BO", "NC", "O", "UH", "UO", "NE")

  # Loop over all resp_vars ----

  aapors <-
    lapply(setNames(nm = resp_vars),
           function(rv) { # rv is the currently examined study variable's name
             rv_vn <- meta_data[meta_data[[label_col]] == rv, # find the variable name
                                VAR_NAMES]
             if (rv %in% colnames(ds1)) { # check, if the status variable really exists in the study data
               .cur_miss <- meta_data[meta_data[[label_col]] == rv, # find the missing code table assigned to the current study variable
                                      MISSING_LIST_TABLE]
               if (length(.cur_miss) == 0) {
                 .cur_miss <- NA_character_ # nocov: Cannot be reached, because if rv is not in the metadata, we fail already in util_correct_variable_use, otherwise, we may have NA in MISSING_LIST_TABLE, but not length(.) == 0
               }
               cur_miss <- try(util_expect_data_frame(x = .cur_miss, # this is up to here the name of the missing table, not the table itself
                                                      dont_assign = TRUE, # don't replace the value of .cur_miss by the data frame, just return it
                                                      col_names = # check/ensure, content of the data frame matches expectations
                                                        list(
                                                          CODE_VALUE = util_is_valid_missing_codes,
                                                          CODE_INTERPRET = function(x) {
                                                            is.character(x) & x %in% AAPOR_STATES
                                                          }
                                                        ),
                                                      custom_errors = list(
                                                        CODE_VALUE = "CODE_VALUE must be numeric or DATETIME",
                                                        CODE_INTERPRET = paste("CODE_INTERPRET must be one of",
                                                                               util_pretty_vector_string(AAPOR_STATES))
                                                      ),
                                                      convert_if_possible = list( # try to convert values to match expectations, if produces NAs, an error is thrown. Thereafter, checks from col_names above will re-run.
                                                        CODE_VALUE = util_as_valid_missing_codes,
                                                        CODE_INTERPRET = function(x) {
                                                          trimws(toupper(x))
                                                        }
                                                      )), silent = TRUE)
               if (inherits(cur_miss, "try-error")) { # some problems with fetching and checking/fixing the data frame by util_expect_data_frame
                 # nocov start: Cannot be reached, because most potential problems will have occurred already in prep_prepare_data_frames > prep_meta_data_v1_to_item_level_meta_data
                 if (!is.na(.cur_miss)) util_warning(
                   "Could not load missing-match-table %s for response variable %s: %s",
                   dQuote(.cur_miss),
                   dQuote(rv),
                   conditionMessage(attr(cur_miss, "condition")),
                   applicability_problem = TRUE) else # nocov end
                     util_warning(
                       "No missing-match-table for response variable %s",
                       dQuote(rv),
                       applicability_problem = TRUE)
                 return(NULL)
               } else { # if a missing table could be properly fetched and is consistent

                 cur_miss <- # remove rows from the missing table, that are explicitly not assigned to the rv
                   util_filter_missing_list_table_for_rv(cur_miss, rv, rv_vn)

                 # really compute stuff ----
                 code_for_P <- head(cur_miss[cur_miss[[CODE_INTERPRET]] == "I",
                                             CODE_VALUE, TRUE], 1) # get the first code for I to replace the real measurements by this code
                 if (length(code_for_P) == 0) {
                   code_for_P <- util_find_free_missing_code(cur_miss$CODE_VALUE)
                   P_df <- data.frame(
                     CODE_VALUE = code_for_P,
                     CODE_LABEL = "Participation",
                     CODE_INTERPRET = "I",
                     stringsAsFactors = FALSE
                   )
                   cur_miss <- util_rbind(cur_miss,
                                          P_df)
                   code_for_P <- head(cur_miss[cur_miss[[CODE_INTERPRET]] == "I",
                                               CODE_VALUE, TRUE], 1) # get the first code for I to replace the real measurements by this code
                 }
                 if (length(code_for_P) != 1) {
                   # nocov start: Cannot be reached, because if there would've been no I code, we would have one after the last block. If we had > 1, we would have chosen the first one already, wlog
                   util_error(
                     applicability_problem = TRUE,
                     c("Could not find any participation related code in %s %s",
                       "for qualified missingness of item %s"),
                     sQuote(MISSING_LIST_TABLE),
                     dQuote(.cur_miss),
                     dQuote(rv)
                   )
                   # nocov end
                 }

                 rx <- ds1[[rv]]

                 rx[!is.na(rx) & !(rx %in% cur_miss[cur_miss[[CODE_INTERPRET]] != "I",
                                                    CODE_VALUE, TRUE])] <- code_for_P

                 rx <- suppressWarnings(as.numeric(rx)) # TODO: Handle dates, handle errors

                 r <- util_table_of_vct(factor( # counts the number of "CODE_INTERPRED"/AAPOR letters for the observations, by converting the value to a factor and tabulating it
                   rx, # the (not-yet) AAPOR codes
                   levels = cur_miss[[CODE_VALUE]], # TODO: Streamline with prep_extract_cause_label_df and prep_add_cause_label_df # CODES used in the rv are on the missing-code table in the column CODE_VALUE
                   labels = cur_miss[[CODE_INTERPRET]]))

                 r <- as.list(setNames(r$Freq, nm = r$Var1)) # convert the data frame with columns Var1 and Freq to a named list, names are the AAPOR codes, values are the frequencies

                 r$N <- sum(!util_empty(ds1[[rv]]))
                 r$N2 <- util_count_expected_observations(rv,
                                                          study_data = ds1,
                                                          meta_data = meta_data,
                                                          label_col = label_col,
                                                          expected_observations =
                                                            expected_observations)

                 if (length(r$P) == 1 && !is.na(r$P) && r$P != 0) {
                   util_warning(
                     c("%0.2f%% of the values in %s have a missing code for",
                       "%s assigned, which is impossible on item level. Treating",
                       "these cases as %s"),
                     round(r$P / r$N * 100, 2),
                     sQuote(rv),
                     sQuote("P"),
                     sQuote("UO"),
                     applicability_problem = TRUE)
                   if (length(r$UO) == 1 && !is.na(r$UO) && r$UO != 0) {
                     r$UO <- r$UO + r$P
                   } else {
                     r$UO <- r$P
                   }
                   r$P <- 0
                 }


                 # Generate replacement list
                 rclean <- r

                 # Enter 0 for NULL in replacement list to always enable computations
                 for (name in AAPOR_STATES) {
                   if (is.null(rclean[[name]])) {
                     rclean[[name]] <- 0
                   }
                 }

                 # Compute response, denial, ... rates ----

                 # DQI-2002    Based on participation only
                 # NRR1 for all who did not participate
                 # Minimal information: Study, Segment: (I | P ) -> NOTE NE not used in denominator
                 # RR1<-(I+P)/((I+P+PL) + (R+BO+NC+O) + (UH+UO)
                 if(!is.null(r$I) | !is.null(r$P) ) {
                   r$RR1 <- (rclean$I + rclean$P) / ((rclean$I+rclean$P+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$UO))
                   r$NRR1 <- 1 - r$RR1
                   r$PCT_com_qum_nonresp <- 100 * r$NRR1
                 } else {
                   # nocov start
                   # we will always have at least I == 0, because this is ensured above.
                   util_warning(
                     "Preconditions to generate Nonresponse Rate 1 not given for study variable %s",
                     dQuote(rv),
                     applicability_problem = TRUE)
                   r$NRR1 <- NA_real_
                   r$RR1 <- NA_real_
                   r$PCT_com_qum_nonresp <- NA_real_
                   # nocov end
                 }

                 # NRR2 based on participation and scheduled dates
                 # Minimal information: Study, Segment: (I | P | PL ) -> NOTE NE not used in denominator
                 # RR2<-(I+P+PL)/((I+P+PL) + (R+BO+NC+O) + (UH+UO)
                 if(!is.null(r$I) | !is.null(r$P)  | !is.null(r$PL) ) {
                   r$RR2 <- (rclean$I + rclean$P + rclean$PL) / ((rclean$I+rclean$P+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$UO))
                   r$NRR2 <- 1 - r$RR2
                 } else {
                   # nocov start
                   # we will always have at least I == 0, because this is ensured above.
                   util_warning(
                     "Preconditions to generate Nonresponse Rate 1 not given for variable %s",
                     dQuote(rv),
                     applicability_problem = TRUE)
                   r$NRR2 <- NA_real_
                   r$RR2 <- NA_real_
                   # nocov end
                 }


                 # DQI-2003    Refusal rate
                 # NRR1 for all who refused at any stage
                 # Minimal information: Study, Segment: R | BO
                 # REF1<-(R+BO)/((I+P+PL) + (R+BO+NC+O) + (UH+UO)
                 if(!is.null(r$R) | !is.null(r$BO) ) {
                   r$REF1 <- (rclean$R + rclean$BO) / ((rclean$I+rclean$P+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$UO))
                   r$PCT_com_qum_refusal <- 100 * r$REF1
                 } else {
                   util_warning(
                     "Preconditions to generate Nonresponse Rate 1 not given for variable %s", # TODO: Write nicer warnings, here.
                     dQuote(rv),
                     applicability_problem = TRUE)
                   r$REF1 <- NA_real_
                   r$PCT_com_qum_refusal <- NA_real_
                 }

                 r <- cbind( # create a new data frame with a column Variables and all the rates and values for the current rv 1 row, only.
                   data.frame(Variables = rv, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL),
                   data.frame(r, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
                 )
               }
             } else { # study data lacks the status variable for the  current variable, omit this segment
               # nocov start
               # unreachabel, becasue if rv would not be in colnames(ds1), util_correct_variable_use would have stopped, already
               util_warning("Missing variable %s from %s",
                            dQuote(rv),
                            sQuote("study_data"),
                            applicability_problem = TRUE,
                            intrinsic_applicability_problem = TRUE)
               return(NULL)
               # nocov end
             }
           })

  SummaryTable <- util_rbind(data_frames_list = aapors) # Combine all the 1-row-tables to a large rate-table
  rownames(SummaryTable) <- NULL

  # --- Start refactored part: use helpers for selection + translation ---
  # Select only indicator metric columns from SummaryTable
  indicator_cols <- util_extract_indicator_metrics(SummaryTable)

  # Assemble SummaryData = Variables + indicator columns
  SummaryData <- cbind(
    data.frame(Variables = SummaryTable[["Variables"]], stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL),
    indicator_cols
  )

  # Format percentage columns BEFORE renaming (we rely on original "PCT_" prefixes)
  pct_cols <- colnames(SummaryData)[startsWith(colnames(SummaryData), "PCT_")]
  if (length(pct_cols)) {
    SummaryData[, pct_cols] <- lapply(SummaryData[, pct_cols, drop = FALSE],
                                      function(cl) paste0(round(cl, 2), "%"))
  }

  # Translate column names using the canonical helper; keep unknown names as-is
  colnames(SummaryData) <- util_translate_indicator_metrics(
    colnames(SummaryData),
    short = FALSE,
    long  = TRUE,
    ignore_unknown = TRUE
  )
  # --- End refactored part ---

  # SummaryTable$GRADING <- SummaryTable[["PCT_com_qum_nonresp"]] > 10 # only to avoid empty output in issue matrix, can be removed, now.

  text_to_display <- util_get_hovertext("[com_qualified_item_missingness_hover]")
  attr(SummaryData, "description") <- text_to_display

  return(list( # return the results
    SummaryTable = SummaryTable,
    SummaryData = SummaryData
  ))
}
