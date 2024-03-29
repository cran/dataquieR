#' Compute Indicators for Qualified Segment Missingness
#'
#' [Indicator]
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param meta_data_segment [data.frame] Segment level metadata
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. Report the
#'                                     number of observations expected using
#'                                     the old `PART_VAR` concept. See
#'                                     [com_item_missingness] for an
#'                                     explanation.
#'
#' @return [list] list with entries:
#'
#' @export
com_qualified_segment_missingness <- function(study_data,
                                              meta_data,
                                              label_col = NULL,
                                              meta_data_segment,
                                        expected_observations = c("HIERARCHY",
                                                                  "ALL",
                                                                  "SEGMENT")) {

  # Preparation of the input ----

  prep_prepare_dataframes(.replace_missings = FALSE)

  expected_observations <- util_match_arg(expected_observations)

  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)

  # allowed AAPOR-States
  AAPOR_STATES <- c("I", "P", "PL", "R", "BO", "NC", "O", "UH", "UO", "NE")

  # Loop over all segments defined on segment-level metadata ----

  aapors <-
    lapply(setNames(nm = meta_data_segment[[STUDY_SEGMENT]]),
           function(segment) { # segment is the currently examined segment's name
      cur_seg <- meta_data_segment[ # only the line(s) from metadata segment, that refer to the currently examined segment
        meta_data_segment[[STUDY_SEGMENT]] == segment, , FALSE]
      util_stop_if_not(nrow(cur_seg) == 1) # TODO: write a proper error message; # TOOD: Don't stop, only return() in case of an error # this checks, that the segment level metadata has exactly 1 line for the current segment
      .cur_state_var <- cur_seg[[SEGMENT_PART_VARS]] # .cur_state_var is the participation status variable of the current segment
      if (length(.cur_state_var) != 1) { # only one variable should be listed in that cell
        util_warning("Missing or doubled %s in %s for %",
                     sQuote(SEGMENT_PART_VARS),
                     sQuote("meta_data_segment"),
                     dQuote(segment),
                     applicability_problem = TRUE)
        return(NULL)
      }
      cur_state_var_vn <- meta_data[meta_data[[label_col]] == .cur_state_var |
                                   meta_data[[VAR_NAMES]] == .cur_state_var,
                                 "VAR_NAMES"] # fetch the name of the current status var based either its name or its label (as defined by `label_col`)
      cur_state_var <- meta_data[meta_data[[label_col]] == .cur_state_var |
                                   meta_data[[VAR_NAMES]] == .cur_state_var,
                                 label_col] # fetch the label of the current status var based either its name or its label (as defined by `label_col`)
      if (length(cur_state_var) != 1) { # if itemlevel metadata do not provide exactly one entry for the participation status variable, omit the current segment
                       util_warning("Missing or doubled %s in %s",
                                    dQuote(.cur_state_var),
                                    sQuote("meta_data"),
                                    applicability_problem = TRUE)
        return(NULL)
      }
      if (cur_state_var %in% colnames(ds1)) { # check, if the status variable really exists in the study data
        .cur_miss <- meta_data[meta_data[[label_col]] == cur_state_var, # find the missing code table assigned to the current segment.
                              MISSING_LIST_TABLE]
        if (length(.cur_miss) == 0) {
          .cur_miss <- NA_character_
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
          if (!is.na(.cur_miss)) util_warning(
  "Could not load missing-match-table %s for %s variable %s for segment %s: %s",
               dQuote(.cur_miss),
               sQuote(SEGMENT_PART_VARS),
               dQuote(cur_state_var),
               dQuote(segment),
               conditionMessage(attr(cur_miss, "condition")),
               applicability_problem = TRUE) else
                 util_warning(
               "No missing-match-table for response variable %s for segment %s",
                   dQuote(cur_state_var),
                   dQuote(segment),
                   applicability_problem = TRUE)
          return(NULL)
        } else { # if a missing table could be properly fetched and is consistent

          cur_miss <- # remove rows from the missing table, that are explicitly not assigned to the cur_state_var
            util_filter_missing_list_table_for_rv(cur_miss, cur_state_var, cur_state_var_vn)

          # really compute stuff ----
          r <- util_table_of_vct(factor( # counts the number of "CODE_INTERPRED"/AAPOR letters for the observations, by converting the value to a factor and tabulating it
            ds1[[cur_state_var]], # the variable with the (not-yet) AAPOR codes
            levels = cur_miss[["CODE_VALUE"]], # TODO: Add constants and streamline with prep_extract_cause_label_df and prep_add_cause_label_df # CODES used in the cur_state_var are on the missing-code table in the column CODE_VALUE
            labels = cur_miss[["CODE_INTERPRET"]]))

          r <- as.list(setNames(r$Freq, nm = r$Var1)) # convert the data frame with columns Var1 and Freq to a named list, names are the AAPOR codes, values are the frequencies

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
            util_warning(
        "Preconditions to generate Nonresponse Rate 1 not given for segment %s",
              dQuote(segment),
              applicability_problem = TRUE)
            r$NRR1 <- NA_real_
            r$RR1 <- NA_real_
            r$PCT_com_qum_nonresp <- NA_real_
          }

          # NRR2 based on participation and scheduled dates
          # Minimal information: Study, Segment: (I | P | PL ) -> NOTE NE not used in denominator
          # RR2<-(I+P+PL)/((I+P+PL) + (R+BO+NC+O) + (UH+UO)
          if(!is.null(r$I) | !is.null(r$P)  | !is.null(r$PL) ) {
            r$RR2 <- (rclean$I + rclean$P + rclean$PL) / ((rclean$I+rclean$P+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$UO))
            r$NRR2 <- 1 - r$RR2
          } else {
            util_warning(
      "Preconditions to generate Nonresponse Rate 1 not given for segment %s",
              dQuote(segment),
              applicability_problem = TRUE)
            r$NRR2 <- NA_real_
            r$RR2 <- NA_real_
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
        "Preconditions to generate Nonresponse Rate 1 not given for segment %s",
              dQuote(segment),
              applicability_problem = TRUE)
            r$REF1 <- NA_real_
            r$PCT_com_qum_refusal <- NA_real_
          }

          r$N <- sum(!util_empty(ds1[[cur_state_var]]))

          r$N2 <-util_count_expected_observations(cur_state_var,
                                                  study_data = study_data,
                                                  meta_data = meta_data,
                                                  label_col = label_col,
                                                  expected_observations =
                                                    expected_observations)


          r <- cbind( # create a new data frame with a column Segment and all the rates and values for the current segment. 1 row, only.
            data.frame(Segment = segment, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL),
            data.frame(r, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
          )
        }
      } else { # study data lacks the status variable for the  current segment, omit this segment
        util_warning("Missing %s variable %s from %s for %s",
                     sQuote(SEGMENT_PART_VARS),
                     dQuote(cur_state_var),
                     sQuote("study_data"),
                     dQuote(segment),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
        return(NULL)
      }
    })

  SegmentTable <- util_rbind(data_frames_list = aapors) # Combine all the 1-row-tables to a large rate-table
  rownames(SegmentTable) <- NULL
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  dqi <- util_get_concept_info("dqi")
  cols_for_output <-
    vapply(colnames(SegmentTable), FUN.VALUE = character(1), FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {
        m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                    "Metrics", drop = TRUE), 1)
        d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                           collapse = "_"),
                    "Name", drop = TRUE), 1)
        if (length(m) == length(d) && length(d) == 1 &&
            !util_empty(m) && !util_empty(d)) {
          sprintf("%s (%s)", d, m)
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
    })
  cols_for_output <- c(Segment = "Segment", cols_for_output)
  cols_for_output <- cols_for_output[!is.na(cols_for_output)]
  cols_for_output <- cols_for_output[names(cols_for_output) %in%
                                       colnames(SegmentTable)]
  SegmentData <- SegmentTable[, names(cols_for_output), FALSE]
  SegmentData[, startsWith(names(cols_for_output), "PCT_")] <-
    SegmentData[, startsWith(names(cols_for_output), "PCT_")]
  colnames(SegmentData) <- cols_for_output
  SegmentData[, startsWith(names(cols_for_output), "PCT_")] <-
    lapply(SegmentData[, startsWith(names(cols_for_output), "PCT_")],
           function(cl) {
             paste0(round(cl, 2), "%")
           })
## TODO: to discuss
#  #check if SegmentTable is empty
#  if(!prod(dim(SegmentTable))){
#    util_error("Can not compute qualified segment missingness",
#               applicability_problem = TRUE)
#  }

  return(list( # return the results
    SegmentTable = SegmentTable,
    SegmentData = SegmentData
  ))
}
