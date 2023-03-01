#' Compute Indicators for Qualified Item Missingness
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. Report the
#'                                     number of observations expected using
#'                                     the old `PART_VAR` concept. See
#'                                     [com_item_missingness] for an
#'                                     explanation.
#'
#' @return [list] list with entries:
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("inst/extdata/Metadata_example_v3-6.xlsx")
#' clean <- prep_get_data_frame("item_level")
#' clean <- subset(clean, `Metadata name` == "Example" &
#'   !dataquieR:::util_empty(VAR_NAMES))
#' clean$`Metadata name` <- NULL
#' clean[, "MISSING_LIST_TABLE"] <- "missing_matchtable1"
#' prep_add_data_frames(item_level = clean)
#' clean <- prep_get_data_frame("missing_matchtable1")
#' clean <- clean[clean$`Metadata name` == "Example", , FALSE]
#' clean <-
#'   clean[suppressWarnings(as.character(as.integer(clean$CODE_VALUE)) ==
#'     as.character(clean$CODE_VALUE)), , FALSE]
#' clean$CODE_VALUE <- as.integer(clean$CODE_VALUE)
#' clean <- clean[!is.na(clean$`Metadata name`), , FALSE]
#' clean$`Metadata name` <- NULL
#' prep_add_data_frames(missing_matchtable1 = clean)
#' ship <- prep_get_data_frame("ship")
#' number_of_mis <- ceiling(nrow(ship) / 20)
#' resp_vars <- sample(colnames(ship), ceiling(ncol(ship) / 20), FALSE)
#' mistab <- prep_get_data_frame("missing_matchtable1")
#' valid_replacement_codes <-
#'   mistab[mistab$CODE_INTERPRET != "P", "CODE_VALUE",
#'     drop =
#'     TRUE] # sample only replacement codes on item level. P uses the actual
#'           # values
#' for (rv in resp_vars) {
#'   values <- sample(as.numeric(valid_replacement_codes), number_of_mis,
#'     replace = TRUE)
#'   if (inherits(ship[[rv]], "POSIXct")) {
#'     values <- as.POSIXct(values, origin = min(as.POSIXct(Sys.Date()), 0))
#'   }
#'   ship[sample(seq_len(nrow(ship)), number_of_mis, replace = FALSE), rv] <-
#'     values
#' }
#' com_qualified_item_missingness(resp_vars = NULL, ship, "item_level", LABEL)
#' com_qualified_item_missingness(resp_vars = "Diabetes Age onset", ship,
#'   "item_level", LABEL)
#' com_qualified_item_missingness(resp_vars = NULL, "study_data", "meta_data",
#'   LABEL)
#' study_data <- ship
#' meta_data <- prep_get_data_frame("item_level")
#' label <- LABEL
#' }
com_qualified_item_missingness <-    function(resp_vars, study_data,
                                              meta_data,
                                              label_col = NULL,
                                              expected_observations = c("HIERARCHY",
                                                                        "ALL",
                                                                        "SEGMENT")) {
  # TODO: Allow also here dates values as missing codes
  # TODO (for the example): Add also missing table assignments to the synthetic cases with PART_VAR working following the old implementation

  # TODO (JM): See library(roxygen2)
  # ?"tags-index-crossref"
  #

  # Preparation of the input ----

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
  AAPOR_STATES <- c("P", "PP", "PL", "R", "BO", "NC", "O", "UH", "OH", "NE")

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
          .cur_miss <- NA_character_
        }
        cur_miss <- try(util_expect_data_frame(x = .cur_miss, # this is up to here the name of the missing table, not the table itself
                                               dont_assign = TRUE, # don't replace the value of .cur_miss by the data frame, just return it
                                               col_names = # check/ensure, content of the data frame matches expectations
                                               list(
          CODE_VALUE = is.numeric,
          CODE_INTERPRET = function(x) {
            is.character(x) & x %in% AAPOR_STATES
          }
        ),
        convert_if_possible = list( # try to convert values to match expectations, if produces NAs, an error is thrown. Thereafter, checks from col_names above will re-run.
          CODE_VALUE = as.numeric,
          CODE_INTERPRET = function(x) {
            trimws(toupper(x))
          }
        )), silent = TRUE)
        if (inherits(cur_miss, "try-error")) { # some problems with fetching and checking/fixing the data frame by util_expect_data_frame
          util_warning(
  "Could not load missing-match-table %s for response variable %s: %s",
             dQuote(.cur_miss),
             dQuote(rv),
             conditionMessage(attr(cur_miss, "condition")),
             applicability_problem = TRUE)
          return(NULL)
        } else { # if a missing table could be properly fetched and is consistent

          cur_miss <- # remove rows from the missing table, that are explicitly not assigned to the rv
            util_filter_missing_list_table_for_rv(cur_miss, rv, rv_vn)

          # really compute stuff ----
          code_for_P <- head(cur_miss[cur_miss[["CODE_INTERPRET"]] == "P",
                                 "CODE_VALUE", TRUE], 1) # get the first code for P to replace the real measurements by this code
          if (length(code_for_P) == 0) {
            code_for_P <- max(cur_miss$CODE_VALUE, na.rm = TRUE) + 1
             cur_miss <- rbind(cur_miss,
                               data.frame(
                                 CODE_VALUE = code_for_P,
                                 CODE_LABEL = "Participation",
                                 CODE_INTERPRET = "P",
                                 stringsAsFactors = FALSE
                               ))
             code_for_P <- head(cur_miss[cur_miss[["CODE_INTERPRET"]] == "P",
                                         "CODE_VALUE", TRUE], 1) # get the first code for P to replace the real measurements by this code
          }
          if (length(code_for_P) != 1) {
            util_error(
              applicability_problem = TRUE,
              c("Could not find any participation related code in %s %s",
                "for qualified missingness of item %s"),
              sQuote(MISSING_LIST_TABLE),
              dQuote(.cur_miss),
              dQuote(rv)
              )
          }

          rx <- ds1[[rv]]

          rx[!is.na(rx) & !(rx %in% cur_miss[cur_miss[["CODE_INTERPRET"]] != "P",
                                             "CODE_VALUE", TRUE])] <- code_for_P

          rx <- suppressWarnings(as.numeric(rx)) # TODO: Handle dates, handle errors

          r <- util_table_of_vct(factor( # counts the number of "CODE_INTERPRED"/AAPOR letters for the observations, by converting the value to a factor and tabulating it
            rx, # the (not-yet) AAPOR codes
            levels = cur_miss[["CODE_VALUE"]], # TODO: Add constants and streamline with prep_extract_cause_label_df and prep_add_cause_label_df # CODES used in the rv are on the missing-code table in the column CODE_VALUE
            labels = cur_miss[["CODE_INTERPRET"]]))

          r <- as.list(setNames(r$Freq, nm = r$Var1)) # convert the data frame with columns Var1 and Freq to a named list, names are the AAPOR codes, values are the frequencies

          # Generate replacement list
          rclean <- r

          # Enter 0 for NULL in replacement list to always enable computations
          for (name in AAPOR_STATES) {
            if (is.null(rclean[[name]])) {
              rclean[[name]]<-0
            }
          }

          # Compute response, denial, ... rates ----

          # DQI-2002    Based on participation only
          # NRR1 for all who did not participate
          # Minimal information: Study, Segment: (P | PP ) -> NOTE NE not used in denominator
          # RR1<-(P+PP)/((P+PP+PL) + (R+BO+NC+O) + (UH+UO)
          if(!is.null(r$P) | !is.null(r$PP) ) {
            r$RR1 <- (rclean$P + rclean$PP) / ((rclean$P+rclean$PP+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$OH))
            r$NRR1 <- 1 - r$RR1
            r$PCT_com_qum_nonresp <- 100 * r$NRR1
          } else {
            util_warning(
"Preconditions to generate Nonresponse Rate 1 not given for study variable %s",
              dQuote(rv),
              applicability_problem = TRUE)
            r$NRR1 <- NA_real_
            r$RR1 <- NA_real_
            r$PCT_com_qum_nonresp <- NA_real_
          }

          # NRR2 based on participation and scheduled dates
          # Minimal information: Study, Segment: (P | PP | PL ) -> NOTE NE not used in denominator
          # RR2<-(P+PP+PL)/((P+PP+PL) + (R+BO+NC+O) + (UH+UO)
          if(!is.null(r$P) | !is.null(r$PP)  | !is.null(r$PL) ) {
            r$RR2 <- (rclean$P + rclean$PP + rclean$PL) / ((rclean$P+rclean$PP+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$OH))
            r$NRR2 <- 1 - r$RR2
          } else {
            util_warning(
      "Preconditions to generate Nonresponse Rate 1 not given for variable %s",
              dQuote(rv),
              applicability_problem = TRUE)
            r$NRR2 <- NA_real_
            r$RR2 <- NA_real_
          }


          # DQI-2003    Refusal rate
          # NRR1 for all who refused at any stage
          # Minimal information: Study, Segment: R | BO
          # REF1<-(R+BO)/((P+PP+PL) + (R+BO+NC+O) + (UH+UO)
          if(!is.null(r$R) | !is.null(r$BO) ) {
            r$REF1 <- (rclean$R + rclean$BO) / ((rclean$P+rclean$PP+rclean$PL) + (rclean$R+rclean$BO+rclean$NC+rclean$O) + (rclean$UH+rclean$OH))
            r$PCT_com_qum_refusal <- 100 * r$REF1
          } else {
            util_warning(
      "Preconditions to generate Nonresponse Rate 1 not given for variable %s",
              dQuote(rv),
              applicability_problem = TRUE)
            r$REF1 <- NA_real_
            r$PCT_com_qum_refusal <- NA_real_
          }

          r$N <- sum(!util_empty(ds1[[rv]]))
          r$N2 <- util_count_expected_observations(rv,
                                                  study_data = ds1,
                                                  meta_data = meta_data,
                                                  label_col = label_col,
                                                  expected_observations =
                                                    expected_observations)

          r <- cbind( # create a new data frame with a column Variables and all the rates and values for the current rv 1 row, only.
            data.frame(Variables = rv, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL),
            data.frame(r, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
          )
        }
      } else { # study data lacks the status variable for the  current variable, omit this segment
        util_warning("Missing variable %s from %s",
                     dQuote(rv),
                     sQuote("study_data"))
        return(NULL)
      }
    })

  SummaryTable <- util_rbind(data_frames_list = aapors) # Combine all the 1-row-tables to a large rate-table
  rownames(SummaryTable) <- NULL
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  dqi <- util_get_concept_info("dqi")
  cols_for_output <-
    vapply(colnames(SummaryTable), FUN.VALUE = character(1), FUN = function(x) {
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
  cols_for_output <- c(Variables = "Variables", cols_for_output)
  cols_for_output <- cols_for_output[!is.na(cols_for_output)]
  cols_for_output <- cols_for_output[names(cols_for_output) %in%
                                       colnames(SummaryTable)]
  SummaryData <- SummaryTable[, names(cols_for_output), FALSE]
  SummaryData[, startsWith(names(cols_for_output), "PCT_")] <-
    SummaryData[, startsWith(names(cols_for_output), "PCT_")]
  colnames(SummaryData) <- cols_for_output
  SummaryData[, startsWith(names(cols_for_output), "PCT_")] <-
    lapply(SummaryData[, startsWith(names(cols_for_output), "PCT_")],
           function(cl) {
            paste0(round(cl, 2), "%")
           })

  return(list( # return the results
    SummaryTable = SummaryTable,
    SummaryData = SummaryData
  ))
}
