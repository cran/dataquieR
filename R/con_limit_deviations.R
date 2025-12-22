#' Detects variable values exceeding limits defined in metadata
#'
#' @description
#'
#' Inadmissible numerical values can be of type integer or float. This
#' implementation requires the definition of intervals in the metadata to
#' examine the admissibility of numerical study data.
#'
#' This helps identify inadmissible measurements according to
#' hard limits (for multiple variables).
#'
#' [Indicator]
#'
#' @details
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific intervals as supplied in the metadata.
#'  - Identification of measurements outside defined limits. Therefore two
#'    output data frames are generated:
#'    - on the level of observation to flag each deviation, and
#'    - a summary table for each variable.
#'  - A list of plots is generated for each variable examined for limit
#'    deviations. The histogram-like plots indicate respective limits as well
#'    as deviations.
#'  - Values exceeding limits are removed in a data frame of modified study data
#'
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#' @param meta_data_cross_item [meta_data_cross]
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param limits [enum] HARD_LIMITS | SOFT_LIMITS | DETECTION_LIMITS. what
#'                                             limits from metadata to check for
#' @param return_flagged_study_data [logical] return `FlaggedStudyData` in the
#'                                            result
#' @param return_limit_categorical [logical] if TRUE return limit deviations also
#'                                           for categorical variables
#' @param show_obs [logical] Should (selected) individual observations be marked
#'                           in the figure for continuous variables?
#'
#' @inheritParams acc_distributions
#'
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual coord_flip labs
#'                     theme_minimal theme geom_bar geom_vline annotate
#'                     scale_linetype_manual
#' @importFrom stats setNames IQR complete.cases
#' @importFrom grDevices colorRampPalette gray.colors
#'
#' @return a list with:
#'   - `FlaggedStudyData` [data.frame] related to the study data by a 1:1
#'                                   relationship, i.e. for each observation is
#'                                   checked whether the value is below or above
#'                                   the limits. Optional, see
#'                                   `return_flagged_study_data`.
#'   - `SummaryTable` [data.frame] summarizing limit deviations for each
#'                                 variable.
#'   - `SummaryData` [data.frame] summarizing limit deviations for each
#'                                 variable for a report.
#'   - `SummaryPlotList` [list] of [ggplot2::ggplot]s The plots for each variable are
#'                            either a histogram (continuous) or a
#'                            barplot (discrete).
#'   - `ReportSummaryTable`: heatmap-like data frame about limit violations
#'
#' @seealso
#' - [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
con_limit_deviations <- function(resp_vars = NULL,
                                 study_data,
                                 label_col,
                                 item_level = "item_level",
                                 meta_data_cross_item = "cross-item_level",
                                 limits = NULL,
                                 flip_mode = "noflip",
                                 return_flagged_study_data = FALSE,
                                 return_limit_categorical = TRUE,
                                 meta_data = item_level,
                                 cross_item_level,
                                 `cross-item_level`,
                                 meta_data_v2,
                                 show_obs = TRUE) {
  # make R CMD check happy
  Limits <- NULL
  Number <- NULL
  Section <- NULL
  all_of <- NULL
  n  <- NULL
  n_viol <- NULL
  values <- NULL
  segment_number <- NULL

  # PREP - steps -------
  # names vector containing the types of limits: e.g., HARD_LIMITS
  known_limits <- unlist(
    WELL_KNOWN_META_VARIABLE_NAMES[intersect(
      grep("LIMIT", WELL_KNOWN_META_VARIABLE_NAMES),
      which(vapply(WELL_KNOWN_META_VARIABLE_NAMES,
                   function(ll) {
                     attr(ll, "var_att_required") != "technical"
                   },
                   FUN.VALUE = logical(1)))
    )]
  )
  # map metadata to study data
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  prep_prepare_dataframes()
  util_correct_variable_use(resp_vars,
                            allow_null = TRUE,
                            allow_more_than_one = TRUE,
                            allow_all_obs_na = FALSE,
                            need_type = "integer|float|datetime|time")
  try({
    meta_data_cross_item <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col)
  }, silent = TRUE
  )
  if (!is.data.frame(meta_data_cross_item))
    meta_data_cross_item <- data.frame()
  util_expect_scalar(return_flagged_study_data, check_type = is.logical)
  util_expect_scalar(return_limit_categorical, check_type = is.logical)

  if (!DATA_PREPARATION %in% colnames(meta_data_cross_item)) {
    meta_data_cross_item[[DATA_PREPARATION]] <-
      rep("LABEL | MISSING_NA", nrow(meta_data_cross_item)) # never use LIMITS here, we are assessing the limits
  } else {
    meta_data_cross_item[[DATA_PREPARATION]] <-
      gsub(
        ignore.case = TRUE,
        perl = TRUE,
        "LIMITS",
        "",
        meta_data_cross_item[[DATA_PREPARATION]]) # never use LIMITS here, we are assessing the limits
  }

  # COMPLEX LIMITS (from cross-item_level metadata)-------
  # if *_LIMITS in cross-item-level, compute contradictions
  # with renamed column,
  # each (renamed to CONTRADICTION_TERM) and amend results
  given_cross_limits <- intersect(known_limits,
                                  colnames(meta_data_cross_item))
  no_warn_no_lim <- FALSE
  if (!all(util_empty(as.vector(meta_data_cross_item[, given_cross_limits,
                                                     FALSE])))) {
    no_warn_no_lim <- TRUE
    #ITERATE over each limit column in cross-item_level metadata
    all_viols <- lapply(given_cross_limits, function(lim) {
      #reduce the cross-item_level content to only the rows that contains limits
      cur_cross <-
        meta_data_cross_item[!util_empty(meta_data_cross_item[[lim]]), , FALSE]
      if (nrow(cur_cross) == 0) {
        return(NULL)
      } else {
        #Select only the limit column of interest in this iteration
        cur_cross <- cur_cross[, intersect(c(VARIABLE_LIST,
                                             CONTRADICTION_TERM,
                                             CONTRADICTION_TYPE,
                                             CHECK_ID,
                                             CHECK_LABEL,
                                             DATA_PREPARATION,
                                             lim),
                                           colnames(cur_cross)
        ),
        FALSE]
        #overwrite the content of the contraction_term with the rule of the limit
        cur_cross[[CONTRADICTION_TERM]][!util_empty(cur_cross[[lim]])] <-
          paste("not (", cur_cross[[lim]], ")")[!util_empty(cur_cross[[lim]])]
        # Set the contraction type to logical for hard limits and empirical for
        # all other types of limit
        if (lim == HARD_LIMITS)
          cur_cross[[CONTRADICTION_TYPE]] <- "LOGICAL"
        else
          cur_cross[[CONTRADICTION_TYPE]] <- "EMPIRICAL"
        # get names of the variables that the limits refer to
        vl <- util_parse_assignments(cur_cross[[VARIABLE_LIST]],
                                     multi_variate_text = TRUE)
        # only accept one variable per limit
        util_stop_if_not(
          "Only one variable can be the reference for complex limits" =
            all(vapply(vl, length, FUN.VALUE = integer(1)) == 1))
        # check the function argument resp_vars
        which_vars <- resp_vars
        if (length(which_vars) == 0) {
          which_vars <- unique(unlist(vl))
        }
        which_rows <- vapply(vl, function(cv) {
          length(intersect(which_vars, cv)) == 1
        }, FUN.VALUE = logical(1))
        cur_cross <- cur_cross[which_rows, , FALSE]
        if (nrow(cur_cross) == 0) {
          return(NULL)
        } else {
          # call contradiction redcap for the limit rules
          r <- suppressWarnings(con_contradictions_redcap(
            study_data = study_data,
            meta_data = meta_data,
            label_col = label_col,
            meta_data_cross_item = cur_cross)$VariableGroupTable)
          # merge by check_id
          r[[VARIABLE_LIST]] <-
            merge(cur_cross, r, by = CHECK_ID, all = FALSE,
                  suffixes = c("", ".y"))[[VARIABLE_LIST]]
          dt <- util_map_labels(
            r[[VARIABLE_LIST]],
            meta_data = meta_data,
            to = DATA_TYPE,
            from = label_col
          )
          #add temporary another column with data_type
          r$data_type <- dt[r$VARIABLE_LIST]
          if (lim == HARD_LIMITS) {
            #create a data frame for datetime and rename the columns
            r_datetime_hard <- r[r$data_type == DATA_TYPES$DATETIME, ]
            if(nrow(r_datetime_hard) > 0) {
              colnames(r_datetime_hard)[
                colnames(r_datetime_hard) == "NUM_con_con_contc"] <-
                "NUM_con_rvv_itdat"

              colnames(r_datetime_hard)[
                colnames(r_datetime_hard) == "PCT_con_con_contc"] <-
                "PCT_con_rvv_itdat"
              #Add empty rows for numeric result
              r_datetime_hard$NUM_con_rvv_inum <- NA
              r_datetime_hard$PCT_con_rvv_inum <- NA
            } else {
              r_datetime_hard$NUM_con_rvv_itdat <- numeric(0)
              r_datetime_hard$PCT_con_rvv_itdat <- numeric(0)
              #Add empty rows for numeric result
              r_datetime_hard$NUM_con_rvv_inum <- numeric(0)
              r_datetime_hard$PCT_con_rvv_inum <- numeric(0)
            }
            # remove unneeded column
            r_datetime_hard$NUM_con_con_contu <- NULL
            r_datetime_hard$PCT_con_con_contu <- NULL
            r_datetime_hard$NUM_con_con_contc <- NULL
            r_datetime_hard$PCT_con_con_contc <- NULL
            r_datetime_hard$NUM_con_con <- NULL
            r_datetime_hard$PCT_con_con <- NULL
            #create a data frame for numeric variables
            r_numeric_hard <- r[r$data_type != DATA_TYPES$DATETIME, ]
            if(nrow(r_numeric_hard) > 0) {
              colnames(r_numeric_hard)[
                colnames(r_numeric_hard) == "NUM_con_con_contc"] <-
                "NUM_con_rvv_inum"
              colnames(r_numeric_hard)[
                colnames(r_numeric_hard) == "PCT_con_con_contc"] <-
                "PCT_con_rvv_inum"
              #Add empty rows for numeric result
              r_numeric_hard$NUM_con_rvv_itdat <- NA
              r_numeric_hard$PCT_con_rvv_itdat <- NA
            } else {
              r_numeric_hard$NUM_con_rvv_inum <- numeric(0)
              r_numeric_hard$PCT_con_rvv_inum <- numeric(0)
              #Add empty rows for numeric result
              r_numeric_hard$NUM_con_rvv_itdat <- numeric(0)
              r_numeric_hard$PCT_con_rvv_itdat <- numeric(0)
            }
            # remove unneeded column
            r_numeric_hard$NUM_con_con_contu <- NULL
            r_numeric_hard$PCT_con_con_contu <- NULL
            r_datetime_hard$NUM_con_con_contc <- NULL
            r_datetime_hard$PCT_con_con_contc <- NULL
            r_numeric_hard$NUM_con_con <- NULL
            r_numeric_hard$PCT_con_con <- NULL
            #Combine the tables
            r_numeric_hard <- r_numeric_hard[, colnames(r_datetime_hard)]
            final_r_hard <- rbind(r_datetime_hard, r_numeric_hard)
            final_r_hard$limits <- lim
          } else {
            #If DETECTION OR SOFT LIMITS
            #create a data frame for datetime and rename the columns
            r_datetime <- r[r$data_type == DATA_TYPES$DATETIME, ]
            if(nrow(r_datetime) > 0) {
              colnames(r_datetime)[
                colnames(r_datetime) == "NUM_con_con_contu"] <-
                "NUM_con_rvv_utdat"
              colnames(r_datetime)[
                colnames(r_datetime) == "PCT_con_con_contu"] <-
                "PCT_con_rvv_utdat"
              #Add empty rows for numeric result
              r_datetime$NUM_con_rvv_unum <- NA
              r_datetime$PCT_con_rvv_unum <- NA
            } else {
              r_datetime$NUM_con_rvv_utdat <- numeric(0)
              r_datetime$PCT_con_rvv_utdat <- numeric(0)
              #Add empty rows for numeric result
              r_datetime$NUM_con_rvv_unum <- numeric(0)

              r_datetime$PCT_con_rvv_unum <- numeric(0)
            }
            # remove unneeded column
            r_datetime$NUM_con_con_contc <- NULL
            r_datetime$PCT_con_con_contc <- NULL
            r_datetime$NUM_con_con_contu <- NULL
            r_datetime$PCT_con_con_contu <- NULL
            r_datetime$NUM_con_con <- NULL
            r_datetime$PCT_con_con <- NULL
            #create a data frame for numeric variables
            r_numeric <- r[r$data_type != DATA_TYPES$DATETIME, ]
            if(nrow(r_numeric) > 0) {
              colnames(r_numeric)[
                colnames(r_numeric) == "NUM_con_con_contu"] <-
                "NUM_con_rvv_unum"
              colnames(r_numeric)[
                colnames(r_numeric) == "PCT_con_con_contu"] <-
                "PCT_con_rvv_unum"
              #Add empty rows for numeric result
              r_numeric$NUM_con_rvv_utdat <- NA
              r_numeric$PCT_con_rvv_utdat <- NA
            } else {
              r_numeric$NUM_con_rvv_unum <- numeric(0)
              r_numeric$PCT_con_rvv_unum <- numeric(0)
              #Add empty rows for numeric result
              r_numeric$NUM_con_rvv_utdat <- numeric(0)
              r_numeric$PCT_con_rvv_utdat <- numeric(0)
            }
            # remove unneeded column
            r_numeric$NUM_con_con_contc <- NULL
            r_numeric$PCT_con_con_contc <- NULL
            r_numeric$NUM_con_con <- NULL
            r_numeric$PCT_con_con <- NULL
            #Combine the tables
            r_numeric <- r_numeric[, colnames(r_datetime)]
            final_r <- rbind(r_datetime, r_numeric)
            final_r$limits <- lim
          }
          #merge the results for different limits
          if (exists("final_r")) {
            if (exists("final_r_hard")) {
              final_r <- merge(
                x = final_r,
                y = final_r_hard,
                by = intersect(names(final_r),
                               names(final_r_hard)),
                all = TRUE
              )
            } else {
              final_r
            }
          } else {
            final_r_hard
          }
        }
      }
    })
    all_viols <- util_rbind(data_frames_list = all_viols)
    # obtain a data frame with all the results for limits from the cross-item level
    # and a column indicating what limit is
    r_complex <-
      all_viols[, intersect(c(VARIABLE_LIST,
                              "NUM_con_rvv_unum",
                              "PCT_con_rvv_unum",
                              "NUM_con_rvv_utdat",
                              "PCT_con_rvv_utdat",
                              "NUM_con_rvv_inum",
                              "PCT_con_rvv_inum",
                              "NUM_con_rvv_itdat",
                              "PCT_con_rvv_itdat",
                              "limits"),
                            colnames(all_viols)
      ),
      FALSE]
    if(nrow(r_complex) > 0) {
      colnames(r_complex)[[1]] <- "Variables"
      r_complex$N <- nrow(study_data)
    } else {
      r_complex <- NULL
    }
  } else {
    r_complex <- NULL
  }


  #Variables with limits in cross-item level----
  if (!is.null(r_complex)) {
    vars_in_cil <- unique(r_complex$Variables)
  } else {
    vars_in_cil <- NULL
  }

  # Which limits should be assessed?
  if (!is.null(limits) && !(all(limits %in% colnames(meta_data)))) {
    limits_miss <- limits[!(limits %in% colnames(meta_data))]
    util_warning(paste0("The limit deviation check cannot be performed for ",
                        paste(dQuote(limits_miss), collapse = ", "),
                        ". There is no matching column in the metadata."),
                 applicability_problem = TRUE)
  }
  known_limits <- unlist(
    WELL_KNOWN_META_VARIABLE_NAMES[intersect(
      grep("LIMIT", WELL_KNOWN_META_VARIABLE_NAMES),
      which(vapply(WELL_KNOWN_META_VARIABLE_NAMES,
                   function(ll) {
                     attr(ll, "var_att_required") != "technical"
                   },
                   FUN.VALUE = logical(1)))
    )]
  )
  given_limits <- colnames(meta_data)[which(colnames(meta_data) %in%
                                              c(known_limits, limits))]
  LIMITS <- given_limits

  if (length(LIMITS) > 0) {
    empty_limit_column <- apply(meta_data[, LIMITS, drop = FALSE], 2,
                                function(cc) { all(util_empty(cc)) })
    if (any(empty_limit_column)) {
      LIMITS <- names(empty_limit_column)[!empty_limit_column]
    }
  }

  if (length(LIMITS) == 0) {
    util_message(paste0("Hint: If your metadata contains a column with limits ",
                        "for which the limit deviation check should be ",
                        "performed, try to pass the name of the column ",
                        "to the ", sQuote("limits"), " argument."))
    util_error(paste0("The limit deviation check cannot be performed without ",
                      "a metadata column specifying suitable limits."),
               applicability_problem = TRUE)
  }

  if (length(resp_vars) == 0) {
    any_limits <- apply(meta_data[, LIMITS, drop = FALSE], 1,
                        function(rr) { !all(util_empty(rr)) })
    if (all(!any_limits)) {
      util_error("No variables with defined limits.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      util_message(paste0("All variables for which limits are specified ",
                          "in the metadata are used."),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      resp_vars <- meta_data[[label_col]][any_limits &
                                            meta_data$DATA_TYPE != "string"]
      resp_vars_iteml <- meta_data[[label_col]][any_limits &
                                                  meta_data$DATA_TYPE != "string"]
      if (length(resp_vars) > 0) {
        rvs_all_na <- apply(ds1[, resp_vars, drop = FALSE], 2,
                            function(cc) { all(is.na(cc)) })
        if (any(rvs_all_na)) {
          resp_vars <- names(rvs_all_na)[!rvs_all_na]
        }
      }
    }
  } else {
    any_limits <- vapply(resp_vars, FUN.VALUE = logical(1), function(rvs) {
      !all(util_empty(meta_data[meta_data[[label_col]] == rvs,
                                LIMITS, drop = FALSE])) |
        rvs %in% r_complex$Variables
    })
    if (all(!any_limits)) {
      util_error("No limits specified.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      rvs_with_lim <- names(any_limits)[any_limits]
      if (length(rvs_with_lim) < length(unique(resp_vars))) {
        util_message(paste0("No limits specified for ",
                            paste(resp_vars[!(resp_vars %in% rvs_with_lim)],
                                  collapse = ", "),
                            "."),
                     applicability_problem = TRUE,
                     intrinsic_applicability_problem = TRUE)
      }
      resp_vars <- rvs_with_lim
      any_item_lev_limits <- vapply(resp_vars,
                                    FUN.VALUE = logical(1), function(rvs) {
                                      !all(util_empty(meta_data[meta_data[[label_col]] == rvs,
                                                                LIMITS, drop = FALSE]))
                                    })
      nmrvl <- names(which(any_item_lev_limits,  useNames = TRUE))
      resp_vars_iteml <-
        nmrvl[util_map_labels(nmrvl, meta_data = meta_data, to = DATA_TYPE,
                              from = label_col, ifnotfound = "",
                              warn_ambiguous = FALSE) %in%
                unlist(setdiff(DATA_TYPES,
                               DATA_TYPES$STRING))]

      resp_vars_iteml <- intersect(resp_vars_iteml, resp_vars) # TODO: needed?
      if (length(resp_vars_iteml) == 0) {
        rm(resp_vars_iteml)
      }
    }
  }

  # Resp_vars----
  # resp_vars now contains all variables with limits in item level or cross item level
  # resp_vars_iteml contains variables with limits in item level
  # vars_in_cil contains variables with limits in cross-item level
  # ONLY cross-item_level metadata present


  # create the subset of variables in the 2 groups: the ones that uses -----
  # item level limits and the ones using complex limits

  #####################################################
  if(!is.null(vars_in_cil) & exists("resp_vars_iteml")) {
    #remove from the vector of the variables inside item_level
    # the variables that are also present in cross-item level
    resp_vars_iteml <- resp_vars_iteml[!resp_vars_iteml %in% vars_in_cil]
  }
  # Variable with only item_level metadata limits ----

  if (exists("resp_vars_iteml") && length(resp_vars_iteml) > 0 ) {
    # In both cases ('resp_vars_iteml' were specified by the user or
    # selected automatically), we have to check whether the given limits can
    # be interpreted as intervals.
    md_lim_int <- setNames(
      nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rvs) {
      lim <- meta_data[meta_data[[label_col]] == rvs, LIMITS, drop = FALSE]
      lim_int <- lapply(lim, util_parse_interval)
      is_interval <- vapply(lim_int, FUN.VALUE = logical(1),
                            function(int) { inherits(int, "interval") })
      wrong_limits <- !is_interval != as.vector(is.na(lim))
      if (any(wrong_limits)) {
        util_warning(
          paste0("The limits given in",
                 paste(sQuote(names(lim)[wrong_limits]), collapse = ", "),
                 " for ", sQuote(rvs), " cannot be interpreted as interval."),
          applicability_problem = TRUE)
      }
      if (all(!is_interval)) {
        return(NA)
      } else {
        if(return_limit_categorical == FALSE) {
          var_scale_lev <- meta_data[meta_data[[label_col]] == rvs, SCALE_LEVEL, drop = FALSE]
          is_categorical <- var_scale_lev %in% c(SCALE_LEVELS$NOMINAL,
                                                 SCALE_LEVELS$ORDINAL)
          if (is_categorical == TRUE) {
            util_message(
              paste0("The limits for ", sQuote(rvs),
                     " cannot be interpreted as the response",
                     " variable is categorical"),
              applicability_problem = TRUE,
              intrinsic_applicability_problem = TRUE)
          }
          if (is_categorical) return(NA)
          return(lim_int[is_interval])
        } else {
          return(lim_int[is_interval])
        }
      }
    }))

  drop_rvs <- is.na(md_lim_int)
  if (all(drop_rvs)) {
    util_error("No feasible check for limit deviations.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }
  if (any(drop_rvs)) {
    resp_vars_iteml <- setdiff(resp_vars_iteml, names(md_lim_int)[drop_rvs])
  }

  # Typkennzeichnung
  is_datetime_var <- vapply(resp_vars_iteml, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] %in% DATA_TYPES$DATETIME
  }, FUN.VALUE = logical(1))
  is_time_var <- vapply(resp_vars_iteml, function(rv) {
    meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] %in% DATA_TYPES$TIME
  }, FUN.VALUE = logical(1))

  # Segmente (below/within/above) je Limit
  md_lim_all_segments <- setNames(
    nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rv) {
      lapply(md_lim_int[[rv]], function(int) {
        ll <- int
        ll$upp <- int$low
        ll$inc_u <- !int$inc_l
        ll$low <- as.numeric(-Inf)
        ll$inc_l <- FALSE
        ul <- int
        ul$upp <- as.numeric(Inf)
        ul$inc_u <- FALSE
        ul$low <- int$upp
        ul$inc_l <- !int$inc_u
        return(list(
          "below" = ll,
          "within" = int,
          "above" = ul
        ))
      })
    }))

  # Werte klassifizieren -------------------------------------------------------
  sd_lim <- setNames(
    nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rvs) {
      col_rv <- ds1[, rvs]
      lim <- md_lim_int[[rvs]]
      col_int <- lapply(lim, function(int) {
        col_out <- rep(NA, length(col_rv))
        if (int$inc_l) below <- col_rv < int$low else below <- col_rv <= int$low
        within <- redcap_env$`in`(col_rv, int)
        if (int$inc_u) above <- col_rv > int$upp else above <- col_rv >= int$upp
        col_out[which(below)]  <- "below"
        col_out[which(within)] <- "within"
        col_out[which(above)]  <- "above"
        return(factor(col_out, levels = c("below", "within", "above")))
      })
      return(col_int)
    })
  )

  # Hard-Limits zuerst ausschließen
  sd_lim_HL_check <- setNames(
    nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rvs) {
      sd_lim_rvs <- sd_lim[[rvs]]
      if ("HARD_LIMITS" %in% names(sd_lim_rvs) && length(sd_lim_rvs) > 1) {
        hl_viol_ind <- which(sd_lim_rvs[["HARD_LIMITS"]] %in% c("below", "above"))
        if (length(hl_viol_ind) > 0) {
          other_lim <- setdiff(names(sd_lim_rvs), "HARD_LIMITS")
          sd_lim_rvs[other_lim] <- lapply(other_lim, function(ol) {
            sd_lim_rvs[[ol]][hl_viol_ind] <- NA
            return(sd_lim_rvs[[ol]])
          })
        }
      }
      return(sd_lim_rvs)
    })
  )

  sd_n_obs <- setNames(
    nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rvs) {
      lapply(sd_lim_HL_check[[rvs]], function(cc) {
        sum(!is.na(cc))
      })
    })
  )

  # get frequency counts of values below, within and above limits
  freq_lim_viol <- setNames(
    nm = resp_vars_iteml,
    lapply(resp_vars_iteml, function(rvs) {
      sd_lim_rvs <- sd_lim_HL_check[[rvs]]
      tab_rvs <- lapply(sd_lim_rvs, function(res_int) {
        t1 <- table(res_int)
        setNames(nm = names(t1), as.vector(t1))
      })
      tab <- as.data.frame(t(as.data.frame(tab_rvs)))
      tab$samplesize <- apply(tab,1, sum)
      tab
    }))

  # Plot-Vorbereitung ----------------------------------------------------------
  ref_env <- environment()
  base_col <- colorRampPalette(
    colors = c("#2166AC","#fdd49e","#fc8d59","#d7301f","#B2182B","#7f0000")
  )(length(given_limits) + 1)
  base_lty <- c(2, 4:6)
  spec_txt <- element_text(colour = "black", hjust = .5, vjust = .5, face = "plain")
  spec_lines <- data.frame(limits = "HARD_LIMITS", color = "#B2182B", lty = 1)
  given_lim_wo_hard <- setdiff(given_limits, "HARD_LIMITS")
  n_spec_lim <- length(given_lim_wo_hard)
  if (n_spec_lim > 0) {
    spec_lines <- rbind(
      spec_lines,
      data.frame(
        limits = given_lim_wo_hard,
        color = rep(gray.colors(n = ceiling(n_spec_lim/length(base_lty))),
                    each = length(base_lty))[1:n_spec_lim],
        lty = c(rep(base_lty, floor(n_spec_lim/length(base_lty))),
                base_lty[seq_len(n_spec_lim %% length(base_lty))])
      )
    )
  }

  # ein Plot pro Variable
  plot_list <- lapply(setNames(nm = resp_vars_iteml), function(rv) {
    # Limits einsammeln (Werte unname(), Namen separat)
    lower_limits <- do.call(c, unname(lapply(md_lim_int[[rv]], '[[', "low")))
    if (any(is.infinite(lower_limits))) lower_limits[is.infinite(lower_limits)] <- NA
    upper_limits <- do.call(c, unname(lapply(md_lim_int[[rv]], '[[', "upp")))
    if (any(is.infinite(upper_limits))) upper_limits[is.infinite(upper_limits)] <- NA
    lower_limit_names <- names(md_lim_int[[rv]])
    upper_limit_names <- names(md_lim_int[[rv]])
    rv_limit_names    <- names(md_lim_int[[rv]])

    # Daten
    rv_data <- data.frame("values" = ds1[[rv]], sd_lim_HL_check[[rv]])
    rv_data <- rv_data[complete.cases(rv_data[, "values"]), ]

    # numerisch für hist()
    if (is_datetime_var[[rv]]) {
      rv_data$values <- as.numeric(rv_data$values)                           # POSIX sec
    } else if (is_time_var[[rv]]) {
      v <- rv_data$values
      if (inherits(v, "hms")) {
        rv_data$values <- suppressWarnings(as.numeric(v))                    # Sekunden
      } else if (inherits(v, "difftime")) {
        rv_data$values <- suppressWarnings(as.numeric(v, units = "secs"))
      } else if (is.character(v)) {
        rv_data$values <- suppressWarnings(as.numeric(util_parse_time(v)))
      } else if (!is.numeric(v)) {
        rv_data$values <- suppressWarnings(as.numeric(hms::as_hms(v)))
      }
    }

    # Bereiche
    max_data <- max(rv_data$values, na.rm = TRUE)
    min_data <- min(rv_data$values, na.rm = TRUE)
    max_plot <- max(c(max_data, lower_limits, upper_limits), na.rm = TRUE)
    min_plot <- min(c(min_data, lower_limits, upper_limits), na.rm = TRUE)

    # Füllungen
    rv_data$viol_n_limits <- apply(rv_data[, rv_limit_names, drop = FALSE], 1,
                                   function(rr) length(which(rr != "within")))
    rv_data$limit_violations <- paste0("detected (", rv_data$viol_n_limits, ")")
    rv_data$limit_violations[rv_data$viol_n_limits == 0] <- "none"
    rv_data$fill <- vapply(rv_data$viol_n_limits,
                           FUN.VALUE = "character(1)",
                           FUN = function(vv) base_col[vv + 1])
    if ("HARD_LIMITS" %in% colnames(rv_data)) {
      rv_data$limit_violations[rv_data$HARD_LIMITS != "within"] <- "severe"
      rv_data$fill[rv_data$HARD_LIMITS != "within"] <- rev(base_col)[1]
    }
    spec_bars <- unique(rv_data[, c(rv_limit_names, "limit_violations", "fill")])

    plot_histogram <-
      meta_data[[SCALE_LEVEL]][which(meta_data[[label_col]] == rv)] %in%
      c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO)

    plot_histogram_integer <-
      meta_data[[SCALE_LEVEL]][which(meta_data[[label_col]] == rv)] %in%
      c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO) &&
      meta_data[[DATA_TYPE]][which(meta_data[[label_col]] == rv)] %in% c("integer")

    xlb <- prep_get_labels(resp_vars = rv,
                           resp_vars_match_label_col_only = TRUE,
                           label_class = "SHORT",
                           label_col = label_col,
                           item_level = meta_data)

    if (plot_histogram) {
      # Segmente bestimmen
      plot_segments <- rv_data %>%
        dplyr::arrange(values) %>%
        dplyr::select(all_of(rv_limit_names)) %>%
        dplyr::group_by_all(.) %>%
        dplyr::count() %>%
        as.data.frame(., drop = FALSE)

      plot_segments_intervals <-
        apply(plot_segments[, rv_limit_names, drop = FALSE], 1, function(rr) {
          rr_int <- lapply(seq_along(rr), function(ii) {
            md_lim_all_segments[[rv]][[rv_limit_names[ii]]][[rr[ii]]]
          })
          out_int <- rr_int[[1]]
          rr_low <- do.call(c, unname(lapply(rr_int, '[[', "low")))
          rr_low_max <- max(rr_low, na.rm = TRUE)
          out_int$low <- rr_low_max
          rr_low_which_max <- which(rr_low == rr_low_max)
          if (length(rr_low_which_max) > 1) {
            inc_l <- do.call(c, unname(lapply(rr_int, '[[', "inc_l")))[rr_low_which_max]
            out_int$inc_l <- if (any(!inc_l)) FALSE else TRUE
          } else {
            out_int$inc_l <- rr_int[[rr_low_which_max]][["inc_l"]]
          }
          rr_upp <- do.call(c, unname(lapply(rr_int, '[[', "upp")))
          rr_upp_min <- min(rr_upp, na.rm = TRUE)
          out_int$upp <- rr_upp_min
          rr_upp_which_min <- which(rr_upp == rr_upp_min)
          if (length(rr_upp_which_min) > 1) {
            inc_u <- do.call(c, unname(lapply(rr_int, '[[', "inc_u")))[rr_upp_which_min]
            out_int$inc_u <- if (any(!inc_u)) FALSE else TRUE
          } else {
            out_int$inc_u <- rr_int[[rr_upp_which_min]][["inc_u"]]
          }
          return(out_int)
        })

      largest_segment <- plot_segments_intervals[[which.max(plot_segments$n)]]

      bin_breaks <- util_optimize_histogram_bins(
        x = rv_data$values,
        interval_freedman_diaconis = largest_segment,
        cuts = sort(as.numeric(unique(c(
          do.call(c, unname(lapply(plot_segments_intervals, '[[', "low"))),
          do.call(c, unname(lapply(plot_segments_intervals, '[[', "upp"))))))),
        nbins_max = 100)

      if (length(plot_segments_intervals) != length(bin_breaks)) {
        bin_segments_with_data <-
          vapply(bin_breaks, FUN.VALUE = logical(1), function(bb) {
            sum(
              vapply(plot_segments_intervals, FUN.VALUE = logical(1),
                     function(p_int) {
                       bb[1] >= p_int$low & bb[1] <= p_int$upp &
                         bb[length(bb)] >= p_int$low & bb[length(bb)] <= p_int$upp
                     })
            ) == 1
          })
        bin_breaks[!bin_segments_with_data] <- NULL
      }

      # Histogramdaten
      plot_data <- lapply(seq_along(plot_segments_intervals), function(ii) {
        subd1 <- rv_data[redcap_env$`in`(rv_data$values,
                                         plot_segments_intervals[[ii]]),
                         c("values", "limit_violations")]
        h1 <- hist(subd1$values, plot = FALSE, breaks = bin_breaks[[ii]])
        return(data.frame(histogram_x = h1$mids,
                          histogram_y = h1$counts,
                          limit_violations = subd1$limit_violations[1],
                          segment_number = ii,
                          row.names = NULL))
      })
      plot_data <- do.call(rbind, plot_data)
      if (is_datetime_var[rv]) {
        plot_data[["histogram_x"]] <- util_parse_date(plot_data[["histogram_x"]])
      }

      # x-Limits
      if (!is_datetime_var[[rv]]) {
        myxlim <- c(floor(min_plot), ceiling(max_plot))
      } else {
        myxlim <- c(min_plot, max_plot)
        myxlim <- util_parse_date(myxlim)
      }

      # Limit-Linien (Werte & Namen getrennt)
      all_limits_vals  <- c(lower_limits, upper_limits)
      all_limits_names <- c(lower_limit_names, upper_limit_names)
      keep <- !is.na(all_limits_vals)
      if (!all(spec_lines$limits %in% colnames(meta_data))) {
        for (cl in setdiff(spec_lines$limits, colnames(meta_data)))
          meta_data[[cl]] <- rep("", nrow(meta_data))
      }
      all_limits_df <- data.frame(
        limits = all_limits_names[keep],
        values = all_limits_vals[keep]
      )

      all_limits_df$limits <- paste(
        all_limits_df$limits,
        meta_data[meta_data[[label_col]] == rv,
                  all_limits_df$limits, drop = FALSE])
      spec_lines$limits <- paste(
        spec_lines$limits,
        meta_data[meta_data[[label_col]] == rv,
                  spec_lines$limits, drop = FALSE])

      if (is_datetime_var[[rv]]) {
        all_limits_df$values <- as.POSIXct(all_limits_df$values,
                                           origin = min(Sys.time(), 0))
      }

      # Histogram-Plot
      p <- util_create_lean_ggplot(
        ggplot(data = plot_data,
               aes(x = .data[["histogram_x"]],
                   y = .data[["histogram_y"]],
                   fill = .data[["limit_violations"]])),
        plot_data = plot_data
      )

      for (ii in seq_along(plot_segments_intervals)) {
        dt <- subset(plot_data, segment_number == ii)
        bb_ii <- as.numeric(bin_breaks[[ii]])
        width_col <- bb_ii[2] - bb_ii[1]
        p <- p %lean+% util_create_lean_ggplot(
          geom_col(data = dt, width = width_col),
          dt = dt,
          width_col = width_col
        )
      }

      # Punkte (optional)
      bb <- do.call(c, unname(bin_breaks))
      bb <- bb[!duplicated(bb)]
      if (plot_histogram_integer) {
        if (mean(bb[-1] - bb[-length(bb)]) < 7 && length(bb) <= 6) show_obs <- FALSE
      }
      if (show_obs) {
        max_bar_height <- max(table(cut(rv_data$value, breaks = bb)))
        ypos <- -0.05 * max_bar_height
        subdata <- NULL
        for (ii in seq_len(length(bb)-1)) {
          if (ii != length(bb) - 1) {
            subd <- subset(rv_data, values >= bb[ii] & values < bb[ii+1])
          } else {
            subd <- subset(rv_data, values >= bb[ii] & values <= bb[ii+1])
          }
          if (nrow(subd) > 7) {
            sel_val <- quantile(as.numeric(subd$values),
                                probs = seq(0, 1, length.out = 7), type = 3)
            sel_ind <- vapply(sel_val, FUN.VALUE = integer(1),
                              FUN = function(x) which(as.numeric(subd$value) == x)[1])
            subd <- subd[sel_ind, ]
          }
          if (nrow(subd) > 0) subdata <- rbind(subdata, subd)
        }
        colnames(subdata)[which(colnames(subdata) == "values")] <- "histogram_x"
        if (is_datetime_var[[rv]]) {
          subdata$histogram_x <- util_parse_date(subdata$histogram_x)
        }

        p <- p %lean+% util_create_lean_ggplot(
          geom_point(aes(y = ypos),
                     data = subdata,
                     col = "darkgray",
                     position = position_jitter(height = 0.01 * max_bar_height,
                                                width = 0),
                     size = 1,
                     alpha = 0.6),
          ypos = ypos,
          subdata = subdata,
          max_bar_height = max_bar_height
        )
      }

      fli <- util_coord_flip(ref_env = ref_env, xlim = myxlim)
      p <- p %lean+% util_create_lean_ggplot(
        geom_vline(data = all_limits_df,
                   aes(xintercept = .data[["values"]],
                       linetype = .data[["limits"]],
                       color = .data[["limits"]])),
        all_limits_df = all_limits_df
      );
      p <- util_lazy_add_coord(p, fli)
      p <- p %lean+%
        scale_fill_manual(values = spec_bars$fill,
                          breaks = spec_bars$limit_violations,
                          guide = "none") %lean+%
        ggplot2::scale_linetype_manual(name = "limits",
                                       values = spec_lines$lty,
                                       breaks = spec_lines$limits) %lean+%
        scale_color_manual(name = "limits",
                           values = spec_lines$color,
                           breaks = spec_lines$limits) %lean+%
        labs(x = paste0(xlb), y = "") %lean+%
        theme_minimal() %lean+%
        theme(
          title = spec_txt,
          axis.text.x = spec_txt,
          axis.text.y = spec_txt,
          axis.title.x = spec_txt,
          axis.title.y = spec_txt
        )

      if (is_datetime_var[[rv]]) {
        p <- p %lean+% util_create_lean_ggplot(
          ggplot2::scale_x_datetime(expand = expansion(mult = 0.1))
        )
      } else if (plot_histogram_integer && !is_time_var[[rv]]) {
        pretty_x_axt <- util_int_breaks_rounded(c(rv_data$values,
                                                  all_limits_df$values,
                                                  myxlim))
        pretty_x_axt <- pretty_x_axt[
          which(pretty_x_axt >= myxlim[1] - 0.1 * abs(myxlim[2] - myxlim[1]) &
                  pretty_x_axt <= myxlim[2] + 0.1 * abs(myxlim[2] - myxlim[1]))]
        p <- p %lean+% util_create_lean_ggplot(
          scale_x_continuous(breaks = pretty_x_axt,
                             expand = expansion(mult = 0.1)),
          pretty_x_axt = pretty_x_axt
        )
      } else {
        if (is_time_var[[rv]]) {
          p <- p %lean+% util_create_lean_ggplot(
            scale_x_continuous(
              expand = expansion(mult = 0.1),
              labels = function(x) {
                util_as_character(hms::as_hms(x))
              }
            )
          )
        } else {
          p <- p %lean+% util_create_lean_ggplot(
            scale_x_continuous(expand = expansion(mult = 0.1))
          )
        }
      }

      # Sizing
      min_bin_height <- min(plot_data$histogram_y)
      max_bin_height <- max(plot_data$histogram_y)
      no_bars <- nrow(plot_data)
      obj1 <- util_create_lean_ggplot(ggplot2::ggplot_build(p), p = p)
      total_w <- c(util_rbind(data_frames_list = obj1$data)$xmin,
                   util_rbind(data_frames_list = obj1$data)$xmax,
                   util_rbind(data_frames_list = obj1$data)$xintercept)
      total_w <- total_w[!is.na(total_w)]
      min_total_w <- min(total_w); max_total_w <- max(total_w)
      total_w <- max_total_w - min_total_w
      min_x_plot <- min(util_rbind(data_frames_list = obj1$data)$xmin, na.rm = TRUE)
      max_x_plot <- max(util_rbind(data_frames_list = obj1$data)$xmax, na.rm = TRUE)
      no_char_y <- nchar(round(max_bin_height, digits = 0))
      no_char_x <- nchar(round(max_total_w, digits = 0))
      rm(obj1)

      min_limits <- min(all_limits_df$values)
      max_limits <- max(all_limits_df$values)

    } else {
      # Balkendiagramm ---------------------------------------------------------
      all_limits_vals  <- c(lower_limits - 0.5, upper_limits + 0.5)
      all_limits_names <- c(lower_limit_names, upper_limit_names)
      keep <- !is.na(all_limits_vals)
      all_limits_df <- data.frame(
        limits = all_limits_names[keep],
        values = all_limits_vals[keep]
      )

      all_limits_df$limits <- paste(
        all_limits_df$limits,
        meta_data[meta_data[[label_col]] == rv,
                  all_limits_df$limits, drop = FALSE])
      spec_lines$limits <- paste(
        spec_lines$limits,
        meta_data[meta_data[[label_col]] == rv,
                  spec_lines$limits, drop = FALSE])

      if (is_datetime_var[[rv]]) {
        all_limits_df$values <- as.POSIXct(all_limits_df$values,
                                           origin = min(Sys.time(), 0))
      }

      max_plot <- max_plot + 0.5
      min_plot <- min_plot - 0.5

      plot_data <- rv_data %>%
        dplyr::add_count(values) %>%
        dplyr::distinct()

      fli <- util_coord_flip(ref_env = ref_env, xlim = c(min_plot, max_plot))
      pretty_x_axt <- util_int_breaks_rounded(c(plot_data$values, max_plot, min_plot))
      pretty_x_axt <- pretty_x_axt[
        which(pretty_x_axt >= min_plot - 0.1 * abs(max_plot - min_plot) &
                pretty_x_axt <= max_plot + 0.1 * abs(max_plot - min_plot))]

      p <- util_create_lean_ggplot(
        ggplot(plot_data, aes(x = .data[["values"]], y = .data[["n"]],
                              fill = .data[["limit_violations"]])) %lean+%
          geom_col(width = 0.8) %lean+%
          geom_vline(data = all_limits_df,
                     aes(xintercept = .data[["values"]],
                         linetype = .data[["limits"]],
                         color = .data[["limits"]])) %lean+%
          fli %lean+%
          scale_fill_manual(values = spec_bars$fill,
                            breaks = spec_bars$limit_violations,
                            guide = "none") %lean+%
          ggplot2::scale_linetype_manual(name = "limits",
                                         values = spec_lines$lty,
                                         breaks = spec_lines$limits) %lean+%
          scale_color_manual(name = "limits",
                             values = spec_lines$color,
                             breaks = spec_lines$limits) %lean+%
          labs(x = paste0(xlb), y = "") %lean+%
          theme_minimal() %lean+%
          theme(
            title = spec_txt,
            axis.text.x = spec_txt,
            axis.text.y = spec_txt,
            axis.title.x = spec_txt,
            axis.title.y = spec_txt
          ) %lean+%
          scale_x_continuous(breaks = pretty_x_axt,
                             expand = expansion(mult = 0.1)),
        plot_data = plot_data,
        all_limits_df = all_limits_df,
        min_plot = min_plot,
        max_plot = max_plot,
        spec_bars = spec_bars,
        spec_lines = spec_lines,
        xlb = xlb,
        spec_txt = spec_txt,
        fli = fli,
        pretty_x_axt = pretty_x_axt
      )

      min_bin_height <- min(plot_data$n)
      max_bin_height <- max(plot_data$n)
      no_bars <- nrow(plot_data)
      obj1 <- util_create_lean_ggplot(ggplot2::ggplot_build(p), p = p)
      total_w <- c(util_rbind(data_frames_list = obj1$data)$xmin,
                   util_rbind(data_frames_list = obj1$data)$xmax,
                   util_rbind(data_frames_list = obj1$data)$xintercept)
      total_w <- total_w[!is.na(total_w)]
      min_total_w <- min(total_w); max_total_w <- max(total_w)
      total_w <- max_total_w - min_total_w
      min_x_plot <- min(util_rbind(data_frames_list = obj1$data)$xmin, na.rm = TRUE)
      max_x_plot <- max(util_rbind(data_frames_list = obj1$data)$xmax, na.rm = TRUE)
      no_char_y <- nchar(round(max_bin_height, digits = 0))
      no_char_x <- nchar(round(max_total_w, digits = 0))
      rm(obj1)

      min_limits <- min(all_limits_df$values)
      max_limits <- max(all_limits_df$values)
    }

    # Sizing-Hints -------------------------------------------------------------
    range_x <- max_x_plot - min_x_plot
    range_height <- max_bin_height - min_bin_height
    no_bars_in_all_w <- round((total_w * no_bars)/range_x, digits = 0)
    attr(p, "sizing_hints") <- list(figure_type_id = "bar_limit",
                                    range = range_height,
                                    no_bars_in_all_w = no_bars_in_all_w,
                                    no_char_x = no_char_x,
                                    no_char_y = no_char_y)
    return(p)
  }) # end lapply plot_list

  # FlaggedStudyData -----------------------------------------------------------
  if (return_flagged_study_data) {
    fsd <- do.call(cbind.data.frame, sd_lim)
    colnames(fsd) <- paste(
      unlist(lapply(names(sd_lim), function(rv) {
        rep(rv, length(sd_lim[[rv]]))
      })),
      unlist(lapply(sd_lim, names)),
      sep = "_")
    fsd <- cbind(ds1, fsd)
  } else {
    fsd <- NULL
  }

  # SummaryData ----------------------------------------------------------------
  sumdat <- do.call(rbind.data.frame,
                    lapply(setNames(nm = resp_vars_iteml), function(rv) {
                      divisor <- rep(unname(unlist(sd_n_obs[[rv]])), each = 4)
                      data.frame(
                        "Variables" = rv,
                        "Section" = rep(colnames(freq_lim_viol[[rv]]),
                                        nrow(freq_lim_viol[[rv]])),
                        "Limits" = rep(rownames(freq_lim_viol[[rv]]),
                                       each = 4),
                        "Number" = c(t(freq_lim_viol[[rv]])),
                        "Percentage" = ifelse(divisor == 0, 0,
                                              round(c(t(freq_lim_viol[[rv]])) /
                                                      divisor * 100, 2)),
                        check.names = FALSE, fix.empty.names = FALSE)
                    }))
  rownames(sumdat) <- NULL

  # Report-Summary-Table -------------------------------------------------------
  report_all_limits <- unique(sumdat$Limits)
  heatmap_tab <- do.call(
    rbind.data.frame,
    lapply(setNames(nm = resp_vars_iteml), function(rv) {
      n_viol <- rowSums(freq_lim_viol[[rv]][, c("below", "above"), drop = FALSE])
      divisor <- unlist(sd_n_obs[[rv]])[rownames(freq_lim_viol[[rv]])]
      n_viol_rel <- ifelse(divisor == 0, 0, n_viol / divisor)
      out_viol <- setNames(rep(NA, length(report_all_limits)), report_all_limits)
      out_viol[names(n_viol_rel)] <- n_viol_rel
      out_viol <- as.data.frame(t(out_viol))
      out_viol$Variables <- rv
      out_viol$N <- 1
      return(out_viol[, c("Variables", "N",
                          setdiff(colnames(out_viol), c("Variables", "N")))])
    }))
  rownames(heatmap_tab) <- NULL
  colnames(heatmap_tab)[3:ncol(heatmap_tab)] <-
    paste(colnames(heatmap_tab)[3:ncol(heatmap_tab)], "violations")

  heatmap_tab <- util_validate_report_summary_table(heatmap_tab,
                                                    meta_data = meta_data,
                                                    label_col = label_col)

  # SummaryTable ---------------------------------------------------------------
  sumtab_inadm_num <- vapply(
    resp_vars_iteml, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        sumdat_sub <- sumdat %>%
          dplyr::filter(Variables == rv & Limits == "HARD_LIMITS")
        if (nrow(sumdat_sub) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          res <- sumdat_sub %>%
            dplyr::select(Section, Number) %>%
            dplyr::mutate(n = sum(Number)) %>%
            dplyr::filter(!Section %in% c("within", "samplesize")) %>%
            dplyr::mutate(n_viol = sum(Number)) %>%
            dplyr::distinct(n_viol, n)
          res <- unlist(res)
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })
  sumtab_inadm_datetime <- vapply(
    resp_vars_iteml, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (!is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        sumdat_sub <- sumdat %>%
          dplyr::filter(Variables == rv & Limits == "HARD_LIMITS")
        if (nrow(sumdat_sub) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          res <- sumdat_sub %>%
            dplyr::select(Section, Number) %>%
            dplyr::mutate(n = sum(Number)) %>%
            dplyr::filter(!Section %in% c("within", "samplesize")) %>%
            dplyr::mutate(n_viol = sum(Number)) %>%
            dplyr::distinct(n_viol, n)
          res <- unlist(res)
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })

  sumtab_unc_num <- vapply(
    resp_vars_iteml, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        rv_sd_int <- sd_lim_HL_check[[rv]]
        rv_sd_int <- rv_sd_int[which(names(rv_sd_int) != "HARD_LIMITS")]
        if (length(rv_sd_int) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          rv_sd_int <- lapply(rv_sd_int, function(ll) {
            levels(ll) <- c(1, 0, 1)
            return(as.numeric(as.character(ll)))
          })
          rv_sd_int <- as.data.frame(rv_sd_int)
          rv_sd_int <- rv_sd_int[complete.cases(rv_sd_int), , drop = FALSE]
          rv_sd_int$N_lim <- rowSums(rv_sd_int)
          res <- c(length(which(rv_sd_int$N_lim > 0)),
                   nrow(rv_sd_int))
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })
  sumtab_unc_datetime <- vapply(
    resp_vars_iteml, FUN.VALUE = numeric(2), FUN = function(rv) {
      if (!is_datetime_var[[rv]]) {
        res <- c(NA, nrow(ds1))
      } else {
        rv_sd_int <- sd_lim_HL_check[[rv]]
        rv_sd_int <- rv_sd_int[which(names(rv_sd_int) != "HARD_LIMITS")]
        if (length(rv_sd_int) == 0) {
          res <- c(0, nrow(ds1))
        } else {
          rv_sd_int <- lapply(rv_sd_int, function(ll) {
            levels(ll) <- c(1, 0, 1)
            return(as.numeric(as.character(ll)))
          })
          rv_sd_int <- as.data.frame(rv_sd_int)
          rv_sd_int <- rv_sd_int[complete.cases(rv_sd_int), , drop = FALSE]
          rv_sd_int$N_lim <- rowSums(rv_sd_int)
          res <- c(length(which(rv_sd_int$N_lim > 0)),
                   nrow(rv_sd_int))
        }
      }
      names(res) <- c("n_viol", "n")
      return(res)
    })

  sumtab <- data.frame(
    "Variables" = resp_vars_iteml,
    "NUM_con_rvv_unum" = sumtab_unc_num["n_viol", ],
    "PCT_con_rvv_unum" = round(sumtab_unc_num["n_viol", ] /
                                 sumtab_unc_num["n", ] * 100, 2),
    "FLG_con_rvv_unum" = ifelse(sumtab_unc_num["n_viol", ] > 0, TRUE, FALSE),
    "NUM_con_rvv_utdat" = sumtab_unc_datetime["n_viol", ],
    "PCT_con_rvv_utdat" = round(sumtab_unc_datetime["n_viol", ] /
                                  sumtab_unc_datetime["n", ] * 100, 2),
    "FLG_con_rvv_utdat" = ifelse(sumtab_unc_datetime["n_viol", ] > 0,
                                 TRUE, FALSE),
    "NUM_con_rvv_inum" = sumtab_inadm_num["n_viol", ],
    "PCT_con_rvv_inum" = round(sumtab_inadm_num["n_viol", ] /
                                 sumtab_inadm_num["n", ] * 100, 2),
    "FLG_con_rvv_inum" = ifelse(sumtab_inadm_num["n_viol", ] > 0, TRUE, FALSE),
    "NUM_con_rvv_itdat" = sumtab_inadm_datetime["n_viol", ],
    "PCT_con_rvv_itdat" = round(sumtab_inadm_datetime["n_viol", ] /
                                  sumtab_inadm_datetime["n", ] * 100, 2),
    "FLG_con_rvv_itdat" = ifelse(sumtab_inadm_datetime["n_viol", ] > 0,
                                 TRUE, FALSE))
  sumtab$PCT_con_rvv_unum[which(sumtab_unc_num["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_utdat[which(sumtab_unc_datetime["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_inum[which(sumtab_inadm_num["n", ] == 0)] <- 0
  sumtab$PCT_con_rvv_itdat[which(sumtab_inadm_datetime["n", ] == 0)] <- 0

  rownames(sumtab) <- NULL

  # SummaryData breit ----------------------------------------------------------
  # sumtab <- util_map_to_other_metrics(sumtab,
  #                                     meta_data,
  #                                     label_col)

  # Modify the SummaryData from long to wide format to be more concise to read
  sumdat_wide <- stats::reshape(sumdat,
                                idvar = c("Variables", "Limits"),
                                timevar = "Section",
                                direction = "wide")
  sumdat_wide$"All.outside.limitsN" <- sumdat_wide$Number.below + sumdat_wide$Number.above
  sumdat_wide$"All.outside.limits%" <- round(sumdat_wide$"All.outside.limitsN"/
                                               sumdat_wide$Number.samplesize,
                                             digits = 2)
  sumdat_wide$`Below.limits-N (%)`  <- paste0(sumdat_wide$Number.below,  " (", sumdat_wide$Percentage.below,")")
  sumdat_wide$`Within.limits-N (%)` <- paste0(sumdat_wide$Number.within, " (", sumdat_wide$Percentage.within,")")
  sumdat_wide$`Above.limits-N (%)`  <- paste0(sumdat_wide$Number.above,  " (", sumdat_wide$Percentage.above,")")
  sumdat_wide$`All outside limits N (%)` <- paste0(sumdat_wide$All.outside.limitsN, " (",
                                                   sumdat_wide$`All.outside.limits%`,")")
  sumdat_wide <- sumdat_wide[, c("Variables", "Limits", "Below.limits-N (%)",
                                 "Within.limits-N (%)", "Above.limits-N (%)",
                                 "All outside limits N (%)")]
  colnames(sumdat_wide) <- c("Variables", "Limits", "Below limits N (%)",
                             "Within limits N (%)", "Above limits N (%)",
                             "All outside limits N (%)")

  text_to_display <- util_get_hovertext("[con_limit_dev_hover]")
  attr(sumdat_wide, "description") <- text_to_display

  if (length(plot_list) == 1) {
    .plot1 <- plot_list[[resp_vars]]
    obj1 <- util_create_lean_ggplot(ggplot2::ggplot_build(.plot1), .plot1 = .plot1)
    min_bin_height <- min(util_rbind(data_frames_list = obj1$data)$ymax, na.rm = TRUE)
    max_bin_height <- max(util_rbind(data_frames_list = obj1$data)$ymax, na.rm = TRUE)
    no_bars <- sum(!is.na(util_rbind(data_frames_list = obj1$data)$ymax))
    total_w <- c(util_rbind(data_frames_list = obj1$data)$xmin,
                 util_rbind(data_frames_list = obj1$data)$xmax,
                 util_rbind(data_frames_list = obj1$data)$xintercept)
    total_w <- total_w[!is.na(total_w)]
    min_total_w <- min(total_w); max_total_w <- max(total_w)
    total_w <- max_total_w - min_total_w
    min_x_plot <- min(util_rbind(data_frames_list = obj1$data)$xmin, na.rm = TRUE)
    max_x_plot <- max(util_rbind(data_frames_list = obj1$data)$xmax, na.rm = TRUE)
    range_x <- max_x_plot - min_x_plot
    no_bars_in_all_w <- round((total_w * no_bars)/range_x, digits = 0)
    range_height <- max_bin_height - min_bin_height

    if (is.infinite(min_bin_height) || is.infinite(max_bin_height)) {
      range_height <- 100
      if (is.infinite(min_bin_height)) min_bin_height <- 0
      if (is.infinite(max_bin_height)) max_bin_height <- 0
    }

    no_char_y <- nchar(round(max_bin_height, digits = 0))
    no_char_x <- nchar(round(total_w, digits = 0))
    rm(obj1)
  }

  }
  ####################################################
  #Complex limits
  # Variable with complex limits (if they also have item_level metadata limits,
  # they will be ignored) -----

  if (!is.null(vars_in_cil)) {
    # Which variables are of type 'datetime'?
    is_datetime_var_cil <- vapply(vars_in_cil, function(rv) {
      meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
        DATA_TYPES$DATETIME
    }, FUN.VALUE = logical(1))
    #create a list of variables in cross-item levels and their limits
    # with total numbers and values within
    cil_limits <- setNames(
      nm = vars_in_cil,
      lapply(vars_in_cil, function(rvs) {
        # Filter r_complex for the current variable (rvs)
        r_complex_var <- r_complex[r_complex$Variables == rvs, ]
        # Extract the limits for the current variable.
        lims <- r_complex_var$limits
        col_int <- lapply(lims, function(ll) {
          #Filter r_complex_var for the current limit (int)
          r_complex_limit <- r_complex_var[r_complex_var$limits == ll, ]
          # Find the column name that starts with "NUM_" and is NOT NA.
          num_cols <- grep("^NUM_", names(r_complex_limit), value = TRUE)
          # Select the one column in num_cols that has a non-NA value
          col_to_use <- num_cols[!is.na(r_complex_limit[, num_cols])]
          # Check if a column was found and, if so, extract the value (all_outliers)
          if (length(col_to_use) == 1) {
            all_outliers <- r_complex_limit[[col_to_use]]
          } else {
            all_outliers <- NA
          }
          n_tot <- length(ds1[, rvs]) # sample size from ds1
          within <- n_tot - all_outliers
          # Return the data frame for this limit
          return(data.frame(N = n_tot,
                            all_outliers = all_outliers,
                            within = within))
        })
        # Name the elements of the list using the limits
        names(col_int) <- lims
        return(col_int)
      })
    )
    # Flagged data and plot_list not possible
    # if limits only in cross-item level metadata -----
    if (!exists("fsd")) {
      fsd <- NULL
    }


    #sumdat from cross-item level-------
    sumdat_complex <-
      do.call(rbind.data.frame,
              lapply(setNames(nm = vars_in_cil,
                              object = vars_in_cil),
                     function(rv) {
                       # Filter r_complex for the current variable
                       r_complex_var <- r_complex[r_complex$Variables == rv, ]
                       # Extract the limits for the current variable
                       lims <- r_complex_var$limits
                       # rbind results for the current variable
                       do.call(rbind.data.frame,
                               lapply(lims,
                                      function(ll) {
                                        rv_data <- cil_limits[[rv]][[ll]]
                                        divisor <- rv_data$N
                                        data.frame(
                                          "Variables" = rv,
                                          "Section" = c("within", "all_outliers"),
                                          "Limits" = rep(ll, 2),
                                          "Number" = c(rv_data$within,
                                                       rv_data$all_outliers),
                                          "Percentage" = c(ifelse(divisor == 0, 0,
                                                                  round(rv_data$within/

                                                                          divisor * 100,
                                                                        2)),
                                                           ifelse(divisor == 0, 0,
                                                                  round(rv_data$all_outliers/
                                                                          divisor * 100,
                                                                        2))),
                                          check.names = FALSE, fix.empty.names = FALSE,
                                          stringsAsFactors = FALSE
                                        )
                                      }))
                     }))
    rownames(sumdat_complex) <- NULL

    # report summary table -----
    report_all_limits <- unique(sumdat_complex$Limits)
    dt_rcomplex <- util_map_labels(
      r_complex[["Variables"]],
      meta_data = meta_data,
      to = DATA_TYPE,
      from = label_col
    )
    # heatmap -------
    heatmap_tab_complex <- do.call(
      rbind.data.frame,
      lapply(setNames(nm = vars_in_cil),
             function(rv) {
               cur_row <- r_complex[r_complex$Variables==rv &
                                      r_complex$limits=="HARD_LIMITS", ]
               if (nrow(cur_row) > 0){
                 if (dt_rcomplex[cur_row[cur_row$Variables==rv,
                                         "Variables"]] != "datetime") {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_inum"]
                 } else {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_itdat"]
                 }

                 divisor <- cur_row[cur_row$Variables==rv, "N"]
                 n_viol_rel <- ifelse(divisor == 0, 0, n_viol / divisor)
                 names(n_viol_rel) <- "HARD_LIMITS"
               }
               cur_row <- r_complex[r_complex$Variables==rv &
                                      r_complex$limits=="SOFT_LIMITS", ]
               if (nrow(cur_row) > 0){
                 if (dt_rcomplex[cur_row[cur_row$Variables==rv,
                                         "Variables"]] != "datetime") {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_unum"]
                 } else {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_utdat"]
                 }
                 divisor <- cur_row[cur_row$Variables==rv, "N"]
                 if(exists("n_viol_rel")){
                   n_viol_rel2 <- ifelse(divisor == 0, 0, n_viol / divisor)
                   names(n_viol_rel2) <- "SOFT_LIMITS"
                   n_viol_rel <- c(n_viol_rel, n_viol_rel2)
                 } else {
                   n_viol_rel <- ifelse(divisor == 0, 0, n_viol / divisor)
                   names(n_viol_rel) <- "SOFT_LIMITS"
                 }
               }
               cur_row <- r_complex[r_complex$Variables==rv &
                                      r_complex$limits=="DETECTION_LIMITS", ]
               if (nrow(cur_row) > 0){
                 if (dt_rcomplex[cur_row[cur_row$Variables==rv,
                                         "Variables"]] != "datetime") {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_unum"]
                 } else {
                   n_viol <- cur_row[cur_row$Variables==rv,
                                     "NUM_con_rvv_utdat"]
                 }
                 divisor <- cur_row[cur_row$Variables==rv, "N"]
                 if(exists("n_viol_rel")){
                   n_viol_rel2 <- ifelse(divisor == 0, 0, n_viol / divisor)
                   names(n_viol_rel2) <- "SOFT_LIMITS"
                   n_viol_rel <- c(n_viol_rel, n_viol_rel2)
                 } else {
                   n_viol_rel <- ifelse(divisor == 0, 0, n_viol / divisor)
                   names(n_viol_rel) <- "SOFT_LIMITS"
                 }
               }
               out_viol <-
                 setNames(rep(NA, length(report_all_limits)), report_all_limits)
               out_viol[names(n_viol_rel)] <- n_viol_rel
               out_viol <- as.data.frame(t(out_viol))
               out_viol$Variables <- rv
               out_viol$N <- 1
               return(out_viol[, c("Variables", "N",
                                   setdiff(colnames(out_viol),
                                           c("Variables", "N")))])
             }))

    rownames(heatmap_tab_complex) <- NULL
    colnames(heatmap_tab_complex)[3:ncol(heatmap_tab_complex)] <-
      paste(colnames(heatmap_tab_complex)[3:ncol(heatmap_tab_complex)], "violations")

    heatmap_tab_complex <- util_validate_report_summary_table(heatmap_tab_complex,
                                                              meta_data = meta_data,
                                                              label_col = label_col)

    #If sumdat does not exists, because limits are only present in the
    # cross item level create an empty one
    if (!exists("sumdat")) {
      sumdat <- data.frame(
        Variables = character(0),
        Section = character(0),
        Limits = character(0),
        Number = numeric(0),
        Percentage = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    #Add datatype to r_complex
    r_complex$dt <- dt_rcomplex[match(r_complex$Variables, names(dt_rcomplex))] #TODO: is this needed?


    # summary table -----
    sumtab_inadm_num_complex <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "HARD_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]
          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })
    sumtab_inadm_datetime_complex <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (!is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "HARD_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]
          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })
    sumtab_unc_num_soft <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "SOFT_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]

          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })
    sumtab_unc_num_detect <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "DETECTION_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]
          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })

    sumtab_unc_num_complex <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          if(is.na(sumtab_unc_num_soft[[1]]) &&
             is.na(sumtab_unc_num_detect[[1]])){
            n_viol <- NA
          } else {
            n_viol <- max(sumtab_unc_num_soft[[1]],
                          sumtab_unc_num_detect[[1]],
                          na.rm = TRUE)
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })
    sumtab_unc_datetime_soft <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (!is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "SOFT_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]
          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })

    sumtab_unc_datetime_detect <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (!is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          sumdat_sub <- sumdat_complex[sumdat_complex$Variables == rv &
                                         sumdat_complex$Limits == "DETECTION_LIMITS" &
                                         sumdat_complex$Section == "all_outliers", ]
          if(length(sumdat_sub[, "Number"])>0) {
            n_viol <- sumdat_sub[, "Number"]
          } else {
            n_viol <- NA
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })
    sumtab_unc_datetime_complex <- vapply(
      vars_in_cil, FUN.VALUE = numeric(2), FUN = function(rv) {
        if (!is_datetime_var_cil[[rv]]) {
          res <- c(NA, nrow(ds1))
        } else {
          if(is.na(sumtab_unc_datetime_soft[[1]]) &&
             is.na(sumtab_unc_datetime_detect[[1]])){
            n_viol <- NA
          } else {
            n_viol <- max(sumtab_unc_datetime_soft[[1]],
                          sumtab_unc_datetime_detect[[1]],
                          na.rm = TRUE)
          }
          n_tot <- nrow(ds1)
          res <- c(n_viol, n_tot)
        }
        names(res) <- c("n_viol", "n")
        return(res)
      })

    #Create sumtab -----
    sumtab_complex <- data.frame(
      "Variables" = vars_in_cil,
      "NUM_con_rvv_unum" = sumtab_unc_num_complex["n_viol", ],
      "PCT_con_rvv_unum" = round(sumtab_unc_num_complex["n_viol", ] /
                                   sumtab_unc_num_complex["n", ] * 100, 2),
      "FLG_con_rvv_unum" = ifelse(sumtab_unc_num_complex["n_viol", ] > 0, TRUE, FALSE),
      "NUM_con_rvv_utdat" = sumtab_unc_datetime_complex["n_viol", ],
      "PCT_con_rvv_utdat" = round(sumtab_unc_datetime_complex["n_viol", ] /
                                    sumtab_unc_datetime_complex["n", ] * 100, 2),
      "FLG_con_rvv_utdat" = ifelse(sumtab_unc_datetime_complex["n_viol", ] > 0,
                                   TRUE, FALSE),
      "NUM_con_rvv_inum" = sumtab_inadm_num_complex["n_viol", ],
      "PCT_con_rvv_inum" = round(sumtab_inadm_num_complex["n_viol", ] /
                                   sumtab_inadm_num_complex["n", ] * 100, 2),
      "FLG_con_rvv_inum" = ifelse(sumtab_inadm_num_complex["n_viol", ] > 0, TRUE, FALSE),
      "NUM_con_rvv_itdat" = sumtab_inadm_datetime_complex["n_viol", ],
      "PCT_con_rvv_itdat" = round(sumtab_inadm_datetime_complex["n_viol", ] /
                                    sumtab_inadm_datetime_complex["n", ] * 100, 2),
      "FLG_con_rvv_itdat" = ifelse(sumtab_inadm_datetime_complex["n_viol", ] > 0,
                                   TRUE, FALSE),
      check.names = FALSE, fix.empty.names = FALSE)
    # for n = 0, entries in PCT columns will be NaN
    # we can replace them by 0 (but we will not replace NA by 0, because they
    # show that an indicator did not apply)
    sumtab_complex$PCT_con_rvv_unum[which(sumtab_unc_num_complex["n", ] == 0)] <- 0
    sumtab_complex$PCT_con_rvv_utdat[which(sumtab_unc_datetime_complex["n", ] == 0)] <- 0
    sumtab_complex$PCT_con_rvv_inum[which(sumtab_inadm_num_complex["n", ] == 0)] <- 0
    sumtab_complex$PCT_con_rvv_itdat[which(sumtab_inadm_datetime_complex["n", ] == 0)] <- 0
    rownames(sumtab_complex) <- NULL

    #Create sumdat_wide -----
    #Modify the SummaryData from long to wide format to be more concise to read
    sumdat_wide_complex <- stats::reshape(sumdat_complex,
                                          idvar = c("Variables", "Limits"),
                                          timevar = "Section",
                                          direction = "wide")

    sumdat_wide_complex$`Below limits N (%)` <-
      paste0(sumdat_wide_complex$Number.below, " (",
             sumdat_wide_complex$Percentage.below,")")
    sumdat_wide_complex$`Within limits N (%)` <-
      paste0(sumdat_wide_complex$Number.within, " (",
             sumdat_wide_complex$Percentage.within,")")
    sumdat_wide_complex$`Above limits N (%)` <-
      paste0(sumdat_wide_complex$Number.above, " (",
             sumdat_wide_complex$Percentage.above,")")
    sumdat_wide_complex$`All outside limits N (%)` <-
      paste0(sumdat_wide_complex$Number.all_outliers, " (",
             sumdat_wide_complex$Percentage.all_outliers,")")
    sumdat_wide_complex <-
      sumdat_wide_complex[, c("Variables", "Limits", "Below limits N (%)",
                              "Within limits N (%)", "Above limits N (%)",
                              "All outside limits N (%)"
      )]
    text_to_display <- util_get_hovertext("[con_limit_dev_hover]")
    attr(sumdat_wide_complex, "description") <- text_to_display

    # text design
    spec_txt <- element_text(
      colour = "black",
      hjust = .5, vjust = .5, face = "plain"
    )

    #Plot complex----
    # iteration to create a plot for each 'resp_var'-----
    plot_list_complex <- lapply(setNames(nm = vars_in_cil), function(rv) {
      r_complex_rv <- r_complex[r_complex$Variables == rv, ]
      # limits for variable `rv`
      rv_limit_names <- unique(r_complex_rv$limits)


      # range of percentage
      max_data <- 100
      min_data <- 0
      # range of values to be included in the plot
      max_plot <- 100
      min_plot <- 0

      # Should the plot be a histogram? If not, it will be a bar chart.
      plot_histogram <-FALSE
      plot_histogram_integer <-FALSE

      xlb <- prep_get_labels(resp_vars = rv,
                             resp_vars_match_label_col_only = TRUE,
                             label_class = "SHORT",
                             label_col = label_col,
                             item_level = meta_data)

      # bar chart --------------------------------------------------------------
      # compute bar heights, prepare data for plotting
      plot_data <- lapply(setNames(nm = r_complex_rv$limits), function(lim) {
        r_complex_rv_lim <- r_complex_rv[r_complex_rv$limits == lim, ]
        PCT_content <-  r_complex_rv_lim[, grep("^PCT_", names(r_complex_rv_lim))]
        PCT_content <- PCT_content[, sapply(PCT_content, function(x) !all(is.na(x)))]
        if (length(PCT_content) != 1) {
          util_error(m =
                       "There should be only one value for the number of violations per limit")
        }
        PCT_content
      })

      plot_data <- data.frame(plot_data)
      plot_data <- utils::stack(plot_data)


      # Empty plot structure that uses your custom variables for consistency
      p <- util_create_lean_ggplot(
        ggplot() %lean+%
          geom_text(
            aes(x = 0.5, y = 0.5, label =
                  "No plot available for complex limits"),
            size = 5,
            color = "gray50",
            check_overlap = TRUE
          ) %lean+%
          ggplot2::xlim(0, 1) %lean+%
          ggplot2::ylim(0, 1) %lean+%
          ggplot2::labs(x = paste0(xlb), y = "") %lean+%
          ggplot2::theme_minimal() %lean+%
          ggplot2::theme(
            title = spec_txt,
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = spec_txt,
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ),
        xlb = xlb,
        spec_txt = spec_txt
      )

      min_bin_height <- min(plot_data$values)
      max_bin_height  <- max(plot_data$values)
      if(max_bin_height == min_bin_height) {range_height <-max_bin_height } else{
        range_height <- max_bin_height - min_bin_height
      }

      no_bars_in_all_w <- nrow(plot_data)
      no_char_x <- 4
      no_char_y <- max(nchar(as.vector(plot_data$ind)))
      # Figure size hint for plot
      attr(p, "sizing_hints") <- list(figure_type_id = "bar_limit",
                                      range = range_height,
                                      no_bars_in_all_w = no_bars_in_all_w,
                                      no_char_x = no_char_x,
                                      no_char_y = no_char_y)
      return(p)
    })


  }

  #unify results if both itel_level limits and complex limits are present-----
  if(!exists("sumtab")) {sumtab <- NULL}
  if(!exists("sumtab_complex")) {sumtab_complex <- NULL}
  if(!exists("sumdat_wide")) {sumdat_wide <- NULL}
  if(!exists("sumdat_wide_complex")) {sumdat_wide_complex <- NULL}
  if(!exists("heatmap_tab")) {heatmap_tab <- NULL}
  if(!exists("heatmap_tab_complex")) {heatmap_tab_complex <- NULL}
  if (!exists("plot_list")) {
    plot_list <- NULL
  }
  if (!exists("plot_list_complex")) {
    plot_list_complex <- NULL
  }
  sumtab_final <- util_rbind(sumtab,sumtab_complex)
  sumdat_wide_final <- util_rbind(sumdat_wide,sumdat_wide_complex)
  sumdat_wide_final$`Below limits N (%)` <-
    gsub("\\(\\)", "0 (0)",
         sumdat_wide_final$`Below limits N (%)`)
  sumdat_wide_final$`Within limits N (%)` <-
    gsub("\\(\\)", "0 (0)",
         sumdat_wide_final$`Within limits N (%)`)
  sumdat_wide_final$`Above limits N (%)` <-
    gsub("\\(\\)", "0 (0)",
         sumdat_wide_final$`Above limits N (%)`)
  sumdat_wide_final$`All outside limits N (%)` <-
    gsub("\\(\\)", "0 (0)",
         sumdat_wide_final$`All outside limits N (%)`)

  heatmap_tab_final <- util_rbind(heatmap_tab,heatmap_tab_complex)
  plot_list_final <- c(plot_list, plot_list_complex)


  # final return statement -----------------------------------------------------
  # return(util_mark_result_layout(dqr = util_attach_attr(list( to. hard-code the layout, but it should be default, then it comes from DQ_OBS
  #   FlaggedStudyData = fsd,
  #   SummaryTable = sumtab,
  #   SummaryData = sumdat_wide,
  #   ReportSummaryTable = heatmap_tab,
  #   SummaryPlotList = plot_list),
  #   as_plotly = "util_as_plotly_con_limit_deviations"), "2-columns-fig-left")
  # )
  return(dqr = util_attach_attr(list(
    FlaggedStudyData = fsd,
    SummaryTable = sumtab_final,
    SummaryData = sumdat_wide_final,
    ReportSummaryTable = heatmap_tab_final,
    SummaryPlotList = plot_list_final),
    as_plotly = "util_as_plotly_con_limit_deviations")
  )
}



#' @family plotly_shims
#' @concept plotly_shims
#' @noRd
util_as_plotly_con_limit_deviations <- function(res, ...) {
  p <- res$SummaryPlotList
  util_stop_if_not(length(p) == 1)
  p <- p[[1]]

  gb <- ggplot2::ggplot_build(p)

  scale2 <- tryCatch(
    {
      # preferred: exactly what you had before, just factored out
      gb$plot$scales$scales[[2]]
    },
    error = function(e) {
      # fallback for exotic ggplot2 versions
      sc_obj <- util_gg_get(gb$plot$scales, "scales")
      if (is.list(sc_obj) && length(sc_obj) >= 2) {
        sc_obj[[2]]
      } else {
        stop(e)
      }
    }
  )

  lb <- scale2$get_labels(1)
  cl <- scale2$palette(1)
  br <- scale2$breaks

  py <- util_ggplotly(p, ...)
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (nm %in% paste0("(", br, ",1)")) {
      py$x$data[[i]]$name <- br[nm == paste0("(", br, ",1)")]
    } else {
      py$x$data[[i]]$showlegend <- FALSE
    }
  }
  py <- plotly::layout(py, legend = list(traceorder = "reversed"))
  for (l_i in which(vapply(lapply(py$x$data, `[[`, "type"), function(e1, e2) {
    if (length(e1) != length(e2)) {
      return(FALSE)
    } else {
      return(e1 == e2)
    }
  },
  "scatter",
  FUN.VALUE = logical(1)))) {
    py$x$data[[l_i]]$x <- as.numeric(py$x$data[[l_i]]$x)
  }
  py
}
