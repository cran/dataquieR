#' Combine missing-lists for a set of variables to be displayed in the same
#' heat-map
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame can be
#'                                    specified here, see [cause_label_df]
#' @param assume_consistent_codes [logical] if TRUE and no labels are given and
#'                                          the same missing/jump code is used
#'                                          for more than one variable, the
#'                                          labels assigned for this code will
#'                                          be the same for all variables.
#' @param expand_codes [logical] if TRUE, code labels are copied from other
#'                               variables, if the code is the same and the
#'                               label is set somewhere
#' @param suppressWarnings [logical] warn about consistency issues with missing
#'                                   and jump lists
#' @param include_sysmiss [logical] Optional, if TRUE system missingness (NAs)
#'                                  is evaluated in the summary plot
#'
#' @return a [list] with:
#'   - `ModifiedStudyData`: data frame with re-coded (if needed) study data
#'   - `cause_label_df`: data frame with re-coded missing codes suitable for all
#'                       variables
util_combine_missing_lists <-
  function(resp_vars, study_data, meta_data, label_col,
           include_sysmiss, cause_label_df, assume_consistent_codes = TRUE,
           expand_codes = assume_consistent_codes,
           suppressWarnings = FALSE) {

    validated <-
      util_validate_missing_lists(meta_data = meta_data,
                                cause_label_df = cause_label_df,
                                assume_consistent_codes =
                                  assume_consistent_codes,
                                expand_codes = expand_codes,
                                label_col = label_col,
                                suppressWarnings = suppressWarnings)
    cause_label_df <- validated$cause_label_df

    util_prepare_dataframes(.replace_missings = FALSE)

    # correct variable use?
    util_correct_variable_use("resp_vars",
                              allow_more_than_one = TRUE,
                              allow_null          = TRUE,
                              allow_any_obs_na    = TRUE
    )

    if (missing(resp_vars) || length(resp_vars) == 0)
      resp_vars <- colnames(ds1)

    if (missing(cause_label_df) || !prod(dim(cause_label_df))) {
      mc_lab <- data.frame(
        CODE_VALUE = NA,
        CODE_LABEL = NA,
        AUTO = NA,
        stringsAsFactors = FALSE
      )[FALSE, , FALSE]
    } else {
      mc_lab <- cause_label_df
    }

    overall_max_val <- max(
      suppressWarnings(as.numeric(as.matrix(study_data))), na.rm = TRUE)

    mc_labs <- lapply(setNames(nm = resp_vars), function(rv) {
      ml <-
        util_get_code_list(rv, MISSING_LIST,
                           split_char = SPLIT_CHAR,
                           mdf = meta_data,
                           label_col = label_col,
                           warning_if_no_list = FALSE)
      jl <-
        util_get_code_list(rv, JUMP_LIST,
                           split_char = SPLIT_CHAR,
                           mdf = meta_data,
                           label_col = label_col,
                           warning_if_no_list = FALSE)
      #### Remove all codes unused by the current variable ####
      mc_lab <- mc_lab[mc_lab$CODE_VALUE %in% c(ml, jl), , drop = FALSE]
      mc_lab$CODE_CLASS[mc_lab$CODE_VALUE %in% ml] <- "MISSING"
      mc_lab$CODE_CLASS[mc_lab$CODE_VALUE %in% jl] <- "JUMP"

      #### Amend all missing codes ####
      if (any(!(ml %in% mc_lab$CODE_VALUE))) {
        code_label <- names(ml)
        if (any(is.na(ml == names(ml)))) { # nocov start
          util_warning(c("Internal problem with %s (for missing list)",
                         "This should not have happened, sorry. Please",
                         "report this issue."),
                       dQuote("util_get_code_list"),
                       applicability_problem = FALSE)
        } # nocov end
        if (assume_consistent_codes) {
          code_label[ml == names(ml)] <- paste("MISSING",
                                               ml[!(ml %in% mc_lab$CODE_VALUE)],
                                               sep = " ")
        } else {
          code_label[ml == names(ml)] <- paste("MISSING", rv,
                                               ml[!(ml %in% mc_lab$CODE_VALUE)],
                                               sep = " ")
        }
        mc_lab <- rbind.data.frame(mc_lab, data.frame(
          CODE_VALUE = ml[!(ml %in% mc_lab$CODE_VALUE)],
          CODE_LABEL = code_label[!(ml %in% mc_lab$CODE_VALUE)],
          resp_vars = rep(rv, sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          CODE_CLASS = rep("MISSING",
                           sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          AUTO = rep(TRUE, sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          stringsAsFactors = FALSE
        ))
      }
      if (any(!(jl %in% mc_lab$CODE_VALUE))) {
        code_label <- names(jl)
        if (any(is.na(jl == names(jl)))) { # nocov start
          util_warning(c("Internal problem with %s (for jump list)",
                         "This should not have happened, sorry. Please",
                         "report this issue."),
                       dQuote("util_get_code_list"),
                       applicability_problem = FALSE)
        } # nocov end
        if (assume_consistent_codes) {
          code_label[jl == names(jl)] <- paste("JUMP",
                                               jl[!(jl %in% mc_lab$CODE_VALUE)],
                                               sep = " ")
        } else {
          code_label[jl == names(jl)] <- paste("JUMP", rv,
                                               jl[!(jl %in% mc_lab$CODE_VALUE)],
                                               sep = " ")
        }
        mc_lab <- rbind.data.frame(mc_lab, data.frame(
          CODE_VALUE = jl[!(jl %in% mc_lab$CODE_VALUE)],
          CODE_LABEL = code_label[!(jl %in% mc_lab$CODE_VALUE)],
          resp_vars = rep(rv, sum(!(jl %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          CODE_CLASS = rep("JUMP",
                           sum(!(jl %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          AUTO = rep(TRUE,
                     sum(!(jl %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          stringsAsFactors = FALSE
        ))
        mc_lab <- rbind.data.frame(mc_lab, data.frame(
          CODE_VALUE = jl[!(jl %in% mc_lab$CODE_VALUE)],
          CODE_LABEL = code_label[!(jl %in% mc_lab$CODE_VALUE)],
          resp_vars = rep(rv, sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          CODE_CLASS = rep("JUMP",
                           sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          AUTO = rep(TRUE,
                     sum(!(ml %in% mc_lab$CODE_VALUE), na.rm = TRUE)),
          stringsAsFactors = FALSE
        ))
      }

      if (nrow(mc_lab)) {
        mc_lab$resp_vars <- rv
      } else {
        mc_lab <- data.frame(
          CODE_VALUE = NA,
          CODE_LABEL = NA,
          resp_vars = rv,
          AUTO = NA,
          stringsAsFactors = FALSE
        )[FALSE, , FALSE]
      }

      mc_lab
    })

    # prepare a generator for globally unused codes

    overall_max_val <- max(overall_max_val,
                           vapply(mc_labs, function(x) {
                             if (is.data.frame(x) && any(!is.finite(
                               x$CODE_VALUE)))
                               max(x$CODE_VALUE, na.rm = TRUE)
                             else
                               NA_integer_
                             }, FUN.VALUE = numeric(1)), na.rm = TRUE)

    get_code_generator <- function(init = overall_max_val + 1) {
      props <- new.env(parent = emptyenv())
      props$cnt <- force(init)
      function(...) {
        r <- props$cnt
        props$cnt <- props$cnt + 1
        r
      }
    }

    code_generator <- get_code_generator()

    large_list <- do.call(rbind.data.frame, mc_labs)

    if (expand_codes) {
      if (assume_consistent_codes) {
        is_auto_code <- function(x) {
          r <- any(x[["CODE_LABEL"]] %in%
                     paste(c("MISSING", "JUMP"), x[["CODE_VALUE"]]))
          r
        }
      } else {
        is_auto_code <- function(x) {
          r <- any(x[["CODE_LABEL"]] %in%
                     paste(c("MISSING", "JUMP"), x[["resp_vars"]], x[["CODE_VALUE"]]))
          r
        }
      }
      detect_auto_codes <- function(dfr) {
        dfr$AUTO <- apply(dfr, 1, is_auto_code)
        dfr
      }
      if (nrow(large_list) > 0) {
        auto_codes <- detect_auto_codes(large_list)
      } else {
        auto_codes <- large_list
        auto_codes <- cbind(auto_codes, data.frame(AUTO = logical(0)))
      }
      l <- split(auto_codes, list(large_list$CODE_VALUE, large_list$CODE_CLASS))
      expand_code <- lapply(l, function(x) {sum(!x$AUTO) == 1})
      l[unlist(expand_code)] <- lapply(l[unlist(expand_code)], function(dfr) {
        dfr$CODE_LABEL <- dfr$CODE_LABEL[!dfr$AUTO]
        dfr
      })
      if (length(l) > 0) {
        large_list <-
          unsplit(l, list(large_list$CODE_VALUE, large_list$CODE_CLASS))
      }
      large_list$AUTO <- NULL
    }
    # browser()
    duplicated_codes <-
      vapply(
        split(large_list, list(large_list$CODE_VALUE, large_list$CODE_CLASS)),
        function(mc_lab) {
        length(unique(mc_lab$CODE_LABEL)) >= 2
    }, FUN.VALUE = logical(1))

    recode <- unlist(lapply(setNames(nm = names(duplicated_codes[duplicated_codes])),
      function(mc) {
        v <- setNames(nm = unique(
          large_list[large_list$CODE_VALUE == mc, "CODE_LABEL", drop = TRUE]))
        lapply(v, function(lab) {
          list(harmonized_code = code_generator(),
               variables = unique(large_list[large_list$CODE_VALUE == mc &
                                             large_list$CODE_LABEL == lab,
                                             "resp_vars", drop = TRUE]),
               specific_code = mc)
        })
    }), recursive = FALSE) # these are re-coded if not auto-created

    recoded_study_data <- study_data

    for (rc in recode) {
      cn <- util_map_labels(rc$variables,
                            meta_data,
                            from = label_col,
                            to = VAR_NAMES)
      recoded_study_data[, cn] <-
        unlist(lapply(recoded_study_data[, cn], function(v) {
          v[v == rc$specific_code] <- rc$harmonized_code
          v
        }), recursive = FALSE)
      large_list[large_list$CODE_VALUE == rc$specific_code &
                 large_list$resp_vars %in% rc$variables, "CODE_VALUE"] <-
        rc$harmonized_code
    }

    cause_label_df <- large_list[, c("CODE_VALUE", "CODE_LABEL"), drop = FALSE]
    rownames(cause_label_df) <- NULL

    sm_code <- code_generator()
    cause_label_df <- rbind.data.frame(data.frame(
      CODE_VALUE = sm_code,
      CODE_LABEL = .SM_LAB,
      stringsAsFactors = FALSE
    ), cause_label_df)
    for (v in colnames(recoded_study_data)) {
      if (inherits(recoded_study_data[[v]], "POSIXt") ||
          inherits(recoded_study_data[[v]], "POSIXct") ||
          inherits(recoded_study_data[[v]], "POSIXlt") ||
          inherits(recoded_study_data[[v]], "Date")) {
        recoded_study_data[is.na(recoded_study_data[[v]]), v] <-
          as.POSIXct.numeric(sm_code,
                             origin = min(as.POSIXct(Sys.Date()), 0))
      } else {
        recoded_study_data[is.na(recoded_study_data[[v]]), v] <-
          sm_code
      }
    }

    cause_label_df <- cause_label_df[!duplicated(cause_label_df), , FALSE]


  list(ModifiedStudyData = recoded_study_data,
       cause_label_df = cause_label_df)
}
