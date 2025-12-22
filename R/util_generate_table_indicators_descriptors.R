#' Create a table summarizing the number of indicators and descriptors
#' in the report
#'
#' @param report a report
#'
#' @return a table containing the number of indicators and descriptors created
#' in the report, separated by data quality dimension.
#'
#' @noRd
util_generate_table_indicators_descriptors <- function(report) {

  util_stop_if_not(inherits(report, "dataquieR_resultset2"))

  #Obtain the names of the group (e.g., observer, device...)
  tab_attrib <- attributes(attributes(report)$matrix_list)$function_alias_map

  alias2functionname <- setNames(tab_attrib$name, nm = tab_attrib$alias)

  #create the summary
  x <- summary(report)

  # this represents the current object
  this <- attr(x, "this")
  withr::with_environment(this, {

  # For robustness: in case none of the assessments work, create an empty result
  # if result is empty or contains only errors
  if (!prod(dim(this$result)) || all(
    startsWith(as.character(this$result$indicator_metric), "CAT_") |
    startsWith(as.character(this$result$indicator_metric), "MSG_"))) {
    # create a table containing only zeros
    info_dim_dq <- data.frame(Dimension = c("Accuracy", "Completeness",
                                            "Consistency", "Integrity"),
                              `No. DQ indicators` = c(0, 0, 0, 0),
                              `No. DQ descriptors` = c(0, 0, 0, 0),
                              check.names = FALSE)
  } else {

    ## import the complete list of functions run or stopped
    stopped_functions <- this$stopped_functions
    # modify from named vector to data frame
    stopped_functions <- t(data.frame(as.list(stopped_functions)))
    stopped_functions <- as.data.frame(stopped_functions)
    stopped_functions  <- cbind(rownames(stopped_functions),
                                 data.frame(stopped_functions ,
                                            row.names=NULL))
    colnames(stopped_functions) <- c("fun_names", "stopped_yn")
    # remove the suffixes(.var_names, .[ALL]) from fun names
    stopped_functions$fun_names1 <- gsub("(^[^.]*).*", "\\1",
                                          stopped_functions$fun_names)

    # translate call-names/aliases to function names,
    # i.e., remove all possible group-suffixes (_device, _observer...)
    stopped_functions$fun_names1 <-
      alias2functionname[`stopped_functions`$fun_names1]

    stopped_functions <- unique(stopped_functions[, c("fun_names1",
                                                "stopped_yn"),
                                            drop = FALSE ])
    # create list TRUE (stopped functions)
    stopped_functions_T <-
      stopped_functions[stopped_functions$stopped_yn ==TRUE, , drop=FALSE]
    # create list FALSE (run functions)
    stopped_functions_F <-
      stopped_functions[stopped_functions$stopped_yn == FALSE, , drop=FALSE]
    # create a vector with only TRUE - only fun not run
    fun_stopped <- setdiff(stopped_functions_T$fun_names1,
             stopped_functions_F$fun_names1) # VECTOR of stopped functions
    # the script works even if fun_stopped is empty
    rm(stopped_functions,
       stopped_functions_T, stopped_functions_F)


    ## import results
    result_t <- this$result


    ## Create a list of function names
    result_novars<- result_t[, c("call_names", "function_name"), drop = FALSE]
    # create the function name that corresponds to the actual call_names,
    # e.g., with con_hard_limits instead of con_limit_deviations
    result_novars$function_name2 <-
     apply(result_novars[, "call_names",
                    drop = FALSE],
           1, FUN = function(x) {
             util_map_by_largest_prefix(x,
                                        haystack = util_all_ind_functions())
           })
    # Create a unique list of function names here before to remove information
    # to use that with alias to see which function was not included in result_t
    functions_in_result_t<- unique(result_novars$function_name2)
    rm(result_novars)


    ## Define which indicator_metric is an indicator based on CAT_indicator_or_descriptor
    # select everything starting with CAT (Error category)
    result_err_cat <-
      result_t[startsWith(as.character(result_t$indicator_metric),
                          "CAT_"),
               c(VAR_NAMES, "call_names",
                 "indicator_metric",
                 "class"),
               drop = FALSE]
    # rename the class for CAT errors as "err_cat"
#    result_err_cat <- dplyr::rename(result_err_cat,
#                                    err_cat = "class")
    names(result_err_cat)[names(result_err_cat) == "class"] <- "err_cat"

    # select only the category "CAT_indicator_or_descriptor"
 #   result_err_cat <-
 #          dplyr::filter(result_err_cat,
 #                        get("indicator_metric") ==
 #                          "CAT_indicator_or_descriptor")
    result_err_cat <- result_err_cat[result_err_cat$indicator_metric ==
                                       "CAT_indicator_or_descriptor", ,
                                     drop = FALSE ]

    # no need to reshape from long to wide, only CAT_indicator_or_descriptor present
 #   result_err_cat <- suppressMessages(reshape::cast(result_err_cat,
#                                                     VAR_NAMES + call_names ~
#                                                       indicator_metric))


    # create a new column "is_indicator" logic T/F T = is an indicator
    result_err_cat$is_indicator <-
         result_err_cat$err_cat == 1
    # reduce no. columns
    result_err_cat <- result_err_cat[, c(VAR_NAMES, "call_names",
                                        "is_indicator"),drop = FALSE]
    # remove rownames
    rownames(result_err_cat) <- NULL



    ## Remove all error messages (CAT and MSG) and obtain an object with actual
    # results (metrics) only
    result_t <- result_t[!startsWith(as.character(result_t$indicator_metric),
                                    "CAT_"), , drop = FALSE]
    result_t <- result_t[!startsWith(as.character(result_t$indicator_metric),
                                    "MSG_"), , drop = FALSE]

    # merge the 2 objects: result_t and result_err_cat
    result_t <- merge(result_t,
                      result_err_cat,
                      by = c(VAR_NAMES, "call_names"),
                      all = TRUE)

    rm(result_err_cat)

    # remove empty indicator_metric, where only an error message was created
    result_t <- result_t[!is.na(result_t$indicator_metric),]

    # remove indicators that have no class (class = NA)
    # result_t <- result_t[!is.na(result_t$class),]
    # remove indicators that have no value (value = NA)
    result_t <- result_t[!is.na(result_t$value),]

    # reduce no. columns to only needed
    result_t <- result_t[, c(VAR_NAMES, "indicator_metric", "is_indicator"), drop = FALSE]


    ## calculate the no. of indicators from the result
    # keep only first row per indicator_metric+var_names,
    # in case of the same indicator metric present as both indicator and
    # descriptor, only indicator is kept
    result_out <-
      dplyr::filter(dplyr::arrange(dplyr::group_by(result_t,
                                                   result_t["indicator_metric"]),
                                   dplyr::desc(get("is_indicator"))),
                    dplyr::row_number()==1 )

    # remove the prefix to indicators
    result_out$indicator_metric <- gsub("(.*?_)(.*)","\\2",
                                        vapply(result_out$indicator_metric,
                                               as.character,
                                               FUN.VALUE = character(1)))

    # remove duplicates by indicators
    result_out <- result_out[, c("indicator_metric", "is_indicator")]
    result_out <- result_out[!duplicated(result_out$indicator_metric), ]

    # create a column with the DQ dimension of the indicator metric
    result_out$Dimension <- gsub("(.*?)_.*","\\1",
                               vapply(result_out$indicator_metric,
                                      as.character,
                                      FUN.VALUE = character(1)))

    # calculate the number of indicators and descriptors per dimension, and
    # rename the DQ dimensions
    info_dim_dq <-
      as.data.frame(dplyr::summarize(dplyr::group_by(result_out,
                                                     result_out["Dimension"],
                                                     result_out["is_indicator"]),
                                     n=dplyr::n()))
    rm(result_out)

    # remove eventual empty rows
    info_dim_dq <- info_dim_dq[!is.na(info_dim_dq$Dimension), , drop = FALSE]

    # rename the T/F with speaking labels
    info_dim_dq$is_indicator[info_dim_dq$is_indicator == "FALSE"] <-
      "descriptors"

    info_dim_dq$is_indicator[info_dim_dq$is_indicator == "TRUE"] <-
      "No_indicators"

    # change from long to wide format
 #   info_dim_dq <- suppressMessages(reshape::cast(info_dim_dq,
 #                                                 Dimension ~ is_indicator))
    info_dim_dq <- stats::reshape(info_dim_dq, idvar = "Dimension",
                                 timevar = "is_indicator",
                                 direction = "wide")

    colnames(info_dim_dq) <- gsub("^n\\.",
                                  "",
                                  colnames(info_dim_dq))



    # replace NAs with 0
    info_dim_dq[is.na(info_dim_dq)] <- 0


    # Create empty columns in case there are no descriptors or no indicators
    if(!"descriptors" %in% colnames(info_dim_dq)) {
      info_dim_dq[, "descriptors"] = 0
    } else if (!"No_indicators" %in% colnames(info_dim_dq)) {
      info_dim_dq[, "No_indicators"] = 0
    }

    ## calculate the no. of descriptors from the function_name
    alias <- unique(this$alias_names)
    # remove from alias functions that were stopped
    alias <- setdiff(alias, fun_stopped)
    # remove from alias functions that created an error comparing it with all
    # functions that created an indicator_metric
    alias <- as.data.frame(setdiff(alias, functions_in_result_t))
    colnames(alias) <- "function_name_descriptors"

    rm(functions_in_result_t, fun_stopped)

    # create a column with the DQ dimension
    alias$Dimension <- gsub("(.*?)_.*","\\1",
                               vapply(alias$function_name_descriptors,
                                      as.character,
                                      FUN.VALUE = character(1)))
    # calculate the no. descriptors per dimension
    n_descriptors <-
      as.data.frame(dplyr::summarize(dplyr::group_by(alias,
                                                     alias["Dimension"]),
                                     n_descr = dplyr::n()))

    # replace NAs with 0
    n_descriptors[is.na(n_descriptors)] <- 0

    rm(result_t, alias)

    n_descriptors <-
      n_descriptors[!is.na(n_descriptors$Dimension) &
                    n_descriptors$Dimension != "des", ]

    # merge the table with the no. indicators and the other with no. descriptors
    info_dim_dq <- merge(info_dim_dq, n_descriptors, by = "Dimension",
                         all = TRUE)
    rm(n_descriptors)

    #In case some dimensions are not present in n_descr, replace NA with 0
    info_dim_dq$n_descr[is.na(info_dim_dq$n_descr)] <- 0

    # sum the no. of descriptors of the 2 tables
    info_dim_dq <- dplyr::mutate(info_dim_dq,
                                 descr_num = get("descriptors")+
                                   get("n_descr"))

    # clean and rename table columns and content
    info_dim_dq <- info_dim_dq[, c("Dimension",
                                   "No_indicators", "descr_num"),
                               drop = FALSE]

    #re-order columns
    colnames(info_dim_dq) <- c("Dimension", "No. DQ indicators",
                               "No. DQ descriptors" )
    col_order <- c("Dimension", "No. DQ indicators",
                   "No. DQ descriptors")
    info_dim_dq <- info_dim_dq[, col_order, drop = FALSE]

    # amend missing dimensions:
    for (dim in c("int", "com", "con", "acc")) {
      if (!dim %in% info_dim_dq$Dimension) {
        info_dim_dq <- rbind(info_dim_dq,
                             data.frame(Dimension = dim,
                                        `No. DQ indicators` = 0,
                                        `No. DQ descriptors` = 0,
                                        stringsAsFactors = FALSE,
                                        check.names = FALSE
                             ))
      }
    }

    info_dim_dq$Dimension[info_dim_dq$Dimension == "acc"] <- "Accuracy"
    info_dim_dq$Dimension[info_dim_dq$Dimension == "com"] <- "Completeness"
    info_dim_dq$Dimension[info_dim_dq$Dimension == "con"] <- "Consistency"
    info_dim_dq$Dimension[info_dim_dq$Dimension == "int"] <- "Integrity"

    info_dim_dq <- info_dim_dq[match(c("Integrity", "Completeness",
                                       "Consistency", "Accuracy"),
                                     info_dim_dq$Dimension),]

    rm(col_order)
    gc()

  }
    info_dim_dq
  })
}
