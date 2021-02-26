#' Summarize missingness columnwise (in variable)
#'
#' @description
#' Item-Missingness (also referred to as item nonresponse (De Leeuw et al.
#' 2003)) describes the missingness of single values, e.g. blanks or empty data
#' cells in a data set. Item-Missingness occurs for example in case a respondent
#' does not provide information for a certain question, a question is overlooked
#' by accident, a programming failure occurs or a provided answer were missed
#' while entering the data.
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Lists of missing codes and, if applicable, jump codes are selected from
#'    the metadata
#'  - The no. of system missings (NA) in each variable is calculated
#'  - The no. of used missing codes is calculated for each variable
#'  - The no. of used jump codes is calculated for each variable
#'  - Two result dataframes (1: on the level of observations, 2: a summary for
#'    each variable) are generated
#'  - *OPTIONAL:* if `show_causes` is selected, one summary plot for all
#'                `resp_vars` is provided
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param show_causes [logical] if TRUE, then the distribution of missing codes
#'                              is shown
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame must be
#'                                    specified here
#' @param include_sysmiss [logical] Optional, if TRUE system missingness (NAs)
#'                                  is evaluated in the summary plot
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                                 from 0-100
#' @param suppressWarnings [logical] warn about mixed missing and jump code
#'                                   lists
#'
#' @importFrom ggpubr ggballoonplot
#'
#' @return a list with:
#'   - `SummaryTable`: data frame about item missingness per response variable
#'   - `SummaryPlot`: ggplot2 heatmap plot, if show_causes was TRUE
#'
#' @export
#' @importFrom ggplot2 ggplot facet_wrap geom_bar theme_minimal theme annotate
#'                     scale_fill_gradientn theme element_blank
#' @seealso
#' [Online Documentation](
#' https://dfg-qa.ship-med.uni-greifswald.de/VIN_com_impl_item_missingness.html
#' )
com_item_missingness <- function(study_data, meta_data, resp_vars = NULL,
                                 label_col, show_causes = TRUE,
                                 cause_label_df, include_sysmiss = NULL,
                                 threshold_value, suppressWarnings = FALSE) {
  if (missing(suppressWarnings) || length(suppressWarnings) != 1 ||
      is.na(as.logical(suppressWarnings))) {
    suppressWarnings <- FALSE
    if (!missing(suppressWarnings)) {
      util_warning(
        "Setting suppressWarnings to its default FALSE")
    }
  } else {
    suppressWarnings <- as.logical(suppressWarnings)
    if (is.na(suppressWarnings)) {
      suppressWarnings <- FALSE
      util_warning(
        "suppressWarnings should be a scalar logical value. Setting FALSE")
    }
  }

  # Map meta to study data
  util_prepare_dataframes(.replace_missings = FALSE)

  # table of missing codes
  if (missing(cause_label_df) || is.null(cause_label_df) ||
      !is.data.frame(cause_label_df)) {
    if (!missing(cause_label_df) && !is.null(cause_label_df) &&
        !is.data.frame(cause_label_df)) {
      util_warning(
        "If given, cause_label_df must be a data frame. Ignored the argument.")
    }
    all_missing_codes <- unique(sort(unlist(lapply(
      as.character(meta_data[[MISSING_LIST]]),
      function(alist) {
        codes <- suppressWarnings(as.numeric(unlist(strsplit(alist, SPLIT_CHAR,
                                                             fixed = TRUE))))
      }
    ))))

    all_jump_codes <- unique(sort(unlist(lapply(
      as.character(meta_data[[JUMP_LIST]]),
      function(alist) {
        codes <- suppressWarnings(as.numeric(unlist(strsplit(alist, SPLIT_CHAR,
                                                             fixed = TRUE))))
      }
    ))))

    if (length(all_missing_codes) > 0) {
      ml <-
        paste("MISSING",
              all_missing_codes,
              sep = " "
        )
    } else {
      ml <- character(0)
    }

    if (length(all_jump_codes) > 0) {
      jl <-
        paste("JUMP",
              all_jump_codes,
              sep = " "
        )
    } else {
      jl <- character(0)
    }


    code_labels <- c(ml, jl)

    # invent labels (if none were supplied)
    cause_label_df <- data.frame(
      CODE_VALUE = as.numeric(c(all_missing_codes, all_jump_codes)),
      CODE_LABEL = code_labels
    )
  }

  mc_lab <- cause_label_df

  mc_lab$CODE_VALUE <- util_as_numeric(mc_lab$CODE_VALUE)

  # special code for SysMiss
  sm_code <- max(mc_lab$CODE_VALUE) + 1
  sm <- data.frame(CODE_VALUE = sm_code, CODE_LABEL = "ADDED: SysMiss")
  # add new missing code for sysmiss to table
  mc_lab <- dplyr::bind_rows(mc_lab, sm)

  if (!suppressWarnings && any(duplicated(cause_label_df$CODE_VALUE))) {
    util_warning("There are codes used for missings and jumps.")
  }

  if (missing(threshold_value)) {
    util_warning(
      c("The mandatory argument threshold_value was not",
        "defined and is set to the default of 90%%."))
    threshold_value <- 90
  }
  .threshold_value <- as.numeric(threshold_value)
  if (is.na(.threshold_value)) {
    util_warning(
      c("Could not convert threshold_value %s to a number.",
        "Set to default value 90%%."),
      dQuote(as.character(threshold_value))
    )
    threshold_value <- 90
  } else {
    threshold_value <- .threshold_value
  }

  # correct variable use?
  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null          = TRUE,
    allow_any_obs_na    = TRUE
  )

  # if no resp_vars specified use all columns of studydata
  if (length(resp_vars) == 0) {
    resp_vars <- names(ds1)
  }

  # initialize result dataframe
  result_table <- data.frame(Variables = resp_vars)
#browser()
  # compute no of observations according a participation variable
  if ("KEY_STUDY_SEGMENT" %in% names(meta_data)) {

    # which study segments are used in resp_vars
    kss <- meta_data$VAR_NAMES %in%
      unique(meta_data$KEY_STUDY_SEGMENT[meta_data[[label_col]] %in% resp_vars])

    # if some VAR_NAMES indicate segments and these are used in resp_vars they
    # might be nested
    if (any(kss)) {

      part_vars <- meta_data[[label_col]][kss]
      n_part_vars <- vapply(FUN.VALUE = numeric(1), ds1[, part_vars,
                                                        drop = FALSE], sum,
                            na.rm = TRUE)

      No_obs <- rep(as.numeric(n_part_vars), times = as.numeric(
        table(meta_data$KEY_STUDY_SEGMENT[meta_data[[label_col]] %in%
                                            resp_vars])
      ))


    } else {
      No_obs <- rep(dim(ds1)[1], length(resp_vars))
    }

  } else {
    No_obs <- rep(dim(ds1)[1], length(resp_vars))
  }


  # compute sysmiss
  SysMiss <- vapply(FUN.VALUE = integer(1), ds1[, resp_vars, FALSE],
                    util_count_NA)
  # data values
  N_no_SysMiss <- No_obs - SysMiss
  # No of missing codes
  N_MC_RES_AND_N <- util_count_codes(
    sdf = ds1[, resp_vars, FALSE], mdf = meta_data,
    variables = resp_vars, name = label_col,
    list = "MISSING_LIST", warn = FALSE
  )
  N_MC <- N_MC_RES_AND_N[[1]]

  # No of jump codes
  N_JC_RES_AND_N <- util_count_codes(
    sdf = ds1[, resp_vars, FALSE],
    mdf = meta_data, variables = resp_vars,
    name = label_col, list = "JUMP_LIST", warn = FALSE
  )
  N_JC <- N_JC_RES_AND_N[[1]]
  # No of measurements
  N_measures <- (No_obs - N_JC) - (SysMiss + N_MC)

  # checks regarding codes
  N_class_MC <- util_count_code_classes(
    sdf = ds1[, resp_vars, FALSE], mdf = meta_data,
    variables = resp_vars, name = label_col,
    list = "MISSING_LIST", warn = FALSE
  )
  N_MC_avail <- N_MC_RES_AND_N[[2]]

  N_class_JC <- util_count_code_classes(
    sdf = ds1[, resp_vars, FALSE], mdf = meta_data,
    variables = resp_vars, name = label_col,
    list = "JUMP_LIST", warn = FALSE
  )
  N_JC_avail <- N_JC_RES_AND_N[[2]]

  # ???
  rownames(meta_data) <- meta_data[[label_col]]

  if (LABEL %in% colnames(meta_data) && missing(label_col)) {
    # dead code, should be handeled by util_prepare_dataframes already
    result_table$Label <- meta_data[resp_vars, LABEL] # nocov
  }

  # calculations of missings
  result_table$No_obs <- No_obs
  result_table$SysMiss <-
    paste0(SysMiss, " (", round(SysMiss / No_obs * 100, digits = 2), ")")
  result_table$N_no_SysMiss <-
    paste0(N_no_SysMiss, " (", round(N_no_SysMiss / No_obs * 100,
                                     digits = 2), ")")
  result_table$N_MC <-
    paste0(N_MC, " (", round(N_MC / No_obs * 100, digits = 2), ")")
  result_table$N_JC <-
    paste0(N_JC, " (", round(N_JC / No_obs * 100, digits = 2), ")")
  result_table$N_measures <-
    paste0(N_measures, " (", round(N_measures / (No_obs - N_JC) * 100,
                                   digits = 2), ")")
  result_table$GRADING <-
    ifelse(round(N_measures / (No_obs - N_JC) * 100, digits = 2) <
             threshold_value, 1, 0)


  result_table$check_MC <- paste0(N_class_MC, " (", N_MC_avail, ")")
  result_table$check_JC <- paste0(N_class_JC, " (", N_JC_avail, ")")

  if (LABEL %in% colnames(meta_data) && missing(label_col)) { # nocov start
    # dead code, should be handeled by util_prepare_dataframes already
    colnames(result_table) <- c(
      "Variables", "Label", "Observations N",
      "Sysmiss N (%)", "Datavalues N (%)",
      "Missing codes N (%)", "Jumps N (%)",
      "Measurements N (%)", "GRADING",
      "Used (available) missing codes N",
      "Used (available) jump codes N"
    )
  } else { # nocov end
    colnames(result_table) <- c(
      "Variables", "Observations N",
      "Sysmiss N (%)", "Datavalues N (%)",
      "Missing codes N (%)", "Jumps N (%)",
      "Measurements N (%)", "GRADING",
      "Used (available) missing codes N",
      "Used (available) jump codes N"
    )
  }

  # Temporary edit based on decision 2019-09-05
  result_table <- result_table[, !(names(result_table) %in% c(
    "Used (available) missing codes N",
    "Used (available) jump codes N"
  ))]


  if (missing(include_sysmiss) || length(include_sysmiss) != 1) {
    if (missing(include_sysmiss)) {
      util_warning("include_sysmiss set to FALSE")
    } else {
      util_warning("include_sysmiss cannot be a vector. Set it to FALSE")
    }
    include_sysmiss <- FALSE
  } else {
    include_sysmiss <- as.logical(include_sysmiss)[[1]]
    if (is.na(include_sysmiss)) {
      util_warning(
        "Cannot parse include_sysmiss as a logical value. Set it to FALSE")
      include_sysmiss <- FALSE
    }
  }

  if (missing(show_causes) || length(show_causes) != 1) {
    if (length(show_causes) == 1) {
      util_warning("show_causes set to TRUE")
    } else {
      util_warning("show_causes cannot be a vector. Set it to TRUE")
    }
    show_causes <- TRUE
  } else {
    show_causes <- as.logical(show_causes)[[1]]
    if (is.na(show_causes)) {
      util_warning(
        "Cannot parse show_causes as a logical value. Set it to TRUE")
      show_causes <- TRUE
    }
  }

  if (show_causes) {
    # solve issue of melt() with date variables
    # copy
    ds2 <- ds1
    # which are date?
    inT <- vapply(FUN.VALUE = logical(1), ds2, function(x) inherits(x,
                                                                    "Date") ||
                    inherits(x, "POSIXt"))
    # convert to character
    if (sum(inT) > 0) {
      ds2[, inT] <- lapply(ds2[, inT, drop = FALSE], as.character)
    }


    # wide to long
    ds3 <- suppressMessages(melt(ds2[, resp_vars, FALSE]))

    # include sysmiss with special code
    if (!missing(include_sysmiss) && isTRUE(include_sysmiss)) {
      # insert missing code for sysmiss
      ds3$value[is.na(ds3$value)] <- sm_code
    } else {
      # omit sysmiss
      ds3 <- ds3[!(is.na(ds3$value)), ]
      mc_lab <- mc_lab[mc_lab$CODE_LABEL != "ADDED: SysMiss", ]
      mc_lab$CODE_LABEL <- factor(mc_lab$CODE_LABEL)
    }

    # delete measurements
    ds3 <- ds3[ds3$value %in% mc_lab$CODE_VALUE, ]

    # factor codes with labels
    # select only codes with presentation in the data
    # (the table of codes might be longer than those in the data)
    ds3$value <- factor(ds3$value, levels = mc_lab$CODE_VALUE, labels =
                          mc_lab$CODE_LABEL)

    # plot usual ggplot
    # ggplot(ds2, aes(x=value)) + facet_wrap(variable ~ ., ncol = 1) +
    # geom_bar() + theme_minimal() +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    my_cols <- c("#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
                 "#fdbb84", "#fdd49e", "#fee8c8", "#2166AC")

    ctab1 <- as.data.frame(table(ds3$value, ds3$variable))
    if (nrow(ctab1) * ncol(ctab1) == 0) {
      ctab1 <- data.frame(Var1 = NA, Var2 = NA, Freq = NA)[FALSE, , FALSE]
      p <- ggplot() +
        annotate("text", x = 0, y = 0, label = "No missing reported.") +
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()
        )
    } else {
      if (length(unique(ctab1$Var1)) > length(unique(ctab1$Var2))) {
        p <- ggballoonplot(ctab1, x = "Var2", y = "Var1", fill = "Freq",
                           ggtheme = theme_minimal()) +
          scale_fill_gradientn(colors = rev(my_cols))
      } else {
        p <- ggballoonplot(ctab1, x = "Var1", y = "Var2", fill = "Freq",
                           ggtheme = theme_minimal()) +
          scale_fill_gradientn(colors = rev(my_cols))
      }
    }
  } else {
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = "show_causes was FALSE") +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }

  return(list(SummaryTable = result_table, SummaryPlot = p))
}
