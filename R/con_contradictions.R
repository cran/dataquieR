#' Checks user-defined contradictions in study data
#'
#' @description
#' This approach considers a contradiction if impossible combinations of data
#' are observed in one participant. For example, if age of a participant is
#' recorded repeatedly the value of age is (unfortunately) not able to decline.
#' Most cases of contradictions rest on comparison of two variables.
#'
#' Important to note, each value that is used for comparison may represent a
#' possible characteristic but the combination of these two values is considered
#' to be impossible. The approach does not consider implausible or inadmissible
#' values.
#'
#' @details
#' ### ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Select all variables in the data with defined contradiction rules (static
#'    metadata column CONTRADICTIONS)
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Remove measurements deviating from limits defined in the metadata
#'  - Assign label to levels of categorical variables (if applicable)
#'  - Apply contradiction checks on predefined sets of variables
#'  - Identification of measurements fulfilling contradiction rules. Therefore
#'    two output data frames are generated:
#'    - on the level of observation to flag each contradictory value
#'      combination, and
#'    - a summary table for each contradiction check.
#'  - A summary plot illustrating the number of contradictions is generated.
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param threshold_value [numeric] from=0 to=100. a numerical value
#'                                                 ranging from 0-100
#' @param check_table [data.frame] contradiction rules table.  Table defining
#'                                 contractions. See details for
#'                                 its required structure.
#' @param summarize_categories [logical] Needs a column 'tag' in the
#'                             `check_table`.
#'                             If set, a summary output is generated for the
#'                             defined categories plus one plot per
#'                             category.
#'
#' @return
#' If `summarize_categories` is `FALSE`:
#' A [list] with:
#'   - `FlaggedStudyData`: The first output of the contradiction function is a
#'                         data frame of similar dimension regarding the number
#'                         of observations in the study data. In addition, for
#'                         each applied check on the variables an additional
#'                         column is added which flags observations with a
#'                         contradiction given the applied check.
#'   - `SummaryTable`: The second output summarizes this information into one
#'                     data frame. This output can be used to provide an
#'                     executive overview on the amount of contradictions. This
#'                     output is meant for automatic digestion within pipelines.
#'   - `SummaryData`: The third output is the same as `SummaryTable` but for
#'                    human readers.
#'   - `SummaryPlot`: The fourth output visualizes summarized information
#'     of `SummaryData`.
#'
#' if `summarize_categories` is `TRUE`, other objects are returned:
#' one per category named by that category (e.g. "Empirical") containing a
#' result for contradictions within that category only. Additionally, in the
#' slot `all_checks` a result as it would have been returned with
#' `summarize_categories` set to `FALSE`. Finally, a slot `SummaryData` is
#' returned containing sums per Category and an according [ggplot] in
#' `SummaryPlot`.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme_minimal
#'                     scale_y_continuous geom_hline coord_flip theme aes_
#'                     geom_text xlab scale_x_continuous sec_axis
#' @importFrom stats setNames reorder
#' @seealso
#' [Online Documentation](
#' https://dfg-qa.ship-med.uni-greifswald.de/VIN_con_impl_contradictions.html
#' )
#' @examples
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' check_table <- read.csv(system.file("extdata",
#'   "contradiction_checks.csv",
#'   package = "dataquieR"
#' ),
#' header = TRUE, sep = "#"
#' )
#' check_table[1, "tag"] <- "Logical"
#' check_table[1, "Label"] <- "Becomes younger"
#' check_table[2, "tag"] <- "Empirical"
#' check_table[2, "Label"] <- "sex transformation"
#' check_table[3, "tag"] <- "Empirical"
#' check_table[3, "Label"] <- "looses academic degree"
#' check_table[4, "tag"] <- "Logical"
#' check_table[4, "Label"] <- "vegetarian eats meat"
#' check_table[5, "tag"] <- "Logical"
#' check_table[5, "Label"] <- "vegan eats meat"
#' check_table[6, "tag"] <- "Empirical"
#' check_table[6, "Label"] <- "non-veg* eats meat"
#' check_table[7, "tag"] <- "Empirical"
#' check_table[7, "Label"] <- "Non-smoker buys cigarettes"
#' check_table[8, "tag"] <- "Empirical"
#' check_table[8, "Label"] <- "Smoker always scrounges"
#' check_table[9, "tag"] <- "Logical"
#' check_table[9, "Label"] <- "Cuff didn't fit arm"
#' check_table[10, "tag"] <- "Empirical"
#' check_table[10, "Label"] <- "Very mature pregnant woman"
#' label_col <- "LABEL"
#' threshold_value <- 1
#' con_contradictions(
#'   study_data = study_data, meta_data = meta_data, label_col = label_col,
#'   threshold_value = threshold_value, check_table = check_table
#' )
#' check_table[1, "tag"] <- "Logical, Age-Related"
#' check_table[10, "tag"] <- "Empirical, Age-Related"
#' con_contradictions(
#'   study_data = study_data, meta_data = meta_data, label_col = label_col,
#'   threshold_value = threshold_value, check_table = check_table
#' )
con_contradictions <- function(resp_vars = NULL, study_data, meta_data,
                               label_col, threshold_value, check_table,
                               summarize_categories = FALSE) {
  rvs <- resp_vars

  # Preps ----------------------------------------------------------------------
  # labels used instead of variable names?
  if (!(missing(label_col)) && label_col != VAR_NAMES) {
    message(
      sprintf(paste("Labels of variables from %s will be used.",
                    "In this case columns A and B in check_tables must",
                    "refer to labels.", collapse = " "),
      dQuote(label_col))
    )
  } else {
    message(paste("Variable names will be used. In this case columns A",
                  "and B in check_tables must refer to variable names."))
  }

  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_any_obs_na = TRUE
  )

  # table of specified contradictions
  if (missing(check_table) || !is.data.frame(check_table)) {
    util_error(
      c("Missing check_table --",
        "cannot apply contradictions checks w/o contradiction rules"))
  }

  if (missing(threshold_value)) {
    threshold_value <- 0
    util_warning("No %s has been set, will use default %d",
                 dQuote("threshold_value"), threshold_value)
  }

  ct <- check_table
  ct$Label <- as.character(ct$Label)

  # colors:
  cols <- c("0" = "#2166AC", "1" = "#B2182B")

  # columns:
  expected_cols <- c(
    "ID",
    "Label",
    "Function_name",
    "A",
    "B",
    "A_value",
    "B_value",
    "A_levels",
    "B_levels"
  )

  missing_cols <- !(expected_cols %in% colnames(ct))

  if (any(missing_cols)) {
    util_error(
      "Missing the following columns in the check_table: %s",
      dQuote(expected_cols[missing_cols])
    )
  }

  if (summarize_categories) {
    # if we want to summarize contradictions per category
    if (!("tag" %in% colnames(ct))) {
      util_error(c(
        "Cannot summerize categories of contractions,",
        "because these are not defined in the check_table as column 'tag'."))
    }
    splitted_tags <- lapply(strsplit(ct$tag, SPLIT_CHAR, fixed = TRUE), trimws)
    tags <- sort(unique(unlist(splitted_tags)))
    tags <- setNames(nm = tags)
    tags_ext <- tags
    tags_ext[["all_checks"]] <- NA
    result <- lapply(tags_ext, function(atag) {
      # generate one output per category (stratified)
      if (is.na(atag)) {
        new_ct <- ct[, -which(colnames(ct) == "tag"), drop = FALSE]
      } else {
        contains_tag <- function(x, tg) {
          any(x == tg, na.rm = TRUE)
        }
        rows_matching_tag <- vapply(splitted_tags, contains_tag, tg = atag,
                                    logical(1))
        new_ct <- ct[rows_matching_tag, -which(colnames(ct) == "tag"),
                     drop = FALSE]
      }
      con_contradictions(
        resp_vars = resp_vars, study_data = study_data,
        meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, check_table = new_ct,
        summarize_categories = FALSE
      )
    })
    rx <- lapply(tags_ext, function(atag) {
      # and summarize the contradictions per category/tag
      if (is.na(atag)) {
        sum(rowSums(result[["all_checks"]]$FlaggedStudyData[, -1, drop = FALSE],
                    na.rm = TRUE) > 0) /
          nrow(result[["all_checks"]]$FlaggedStudyData) * 100
      } else {
        sum(rowSums(result[[atag]]$FlaggedStudyData[, -1, drop = FALSE],
                    na.rm = TRUE) > 0) /
          nrow(result[[atag]]$FlaggedStudyData) * 100
      }
    })
    rx <- data.frame(
      category = names(rx),
      percent = unlist(rx),
      GRADING = ordered(ifelse(unlist(rx) > threshold_value, 1, 0))
    )
    result$SummaryData <- rx
    result$SummaryPlot <-
      ggplot(rx, aes_(x = ~category, y = ~percent, fill = ~GRADING)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = cols, name = " ", guide = FALSE) +
      theme_minimal() +
      scale_y_continuous(name = "(%)", limits = (c(0, max(rx$percent) + 1))) +
      geom_hline(yintercept = threshold_value, color = "red", linetype = 2) +
      coord_flip() +
      theme(text = element_text(size = 20))

    return(result)
  } else {
    ct$A_levels <- as.character(ct$A_levels)
    ct$B_levels <- as.character(ct$B_levels)

    # check and prep meta data
    if (!(CONTRADICTIONS %in% colnames(meta_data))) {
      util_error(
        c("Missing column %s in meta data cannot apply",
          "contradictions checks w/o contradiction rules"),
        dQuote(CONTRADICTIONS)
      )
    }

    meta_data[["CONTRADICTIONS"]] <-
      as.character(meta_data[["CONTRADICTIONS"]])

    # no variables defined?
    if (length(rvs) == 0) {
      if (all(is.na(meta_data[[CONTRADICTIONS]]))) {
        util_error(paste0("No Variables with defined CONTRADICTIONS."))
      } else {
        util_warning(paste0(
          "All variables with CONTRADICTIONS in the metadata are used."))
        rvs <- meta_data[[label_col]][!(is.na(meta_data[[CONTRADICTIONS]]))]
        rvs <- intersect(rvs, colnames(ds1))
      }
    } else {
      # contradictions defined at all?
      if (all(is.na(meta_data[[CONTRADICTIONS]][meta_data[[label_col]] %in%
                                                rvs]))) {
        util_error(paste0("No Variables with defined CONTRADICTIONS."))
      }
      # no contradictions for some variables?
      rvs2 <- meta_data[[label_col]][!(is.na(meta_data[[CONTRADICTIONS]])) &
                                       meta_data[[label_col]] %in% rvs]
      if (length(rvs2) < length(rvs)) {
        util_warning(paste0("The variables ", rvs[!(rvs %in% rvs2)],
                            " have no defined CONTRADICTIONS.",
                            collapse = ", "))
      }
      rvs <- rvs2
    }

    # Inadmissible values must be removed --------------------------------------
    # temporary studydata for the check of contradictions
    ds1_ll <- ds1

    # interpret limit intervals
    imdf <- util_interpret_limits(meta_data)

    for (i in seq_along(rvs)) {
      if (HARD_LIMITS %in% names(imdf)) {
        # values below hard limit?
        minx1 <- imdf[[HARD_LIMIT_LOW]][imdf[[label_col]] == rvs[[i]]]
        minx2 <- min(ds1_ll[[rvs[[i]]]], na.rm = TRUE)

        if (!is.na(minx1) & minx1 > minx2) {
          n_below <- sum(ds1_ll[[rvs[[i]]]] < minx1,
                         na.rm = TRUE)
          ds1_ll[[rvs[[i]]]][ds1_ll[[rvs[[i]]]] < minx1] <- NA
          util_warning(paste0("N = ", n_below, " values in ", rvs[[i]],
                              " have been below HARD_LIMITS and were removed."))
        }

        # values above hard limit?
        maxx1 <- imdf[[HARD_LIMIT_UP]][imdf[[label_col]] == rvs[[i]]]
        maxx2 <- max(ds1_ll[[rvs[[i]]]], na.rm = TRUE)

        if (!is.na(maxx1) & maxx1 < maxx2) {
          n_above <- sum(ds1_ll[[rvs[[i]]]] > maxx1,
                         na.rm = TRUE)
          ds1_ll[[rvs[[i]]]][ds1_ll[[rvs[[i]]]] > maxx1] <- NA
          util_warning(paste0("N = ", n_above, " values in ", rvs[[i]],
                              " have been above HARD_LIMITS and were removed."))
        }
      }
    }

    # Label assignment ---------------------------------------------------------

    # all labelled variables
    levlabs <- meta_data$VALUE_LABELS[meta_data[[label_col]] %in% rvs]

    # any variables without labels?
    if (any(is.na(levlabs))) {
      util_warning(paste0("Variables: ", paste0(rvs[is.na(levlabs)],
                                                collapse = ", "),
                          " have no assigned labels and levels."))
    }

    # only variables with labels
    if (!all(is.na(levlabs))) {
      rvs_ll <- rvs[!is.na(levlabs)]
      levlabs <- levlabs[!is.na(levlabs)]

      for (i in seq_along(rvs_ll)) {
        ds1_ll[[rvs_ll[i]]] <- util_assign_levlabs(
          variable = ds1_ll[[rvs_ll[i]]],
          string_of_levlabs = levlabs[i],
          splitchar = SPLIT_CHAR,
          assignchar = " = ",
          ordered = TRUE
        )
      }
    }

    # select contradiction checks
    # get checks from metadata
    cl <- meta_data[[CONTRADICTIONS]][!(is.na(meta_data$CONTRADICTIONS))]

    # is list ?
    cl <- unlist(cl)

    # select unique checks
    cl <- unique(as.numeric(unlist(strsplit(as.character(cl), SPLIT_CHAR,
                                            fixed = TRUE))))

    cl <- intersect(cl, ct$ID)

    cl <- cl[order(cl)]

    summary_df1 <- data.frame(Obs = 1:dim(ds1_ll)[1])

    summary_df2 <- data.frame(
      Check_type = rep(NA, length(cl)),
      Check_ID = rep(NA, length(cl)),
      Study_variables = rep(NA, length(cl)),
      A_levels = rep(NA, length(cl)),
      B_levels = rep(NA, length(cl)),
      N = rep(NA, length(cl)),
      Percent = rep(NA, length(cl)),
      Grading = rep(NA, length(cl)),
      Label = rep(NA, length(cl))
    )

    for (i in seq_along(cl)) {
      prior_names <- names(summary_df1)

      # prepare columns and name of respective check
      summary_df1[i + 1] <- NA
      names(summary_df1) <- c(prior_names, paste0("grading_", cl[i]))

      # which check function is to be applied
      check <- paste(ct$Function_name[ct$ID == cl[i]])

      # parse levels
      a_lev <- gsub("'", "", ct$A_levels[ct$ID == cl[i]])
      a_lev <- unlist(strsplit(a_lev, SPLIT_CHAR, fixed = TRUE))
      a_lev <- trimws(a_lev)

      b_lev <- gsub("'", "", ct$B_levels[ct$ID == cl[i]])
      b_lev <- unlist(strsplit(b_lev, SPLIT_CHAR, fixed = TRUE))
      b_lev <- trimws(b_lev)
      # apply check
      summary_df1[i + 1] <-
        contradiction_functions[[check]](study_data = ds1_ll,
        A = paste(ct$A[ct$ID == cl[i]]),
        A_levels = a_lev,
        A_value = ct$A_value[ct$ID == cl[i]],
        B = paste(ct$B[ct$ID == cl[i]]),
        B_levels = b_lev,
        B_value = ct$B_value[ct$ID == cl[i]]
      )

      # summarize checks
      summary_df2[i, 1] <- cl[i]
      summary_df2[i, 2] <- check
      summary_df2[i, 3] <- paste0(
        "A is: ", ct$A[ct$ID == cl[i]], "; ",
        "B is: ", ct$B[ct$ID == cl[i]]
      )
      summary_df2[i, 4] <- paste(a_lev, collapse = SPLIT_CHAR)
      summary_df2[i, 5] <- paste(b_lev, collapse = SPLIT_CHAR)
      summary_df2[i, 6] <- sum(summary_df1[, i + 1], na.rm = TRUE)
      summary_df2[i, 7] <- sum(summary_df1[, i + 1], na.rm = TRUE) /
        dim(ds1)[1] * 100
      summary_df2[i, 8] <- ifelse(summary_df2[i, 7] > threshold_value, 1, 0)
      summary_df2[i, 9] <- ct$Label[ct$ID == cl[i]]
    }

    summary_df2$Percent <- round(summary_df2$Percent, digits = 2)

    names(summary_df2) <- c(
      "Check ID", "Check type", "Variables A and B", "A Levels",
      "B Levels", "Contradictions (N)", "Contradictions (%)",
      "Grading", "Label"
    )

    summary_df2$Grading <- ordered(summary_df2$Grading)

    x <- util_as_numeric(reorder(summary_df2[, 1], -summary_df2[, 1]))
    lbs <- as.character(reorder(summary_df2[, 9], -summary_df2[, 1]))
    # plot summary_df2
    p <- ggplot(summary_df2, aes_(x = ~x, y = ~ summary_df2[, 7], fill =
                                    ~ as.ordered(Grading))) +
      geom_bar(stat = "identity") +
      geom_text(
        y = round(summary_df2[, 7], 1) + 0.5,
        label = paste0(round(summary_df2[, 7], digits = 2), "%")
      ) +
      scale_fill_manual(values = cols, name = " ", guide = FALSE) +
      theme_minimal() +
      xlab("IDs of applied checks") +
      scale_y_continuous(name = "(%)",
                         limits = (c(0, max(summary_df2[, 7]) + 1))) +
      scale_x_continuous(breaks = x, sec.axis =
                           sec_axis(~., breaks = x, labels = lbs)) +
      geom_hline(yintercept = threshold_value, color = "red", linetype = 2) +
      coord_flip() +
      theme(text = element_text(size = 20))


    # create SummaryTable object
    st1 <- summary_df2
    st1$`Variables A and B` <- gsub("A is: ", "", st1$`Variables A and B`)
    st1$`Variables A and B` <- gsub("B is: ", "", st1$`Variables A and B`)
    st1$Variables <- unlist(lapply(st1$`Variables A and B`,
                                   function(x) unlist(strsplit(x, ";",
                                                               fixed =
                                                                 TRUE))[1]))
    st1$`Reference variable` <- unlist(lapply(st1$`Variables A and B`,
                                              function(x) unlist(
                                                strsplit(x, ";", fixed =
                                                           TRUE))[2]))
    st1$`Variables A and B` <- NULL
    st1 <- st1[, c(9, 10, 1:8)]
    st1 <- dplyr::rename(st1, c("GRADING" = "Grading"))

    # Output
    return(list(
      FlaggedStudyData = summary_df1,
      SummaryTable = st1,
      SummaryData = summary_df2,
      SummaryPlot = p
    ))
  }

  # Never called, just for documentation.
  return(list(
    FlaggedStudyData = summary_df1,
    SummaryTable = st1,
    SummaryData = summary_df2,
    SummaryPlot = p
  ))
}
