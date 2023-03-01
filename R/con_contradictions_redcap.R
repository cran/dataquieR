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
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Remove measurements deviating from limits defined in the metadata
#'  - Assign label to levels of categorical variables (if applicable)
#'  - Apply contradiction checks (given as `REDCap`-like rules in a separate
#'    metadata table)
#'  - Identification of measurements fulfilling contradiction rules. Therefore
#'    two output data frames are generated:
#'    - on the level of observation to flag each contradictory value
#'      combination, and
#'    - a summary table for each contradiction check.
#'  - A summary plot illustrating the number of contradictions is generated.
#'
#' List function.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param threshold_value [numeric] from=0 to=100. a numerical value
#'                                                 ranging from 0-100
#' @param meta_data_cross_item [data.frame] contradiction rules table.  Table
#'                                 defining contractions. See details for
#'                                 its required structure.
#' @param summarize_categories [logical] Needs a column 'CONTRADICTION_TYPE' in
#'                             the `meta_data_cross_item`.
#'                             If set, a summary output is generated for the
#'                             defined categories plus one plot per
#'                             category.
#' @param use_value_labels [logical] If set to `TRUE`, labels can be used in the
#'                             REDCap syntax to specify contraction checks for
#'                             categorical variables. If set to `FALSE`,
#'                             contractions have to be specified using the coded
#'                             values. In case that this argument is not set in
#'                             the function call, it will be set to `TRUE` if
#'                             the metadata contains a column `VALUE_LABELS`
#'                             which is not empty.
#'
#' `inheritParams` `acc_distributions`
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
#'   - `SummaryData`: The second output summarizes this information into one
#'                    data frame. This output can be used to provide an
#'                    executive overview on the amount of contradictions.
#'   - `VariableGroupTable`: A subset of `SummaryData` used within the pipeline.
#'   - `SummaryPlot`: The third output visualizes summarized information
#'                    of `SummaryData`.
#'
#' If `summarize_categories` is `TRUE`, other objects are returned:
#' One per category named by that category (e.g. "Empirical") containing a
#' result for contradiction checks within that category only. Additionally, in the
#' slot `all_checks`, a result as it would have been returned with
#' `summarize_categories` set to `FALSE`. Finally, a slot `SummaryData` is
#' returned containing sums per Category and an according [ggplot] in
#' `SummaryPlot`.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme_minimal
#'                     scale_y_continuous geom_hline coord_flip theme aes
#'                     geom_text xlab scale_x_continuous sec_axis
#' @importFrom stats setNames
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html
#' )
#' @examples
#' \dontrun{ # slow
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' meta_data_cross_item <- prep_get_data_frame("meta_data_v2|cross-item_level")
#' label_col <- "LABEL"
#' threshold_value <- 1
#' con_contradictions_redcap(
#'   study_data = study_data, meta_data = meta_data, label_col = label_col,
#'   threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item
#' )
#' con_contradictions_redcap(
#'   study_data = study_data, meta_data = meta_data, label_col = label_col,
#'   threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item,
#'   summarize_categories = TRUE
#' )
#' }
con_contradictions_redcap <- function(study_data, meta_data,
                               label_col, threshold_value,
                               meta_data_cross_item = "cross-item_level",
                               use_value_labels,
                               summarize_categories = FALSE
                               # flip_mode = "flip" # TODO: Fix noflip graph
                               ) {

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
  }
  util_expect_scalar(use_value_labels, check_type = is.logical)

  # table of specified contradictions
  util_expect_data_frame(meta_data_cross_item, list(
    CONTRADICTION_TERM = is.character,
    CHECK_LABEL = is.character
  ))

  # There might be rows without contradiction rules (NAs), which should be removed first.
  if (any(is.na(meta_data_cross_item$CONTRADICTION_TERM))) {
    meta_data_cross_item <- meta_data_cross_item[-which(is.na(meta_data_cross_item$CONTRADICTION_TERM)), ]
  }

  if ("VARIABLE_LIST" %in% colnames(meta_data_cross_item)) {
    util_warning("VARIABLE_LIST is ignored for contradictions. The rule will be used for finding all related variables.")
    meta_data_cross_item$VARIABLE_LIST <- NULL
  }

  # generate VARIABLE_LIST entries
  # TODO: Support "[" and "]" in variable labels
  # TODO: handle also VAR_NAMES here. The VARIABLE_LIST below should then likely be mapped back to one soft of identifiers.
  needles_var_names <- unique(c(meta_data[[VAR_NAMES]], meta_data[[label_col]]))
  needles <- paste0("[",
                    needles_var_names,
                    "]")
  x <- vapply(setNames(needles, nm = needles_var_names),
        grepl,
        setNames(nm = meta_data_cross_item$CONTRADICTION_TERM),
        fixed = TRUE,
        FUN.VALUE = logical(length = nrow(meta_data_cross_item))
  )
  if (is.vector(x)) {
    x <- as.matrix(t(x))
  }
  # variablelist <- lapply(lapply(lapply(apply(x, 1, which, simplify = FALSE), # apply supports simplify from R 4.1.0
  #                                      names), sort), unique)                # so use the following, less intuitive code line
  variablelist <- unname(lapply(as.data.frame(t(x)), function(xx) unique(sort(colnames(x)[xx]))))
  variablelist <-
    lapply(variablelist, paste0, collapse = sprintf(" %s ", SPLIT_CHAR))

  meta_data_cross_item$VARIABLE_LIST <- variablelist # TODO: To 000_globs.R

  if (missing(threshold_value)) {
    threshold_value <- 0
    util_warning("No %s has been set, will use default %d",
                 dQuote("threshold_value"), threshold_value,
                 applicability_problem = TRUE)
  } else {
    util_expect_scalar(threshold_value, check_type = function(x) {
      is.numeric(x) && x >= 0 && x <= 100
    })
  }

  util_expect_scalar(summarize_categories, check_type = is.logical)

  # parse redcap rules to obtain interpretable contradiction checks
  compiled_rules <- lapply(setNames(nm = meta_data_cross_item$CONTRADICTION_TERM),
                           util_parse_redcap_rule)

  # colors
  cols <- c("0" = "#2166AC", "1" = "#B2182B")

  # summarize contradictions per category given in CONTRADICTION_TYPE -------------------------------------
  if (summarize_categories) {
    if (!("CONTRADICTION_TYPE" %in% colnames(meta_data_cross_item))) {
      util_error(c(
        "Cannot summerize categories of contradictions,",
        "because these are not defined in the meta_data_cross_item as column 'CONTRADICTION_TYPE'."),
        applicability_problem = TRUE)
    }

    split_tags <- lapply(strsplit(meta_data_cross_item$CONTRADICTION_TYPE, SPLIT_CHAR, fixed = TRUE), trimws)
    tags <- sort(unique(unlist(split_tags)))
    tags <- setNames(nm = tags)
    tags_ext <- tags
    tags_ext[["all_checks"]] <- NA

    result <- lapply(tags_ext, function(atag) {
      # generate one output per category (stratified)
      if (is.na(atag)) {
        new_ct <- meta_data_cross_item[, -which(colnames(meta_data_cross_item) == "CONTRADICTION_TYPE"), drop = FALSE]
      } else {
        contains_tag <- function(x, tg) {
          any(x == tg, na.rm = TRUE)
        }
        rows_matching_tag <- vapply(split_tags, contains_tag, tg = atag,
                                    logical(1))
        new_ct <- meta_data_cross_item[rows_matching_tag, -which(colnames(meta_data_cross_item) == "CONTRADICTION_TYPE"),
                              drop = FALSE]
      }

      # recursive call of the function only for the contradiction checks of the currently selected category in "atag"
      con_contradictions_redcap(
        study_data = study_data,
        meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = new_ct,
        summarize_categories = FALSE
        # , flip_mode = flip_mode TODO
      )
    })

    # summarize the outputs of the recursive calls
    rx <- lapply(tags_ext, function(atag) {
      if (is.na(atag)) {
        round(sum(rowSums(result[["all_checks"]]$FlaggedStudyData[, -1, drop = FALSE],
                          na.rm = TRUE) > 0) /
                nrow(result[["all_checks"]]$FlaggedStudyData) * 100, digits = 2)
      } else {
        round(sum(rowSums(result[[atag]]$FlaggedStudyData[, -1, drop = FALSE],
                          na.rm = TRUE) > 0) /
                nrow(result[[atag]]$FlaggedStudyData) * 100, digits = 2)
      }
    })

    rx <- data.frame(
      category = names(rx),
      PCT_con_con = unlist(rx),
      GRADING = ordered(ifelse(unlist(rx) > threshold_value, 1, 0))
    )

    result$SummaryData <- rx

    # Plot for summarized contradiction checks -----------------------------------------------------
    # TODO: work on the changes here and below with position = "top"
    p <- ggplot(rx, aes(x = seq_len(nrow(rx)), y = PCT_con_con,
                          fill = as.ordered(GRADING))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = cols, name = " ", guide = "none") +
      theme_minimal() +
      scale_y_continuous(name = "(%)",
                         limits = (c(0, max(1.2 * max(rx$PCT_con_con),
                                            threshold_value))),
                         expand = expansion(mult = c(0,0.05))) +
      scale_x_continuous(breaks = seq_len(nrow(rx)),
                         labels = rx$category,
                         position = "top",
                         trans = "reverse") +
      xlab("Category of applied contradiction checks") +
      geom_hline(yintercept = threshold_value, color = "red", linetype = 2) +
      geom_text(label = paste0(" ", rx$PCT_con_con, "%"),
                hjust = 0, vjust = 0.5) +
      coord_flip() + # TODO
      theme(axis.text.y.right = element_text(size = 14),
            axis.text.y.left = element_blank())

    # p <- p + util_coord_flip(p = p) # TODO: estimate w and h, if p is not using discrete axes

    # https://stackoverflow.com/a/51795017
    bp <- ggplot_build(p)
    w <- 2 * length(bp$layout$panel_params[[1]]$x$get_labels())
    if (w == 0) {
      w <- 10
    }
    w <- w + 2 +
      max(nchar(bp$layout$panel_params[[1]]$y$get_labels()),
          na.rm = TRUE)
    h <- 2 * length(bp$layout$panel_params[[1]]$y$get_labels())
    if (h == 0) {
      h <- 10
    }
    h <- h + 15

    p <- util_set_size(p, width_em = w, height_em = h)

    result$SummaryPlot <- p

    return(result)

  } else {
    # run contradiction checks without summarizing -------------------------------------------------------
    # apply contradiction checks -------------------------------------------------------------------------
    rule_match <- lapply(compiled_rules, function(rule) {
      util_eval_rule(rule = rule,
                     ds1 = ds1,
                     meta_data = meta_data,
                     use_value_labels = use_value_labels)
    })

    rule_match <- lapply(rule_match, as.logical) # TODO: if a rule returned "", this is NA, if it did not return TRUE, FALSE or "", this would be a warning

    summary_df1 <- cbind(data.frame(Obs = seq_len(nrow(ds1))),
                         as.data.frame(rule_match))
    colnames(summary_df1)[-1] <- paste0("flag_con",
                                        formatC(seq_len(nrow(meta_data_cross_item)),
                                                width = nchar(nrow(meta_data_cross_item)),
                                                format = "d",
                                                flag = "0"))

    summary_df2 <- meta_data_cross_item
    summary_df2$NUM_con_con <- as.numeric(lapply(rule_match, sum, na.rm = TRUE))
    summary_df2$PCT_con_con <- round(summary_df2$NUM_con_con / nrow(ds1) * 100,
                                     digits = 2)
    summary_df2$GRADING <- ifelse(summary_df2$PCT_con_con > threshold_value,
                                  1, 0)

    summary_df3 <- summary_df2[, c("VARIABLE_LIST", "NUM_con_con",
                                   "PCT_con_con", "GRADING")]

    # Plot for all contradiction checks --------------------------------------------------------
    p <- ggplot(summary_df2, aes(x = seq_len(nrow(meta_data_cross_item)), y = PCT_con_con,
                                   fill = as.ordered(GRADING))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = cols, name = " ", guide = "none") +
      theme_minimal() +
      xlab("Applied contradiction checks") +
      scale_y_continuous(name = "(%)",
                         limits = (c(0, max(1.2 * max(summary_df2$PCT_con_con),
                                            threshold_value))),
                         expand = expansion(mult = c(0,0.05))) +
      scale_x_continuous(breaks = seq_len(nrow(meta_data_cross_item)),
                         labels = meta_data_cross_item$CHECK_LABEL,
                         position = "top",
                         trans = "reverse") +
      geom_hline(yintercept = threshold_value, color = "red", linetype = 2) +
      geom_text(label = paste0(" ", summary_df2$PCT_con_con, "%"),
                hjust = 0, vjust = 0.5) +
      coord_flip() + # TODO
      theme(axis.text.y.right = element_text(size = 14),
            axis.text.y.left = element_blank())

    # p <- p + util_coord_flip(p = p) # TODO: estimate w and h, if p is not using discrete axes

    suppressWarnings({
      # suppress wrong warnings: https://github.com/tidyverse/ggplot2/pull/4439/commits
      # find out size of the plot https://stackoverflow.com/a/51795017
      bp <- ggplot_build(p)
      w <- 2 * length(bp$layout$panel_params[[1]]$x$get_labels())
      if (w == 0) {
        w <- 10
      }
      w <- w + 2 +
        max(nchar(bp$layout$panel_params[[1]]$y$get_labels()),
            na.rm = TRUE)
      w <- w +
        max(nchar(bp$layout$panel_params[[1]]$y.sec$get_labels()),
            na.rm = TRUE)
      h <- 2 * length(bp$layout$panel_params[[1]]$y$get_labels())
      if (h == 0) {
        h <- 10
      }
      h <- h + 15
      p <- util_set_size(p, width_em = w, height_em = h)
    })

    # Output
    return(list(
      FlaggedStudyData = summary_df1,
      SummaryTable = summary_df2,
      VariableGroupTable = summary_df3,
      SummaryPlot = p
    ))
  }

  # Never called, just for documentation.
  return(list( # nocov start
    FlaggedStudyData = summary_df1,
    SummaryTable = summary_df2,
    VariableGroupTable = summary_df3,
    SummaryPlot = p
  )) # nocov end
}
