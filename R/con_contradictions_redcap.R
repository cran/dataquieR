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
#' [Indicator]
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
#' @inheritParams .template_function_indicator
#'
#' @param threshold_value [numeric] from=0 to=100. a numerical value
#'                                                 ranging from 0-100
#' @param meta_data_cross_item [data.frame] contradiction rules table.  Table
#'                                 defining contradictions. See
#'                                 [online documentation](https://dataquality.qihs.uni-greifswald.de/VIN_Cross_Item_Level_Metadata.html)
#'                                 for its required structure.
#' @param summarize_categories [logical] Needs a column `CONTRADICTION_TYPE` in
#'                             the `meta_data_cross_item`.
#'                             If set, a summary output is generated for the
#'                             defined categories plus one plot per
#'                             category. TODO: Not yet controllable by metadata.
#' @param use_value_labels [logical] Deprecated in favor of [DATA_PREPARATION].
#'                             If set to `TRUE`, labels can be used in the
#'                             `REDCap` syntax to specify contraction checks for
#'                             categorical variables. If set to `FALSE`,
#'                             contractions have to be specified using the coded
#'                             values. In case that this argument is not set in
#'                             the function call, it will be set to `TRUE` if
#'                             the metadata contains a column `VALUE_LABELS`
#'                             which is not empty.
#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`
#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`
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
#'   - `VariableGroupData`: The second output summarizes this information
#'                     into one
#'                     data frame. This output can be used to provide an
#'                     executive overview on the amount of contradictions.
#'   - `VariableGroupTable`: A subset of `VariableGroupData` used within the
#'                           pipeline.
#'   - `SummaryPlot`: The third output visualizes summarized information
#'                    of `SummaryData`.
#'
#' If `summarize_categories` is `TRUE`, other objects are returned:
#' A list with one element `Other`, a list with the following entries:
#' One per category named by that category (e.g. "Empirical") containing a
#' result for contradiction checks within that category only. Additionally, in the
#' slot `all_checks`, a result as it would have been returned with
#' `summarize_categories` set to `FALSE`. Finally, in
#' the top-level list, a slot `SummaryData` is
#' returned containing sums per Category and an according [ggplot2::ggplot] in
#' `SummaryPlot`.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme_minimal
#'                     scale_y_continuous geom_hline coord_flip theme aes
#'                     geom_text xlab scale_x_continuous sec_axis
#' @importFrom stats setNames
#' @seealso
#' [Online Documentation for the function](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html
#' )
#' [meta_data_cross]
#' [Online Documentation for the required cross-item-level metadata](
#' https://dataquality.qihs.uni-greifswald.de/Cross_Item_Level_Metadata.html
#' )
con_contradictions_redcap <- function(study_data,
                                      item_level = "item_level",
                                      label_col, threshold_value,
                                      meta_data_cross_item = "cross-item_level",
                                      use_value_labels,
                                      summarize_categories = FALSE,
                                      # flip_mode = "flip", # TODO: Fix noflip graph
                                      meta_data = item_level,
                                      cross_item_level,
                                      `cross-item_level`,
                                      meta_data_v2
) {

  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = FALSE,
                          .replace_missings = FALSE) # replacements are performed later

  if (!missing(use_value_labels)) {
    lifecycle::deprecate_stop(when = "2.1.0",
                              what = "con_contradictions_redcap(use_value_labels)",
                              details =
                                "Please use DATA_PREPARATION in meta_data_cross_item now."
    )
  }

  # table of specified contradictions
  util_expect_data_frame(meta_data_cross_item, list(
    CONTRADICTION_TERM = is.character,
    CHECK_LABEL = is.character
  ))

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  # There might be rows without contradiction rules (NAs), which should be removed first.
  if (any(is.na(meta_data_cross_item[[CONTRADICTION_TERM]]))) {
    meta_data_cross_item <- meta_data_cross_item[-which(is.na(meta_data_cross_item[[CONTRADICTION_TERM]])), ]
  }

  if (missing(threshold_value)) {
    threshold_value <- NA_real_
    if (!.called_in_pipeline)
      util_message("No %s has been set, will use default %d",
                   dQuote("threshold_value"), threshold_value,
                   applicability_problem = TRUE)
  } else {
    util_expect_scalar(threshold_value, allow_na = TRUE,
                       check_type = function(x) {
                         if (is.na(x)) {
                           return(TRUE)
                         }
                         is.numeric(x) && !is.na(x) && x >= 0 && x <= 100
                       },
                       error_message = sprintf("%s must be a number between %d and %d",
                                               sQuote("threshold_value"),
                                               0,
                                               100)
    )
    threshold_value <- as.numeric(threshold_value)
  }

  util_expect_scalar(summarize_categories, check_type = is.logical)

  # parse redcap rules to obtain interpretable contradiction checks
  compiled_rules <- lapply(setNames(nm = meta_data_cross_item[[CONTRADICTION_TERM]]),
                           util_parse_redcap_rule)

  # colors
  cols <- c("0" = "#2166AC", "1" = "#B2182B")

  # summarize contradictions per category given in CONTRADICTION_TYPE -------------------------------------
  if (summarize_categories) {
    if (!(CONTRADICTION_TYPE %in% colnames(meta_data_cross_item))) {
      util_error(c(
        "Cannot summerize categories of contradictions,",
        "because these are not defined in the meta_data_cross_item",
        "as column %s."),
        sQuote(CONTRADICTION_TYPE),
        applicability_problem = TRUE)
    }

    split_tags <- lapply(strsplit(meta_data_cross_item[[CONTRADICTION_TYPE]], SPLIT_CHAR, fixed = TRUE), trimws)
    tags <- sort(unique(unlist(split_tags)))
    tags <- setNames(nm = tags)
    tags_ext <- tags
    tags_ext[["all_checks"]] <- NA

    result <- lapply(tags_ext, function(atag) {
      # generate one output per category (stratified)
      if (is.na(atag)) {
        new_ct <- meta_data_cross_item[, , #-which(colnames(meta_data_cross_item) == CONTRADICTION_TYPE),
                                       drop = FALSE]
      } else {
        contains_tag <- function(x, tg) {
          any(x == tg, na.rm = TRUE)
        }
        rows_matching_tag <- vapply(split_tags, contains_tag, tg = atag,
                                    logical(1))
        new_ct <- meta_data_cross_item[rows_matching_tag, , #-which(colnames(meta_data_cross_item) == CONTRADICTION_TYPE),
                                       drop = FALSE]
      }
      # recursive call of the function only for the contradiction checks of the currently selected category in "atag"
      r <- try(con_contradictions_redcap(
        study_data = study_data,
        meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = new_ct,
        summarize_categories = FALSE
        # , flip_mode = flip_mode TODO
      ), silent = TRUE)
      if (inherits(r, "try-error")) {
        list(FlaggedStudyData = data.frame())
      } else {
        r
      }
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
    rx_num <- lapply(tags_ext, function(atag) {
      if (is.na(atag)) {
        sum(rowSums(result[["all_checks"]]$FlaggedStudyData[, -1, drop = FALSE],
                    na.rm = TRUE) > 0)
      } else {
        sum(rowSums(result[[atag]]$FlaggedStudyData[, -1, drop = FALSE],
                    na.rm = TRUE) > 0)
      }
    })
    rx <- data.frame(
      CONTRADICTION_TYPE = names(rx),
      PCT_con_con = unlist(rx),
      NUM_con_con = unlist(rx_num),
      GRADING = ordered(ifelse(unlist(rx) > threshold_value, 1, 0))
    )
    if ("LOGICAL" %in% rx[[CONTRADICTION_TYPE]]) {
      rx$PCT_con_con_contc <- rep(NA_real_, nrow(rx))
      rx$PCT_con_con_contc[
        rx[[CONTRADICTION_TYPE]] %in% c("LOGICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
                          %in% c("LOGICAL")]
      rx$NUM_con_con_contc <- rep(NA_integer_, nrow(rx))
      rx$NUM_con_con_contc[
        rx[[CONTRADICTION_TYPE]] %in% c("LOGICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
                          %in% c("LOGICAL")]
    }
    if ("EMPIRICAL" %in% rx[[CONTRADICTION_TYPE]]) {
      rx$PCT_con_con_contu <- rep(NA_real_, nrow(rx))
      rx$PCT_con_con_contu[
        rx[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
                          %in% c("EMPIRICAL")]
      rx$NUM_con_con_contu <- rep(NA_integer_, nrow(rx))
      rx$NUM_con_con_contu[
        rx[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
                          %in% c("EMPIRICAL")]
    }

    result$OtherTable <- rx
    # Create Data Slot
    result$OtherData <- rx
    result$OtherData$PCT_con_con_contc <- NULL
    result$OtherData$PCT_con_con_contu <- NULL
    result$OtherData$GRADING <- NULL
    result$OtherData <- util_make_data_slot_from_table_slot(result$OtherData)

    cls_rx <- NULL
    rm("cls_rx")
    makeActiveBinding("cls_rx", util_make_cls_binding(rx,
                                                      meta_data = meta_data),
                      environment())

    # Plot for summarized contradiction checks -----------------------------------------------------
    # TODO: work on the changes here and below with position = "top"
    p <- ggplot(rx, aes(x = seq_along(CONTRADICTION_TYPE), y = PCT_con_con,
                        fill = (if (!is.na(threshold_value)) {
                          as.ordered(GRADING)
                        } else {
                          cls_rx
                        }))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_y_continuous(name = "(%)",
                         limits = (c(0, max(1.2 * suppressWarnings(
                           max(rx$PCT_con_con)),
                           threshold_value))),
                         expand = expansion(add = 0.5, mult = c(0, 0.1))) +
      scale_x_continuous(breaks = seq_len(nrow(rx)),
                         labels = rx[[CONTRADICTION_TYPE]],
                         position = "top",
                         trans = "reverse") +
      # xlab("Category of applied contradiction checks") +
      xlab("") +
      (if (!is.na(threshold_value)) {
        scale_fill_manual(values = cols, name = " ", guide = "none")
      } else {
        util_scale_fill_dataquieR()
      }) +
      (if (!is.na(threshold_value)) {
        geom_hline(yintercept = threshold_value,
                   color = "red", linetype = 2)
      }) +
      geom_text(label = util_paste0_with_na(" ", rx$PCT_con_con, "%"),
                hjust = 0, vjust = 0.5, size = 3.5) +
      coord_flip() + # TODO
      theme(axis.text.x = element_text(size = 10),
            axis.text.y.right = element_text(size = 10),
            axis.text.y.left = element_blank(),
            legend.title = element_blank())

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

    to_Other <- setdiff(names(result), c("SummaryData",
                                         "OtherTable",
                                         "OtherData",
                                         "SummaryPlot"))

    Other <- result[to_Other]

    result[to_Other] <- NULL

    result$Other <- Other


    return(util_attach_attr(
      result,
      as_plotly = "util_as_plotly_con_contradictions_redcap",
      sizing_hints = list(
        figure_type_id = "bar_chart",
        rotated = TRUE,
        number_of_bars = nrow(p$data),
        range = max(p$data$PCT_con_con) - min(p$data$PCT_con_con)
      )
    ))

  } else {
    # run contradiction checks without summarizing -------------------------------------------------------
    # apply contradiction checks -------------------------------------------------------------------------
    rule_match <- mapply(
      SIMPLIFY = FALSE,
      rule = compiled_rules,
      prep = util_parse_assignments(
        meta_data_cross_item[[DATA_PREPARATION]], multi_variate_text = TRUE),
      FUN = function(rule, prep) {
        prep <- as.character(names(prep))
        use_value_labels <- ("LABEL"  # meta_data_cross_item has been normalized, already
                             %in% prep)
        if ("MISSING_NA" %in% prep) {
          replace_missing_by <- "NA"
        } else if ("MISSING_LABEL" %in% prep) {
          replace_missing_by <- "LABEL"
        } else if ("MISSING_INTERPRET" %in% prep) {
          replace_missing_by <- "INTERPRET"
        } else {
          replace_missing_by <- ""
        }
        replace_limits <- ("LIMITS"
                           %in% prep)
        if (is.list(rule) && !length(rule) && is.null(attr(rule, "class"))) {
          r <- try(util_error("Parser error"), silent = TRUE)
        } else {
          r <- try(util_eval_rule(rule = rule,
                                  ds1 = ds1,
                                  meta_data = meta_data,
                                  use_value_labels = use_value_labels,
                                  replace_missing_by = replace_missing_by,
                                  replace_limits = replace_limits
          ),
          silent = TRUE)
        }
        if (inherits(r, "try-error")) {
          rule_src <- attr(rule, "src")
          if (length(rule_src) == 0) {
            rule_src <- util_deparse1(rule)
          }
          util_warning("Could not evaluate rule %s: %s",
                       dQuote(rule_src),
                       conditionMessage(attr(r, "condition")))
          r <- "error"
        }
        r
      })

    rule_errors <- vapply(rule_match, identical, "error",
                          FUN.VALUE = logical(1))
    rule_match <- lapply(rule_match, as.logical)

    list_element_length <- vapply(rule_match, length, FUN.VALUE = integer(1))
    if (any(list_element_length == 1)) {
      # not all columns of same length, fix this for as.data.frame
      rule_match[list_element_length == 1] <- lapply(
        rule_match[list_element_length == 1],
        function(to_recycle) {
          rep(to_recycle, nrow(ds1))
        })
    }

    if (length(unique(vapply(rule_match, length, FUN.VALUE = integer(1)))) > 1) {
      util_error(c("Internal error: unexpected inhomogeneous length of rules result.",
                   "This is an internal error, please excuse and contact the dataquieR developers."))
    }

    if (length(unique(vapply(rule_match, length, FUN.VALUE = integer(1)))) == 0) {
      summary_df1 <- data.frame(Obs = seq_len(nrow(ds1)))
    } else {
      summary_df1 <- cbind(data.frame(Obs = seq_len(nrow(ds1))),
                           as.data.frame(rule_match))
    }

    colnames(summary_df1)[-1] <- paste0("flag_con",
                                        formatC(seq_len(nrow(meta_data_cross_item)),
                                                width = nchar(nrow(meta_data_cross_item)),
                                                format = "d",
                                                flag = "0"))

    summary_df2 <- meta_data_cross_item

    summary_df2$NUM_con_con <- as.numeric(lapply(rule_match, sum, na.rm = TRUE))
    summary_df2$NUM_con_con[rule_errors] <- rep(NA_integer_, sum(rule_errors))

    summary_df2$PCT_con_con <- round(summary_df2$NUM_con_con / nrow(ds1) * 100,
                                     digits = 2)


    if (CONTRADICTION_TYPE %in% colnames(summary_df2)) {
      summary_df2[["CONTRADICTION_TYPE"]] <-
        trimws(toupper(summary_df2[["CONTRADICTION_TYPE"]]))
      # logical
      summary_df2$NUM_con_con_contc <- rep(NA_integer_, nrow(summary_df2))
      summary_df2$NUM_con_con_contc[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("LOGICAL")
      ] <- summary_df2$NUM_con_con[summary_df2[["CONTRADICTION_TYPE"]]
                                   %in% c("LOGICAL")]
      summary_df2$PCT_con_con_contc <- rep(NA_real_, nrow(summary_df2))
      summary_df2$PCT_con_con_contc[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("LOGICAL")
      ] <- summary_df2$PCT_con_con[summary_df2[["CONTRADICTION_TYPE"]]
                                   %in% c("LOGICAL")]


      summary_df2$NUM_con_con_contu <- rep(NA_integer_, nrow(summary_df2))
      summary_df2$NUM_con_con_contu[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("EMPIRICAL")
      ] <- summary_df2$NUM_con_con[summary_df2[["CONTRADICTION_TYPE"]]
                                   %in% c("EMPIRICAL")]
      summary_df2$PCT_con_con_contu <- rep(NA_real_, nrow(summary_df2))
      summary_df2$PCT_con_con_contu[
        summary_df2[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- summary_df2$PCT_con_con[summary_df2[[CONTRADICTION_TYPE]]
                                   %in% c("EMPIRICAL")]

    } else {
      summary_df2[["CONTRADICTION_TYPE"]] <- rep(NA_character_, nrow(summary_df2))
    }

    summary_df2$GRADING <- ifelse(summary_df2$PCT_con_con > threshold_value,
                                  1, 0)

    summary_df2 <- summary_df2[order(summary_df2[["PCT_con_con"]],
                                     decreasing = TRUE), ]
    summary_df2 <- summary_df2[order(summary_df2[[CONTRADICTION_TYPE]],
                                     decreasing = TRUE), ]

    cls_summary_df2 <- NULL
    rm("cls_summary_df2")
    makeActiveBinding("cls_summary_df2", util_make_cls_binding(summary_df2,
                                                               meta_data =
                                                                 meta_data),
                      environment())

    ctype_pal <- setNames( # does not really depend on grading formats
      scales::pal_hue(h = c(0,360))(n =
                                      length(unique(summary_df2[[CONTRADICTION_TYPE]]
                                      ))
      ), nm = unique(summary_df2[[CONTRADICTION_TYPE]]))
    ctype_pal[["LOGICAL"]] <- getOption("dataquieR.col_con_con_logical",
                                        dataquieR.col_con_con_logical_default)
    ctype_pal[["EMPIRICAL"]] <- getOption("dataquieR.col_con_con_empirical",
                                        dataquieR.col_con_con_empirical_default)
    # Plot for all contradiction checks --------------------------------------------------------
    p <- ggplot(summary_df2, aes(x = seq_along(CHECK_ID),
                                 y = PCT_con_con,
                                 color = CONTRADICTION_TYPE,
                                 fill = (if (!is.na(threshold_value)) {
                                   as.ordered(GRADING)
                                 } else {
                                   cls_summary_df2
                                 }))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      # xlab("Applied contradiction checks") +
      xlab("") +
      scale_y_continuous(name = "(%)",
                         limits = c(0, max(1.2 * suppressWarnings(
                           max(summary_df2$PCT_con_con)),
                           threshold_value)),
                         expand = expansion(add = 0.5, mult = c(0, 0.1))) +
      scale_x_continuous(breaks = seq_len(nrow(summary_df2)),
                         labels = summary_df2$CHECK_LABEL,
                         position = "top",
                         trans = "reverse") +
      ggplot2::scale_color_discrete(type = ctype_pal) +
      (if (!is.na(threshold_value)) {
        scale_fill_manual(values = cols, name = " ", guide = "none")
      } else {
        util_scale_fill_dataquieR()
      }) +
      (if (!is.na(threshold_value)) {
        geom_hline(yintercept = threshold_value,
                   color = "red", linetype = 2)
      }) +
      geom_text(label = util_paste0_with_na(" ", summary_df2$PCT_con_con, "%"),
                hjust = 0, vjust = 0.5, size = 3.5) +
      coord_flip() + # TODO
      theme(axis.text.x = element_text(size = 10),
            axis.text.y.right = element_text(size = 10),
            axis.text.y.left = element_blank(),
            legend.title = element_blank())

    if (!prod(dim(summary_df2))) {
      util_error("No contradiction check defined",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }

    # create Data Slot
    summary_df3 <- summary_df2[, intersect(c(VARIABLE_LIST, CHECK_LABEL,
                                             "NUM_con_con",
                                             "PCT_con_con",
                                             CONTRADICTION_TYPE,
                                             "GRADING"), colnames(summary_df2))]
    summary_df3 <- util_make_data_slot_from_table_slot(summary_df3)

    #add sizing information
    obj1 <- ggplot2::ggplot_build(p)
    number_of_bars <- nrow(obj1$data[[1]])
    range <- max(c(
      util_rbind(data_frames_list = obj1$data)$ymax,
      util_rbind(data_frames_list = obj1$data)$yintercept
    ), na.rm = TRUE) -
      min(c(
        util_rbind(data_frames_list = obj1$data)$ymax,
        util_rbind(data_frames_list = obj1$data)$yintercept
      ), na.rm = TRUE)

    no_char_vars = max(c(0, nchar(summary_df2$CHECK_LABEL)))
    no_char_numbers = max(c(0, nchar(round(obj1$data[[1]]$ymax,digits = 0)),
                          na.rm = TRUE))

    # Output
    return(util_attach_attr(list(
      FlaggedStudyData = summary_df1,
      VariableGroupTable = summary_df2,
      VariableGroupData = summary_df3,
      SummaryPlot = p
    ),
    as_plotly = "util_as_plotly_con_contradictions_redcap",
    sizing_hints = list(
      figure_type_id = "bar_chart",
      rotated = TRUE,
      number_of_bars = number_of_bars,
      range = range,
      no_char_vars = no_char_vars,
      no_char_numbers = no_char_numbers
    )
    ))
  }

  # Never called, just for documentation.
  return(list( # nocov start
    FlaggedStudyData = summary_df1,
    VariableGroupTable = summary_df2,
    VariableGroupData = summary_df3,
    SummaryPlot = p
  )) # nocov end
}

util_make_cls_binding <- function(rx, meta_data) {
  function() {
    # FIXME: Do not expect all metrics
    grading_labs <- util_get_labels_grading_class()
    grading_colors <- util_get_colors()
    grading_colors["NA"] <- "#888888"
    grading_rules <- util_get_rule_sets()[["0"]]
    if (is.data.frame(grading_rules) &&
        is.character(grading_labs) &&
        length(grading_labs) > 0) {
      rx$.orig_order <- seq_len(nrow(rx))
      idvars <- intersect(c(VARIABLE_LIST, CHECK_LABEL, CONTRADICTION_TYPE,
                            CHECK_ID, ".orig_order"),
                          colnames(rx))
      summ <- stats::reshape(data = rx,
                             timevar = "indicator_metric",
                             idvar = idvars,
                             times = c("PCT_con_con",
                                       "PCT_con_con_contc",
                                       "PCT_con_con_contu",
                                       "NUM_con_con",
                                       "NUM_con_con_contc",
                                       "NUM_con_con_contu"),
                             varying = list(c("PCT_con_con",
                                              "PCT_con_con_contc",
                                              "PCT_con_con_contu",
                                              "NUM_con_con",
                                              "NUM_con_con_contc",
                                              "NUM_con_con_contu")),
                             v.names = "values_raw",
                             direction = "long")
      summ$call_names <- paste0("con_contradictions_redcap") # TODO: after exgtension of metadat model, handle a grading rule column in cross-item paste0("con_contradictions_redcap.", summ$CHECK_ID)
      summ$function_name <- paste0("con_contradictions_redcap")
      summ <- util_metrics_to_classes(summ, meta_data, entity = "CROSS_ITEM")
      cls <- summ[, c(idvars, "class"), drop = FALSE]
      cls <- unsplit(lapply(
        split(cls, cls[, idvars]), FUN =
          function(x) {
            if (any(!is.na(as.numeric(x$class)))) {
              x$class <- max(as.numeric(x$class), na.rm = TRUE)
            } else {
              x$class <- rep(NA_integer_, nrow(x))
            }
            x
          }), cls[, idvars])
      cls <- unique(cls)
      rx <- merge(rx, cls, by = idvars)
      # rx <- unique(rx) this changes the order
      rx$class <- grading_colors[paste(rx$class)]
      # rx$class <- factor(rx$class,
      #                    ordered = TRUE,
      #                    levels = unname(grading_colors),
      #                    labels = grading_labs)
      rx <- rx[order(rx[[".orig_order"]]), , FALSE]
      rx$class
    } else {
      return(NA)
    }
  }
}

util_scale_fill_dataquieR <- function(...) {
  r <- ggplot2::scale_fill_identity(
    name = " ",
    labels = setNames(
      util_get_labels_grading_class(),
      util_get_colors()
    ),
    guide = ggplot2::guide_legend()
  )
  r$get_labels <- function(x, ..., self) {
    p <- setNames(nm = util_get_colors(),
                  util_get_labels_grading_class())
    p[x]
  }
  r
}

#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_con_contradictions_redcap <- function(res, ...) {
  if (!util_ensure_suggested("plotly", err = FALSE)) {
    return(htmltools::HTML("No Plotly"))
  }
  # Maybe, we have Other, but certainly, we have SummaryPlot
  # fix the Legend
  col_map <- setNames(nm = util_get_colors(), util_get_labels_grading_class())
  col_map["#888888"] <- "NA"
  # hline, no legnedn from .. or guide hidden --> no legend needed, legacy mode
  # with threshold
  all_geoms <- lapply(res$SummaryPlot$layers, `[[`, "geom")
  vlines <- vapply(all_geoms, inherits, "GeomVline",
                   FUN.VALUE = logical(1))
  hlines <- vapply(all_geoms, inherits, "GeomHline",
                   FUN.VALUE = logical(1))
  legacy_mode <- !!sum(vlines, hlines)
  py <- util_plot_figure_plotly(res[["SummaryPlot"]], attr(res, "sizing_hints"))
  if (legacy_mode) { # no legend in gg, legend static, here
    if (any(vlines)) {
      all_geoms[vlines][[1]]$parameters
      thr <- res$SummaryPlot$layers[vlines][[1]]$data$xintercept
    } else {
      all_geoms[hlines][[1]]$parameters
      thr <- res$SummaryPlot$layers[hlines][[1]]$data$yintercept
    }
    col_map_yn <- c("1" = sprintf("> %.4g%%", thr),
                    "0" = sprintf("\u2264 %.4g%%", thr))
    for (i in seq_along(py$x$data)) {
      py$x$data[[i]]$name <- lapply(
        strsplit(gsub("^\\((.*)\\)$", "\\1", py$x$data[[i]]$name), split = ","),
        function(scales) {
          scales[scales %in% names(col_map_yn)] <-
            col_map_yn[scales[scales %in% names(col_map_yn)]]
          paste0("(", paste0(scales, collapse = ","), ")")
        }
      )
    }
  } else {
    for (i in seq_along(py$x$data)) {
      py$x$data[[i]]$name <- lapply(
        strsplit(gsub("^\\((.*)\\)$", "\\1", py$x$data[[i]]$name), split = ","),
        function(scales) {
          scales[scales %in% names(col_map)] <-
            col_map[scales[scales %in% names(col_map)]]
          paste0("", paste0(scales, collapse = ", "), "")
        }
      )
    }
  }
  py
}
