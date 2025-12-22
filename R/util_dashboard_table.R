#' Create a dashboard-table from a report summary
#'
#' @param repsum a report summary from `summary(report)`
#'
#' @family html
#' @concept process
#' @noRd
util_dashboard_table <- function(repsum) {
  this <- attr(repsum, "this")
  bigTable <- this$result
  tb <- suppressWarnings(
    bigTable[!is.na(as.numeric(bigTable$values_raw)), , FALSE])
  label_col <- this$label_col
  meta_data <- this$meta_data

  rowmaxes <- this$rowmaxes
  var_classes <- setNames(util_as_integer_cat(
    rowmaxes$rowmax), nm = rowmaxes$VAR_NAMES)
  var_classes <- ordered(var_classes,
                         levels = names(util_get_labels_grading_class()),
                         labels = util_get_labels_grading_class())

  tb[[label_col]] <-
    prep_map_labels(tb$VAR_NAMES, meta_data = meta_data, to = label_col)

  indicator_metric <- NULL # to make R CMD check happy

  if (length(tb) > 0 && prod(dim(tb)) > 0) {
    tb <- subset(tb,
                 !startsWith(indicator_metric, "CAT_") &
                   !startsWith(indicator_metric, "MSG_")
    )
  }

  tb$var_class <- var_classes[tb$VAR_NAMES]

  cols <- util_get_colors()[tb$class]
  labs <- util_get_labels_grading_class()[tb$class]
  fg_cols <- util_get_fg_color(cols)

  tb$`Class` <- ifelse(is.na(labs), "", paste0(
    labs
  ))

  tb$`Metric` <- util_translate_indicator_metrics(tb$indicator_metric)
  tb$`Call` <- vapply(tb$call, util_alias2caption, long = TRUE,
                      FUN.VALUE = character(1))
  tb$n_classes <- NULL

  tb <- merge(tb, meta_data)

  tb$class <- ordered(tb$class,
                      levels = names(util_get_labels_grading_class()),
                      labels = util_get_labels_grading_class())

  my_order1 <- c(
    label_col,
    VAR_NAMES,
    STUDY_SEGMENT,
    "Call",
    "Metric",
    "value",
    "Class",
    "call_names",
    "values_raw",
    "function_name",
    "indicator_metric",
    "class",
    "var_class",
    LABEL,
    DATA_TYPE,
    SCALE_LEVEL
  )

  my_order2 <- c(
    STANDARDIZED_VOCABULARY_TABLE,
    MISSING_LIST_TABLE,
    HARD_LIMITS,
    DETECTION_LIMITS,
    SOFT_LIMITS,
    DISTRIBUTION,
    DECIMALS,
    GROUP_VAR_OBSERVER,
    GROUP_VAR_DEVICE,
    TIME_VAR,
    PART_VAR,
    VARIABLE_ROLE,
    VARIABLE_ORDER,
    LONG_LABEL,
    "ELEMENT_HOMOGENITY_CHECKTYPE",
    UNIVARIATE_OUTLIER_CHECKTYPE,
    N_RULES,
    LOCATION_METRIC,
    LOCATION_RANGE,
    PROPORTION_RANGE,
    "REPEATED_MEASURES_VARS",
    "REPEATED_MEASURES_GOLDSTANDARD",
    CO_VARS,
    END_DIGIT_CHECK,
    VALUE_LABEL_TABLE,
    MISSING_LIST,
    JUMP_LIST
  )

  my_order <- unique(c(intersect(my_order1, colnames(tb)),
                sort(setdiff(colnames(tb), union(my_order1, my_order2))),
                intersect(my_order2, colnames(tb))))

  tb <- tb[, my_order, FALSE]

  return(util_attach_attr(tb,
                          label_col = label_col))
}
