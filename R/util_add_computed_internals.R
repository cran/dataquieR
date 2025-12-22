#' Add computation rules from cross-item-level-indicator-columns
#'
#' @param meta_data_item_computation existing computation rules
#' @param meta_data_cross_item cross-item metadata
#' @param meta_data main metadata
#' @param label_col column to store label copies
#'
#' @noRd
#'
#' @returns list with updated `meta_data_item_computation` and `meta_data`
#' @examples
#' \dontrun{
#' dq_report2("study_data", meta_data_v2 = "~/tmp/mls.xlsx")
#' }
util_add_computed_internals <- function(meta_data_item_computation,
                                        meta_data_cross_item,
                                        meta_data,
                                        label_col) {
  if (missing(meta_data_cross_item)) {
    meta_data_cross_item <- NULL
  }
  suppressWarnings(suppressMessages(
    meta_data_cross_item <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col
    )
  ))

  meta_data_item_computation_internal <- data.frame(
    VAR_NAMES = character(0),
    COMPUTATION_RULE = character(0),
    DATA_PREPARATION = character(0),
    CHECK_ID = character(0)
  )

  meta_data_internal <- data.frame(
    VAR_NAMES = character(0),
    DATA_TYPE = character(0)
  )

  COLUMNS_WITH_SSI <- setdiff(names(COMPUTED_VARIABLE_ROLES), "NA")

  for (ssi_col in COLUMNS_WITH_SSI) {
    if (!ssi_col %in% colnames(meta_data_cross_item)) next

    rows <- which(!util_empty(meta_data_cross_item[[ssi_col]]))
    if (length(rows) == 0) next

    rule_info <- util_get_concept_info("ssi", get("SSI_METRICS") == ssi_col)
    rule_template <- rule_info$rule_template[1]
    data_preparation <- rule_info$data_preparation[1]
    extra_col <- rule_info$extra_arguments[1]
    human_prefix <- rule_info$long_label[1]
    scale_level <- SCALE_LEVELS[[rule_info$scale_level[1]]]
    data_type <- DATA_TYPES[[rule_info$data_type[1]]]

    e <- new.env(parent = emptyenv())
    e$given <- character(0)
    e$given_lab <- character(0)
    e$given_llab <- character(0)
    e$given_seg <- character(0)

    short_prefix <- gsub("[^A-Z0-9_]", "", toupper(ssi_col))

    to_add <- lapply(rows, function(rw, e) {
      parsed <- suppressWarnings(
        tryCatch(
          util_parse_interval(meta_data_cross_item[rw, ssi_col]),
          warning = function(w) {
            util_warning(
              "Invalid interval in column %s at row %d: %s - %s.",
              ssi_col,
              rw,
              meta_data_cross_item[rw, ssi_col],
              conditionMessage(w),
              applications_problem = TRUE
            )
            NA
          }
        )
      )
      if (!inherits(parsed, "interval")) {
        return(NULL)
      }

      check_id <- meta_data_cross_item[rw, CHECK_ID]
      hard_limits <- meta_data_cross_item[rw, ssi_col]
      check_label <- meta_data_cross_item[rw, CHECK_LABEL]
      scale_name <- meta_data_cross_item[rw, SCALE_NAME]
      scale_acronym <- meta_data_cross_item[rw, SCALE_ACRONYM]
      computed_variable_role <- COMPUTED_VARIABLE_ROLES[[ssi_col]]
      if (!length(computed_variable_role)) {
        util_error(
          c("Internal error, sorry, please report: Detected unkown",
            "computed varialbe role: %s"),
          ssi_col
        )
      }

      study_segment <- util_free_varname(
        meta_data,
        paste0("COMPUTED_", short_prefix),
        target = STUDY_SEGMENT,
        also_not = e$given_seg
      )
      e$given_seg <- c(e$given_seg, study_segment)

      prefix <- short_prefix
      if (!!length(scale_name) && !util_empty(scale_name)) {
        prefix <- paste0(prefix, "_", scale_name)
      } else if (!!length(check_label) && !util_empty(check_label)) {
        prefix <- paste0(prefix, "_", check_label)
      } else if (!!length(scale_acronym) && !util_empty(scale_acronym)) {
        prefix <- paste0(prefix, "_", scale_acronym)
      }
      var_names <- util_free_varname(
        meta_data, prefix, target = VAR_NAMES, also_not = e$given
      )
      e$given <- c(e$given, var_names)

      prefix <- paste0(short_prefix, ": ")
      if (!!length(scale_name) && !util_empty(scale_name)) {
        prefix <- paste0(prefix, scale_name)
      }
      if (!!length(check_label) && !util_empty(check_label)) {
        prefix <- paste0(prefix, " (", check_label, ")")
      }
      label <- util_free_varname(
        meta_data, prefix, target = LABEL, also_not = e$given_lab
      )
      e$given_lab <- c(e$given_lab, var_names)

      long_prefix <- human_prefix
      if (!!length(scale_name) && !util_empty(scale_name)) {
        long_prefix <- paste0(long_prefix, " for ", scale_name)
      }
      if (!!length(scale_acronym) && !util_empty(scale_acronym) &&
          !!length(check_label) && !util_empty(check_label)) {
        long_prefix <- paste0(long_prefix, " (", scale_acronym, " -- ",
                              check_label, ")")
      } else if (!!length(check_label) && !util_empty(check_label)) {
        long_prefix <- paste0(long_prefix, " (", check_label, ")")
      } else if (!!length(scale_acronym) && !util_empty(scale_acronym)) {
        long_prefix <- paste0(long_prefix, " (", scale_acronym, ")")
      }
      long_label <- util_free_varname(
        meta_data, long_prefix, target = LONG_LABEL, also_not = e$given_llab
      )
      e$given_llab <- c(e$given_llab, var_names)

      var_expr <- paste(
        collapse = ", ",
        paste0("[", util_parse_assignments(
          meta_data_cross_item[rw, VARIABLE_LIST_ORDER]), "]")
      )

      rule_filled <- rule_template
      rule_filled <- gsub("!!!VAR_NAMES", var_expr, rule_filled, fixed = TRUE)

      if (!is.na(extra_col)) {
        extra_args <- NA
        if (extra_col %in% colnames(meta_data_cross_item))
        if (util_empty(meta_data_cross_item[rw, extra_col])) {
          extra_args <- "[.]"
        } else {
          extra_args <-
            util_parse_assignments(meta_data_cross_item[rw, extra_col])
        }
        rule_filled <- gsub(paste0("!!!", extra_col), extra_args,
                            rule_filled, fixed = TRUE)
      }

      computation_rule <- rule_filled
      variable_role <- VARIABLE_ROLES$PRIMARY

      list(
        check_id = check_id,
        var_names = var_names,
        computation_rule = computation_rule,
        data_preparation = data_preparation,
        scale_level = scale_level,
        data_type = data_type,
        variable_role = variable_role,
        computed_variable_role = computed_variable_role,
        hard_limits = hard_limits,
        label = label,
        long_label = long_label,
        study_segment = study_segment
      )
    }, e = e)

    to_add <- Filter(Negate(is.null), to_add)

    .meta_data_item_computation_internal <- util_rbind(
      data_frames_list = lapply(lapply(to_add, `[`, tolower(
        c(VAR_NAMES, COMPUTATION_RULE, DATA_PREPARATION, CHECK_ID))),
        as.data.frame)
    )
    colnames(.meta_data_item_computation_internal) <-
      toupper(colnames(.meta_data_item_computation_internal))
    meta_data_item_computation_internal <- util_rbind(
      meta_data_item_computation_internal,
      .meta_data_item_computation_internal
    )

    .meta_data_internal <- util_rbind(
      data_frames_list = lapply(lapply(to_add, function(x) {
        row <- as.data.frame(x[tolower(c(
          VAR_NAMES,
          SCALE_LEVEL,
          DATA_TYPE,
          VARIABLE_ROLE,
          COMPUTED_VARIABLE_ROLE,
          HARD_LIMITS,
          LABEL,
          LONG_LABEL,
          STUDY_SEGMENT
        ))])
        names(row) <- toupper(names(row))
        if (!is.null(label_col) &&
            !(toupper(label_col) %in% c(LABEL, LONG_LABEL, VAR_NAMES))) {
          row[[label_col]] <- x$label
        }
        row
      }), identity)
    )
    colnames(.meta_data_internal) <- toupper(colnames(.meta_data_internal))
    meta_data_internal <- util_rbind(meta_data_internal, .meta_data_internal)
  }

  list(
    meta_data_item_computation = util_rbind(
      meta_data_item_computation,
      meta_data_item_computation_internal
    ),
    meta_data = util_rbind(
      meta_data,
      meta_data_internal
    )
  )
}
