#' Heuristics to amend a SCALE_LEVEL column and a UNIT column in the metadata
#'
#' ...if missing
#'
#' @param resp_vars [variable list] deprecated, the function always addresses
#'                                  all variables.
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return [data.frame] modified metadata
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   prep_load_workbook_like_file("meta_data_v2")
#'   prep_scalelevel_from_data_and_metadata(study_data = "study_data")
#' }
prep_scalelevel_from_data_and_metadata <- function(resp_vars =
                                                     lifecycle::deprecated(),
                                                   study_data,
                                                   item_level = "item_level",
                                                   label_col = LABEL,
                                                   meta_data = item_level,
                                                   meta_data_v2) {
  if (lifecycle::is_present(resp_vars)) {
    # Signal the deprecation to the user
    lifecycle::deprecate_warn(
      "2.5.0",
      "dataquieR::prep_scalelevel_from_data_and_metadata(resp_vars = )")
  }
  resp_vars <- NULL
  util_maybe_load_meta_data_v2()
  # TODO: shorten function name, e.g., 'prep_amend_scale_level'
  if (
    inherits(
      try(
        util_expect_data_frame(meta_data),
        silent = TRUE),
      "try-error")) {
    w <-
      "No item-level metadata provided at all. Will try to predict them."
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(w))
    }

    util_warning(w, immediate = TRUE)
    util_warning(w, immediate = TRUE)
    meta_data <- prep_study2meta(study_data)
  }

  prep_prepare_dataframes(
    .replace_hard_limits = TRUE,
    .amend_scale_level = FALSE, # prevent recursion
    .replace_missings = TRUE
  )

  util_correct_variable_use(resp_vars,
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            overwrite = TRUE,
                            remove_not_found = TRUE)

  if (is.null(resp_vars)) {
    resp_vars <- colnames(ds1)
  }

  md <- meta_data[meta_data[[label_col]] %in% resp_vars,
                  intersect(c(label_col, SCALE_LEVEL, DATA_TYPE, UNIT,
                              VALUE_LABELS, VALUE_LABEL_TABLE),
                            colnames(meta_data)), FALSE]
  distsel <- util_dist_selection(ds1)
  # TODO: What about missing codes like ".A" in metadata? They are considered
  # to be negative values, even in string columns! (".A" < 0 gives TRUE)
  # (problem with util_dist_selection, not actually here)

  meta_info <-
    merge(md, distsel, by.x = label_col, by.y = "Variables", all = TRUE)

  for (cl in c(SCALE_LEVEL, DATA_TYPE, UNIT, VALUE_LABELS, VALUE_LABEL_TABLE)) {
    if (!(cl %in% colnames(meta_info))) {
      meta_info[[cl]] <- NA_character_
    }
  }

  meta_info <- meta_info[,
                         intersect(
                           c(
                             label_col,
                             DATA_TYPE,
                             VALUE_LABELS,
                             VALUE_LABEL_TABLE,
                             "IsInteger",
                             "IsMultCat",
                             "NCategory",
                             "AnyNegative",
                             SCALE_LEVEL,
                             UNIT
                           ),
                           colnames(meta_info)
                         ), drop = FALSE]

  varrefs <- util_variable_references(meta_data = meta_data)

  # 1. Variables with value labels ---------------------------------------------
  # - the value labels indicate an order: ordinal scale
  # - the value labels are separated by `|`: nominal scale
  # assignments <- util_parse_assignments(meta_info[[VALUE_LABELS]],
  #                                       split_char = c(SPLIT_CHAR, "<"),
  #                                       multi_variate_text = TRUE)
  #
  assignments <- lapply(meta_info[[VALUE_LABEL_TABLE]],
                        function(vltnm) {
                          if (vltnm %in% prep_list_dataframes()) {
                            rdf <- prep_get_data_frame(vltnm)
                            if (!(CODE_VALUE %in% colnames(rdf))) {
                              util_warning(
                            "Missing at least a column %s in %s %s. Ignoring.",
                                sQuote(CODE_VALUE),
                                VALUE_LABEL_TABLE,
                                dQuote(vltnm),
                                applicability_problem = TRUE
                              )
                              r <- setNames(nm = character(0))
                              split_char <- SPLIT_CHAR
                              util_attach_attr(r, split_char = split_char)
                            }
                            if (!(CODE_LABEL %in% colnames(rdf))) {
                              rdf[[CODE_LABEL]] <- rdf[[CODE_VALUE]]
                            }
                            r <- setNames(rdf[[CODE_LABEL]],
                                          nm = rdf[[CODE_VALUE]])
                            split_char <- ifelse(
                              CODE_ORDER %in% colnames(rdf),
                              "<",
                              SPLIT_CHAR
                            )
                            util_attach_attr(r, split_char = split_char)
                          } else {
                            r <- setNames(nm = character(0))
                            split_char <- SPLIT_CHAR
                            util_attach_attr(r, split_char = split_char)
                          }
                        })

  meta_info$is_ordered <- vapply(assignments, attr, "split_char",
                                 FUN.VALUE = character(1)) == "<"

  if (any(meta_info$is_ordered)) { # check for counter-intuitive ordered lists
    # e.g., 2 = low < 1 = high
    # QUESTION TO STS: Should this check rather be part of prep_prepare_dataframes?
    for (a in assignments[meta_info$is_ordered]) {
      if (all(suppressWarnings(is.na(as.integer(names(a)))) ==
              is.na(names(a))) &&  # integer codes
          !all(diff(as.integer(names(a))) > 0)
      ) {
        util_message("Found counter-intuitive %s/%s: %s",
                     VALUE_LABELS,
                     VALUE_LABEL_TABLE,
                     prep_deparse_assignments(labels = vapply(a, identity,
                                                              FUN.VALUE = character(1)),
                                              codes = names(a),
                                              split_char = "<"))
      }
    }
  }

  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$STRING, DATA_TYPES$INTEGER) &
      (!util_empty(meta_info$VALUE_LABELS) | !util_empty(meta_info$VALUE_LABEL_TABLE)), SCALE_LEVEL] <-
    ifelse(meta_info$is_ordered[util_empty(meta_info$SCALE_LEVEL) &
                                  meta_info$DATA_TYPE %in%
                                    c(DATA_TYPES$STRING,
                                    DATA_TYPES$INTEGER) &
                                  (!util_empty(meta_info$VALUE_LABELS) | !util_empty(meta_info$VALUE_LABEL_TABLE))],
           SCALE_LEVELS$ORDINAL,
           SCALE_LEVELS$NOMINAL)

  # identical(attr(util_parse_assignments("a < 2", split_char = c(SPLIT_CHAR, "<")), "split_char"), "<")

  # FIXME: also consider STANDARDIZED_VOCABULARY (once it is implemented)

  # 2. Variables used as grouping variables: nominal scale ---------------------
  # for example examiners and devices
  all_group_vars <- varrefs[
    startsWith(varrefs, "GROUP_VAR_")]# TODO: What about the old schema using KEY_OBSERVER, etc.?

  if (length(all_group_vars) > 0) {
    used_as_group_var <- unique(sort(unlist(apply(meta_data[all_group_vars],
                                                  1:2,
                                                  util_find_var_by_meta,
                                                  meta_data = meta_data,
                                                  label_col = label_col,
                                                  target = label_col
    ), recursive = TRUE)))

    meta_info$used_as_group_var <- meta_info[[label_col]] %in% used_as_group_var

    meta_info[
      util_empty(meta_info$SCALE_LEVEL) &
        meta_info$used_as_group_var, SCALE_LEVEL] <-
      SCALE_LEVELS$NOMINAL
  }

  # 3. Variables used as time variables: interval scale ------------------------
  all_time_vars <- varrefs[
    startsWith(varrefs, "TIME_VAR")]

  if (length(all_time_vars) > 0) {
    used_as_time_var <- unique(sort(unlist(apply(meta_data[all_time_vars],
                                                 1:2,
                                                 util_find_var_by_meta,
                                                 meta_data = meta_data,
                                                 label_col = label_col,
                                                 target = label_col
    ), recursive = TRUE)))

    meta_info$used_as_time_var <- meta_info[[label_col]] %in% used_as_time_var

    meta_info[
      util_empty(meta_info$SCALE_LEVEL) &
        meta_info$used_as_time_var, SCALE_LEVEL] <-
      SCALE_LEVELS$INTERVAL
  }

  # 4. Variables of type 'datetime': interval scale ----------------------------
  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$DATETIME, DATA_TYPES$TIME),
    SCALE_LEVEL] <-
    SCALE_LEVELS$INTERVAL

  # 5. Variables of type 'string': nominal scale or 'na' -----------------------
  string_vars <- meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$STRING),
    label_col]

  if (length(string_vars) > 0) {
    string_not_cat <- vapply(ds1[, string_vars, drop = FALSE],
                             FUN.VALUE = logical(1),
                             FUN = util_string_is_not_categorical)

    meta_info[meta_info[[label_col]] %in% string_vars, SCALE_LEVEL] <-
      ifelse(string_not_cat, SCALE_LEVELS$`NA`, SCALE_LEVELS$NOMINAL)
  }

  # 6. Classify remaining integer and float values -----------------------------
  binaryrecodelimit <-
    getOption("dataquieR.scale_level_heuristics_control_binaryrecodelimit",
            dataquieR.scale_level_heuristics_control_binaryrecodelimit_default)

  metriclevels <-
    getOption("dataquieR.scale_level_heuristics_control_metriclevels",
            dataquieR.scale_level_heuristics_control_metriclevels_default)

  if (metriclevels < binaryrecodelimit) {
    metriclevels <- ifelse(
      dataquieR.scale_level_heuristics_control_metriclevels_default <
        binaryrecodelimit,
      binaryrecodelimit + 10,
      dataquieR.scale_level_heuristics_control_metriclevels_default)

    util_warning(paste(
      "The threshold for metric variables was set too low and set to",
      metriclevels,
      "instead."),
      applicability_problem = TRUE)
  }

  # number of distinct values > 'metriclevels' (default: 25) and
  # minimum value >= 0: ratio scale
  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
    meta_info$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT) &
    ((meta_info$IsInteger & meta_info$NCategory > metriclevels) |
     !meta_info$IsInteger) &
    !meta_info$AnyNegative, SCALE_LEVEL] <-
    SCALE_LEVELS$RATIO

  # number of distinct values > 'metriclevels' (default: 25) and
  # minimum value < 0: interval scale
  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT) &
      ((meta_info$IsInteger & meta_info$NCategory > metriclevels) |
      !meta_info$IsInteger) &
      meta_info$AnyNegative, SCALE_LEVEL] <-
    SCALE_LEVELS$INTERVAL

  # number of distinct values <= 'metriclevels' (default: 25) and
  # number of distinct values > 'binaryrecodelimit' (default: 8): ordinal
  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT) &
      meta_info$IsInteger &
      meta_info$NCategory <= metriclevels &
      meta_info$NCategory > binaryrecodelimit, SCALE_LEVEL] <-
    SCALE_LEVELS$ORDINAL

  # number of distinct values <= 'binaryrecodelimit' (default: 8): nominal
  meta_info[
    util_empty(meta_info$SCALE_LEVEL) &
      meta_info$DATA_TYPE %in% c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT) &
      meta_info$IsInteger &
      meta_info$NCategory <= binaryrecodelimit, SCALE_LEVEL] <-
    SCALE_LEVELS$NOMINAL

### TODO: This should not be required here anymore, but let's test it first.
  # write also something for variables that do not fit the `Stevens's` Typology
  # e.g., notes, letters, free text, other structured content (xml, json, ...)
  meta_info[
    util_empty(meta_info$SCALE_LEVEL), SCALE_LEVEL] <-
      SCALE_LEVELS$`NA`

  meta_data[[SCALE_LEVEL]] <- setNames(meta_info[[SCALE_LEVEL]],
                                       nm = meta_info[[label_col]])[meta_data[[label_col]]]

  meta_data
}


# https://gitlab.com/libreumg/dataquier/-/issues/125
# https://gitlab.com/libreumg/dataquier/-/issues/148

# TODO: Create Test Issue: We should improve the handling of VALUE_LABLELS like male | female (i.e., w/o a coding, the actual values are in the data, which is not so uncommon) -> maybe, we should convert these to our standard case similarly to prep_apply_coding and prep_valuelabels_from_data

# TODO: Discuss: How to reduce the message about inconsistent "low < high" but "nominal" in the metadata
# TODO: Share Updated metadata with examples for all new features (< in value labels, value labels w/o number -- old feature actually, scale_level set vs. missing; add some lorem-ipsum column; add suitable variable role secondary for variables, which are now mostly missing)

# IDEA: Use a scale-free-network assumption as a foundation to expect exponentionally distributed values for key columns

# For testing:
#' prep_load_workbook_like_file("meta_data_v2")
#' View(prep_scalelevel_from_data_and_metadata(study_data = "study_data")[, union(SCALE_LEVEL, colnames(prep_scalelevel_from_data_and_metadata(study_data = "study_data")))])
