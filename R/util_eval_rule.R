#' Evaluate a parsed redcap rule for given study data
#'
#' also allows to use `VAR_NAMES` in the rules,
#' if other labels have been selected
#'
#' @param rule the redcap rule (parsed, already)
#' @param ds1 the study data as prepared by `prep_prepare_dataframes`
#' @param meta_data the metadata
#' @param use_value_labels map columns with `VALUE_LABELS` as factor variables
#'
#' @return the result of the parsed rule
util_eval_rule <- function(rule, ds1, meta_data = "item_level",
                           use_value_labels) {
  if (missing(use_value_labels)) {
    use_value_labels <- FALSE
  }
  if (missing(meta_data)) { # TODO: Have a default, now?!
    use_value_labels <- FALSE
    meta_data <- data.frame()
  }
  util_expect_data_frame(meta_data)
  if (missing(ds1)) {
    ds1 <- parent.frame()
  }
  if (is.environment(ds1)) {
    ds1 <- as.list(ds1)
  }
  if (!is.list(ds1)) {
    util_error("%s must be an environment or a list (or a dataframe)",
               dQuote("ds1"))
  }
  redcap_rule_env <- util_get_redcap_rule_env()
  label_col <- attr(ds1, "label_col")
  if (use_value_labels) {
    util_stop_if_not(attr(ds1, "MAPPED"))
    util_stop_if_not(!util_empty(label_col))
    if (!(VALUE_LABELS %in% colnames(meta_data))) {
      meta_data[[VALUE_LABELS]] <- NA_character_
    }
    cols_with_valuelabels <- meta_data[!util_empty(meta_data[[VALUE_LABELS]]), # TODO: address VALUE_LABELS = c("m|f", "") ...
                                       label_col, drop = TRUE]
    ds1_with_labels <- ds1
    ds1_with_labels[, cols_with_valuelabels] <- lapply(cols_with_valuelabels,
      function(cn) {
        labs <-
          meta_data[
            meta_data[[label_col]] == cn,
            VALUE_LABELS,
            drop = TRUE]
        labs <- as.character(labs)
        # Are missing codes replaced?
        if (!("Codes_to_NA" %in% names(attributes(ds1)))) {
          ml <- util_parse_assignments(multi_variate_text = TRUE,
            meta_data[
              meta_data[[label_col]] == cn,
              MISSING_LIST,
              drop = TRUE])[[1]]
          ml <- ml[!util_empty(gsub(SPLIT_CHAR, "", ml))]
          jl <- util_parse_assignments(multi_variate_text = TRUE,
            meta_data[
              meta_data[[label_col]] == cn,
              JUMP_LIST,
              drop = TRUE])[[1]]
          jl <- jl[!util_empty(gsub(SPLIT_CHAR, "", jl))]
          jl <-
            prep_deparse_assignments(codes =
                                       suppressWarnings(trimws(names(jl))),
                                     labels = jl)
          ml <-
            prep_deparse_assignments(codes =
                                       suppressWarnings(trimws(names(ml))),
                                     labels = ml)
          jl <- jl[!is.na(jl)]
          ml <- ml[!is.na(ml)]
          labs <- paste(c(jl, ml, labs), collapse = sprintf(" %s ", SPLIT_CHAR))
        }
        col_as_factor <-
          util_assign_levlabs(variable = ds1[[cn]],
                            string_of_levlabs = labs,
                            splitchar = SPLIT_CHAR,
                            assignchar = "=", variable_name = cn)
        if (!is.factor(col_as_factor)) {
          col_as_factor <- as.factor(col_as_factor)
        }
        col_as_factor
      }
    )
    ds2 <- ds1_with_labels
  } else {
    ds2 <- ds1
  }
  if (length(label_col) == 1 && label_col != VAR_NAMES) {
    ds2_varnames <- ds2
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = VAR_NAMES,
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  if (length(label_col) == 1 && label_col != LABEL) {
    ds2_varnames <- ds2
    colnames(ds2_varnames) <-
      util_map_labels(colnames(ds2_varnames),
                      meta_data = meta_data,
                      to = LABEL,
                      from = label_col)
    ds2 <- cbind(ds2, ds2_varnames)
  }
  eval(expr = rule,
       envir = ds2,
       enclos = redcap_rule_env)
}
