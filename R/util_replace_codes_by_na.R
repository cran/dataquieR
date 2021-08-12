#' Utility function to replace missing codes by `NA`s
#'
#' Substitute all missing codes in a [data.frame] by `NA`.
#'
#' @param study_data Study data including jump/missing codes as specified in the
#'                   code conventions
#' @param meta_data Metadata as specified in the code conventions
#' @param split_char Character separating missing codes
#'
#' Codes are expected to be numeric.
#'
#' @importFrom stats setNames
util_replace_codes_by_NA <- function(study_data, meta_data,
                                     split_char = SPLIT_CHAR) {
  sdf <- study_data
  mdf <- meta_data
  # data_records is edited but not explicitly copied, so we can expect the
  # MAPPED-attribute to be preserved.
  stopifnot(is.data.frame(sdf))
  stopifnot(is.data.frame(mdf))

  label_col <- VAR_NAMES
  if (!is.null(attr(sdf, "label_col"))) {
    label_col <- attr(sdf, "label_col")
  }

  for (code_name in c(MISSING_LIST, JUMP_LIST)) {
    if (!(code_name %in% names(mdf)) || all(is.na(mdf[[code_name]]) |
                                            trimws(mdf[[code_name]]) == "")) {
      util_warning(
        c("Meta data does not provide a filled column",
          "called %s for replacing codes with NAs."),
        dQuote(code_name),
        applicability_problem = TRUE)
    }
  }

  # apply functions on studydata and create list
  miss <- lapply(setNames(nm = names(sdf)), util_get_code_list,
                 MISSING_LIST, split_char,
    mdf = mdf, label_col = label_col, warning_if_no_list = FALSE
  )
  jump <- lapply(setNames(nm = names(sdf)), util_get_code_list,
                 JUMP_LIST, split_char,
    mdf = mdf, label_col = label_col, warning_if_no_list = FALSE
  )

  miss_n <- list()
  jump_n <- list()

  miss_n <- lapply(
    mapply(`%in%`, sdf, miss, SIMPLIFY = FALSE, USE.NAMES = TRUE), sum,
    na.rm = TRUE)
  jump_n <- lapply(
    mapply(`%in%`, sdf, jump, SIMPLIFY = FALSE, USE.NAMES = TRUE), sum,
    na.rm = TRUE)

  replace <- function(x, l) {
    x[x %in% l] <- NA
    x
  }
  environment(replace) <- parent.env(environment())

  sdf[] <- mapply(replace, sdf, miss, SIMPLIFY = FALSE)
  sdf[] <- mapply(replace, sdf, jump, SIMPLIFY = FALSE)

  attr(sdf, "Codes_to_NA") <- TRUE
  attr(sdf, "MAPPED") <- attr(study_data, "MAPPED")

  return(list(study_data = sdf, list_N_MC_replaced = miss_n,
              list_N_JC_replaced = jump_n))
}
