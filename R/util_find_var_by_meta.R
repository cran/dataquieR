#' Try hard, to map a variable
#'
#' does not warn on ambiguities nor if not found (but in the latter case,
#' it returns `ifnotfound`)
#'
#' @param resp_vars variables to map from
#' @param meta_data metadata
#' @param allowed_sources allowed names to map from (as metadata columns)
#' @param target metadata attribute to map to
#' @param label_col label-col to map from, if not `allowed_sources` should be
#'                  entirely passed
#' @param ifnotfound [list] A list of values to be used if the item is not
#'                          found: it will be coerced to a list if necessary.
#'
#' @return vector of mapped target names of resp_vars
#'
#' @family metadata_management
#' @concept metadata_management
#' @keywords internal
util_find_var_by_meta <- function(resp_vars,
                                  meta_data = "item_level",
                                  label_col = LABEL,
                                  allowed_sources = c(VAR_NAMES,
                                                      label_col,
                                                      LABEL,
                                                      LONG_LABEL,
                                                      "ORIGINAL_VAR_NAMES",
                                                      "ORIGINAL_LABEL"),
                                  target = VAR_NAMES,
                                  ifnotfound = NA_character_) {
  util_expect_scalar(resp_vars, allow_more_than_one = TRUE,
                     allow_na = TRUE, allow_null = TRUE,
                     check_type = is.character)
  util_expect_scalar(ifnotfound, allow_more_than_one = TRUE, allow_na = TRUE,
                     allow_null = TRUE)
  if (length(ifnotfound) == 0) {
    ifnotfound <- NA_character_
  }
  if (!(length(ifnotfound) %in% c(1, length(resp_vars))))
    util_error("%s must have either length 1 or the same length as %s",
               sQuote("ifnotfound"), sQuote("resp_vars"))
  if (length(ifnotfound) == 1) ifnotfound <- rep(ifnotfound, length(resp_vars))
  if (all(is.na(resp_vars))) {
    return(ifnotfound)
  }
  util_expect_data_frame(meta_data)
  if (!any(allowed_sources %in% colnames(meta_data))) {
    util_error(c("Did",
                 "not find any mappable column in item-level metadata."))
  }

  # This is not needed, since labels have not significantly changed
  # meta_data <- prep_meta_data_v1_to_item_level_meta_data(meta_data = meta_data,
  #                                                        verbose = FALSE,
  #                                                        label_col = label_col)
  allowed_sources <- unique(allowed_sources)
  allowed_sources <- intersect(allowed_sources, colnames(meta_data))
  target <- util_ensure_in(target, colnames(meta_data))
  if ((length(target) != 1) || (length(allowed_sources) < 1)) {
    map_res <- lapply(setNames(nm = allowed_sources), function(oc) {
      rep(NA_character_, length(resp_vars))
    })
  } else {
    map_res <- lapply(setNames(nm = allowed_sources), function(oc) {
      unname(
        util_map_labels(
          resp_vars,
          warn_ambiguous = FALSE,
          meta_data = meta_data,
          to = target,
          from = oc,
          ifnotfound = NA_character_
        )
      )
    })
  }
  map_res <- as.data.frame(map_res)
  # check whether the user-specified variable could not be found in any of the other columns:
  map_res_NA <- vapply(
    seq_len(nrow(map_res)), # TODO: Use seq_len in all places, where this mapping is used (likely, util_correct_variable_use and prep_meta_data_v1_to_item_level_meta_data)
    FUN.VALUE = logical(1),
    FUN = function(i) {
      all(is.na(map_res[i, ]))
    }
  )
  r <- vapply(
    seq_len(nrow(map_res)),
    FUN.VALUE = character(1),
    FUN = function(i) {
      unique(na.omit(as.character(map_res[i, ])))[1]
      # We select here the first element, such that in case of ambiguities
      # VAR_NAME is preferred over LABEL and LONG_LABEL, and LABEL over LONG_LABEL.
      # If there are no ambiguities, this works as well.
    }
  )
  not_found <- !util_empty(resp_vars) & is.na(r)
  r[not_found] <- ifnotfound[not_found]
  r
}
