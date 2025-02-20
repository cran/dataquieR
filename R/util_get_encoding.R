#' Get encoding from metadata or guess it from data
#'
#' @inheritParams .template_function_indicator
#'
#' @return named vector of valid encoding strings matching `resp_vars`
util_get_encoding <- function(resp_vars = colnames(study_data),
                              study_data,
                              label_col,
                              meta_data,
                              meta_data_dataframe) {
  result <- setNames(rep(NA_character_, length(resp_vars)),
                     nm = resp_vars)
  # from item-level
  if (ENCODING %in% colnames(meta_data)) {
    meta_data[[ENCODING]] <- trimws(meta_data[[ENCODING]])
    meta_data[[ENCODING]][meta_data[[ENCODING]] == ""] <- NA_character_
    rvs <- resp_vars[is.na(result[resp_vars])]
    result[rvs] <-
      setNames(meta_data[[ENCODING]], nm = meta_data[[label_col]])[rvs]
    result[rvs] <- toupper(result[rvs])
    unkown <- setdiff(result, .encs_avail)
    if (length(unkown) > 0) {
      util_warning("Found unkown encodings in metadata: %s",
                   util_pretty_vector_string(unkown))
      result[(!(result[rvs] %in% .encs_avail))] <- NA_character_
    }
  }
  # from data-frame-level
  if (DATAFRAMES %in% colnames(meta_data) &&
      ENCODING %in% colnames(meta_data_dataframe) &&
      DF_CODE %in% colnames(meta_data_dataframe)) {
    meta_data_dataframe[[ENCODING]] <- trimws(meta_data_dataframe[[ENCODING]])
    meta_data_dataframe[[ENCODING]][meta_data_dataframe[[ENCODING]] == ""] <- NA_character_
    rvs <- resp_vars[is.na(result[resp_vars])]
    dfs <- setNames(meta_data[[DATAFRAMES]], nm = meta_data[[label_col]])
    result[rvs] <-
      vapply(rvs, function(rv) {
        dfcodes <- util_parse_assignments(dfs[[rv]])
        enc <- meta_data_dataframe[meta_data_dataframe[[DF_CODE]] %in%
                                         dfcodes, ENCODING]
        enc <- unique(enc)
        enc <- enc[!util_empty(enc)]

        if (length(enc) > 1) {
          util_warning(c("Found more than one encodings for variable %s",
                         "from dataframe level metadata, namely %s,",
                         "because the variable exists in the dataframes %s.",
                         "This is not yet supported. I'll use the first, %s."),
                       sQuote(rv),
                       util_pretty_vector_string(enc),
                       util_pretty_vector_string(dfcodes),
                       dQuote(head(enc, 1)),
                       applicability_problem = TRUE,
                       intrinsic_applicability_problem = TRUE)
          enc <- head(enc, 1)
        } else if (length(enc) < 1) {
          enc <- NA_character_
        }
        enc
      }, FUN.VALUE = character(1))
    result[rvs] <- toupper(result[rvs])
    unkown <- setdiff(result, .encs_avail)
    if (length(unkown) > 0) {
      util_warning("Found unkown encodings in metadata: %s",
                   util_pretty_vector_string(unkown))
      result[(!(result[rvs] %in% .encs_avail))] <- NA_character_
    }
  }
  # from guessing
  rvs <- resp_vars[is.na(result[resp_vars])]
  if (length(rvs) > 0) {
    util_warning(c("Encoding information neither in item-",
                 "nor dataframe-level-metadata found for %s, guessing from",
                 "data."),
                 util_pretty_vector_string(rvs), applicability_problem = TRUE)
  }
  result[rvs] <- vapply(setNames(nm = rvs),
         function(rv) {
           prep_guess_encoding(x = study_data[[rv]])$encoding[[1]]
          }, FUN.VALUE = character(1))
  result[rvs] <- toupper(result[rvs])
  result[(!(result[rvs] %in% .encs_avail))] <- NA_character_
  result[resp_vars]
}
.encs_avail <- toupper(c(iconvlist(), NA_character_, "unknown"))
