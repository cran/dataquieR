#' utility function to dichotomize variables
#'
#' use the meta data attribute `RECODE` (=`r dQuote(RECODE)`) to
#' dichotomize the data
#'
#' @param study_data Study data including jump/missing codes as specified in the
#'                   code conventions
#' @param meta_data Meta data as specified in the code conventions
#'
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
util_dichotomize <- function(study_data,
                             meta_data,
                             label_col = VAR_NAMES) {


  if (!("Codes_to_NA" %in% names(attributes(study_data)))) {
    util_error("Expect to have missings already replaced.")
  }

  vrcnt <- 0
  vlcnt <- 0

  for (var in colnames(study_data)) {
    recode <- meta_data[meta_data[[label_col]] == var, RECODE]
    if (!is.null(recode) && !is.na(recode) && trimws(recode) != "") {
      vrcnt <- vrcnt + 1
      col <- study_data[[var]]
      na <- is.na(col)
      vlcnt <- vlcnt + sum(!na)
      ref <- col %in% trimws(strsplit(recode, SPLIT_CHAR, fixed = TRUE)[[1]])
      col[ref] <- 0
      col[!ref] <- 1
      col[na] <- NA
      study_data[, var] <- col
    }
  }

  if (vrcnt > 0) {
    message(sprintf("Recoded %d variables (%d not NA values)", vrcnt, vlcnt))
  }

  return(study_data)
}
