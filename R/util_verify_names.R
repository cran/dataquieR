#' Test for likely misspelled data frame references
#'
#' checks, if some data frame names may have typos in their names
#'
#' @param name_of_study_data [character] names of study data, such are expected
#'
#' @return `invisible(NULL)`, messages / warns only.
#'
#' @keywords internal
util_verify_names <- function(name_of_study_data = character(0)) {
  x <- .util_verify_names(observed_names = prep_list_dataframes(),
                          name_of_study_data = name_of_study_data)
  if (length(x$warn) > 0) {
    util_warning("Found potentially misspelled tables: %s",
                 paste(dQuote(x$warn2), "=?=>", sQuote(x$warn),
                       collapse = ", "), immediate = TRUE)
  }
  if (length(x$dontknow > 0)) {
    util_message("Found unexpected tables: %s",
                 util_pretty_vector_string(x$dontknow))
  }
  return(invisible(NULL))
}

.util_verify_names <- function(standard_names = c("CODE_LIST_TABLE",
                                                  "cross-item_level",
                                                  "dataframe_level",
                                                  "item_level",
                                                  "segment_level",
                                                  "item_computation_level",
                                                  "study_data",
                                                  "<>",
                                                  name_of_study_data),
                              observed_names = prep_list_dataframes(),
                              name_of_study_data = character(0)) {


  observed_names <- setdiff(observed_names,
         c(
           "https://dataquality.qihs.uni-greifswald.de/extdata/study_data.RData",
           "https://dataquality.qihs.uni-greifswald.de/extdata/ship.RDS",
           "https://dataquality.qihs.uni-greifswald.de/extdata/meta_data.RData",
           "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData",
           "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData"
          )
  )
  observed_names <- observed_names[!startsWith(observed_names,
    system.file(package = utils::packageName()))]
  observed_names <-
    grep(
      "^LABS_.*::[a-f0-9]",
      observed_names,
      invert = TRUE,
      value = TRUE)

  # fill referred_names
  len_ref_nm <- -1
  referred_names <- c()
  #Loops runs while new referred names are found
  while (len_ref_nm < length(referred_names)) {
    len_ref_nm <- length(referred_names)
    #check for all observed names and return them
    referred_names_df <- lapply(observed_names, function(x) {
      r <- NULL; try(r <- prep_get_data_frame(x), silent = TRUE); r})
    # check if they are data frames and takes the columns ending with _table
    referred_names_cur <- lapply(referred_names_df, function(x) {
      if (is.data.frame(x) && ncol(x) > 0) {
        x[, endsWith(colnames(x), "_TABLE")]
      } else {
        character(0)
      }
    })
    #update referred_names
    referred_names <- unique(c(referred_names,
                               as.vector(unlist(referred_names_cur, recursive = TRUE))))
  }
  # Note: we started from observed_names but that may not be
  # the expected names, because the user can overwrite it

  #add all data frames from dataframe_level
  if ("dataframe_level" %in% prep_list_dataframes()) {
    referred_names <- c(referred_names,
                        prep_get_data_frame("dataframe_level")[[DF_NAME]])
  }

  #normalize all names
  referred_names <- referred_names[!is.na(referred_names)]
  referred_names <-  trimws(referred_names)
  referred_names <-  gsub(sprintf("\\s*([%s])\\s*", SPLIT_CHAR), "\\1",
                          referred_names)

  observed_names <- trimws(observed_names)
  observed_names <-  gsub(sprintf("\\s*([%s])\\s*", SPLIT_CHAR), "\\1",
                           observed_names)

  prefixes <- unique(trimws(
    gsub(sprintf("^(.*?)[%s].*", SPLIT_CHAR), "\\1",
         grep(SPLIT_CHAR, value = TRUE, fixed = TRUE, observed_names))))

  prefixes_to_be_processed <- grep(".",
                                   prefixes,
                                   value = TRUE,
                                   fixed = TRUE,
                                   invert = TRUE)
  prefixes_to_be_processed <-
    prefixes_to_be_processed[!grepl( ":",
                                     prefixes_to_be_processed,
                                     fixed = TRUE)]

  prefixes <- c(prefixes,
                paste0("https://dataquality.qihs.uni-greifswald.de/extdata/",
                       prefixes_to_be_processed,
                       ".xlsx"))


  referred_names <-  c(referred_names, vapply(prefixes, FUN = function(prefix) {
    paste0(prefix, SPLIT_CHAR, c(standard_names, referred_names)) },
    FUN.VALUE = character(length(c(standard_names, referred_names)))))



  #Add the referred names to standard (-> expected names)
  standard_names <- c(standard_names, referred_names)
  standard_names <- standard_names[!is.na(standard_names)]

  #try to compare expected names with names available, warn about data frames not expected
  d1 <- adist(toupper(standard_names), toupper(observed_names))
  d2 <- adist((standard_names), (observed_names))
  rownames(d1) <- rownames(d2) <- standard_names
  colnames(d1) <- colnames(d2) <- observed_names
  names(dimnames(d1)) <- names(dimnames(d2)) <- c("standard_names", "observed_names")
  dn <- t(t(d2) / nchar(rownames(d2))) # https://stackoverflow.com/a/20596490
  dn1 <- t(t(d1) / nchar(rownames(d1))) # https://stackoverflow.com/a/20596490
  # redo the matrices with stringdist if that is available
  if (util_ensure_suggested("stringdist", err = FALSE, goal = "guessing typos")) {
    dn <- stringdist::stringdistmatrix(standard_names,
                                       observed_names, method = "jw", p = 0.1)
    dn1 <- stringdist::stringdistmatrix(toupper(standard_names),
                                        toupper(observed_names),
                                        method = "jw", p = 0.1)
  }
  # compute the minimum distance to an expected name for each column (observed names)
  xx <- suppressWarnings(apply(dn, 2, min))

  #initialize a new matrix
  non_standard <- matrix()
  #find non-matching columns (observed names) and keep only them
  try(non_standard <- d2[, xx > 0, drop = FALSE], silent = TRUE)

  #find the closest expected names for this non-matching
  likely <- setNames(rownames(d1)[apply(d1, 2, which.min)], nm = colnames(d1))
  #compute likelihood
  suppressWarnings(llhd <- 1 - apply(dn1, 2, min))
  props <- setNames(llhd, nm = likely)
  #identify proposals that are not closer than 90%
  dontknow <- names(likely[llhd <= 0.9])
  if (!is.null(dontknow)) for (sn in c(standard_names, observed_names))
    dontknow <- dontknow[!endsWith(dontknow, paste0("|", sn))]
  list(case_insensitive = d1, case_sensitive = d2, mismatches = non_standard,
       likely = likely, llhd = llhd, warn = likely[llhd > 0.9 & llhd < 1.0],
       warn2 = names(likely)[llhd > 0.9 & llhd < 1.0],
       dontknow = dontknow)
}
