#' Utility function to interpret mathematical interval notation
#'
#' Utility function to split limit definitions into interpretable elements
#'
#' @param mdata [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @importFrom utils head
#'
#' @return augments metadata by interpretable limit columns
#'
util_interpret_limits <- function(mdata) {

  # grep all columns of data with the notation of LIMITS
  lv <- colnames(mdata[grep("LIMIT", colnames(mdata))])

  if (length(lv) == 0) {
    util_error("No column containing the term LIMIT.")
  }

  # all not empty?
  ne <- apply(mdata[, lv, drop = FALSE], 2, function(x) all(is.na(x)))

  if (any(ne)) {
    util_warning(paste0("The column: ", lv[ne],
                        " has no defined intervals and is omitted."))
  }

  # don't consider empty columns at all
  lv <- lv[!(ne)]

  if (length(lv) == 0) {
    return(mdata)
  }

  # prefix/type of LIMITS
  pv <- unlist(lapply(strsplit(lv, split = "_", fixed = TRUE), `[[`, 1))

  # result
  mdata_ext <- mdata

  # the code below works only on non-empty elements of the column
  # the number of elements will most likely vary between columns
  for (i in seq_along(lv)) {
   valid <- grepl(
    "[\\[(]([0-9\\.Ee+\\-]*|[\\+\\-]?Inf);([0-9\\.Ee+\\-]*|[\\+\\-]?Inf)[\\])]",
      gsub(" ", "", mdata[[lv[i]]], fixed = TRUE), perl = TRUE)

    if (any(!valid & !util_empty(mdata[[lv[i]]]))) {
      util_warning(
        "Found invalid limits for %s: %s%s -- will ignore these",
        sQuote(lv[i]),
        paste(head(dQuote(mdata[[lv[i]]][!valid & !is.na(mdata[[lv[i]]])]), 5),
              collapse = ", "),
        (ifelse(sum(!valid & !is.na(mdata[[lv[i]]])) > 5, ", ...", ""))
      )
    }

    # select rows with entries in limits
    X <- mdata[valid & !(is.na(mdata[lv[i]])), c(VAR_NAMES, DATA_TYPE, lv[i])]

    # Split limits
    myfun1 <- function(x) {
      trimws(unlist(strsplit(as.character(x), split = ";"), "both"))
    }

    # add to dataframe
    if (any(valid)) {
      X <- cbind.data.frame(X, (t(apply(as.data.frame(X[[lv[i]]]), 1, myfun1))))

      # extract values
      X_LOWER <- (gsub(X[, "1"],
        replacement = "",
        pattern = "[\\(|\\[]",
        perl = TRUE
      ))
      X_UPPER <- (gsub(X[, "2"],
        replacement = "",
        pattern = "[\\)|\\]]",
        perl = TRUE
      ))
      ## date
      date_vars <- X[[DATA_TYPE]] == DATA_TYPES$DATETIME
      date_vars[is.na(date_vars)] <- FALSE

      IS_LOWER_INF <- grepl(perl = TRUE, ignore.case = TRUE,
                            "^\\s*[\\+\\-]?\\s*Inf\\s*$", X_LOWER[date_vars])
      IS_UPPER_INF <- grepl(perl = TRUE, ignore.case = TRUE,
                            "^\\s*[\\+\\-]?\\s*Inf\\s*$", X_UPPER[date_vars])
      VAL_LOWER_INF <- X_LOWER[date_vars][IS_LOWER_INF]
      VAL_UPPER_INF <- X_UPPER[date_vars][IS_UPPER_INF]

      X_LOWER[date_vars][IS_LOWER_INF] <- NA
      X_UPPER[date_vars][IS_UPPER_INF] <- NA
      X_LOWER[date_vars][trimws(X_LOWER[date_vars]) == ""] <- NA
      X_UPPER[date_vars][trimws(X_UPPER[date_vars]) == ""] <- NA

      # extract values
      X$LOWER[date_vars] <- suppressWarnings(
        as.numeric(as.POSIXct(X_LOWER[date_vars],
                                                  optional = TRUE)))
      X$UPPER[date_vars] <- suppressWarnings(
        as.numeric(as.POSIXct(X_UPPER[date_vars],
                                                  optional = TRUE)))

      X$LOWER[date_vars][IS_LOWER_INF] <- suppressWarnings(
        as.numeric(VAL_LOWER_INF))
      X$UPPER[date_vars][IS_UPPER_INF] <- suppressWarnings(
        as.numeric(VAL_UPPER_INF))

      X$LOWER[!date_vars] <- suppressWarnings(as.numeric(X_LOWER[!date_vars]))
      X$UPPER[!date_vars] <- suppressWarnings(as.numeric(X_UPPER[!date_vars]))

      damaged_lower_limit <- ((!is.na(X_LOWER) & X_LOWER != "") &
                                is.na(X$LOWER))
      if (any(damaged_lower_limit)) {
        field <- sQuote(lv[[i]])
        defects <- paste(head(dQuote(X_LOWER[damaged_lower_limit]), 5), "in",
                         head(sQuote(X$VAR_NAMES[damaged_lower_limit]), 5),
                         collapse = ", ")
        dots <- ifelse(sum(damaged_lower_limit) > 5, ", ...", "")
        util_warning(
          "Damaged lower %s: %s%s",
          field,
          defects,
          dots
        )
      }

      damaged_upper_limit <- ((!is.na(X_UPPER) & X_UPPER != "") &
                                is.na(X$UPPER))
      if (any(damaged_upper_limit)) {
        field <- sQuote(lv[[i]])
        defects <- paste(head(dQuote(X_UPPER[damaged_upper_limit]), 5), "in",
                         head(sQuote(X$VAR_NAMES[damaged_upper_limit]), 5),
                         collapse = ", ")
        dots <- ifelse(sum(damaged_upper_limit) > 5, ", ...", "")
        util_warning(
          "Damaged upper %s: %s%s",
          field,
          defects,
          dots
        )
      }

      damaged_upper_limit <- ((!is.na(X_UPPER) & X_UPPER != "") &
                                is.na(X$UPPER))

      # identify brace type
      p1 <- "("
      p2 <- ")"

      # interpret brace to logical
      X$INCL_LOWER <- ifelse(grepl(p1, X[, "1"], fixed = TRUE), FALSE, TRUE)
      X$INCL_UPPER <- ifelse(grepl(p2, X[, "2"], fixed = TRUE), FALSE, TRUE)

      X <- X[, c("VAR_NAMES", "LOWER", "UPPER", "INCL_LOWER", "INCL_UPPER")]

      colnames(X) <- c(
        "VAR_NAMES",
        paste0(pv[i], "_LIMIT_LOW"),
        paste0(pv[i], "_LIMIT_UP"),
        paste0("INCL_", paste0(pv[i], "_LIMIT_LOW")),
        paste0("INCL_", paste0(pv[i], "_LIMIT_UP"))
      )
# --> for dates, use as.POSIXct(X$UPPER, origin = "1970-01-01")
#     to translate back.

      mdata_ext <- merge(mdata_ext, X, by = "VAR_NAMES", all.x = TRUE)
    }
  }

  return(mdata_ext)
}
