#' Utility function to interpret mathematical interval notation for numeric
#' ranges
#'
#' Utility function to split range definitions into interpretable elements
#'
#' @param mdata [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @importFrom utils head
#'
#' @return augments metadata by interpretable limit columns
#'
util_interpret_range <- function(mdata) {
  # grep specific columns with limit notation: LOCATION_RANGE and PROPORTION_RANGE
  lv <- colnames(mdata[grep("_RANGE", colnames(mdata))])

  if (length(lv) == 0) {
    util_error("No column containing the term _RANGE.",
               applicability_problem = TRUE)
  }

  # all not empty?
  ne <- apply(mdata[, lv, drop = FALSE], 2, function(x) all(is.na(x)))

  if (any(ne)) {
    util_warning(paste0("The column ", lv[ne],
                        " has no defined intervals and is omitted."),
                 applicability_problem = TRUE)
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
    valid1 <- grepl(
      "[\\[(]([0-9\\.Ee+\\-]*|[\\+\\-]?Inf);([0-9\\.Ee+\\-]*|[\\+\\-]?Inf)[\\])]",
      gsub(" ", "", mdata[[lv[i]]], fixed = TRUE), perl = TRUE)
    valid2 <- vapply(mdata[[lv[i]]], FUN.VALUE = logical(1),
                     function(x) {
                       if (is.na(x) || trimws(x) == "") {
                         return(FALSE)
                       }
                       x <- gsub("^[(\\[]+", "", x, perl = TRUE)
                       x <- gsub("[)\\]]+$", "", x, perl = TRUE)
                       x <- paste("", x, "")
                       xs <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
                       if (length(xs) != 2) {
                         return(FALSE)
                       }
                       xs1 <- xs[[1]]
                       xs2 <- xs[[2]]
                       if (xs1 %in% c("", "Inf", "+Inf", "-Inf")) {
                         a <- TRUE
                       } else {
                         a <- !inherits(try(as.POSIXct(xs1), silent = TRUE),
                                        "try-error")
                       }
                       if (xs2 %in% c("", "Inf", "+Inf", "-Inf")) {
                         b <- TRUE
                       } else {
                         b <- !inherits(try(as.POSIXct(xs2), silent = TRUE),
                                        "try-error")
                       }
                       a && b
                     })

    valid <- valid1 | valid2

    if (any(!valid & !util_empty(mdata[[lv[i]]]))) {
      util_warning(
        "Found invalid limits for %s: %s%s -- will ignore these",
        sQuote(lv[i]),
        paste(head(dQuote(mdata[[lv[i]]][!valid & !is.na(mdata[[lv[i]]])]), 5),
              collapse = ", "),
        (ifelse(sum(!valid & !is.na(mdata[[lv[i]]])) > 5, ", ...", "")),
        applicability_problem = TRUE
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

      X$LOWER <- suppressWarnings(as.numeric(X_LOWER))
      X$UPPER <- suppressWarnings(as.numeric(X_UPPER))

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
          dots,
          applicability_problem = TRUE
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
          dots,
          applicability_problem = TRUE
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

      mdata_ext <- merge(mdata_ext, X, by = "VAR_NAMES", all.x = TRUE)
    }
  }

  return(mdata_ext)
}
