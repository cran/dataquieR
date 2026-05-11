#' Utility function to calculate Mahalanobis distances for a group of variables
#'
#' @param x [data.frame] containing the variables to use to
#'                        calculate the Mahalanobis distance
#' @param rv a vector containing the names of the variables
#' @param check_label_rv [character] the label of the check list used to define
#'                                    the group of variables in the cross-item metadata
#'                                    or "variable_group" if the list of variables
#'                                    is provided as an argument
#'
#' @return a list with:
#'   - `x_with_MD`: [data.frame] containing the original variables with the
#'                                addition of a new column containing the
#'                                Mahalanobis distance (only for complete cases)
#'   - `df`: [numeric] the degree of freedom (no.variables)
#'
#' @family util_functions
#' @concept outlier
#' @keywords internal
#'
#' @noRd
util_generate_mahalanobis_dist <- function(x,
                                           rv,
                                           check_label_rv){
  x$row_numbers <- c(1:nrow(x))
  n_prior <- dim(x)[1]
  ds1completecases <- x[rowSums(is.na(x[, rv, drop = FALSE])) == 0, ,
                                drop = FALSE]


  n_post <- dim(ds1completecases)[1]

  if (n_post == 0) {
    util_error("No observational unit with complete cases for all variables %s. Aborting.",
               paste0(sQuote(rv), collapse = ", "),
               applicability_problem = FALSE)
  }

  if (n_post < n_prior) {
    util_message(paste0(
      "Due to missing values",
      " N=", n_prior - n_post,
      " observational units were excluded."
    ), applicability_problem = FALSE)
  }

  # Mahalanobis ----------------------------------------------------------------
  # no. variables used to calculate the MD
  degree_freedom <- ncol(ds1completecases[, rv]) #NOTE: this is the no. columns or no. variables

  # Estimate covariance of response variables
  Sx <- cov(ds1completecases[, rv, drop = FALSE])

  # Calculate squared Mahalanobis distance
  ds1completecases[[paste0("MD_", check_label_rv)]] <-
    mahalanobis(ds1completecases[, rv],
                colMeans(ds1completecases[, rv, drop = FALSE]),
                Sx)

  ds1completecases <- merge(x,
            ds1completecases,
            by = intersect(colnames(x), colnames(ds1completecases)),
            all.x = TRUE)

  ds1completecases <- ds1completecases[, names(ds1completecases) != "row_numbers"]


  return(list(x_with_MD = ds1completecases,
              df = degree_freedom))

}

