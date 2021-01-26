#' Function to calculate and plot Mahalanobis distances
#'
#' @description
#' A standard tool to detect multivariate outliers is the Mahalanobis distance.
#' This approach is very helpful for the interpretation of the plausibility of a
#' measurement given the value of another.
#' In this approach the Mahalanobis distance is used as a univariate measure
#' itself. We apply the same rules for the identification of outliers as in
#' univariate outliers:
#' - the classical approach from Tukey: \eqn{1.5 * IQR} from the
#'   1st (\eqn{Q_{25}}) or 3rd (\eqn{Q_{75}}) quartile.
#' - the \eqn{6* \sigma} approach, i.e. any measurement of the Mahalanobis
#'   distance not in the interval of \eqn{\bar{x} \pm 3*\sigma} is considered an
#'   outlier.
#' - the approach from Hubert for skewed distributions which is embedded in the
#'   R package \pkg{robustbase}
#' - a completely heuristic approach named \eqn{\sigma}-gap.
#'
#' For further details, please see the vignette for univariate outlier.
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type float
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all resp_vars
#' - The Mahalanobis distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The four rules mentioned above are applied on this distance for
#'   each observation in the study data
#' - An output data frame is generated that flags each outlier
#' - A parallel coordinate plot indicates respective outliers
#'
#' @param resp_vars [variable list] len=1-2. the name of the continuous
#'                                           measurement variable
#' @param id_vars [variable list] [variable list] optional, an ID variable of
#'                        the study data. If not specified row numbers are used.
#' @param n_rules [numeric] from=1 to=4. the no. of rules that must be violated
#'                                       to classify as outlier
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2] outlier plot
#'   - `FlaggedStudyData` [data.frame] contains the original data frame with
#'                                     the additional columns `tukey`,
#'                                     `sixsigma`,
#'                                     `hubert`, and `sigmagap`. Every
#'                                     observation
#'                                     is coded 0 if no outlier was detected in
#'                                     the respective column and 1 if an
#'                                     outlier was detected. This can be used
#'                                     to exclude observations with outliers.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_path  scale_color_manual geom_point
#'                     scale_size_manual theme_minimal scale_alpha_manual
#'
#' @importFrom stats mahalanobis
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dfg-qa.ship-med.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
acc_multivariate_outlier <- function(resp_vars, id_vars = NULL, label_col,
                                     n_rules = 4, study_data, meta_data) {
  rvs <- resp_vars

  # rules in 1:4?
  if (n_rules != 4) {
    if (!(n_rules %in% 1:4)) {
      util_warning(
        "The formal n_rules is not an integer of 1 to 4, default (4) is used. ")
      n_rules <- 4
    }
  }

  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer | float"
  )

  if (length(resp_vars) == 1) {
    util_error("Need at least two variables for multivariate outliers.")
  }

  # check correct use of ID-variable if specified
  if (!is.null(id_vars)) {
    util_correct_variable_use("id_vars",
      allow_any_obs_na = TRUE,
      need_type = "character | integer"
    )
  }

  # no use of id_vars?
  if (is.null(id_vars)) {
    ds1$dq_id <- rownames(ds1)
    id_vars <- "dq_id"
    util_warning("As no ID-var has been specified the rownumbers will be used.")
  }

  if (length(id_vars) > 1) {
    id_vars <- id_vars[1]
  }

  # missing data in any of the variables?
  vars <- c(resp_vars, id_vars)
  vars <- vars[!is.na(vars)]

  n_prior <- dim(ds1)[1]
  ds1plot <- ds1[rowSums(is.na(ds1[, vars, drop = FALSE])) == 0, ,
    drop = FALSE
  ]
  n_post <- dim(ds1plot)[1]

  if (n_post == 0) {
    util_error("No samples with without missing values in one of %s. Aborting.",
               paste0(dQuote(vars), collapse = ", "))
  }

  if (n_post < n_prior) {
    util_warning(paste0(
      "Due to missing values in ", paste0(rvs, collapse = ", "),
      " or ", id_vars, " N=", n_prior - n_post,
      " observations were excluded."
    ))
  }

  rvs <-
    util_no_value_labels(
      resp_vars = rvs,
      meta_data = meta_data,
      label_col = label_col,
      warn = TRUE,
      stop = TRUE
    )

  # Mahalanobis ----------------------------------------------------------------

  # Estimate covariance of response variables
  Sx <- cov(ds1plot[, rvs, drop = FALSE])

  # Calculation of the Mahalanobis distance
  ds1plot$MD <- mahalanobis(ds1plot[, rvs], colMeans(ds1plot[, rvs]), Sx)

  # browser()

  # outlier identification
  # Initialize with NA
  ds1plot$tukey <- NA
  ds1plot$sixsigma <- NA
  ds1plot$hubert <- NA
  ds1plot$sigmagap <- NA
  # View(ds1)
  # apply outlier functions to plot-df
  # after export/final built  correct the call of the utility functions
  ds1plot$tukey <- util_tukey(ds1plot$MD)
  ds1plot$sixsigma <- util_sixsigma(ds1plot$MD)
  ds1plot$hubert <- util_hubert(ds1plot$MD)
  ds1plot$sigmagap <- util_sigmagap(ds1plot$MD)

  # calculate summary of all outlier definitions
  ds1plot$Rules <-
    apply(ds1plot[, c("tukey", "sixsigma", "hubert", "sigmagap")], 1, sum)

  # create summary table
  # outlier deviating according to all rules?
  n_devs <- ifelse(ds1plot$Rules >= n_rules, 1, 0)
  grading <- max(n_devs, na.rm = TRUE)

  st1 <- data.frame(
    Variables = paste0(rvs, collapse = " | "),
    Tukey = sum(ds1plot$tukey),
    SixSigma = sum(ds1plot$sixsigma),
    Hubert = sum(ds1plot$hubert),
    SigmaGap = sum(ds1plot$sigmagap),
    Grading = grading
  )


  # reshape wide to long
  ds2plot <-
    melt(ds1plot[, c(rvs, id_vars, "Rules")], measure.vars = rvs, id.vars =
           c(id_vars, "Rules"))

  # use neamed color vector
  disc_cols <- c("#2166AC", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000")
  names(disc_cols) <- c(0:4)

  # use ID/Rules as factor
  ds2plot[[id_vars]] <- factor(ds2plot[[id_vars]])
  ds2plot$Rules <- factor(ds2plot$Rules)


  # transparency
  .a <- c(0.2, 0.7, 0.8, 0.9, 1)
  .a <- .a[seq_along(levels(ds2plot$Rules))]
  names(.a) <- levels(ds2plot$Rules)

  # size
  .s <- c(0.05, 0.2, 0.3, 0.4, 0.5)
  .s <- .s[seq_along(levels(ds2plot$Rules))]
  names(.s) <- levels(ds2plot$Rules)

  # browser()

  # PLOT
  p <- ggplot(ds2plot, aes(x = variable, y = value, colour = Rules,
                           group = .data[[id_vars]])) +
    geom_path(aes(alpha = Rules, size = Rules), position = "identity") +
    scale_color_manual(values = disc_cols) +
    geom_point(aes(alpha = Rules)) +
    scale_alpha_manual(values = .a) +
    scale_size_manual(values = .s) +
    theme_minimal()

  return(list(FlaggedStudyData = ds1plot, SummaryTable = st1, SummaryPlot = p))
}
