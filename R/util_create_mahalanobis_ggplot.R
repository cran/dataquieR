#' Create a Q-Q Plot for squared `Mahalanobis` distance
#'
#' This helper is meant to create \code{ggplot2} objects starting from the
#' Mahalanobis ratios.
#'
#' @param MD_ratio [numeric] the `Mahalanobis` distance values divided by the
#'                            threshold value beyond which a data point
#'                            is considered an `outlier`.
#' @param mahalanobis_threshold [numeric] if not set by the user, the confidence
#'                                        level to identify multivariate `outliers`
#'                                        is 0.975 quantile of a
#'                                        chi-square distribution
#'                                        with p degrees of freedom,
#'                                        where p = variables
#'                                        `(Mayrhofer and Filzmoser, 2023)`
#' @param df [numeric] the degree of freedom, which
#'                      corresponds to the number of variables used to
#'                      calculate the `Mahalanobis` distance
#'
#' @return a list with:
#'   - `plot_MD`: [ggplot2::ggplot2] the Q-Q plot for the squared
#'                                   `Mahalanobis` distance vs.
#'                                   Chi-squared distribution
#'   - `MD_values`: [data.frame] the squared `Mahalanobis` distance values
#'   - `theoretical_q`: values expected from a chi-squared distribution
#'                      with the degree of freedom that
#'                      corresponds to the number of variables
#'
#'
#' @family util_functions
#' @concept outlier
#' @keywords internal
#'
#' @noRd
util_create_mahalanobis_ggplot <- function(MD_ratio,
                                           mahalanobis_threshold,
                                           df) {
  #Set up of colours and labels for plot ----
  # Define colours for the plot USE SHAPES!
  #col1 <-  "#2166AC"
  #col_outliers <-  "#7f0000"
  shape_non_out <- 1  #empty circle
  shape_outliers <- 4 # plus symbol

  #Define axis labels for plot
  x_label <- paste0("Quantiles  of Chi-squared (df=", df, ")")
  y_label <- "Squared Mahalanobis distance"

  #Order the MD_ratio
  MD_ratio <- sort(MD_ratio)

  # Obtain values expected from a theoretical chi-squared distribution
  # (same no. points as the sample and degree of freedom equal to the no. variables)
  probabilities <- stats::ppoints(length(MD_ratio))
  theoretical_quantiles <- stats::qchisq(probabilities, df = df)

  # Obtain threshold to identify multivariate outliers (argument mahalanobis_threshold)
  # it is by default 0.975 quantile of a chi-square distribution
  # with p degrees of freedom, where p = variables (Mayrhofer and Filzmoser, 2023)
  MD_outliers_threshold <- stats::qchisq(mahalanobis_threshold, df = df)

  #Obtain squared Mahalanobis distance from Mahalanobis ratio----
  MD <- MD_ratio * MD_outliers_threshold

  #create the data frame
  plot_data <- data.frame(
    theoretical = theoretical_quantiles,
    MD_values = MD
  )
  #order the squared Mahal. distance, actually not needed, as ordered before----
  #plot_data <- plot_data[order(plot_data$MD_values), ]

  #create a column with identified outliers based on threshold----
  plot_data$MD_outliers <- NA
  plot_data$MD_outliers <- ifelse(plot_data$MD_values > MD_outliers_threshold, 1, 0)
  plot_data$MD_outliers <- as.factor(plot_data$MD_outliers)


  #create the plot----
  p1 <- util_create_lean_ggplot({
    ggplot(plot_data, aes(x = .data[["theoretical"]],
                          y = .data[["MD_values"]])) +
      geom_point(aes(shape=as.factor(.data[["MD_outliers"]])),
         alpha = 0.5) +
      ggplot2::scale_shape_manual(name = "Outliers",
                                  values = c(shape_non_out, shape_outliers),
                                  labels = c("0" = "No",
                                             "1" = "Yes"))+
      geom_hline(yintercept = MD_outliers_threshold,
                 linetype = "dashed",
                 color = "black",
                 linewidth = 0.6) +
      labs(x = x_label,
           y = y_label) +
      theme_minimal() +
      theme(legend.position = "right")
  },
  plot_data = plot_data,
  MD_outliers_threshold = MD_outliers_threshold,
  x_label = x_label,
  y_label = y_label,
  shape_non_out = shape_non_out,
  shape_outliers = shape_outliers
  )

  # Information for sizing----
  range_y <- max(plot_data$MD_values, na.rm = TRUE) -
    min(plot_data$MD_values, na.rm = TRUE)
  range_x <- max(plot_data$theoretical, na.rm = TRUE) -
    min(plot_data$theoretical, na.rm = TRUE)

  attr(p1, "sizing_hints") <- list(
    figure_type_id = "mahalanobis_plot",
    range_x = range_x,
    range_y = range_y
  )
  rm(range_y, range_x)


 return(list(plot_MD = p1,
             MD_values = plot_data$MD_values,
             theoretical_q = plot_data$theoretical))

}
