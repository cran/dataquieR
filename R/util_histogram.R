#' Utility function to create histograms
#'
#' A helper function for simple histograms.
#'
#' @param plot_data a `data.frame` without missing values
#' @param num_var column name of the numerical or datetime variable
#'                in `plot_data` (if omitted, the first column is assumed to
#'                contain this variable)
#' @param fill_var column name of the categorical variable in `plot_data` which
#'                 will be used for coloring stacked histograms
#' @param facet_var column name of the categorical variable in `plot_data`
#'                  which will be used to create facets
#' @param nbins_max the maximum number of bins for the histogram (see
#'                  `util_optimize_histogram_bins`)
#' @param colors vector of colors, or a single color
#' @param is_datetime if `TRUE`, the x-axis will be adapted for the
#'              datetime format
#'
#' @return a histogram
#'
util_histogram <- function(plot_data,
                           num_var = colnames(plot_data)[1],
                           fill_var = NULL,
                           facet_var = NULL,
                           nbins_max = 100,
                           colors = "#2166AC",
                           is_datetime = FALSE) {
  # compute bin breaks
  # if the plot is faceted, optimize bin breaks for the most frequent category
  bb_opt_sel <- seq_len(nrow(plot_data))
  if (!is.null(facet_var)) {
    # Which category occurs most frequently?
    tab_cat <- table(plot_data[, facet_var])
    most_freq <- names(tab_cat)[which.max(tab_cat)]
    bb_opt_sel <- which(plot_data[, facet_var] == most_freq)
    # We have to ensure that the list of bin breaks includes also the overall
    # minimum and maximum value. Otherwise the faceted plots will be limited to
    # the range of values from the most frequent category.
    bb_opt_sel <- unique(c(bb_opt_sel,
                           which.min(plot_data[, num_var]),
                           which.max(plot_data[, num_var])))
  }

  bin_breaks <- suppressMessages(util_optimize_histogram_bins(
    x = plot_data[bb_opt_sel, num_var],
    nbins_max = nbins_max
  ))
  breaks_x <- bin_breaks[[1]]

  # create histogram
  his <- ggplot(data = plot_data, aes(x = .data[[num_var]])) +
    theme_minimal() +
    xlab("") +
    ylab("")

  if (!is.null(fill_var)) {
    if (length(colors) < length(levels(plot_data[, fill_var]))) {
      if (length(colors) == 1) {
        colors <- c("gray90", colors, "gray20")
      }
      colors <- colorRampPalette(colors)(length(levels(plot_data[, fill_var])))
    }
    his <- his +
      geom_histogram(aes(fill = .data[[fill_var]]),
                     breaks = breaks_x) +
      scale_fill_manual(values = colors, name = "")
  } else {
    his <- his +
      geom_histogram(breaks = breaks_x,
                     fill = colors[1],
                     color = colors[1])
  }

  if (!is_datetime) {
    his <- his + scale_x_continuous(expand = expansion(mult = 0.1))
  } else {
    his <- his + ggplot2::scale_x_datetime(expand = expansion(mult = 0.1))
  }

  if (!is.null(facet_var)) {
    his <- his +
      facet_grid(.data[[facet_var]] ~ ., scales = "free_y") +
      theme(strip.text = element_text(size = 14))
  }

  return(his)
}
