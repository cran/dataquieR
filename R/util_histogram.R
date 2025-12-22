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
#' @param is_time if `TRUE`, the x-axis will be adapted for the
#'              time-only format
#'
#' @return a histogram
#'
#' @importFrom ggplot2 ggplot aes theme_minimal xlab ylab geom_col
#'                     scale_x_continuous scale_x_datetime expansion facet_grid
#'                     theme element_text
#' @importFrom grDevices colorRampPalette
#'
#' @noRd
util_histogram <- function(plot_data,
                           num_var = colnames(plot_data)[1],
                           fill_var = NULL,
                           facet_var = NULL,
                           nbins_max = 100,
                           colors = "#2166AC",
                           is_datetime = FALSE,
                           is_time = FALSE) {
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

  # compute bin heights, prepare data for plotting
  if (!is.null(facet_var) | !is.null(fill_var)) { # histogram with groups
    split_var <- c(facet_var, fill_var)
    split_data <- split(plot_data, plot_data[, split_var])
    split_data <- split_data[vapply(split_data, nrow, integer(1)) > 0]
    plot_data2 <- lapply(seq_along(split_data), function(ll) {
      h1 <- hist(split_data[[ll]][, num_var], plot = FALSE, breaks = breaks_x)
      return(data.frame(histogram_x = h1$mids,
                        histogram_y = h1$counts,
                        split_data[[ll]][1, split_var, drop = FALSE],
                        row.names = NULL))
    })
    plot_data2 <- do.call(rbind, plot_data2)
    if (is_datetime) {
      plot_data2[["histogram_x"]] <- util_parse_date(plot_data2[["histogram_x"]])
    } else if (is_time) {
      plot_data2[["histogram_x"]] <- util_parse_time(plot_data2[["histogram_x"]])
    }
  } else { # plain histogram
    bin_heights <- hist(as.numeric(plot_data[, num_var]),
                        plot = FALSE,
                        breaks = breaks_x)
    if (is_datetime) {
      plot_data2 <- data.frame(histogram_x = util_parse_date(bin_heights$mids),
                               histogram_y = bin_heights$counts)
    } else if (is_time) {
      plot_data2 <- data.frame(histogram_x = util_parse_time(bin_heights$mids),
                               histogram_y = bin_heights$counts)
    } else {
      plot_data2 <- data.frame(histogram_x = bin_heights$mids,
                               histogram_y = bin_heights$counts)
    }
  }
  width_col <- as.numeric(breaks_x)
  width_col <- width_col[2] - width_col[1]

  # create histogram
  if (!is.null(fill_var)) { # histogram with color-coded groups
    if (length(colors) < length(levels(plot_data[, fill_var]))) {
      if (length(colors) == 1) {
        colors <- c("gray90", colors, "gray20")
      }
      colors <- colorRampPalette(colors)(length(levels(plot_data[, fill_var])))
    }
    his <- util_create_lean_ggplot(
      ggplot(data = plot_data2, aes(x = .data[["histogram_x"]],
                                    y = .data[["histogram_y"]],
                                    fill = .data[[fill_var]])) +
        theme_minimal() +
        xlab("") +
        ylab("") +
        geom_col(width = width_col) +
        scale_fill_manual(values = colors, name = ""),
      plot_data2 = plot_data2,
      fill_var = fill_var,
      width_col = width_col,
      colors = colors
    )
  } else { # 'plain' histogram
    his <- util_create_lean_ggplot(
      ggplot(data = plot_data2, aes(x = .data[["histogram_x"]],
                                    y = .data[["histogram_y"]])) +
        theme_minimal() +
        xlab("") +
        ylab("") +
        geom_col(width = width_col,
                 fill = colors[1],
                 color = colors[1]),
      plot_data2 = plot_data2,
      width_col = width_col,
      colors = colors
    )
  }

  if (!is_datetime && !is_time) {
    his <- his + util_create_lean_ggplot(
      scale_x_continuous(expand = expansion(mult = 0.1)))
  } else if (is_datetime && !is_time) {
    his <- his + util_create_lean_ggplot(
      scale_x_datetime(expand = expansion(mult = 0.1)))
  } else {
    # should be time
    his <- his + util_create_lean_ggplot(
      ggplot2::scale_x_time(expand = expansion(mult = 0.1)))
  }

  if (!is.null(facet_var)) {
    his <- his + util_create_lean_ggplot(
      facet_grid(.data[[facet_var]] ~ ., scales = "free_y"),
      facet_var = facet_var) +
        theme(strip.text = element_text(size = 14))
  }

  return(his)
}
