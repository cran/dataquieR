#' Pairwise numeric variable visualization using ggplot2
#'
#' This function creates a grid of plots showing pairwise relationships between
#' numeric columns in a dataset using `ggplot2`. The diagonal contains histograms
#' or density plots, the upper triangle shows correlation coefficients, and the
#' lower triangle shows scatterplots.
#'
#' @param data A data.frame to visualize.
#' @param columns Optional character vector of column names to include.
#' @param bins Number of histogram bins. Default is 30.
#' @param title Optional title string.
#' @param columnLabels Optional vector of column labels for axis titles.
#' @param diag Character string specifying the plot type on the diagonal.
#'   Either "density" (default) or "histogram".
#'
#' @return An object of class `util_pairs_ggplot_panels`, which can be printed.
#'
#' @noRd
util_pairs_ggplot <- function(data, columns = NULL, bins = 30, title = NULL,
                              columnLabels = NULL, diag = c("density", "histogram"),
                              correlation_method = c("pearson", "spearman")) {

  util_expect_scalar(correlation_method,
                     check_type = is.character,
                     allow_more_than_one = TRUE)
  correlation_method <- util_match_arg(correlation_method)

  util_expect_scalar(diag,
                     check_type = is.character,
                     allow_more_than_one = TRUE)
  diag <- util_match_arg(diag)

  cols <- if (is.null(columns)) names(data) else intersect(columns, names(data))
  df <- data[, cols, drop = FALSE]
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  n <- ncol(df)
  if (n < 2) util_error("At least two numeric columns required.",
                        applicability_problem = TRUE)
  if (correlation_method == "spearman") {
    df <- as.data.frame(lapply(df, function(x) rank(x, na.last = "keep")))
  }

  palette <- c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#CC79A7", "#56B4E9", "#E69F00", "#999999")
  palette <- rep(palette, length.out = n)

  var_colors <- setNames(palette[seq_len(n)], names(df))

  make_panel <- function(i, j) {
    label_x <- if (!is.null(columnLabels)) columnLabels[j] else names(df)[j]
    label_y <- if (!is.null(columnLabels)) columnLabels[i] else names(df)[i]
    x <- names(df)[j]
    y <- names(df)[i]

    if (i == j) {
      p <- ggplot(df, aes(x = !!rlang::sym(x))) +
        xlab(label_x)
      if (diag == "histogram") {
        p <- p + geom_histogram(bins = bins, fill = var_colors[[x]], color = "white")
      } else {
        p <- p + geom_density(color = var_colors[[x]], fill = var_colors[[x]], alpha = 0.7)
      }
      p + theme_minimal() +
        theme(axis.title.y = element_blank(),
              axis.text.y  = element_blank(),
              axis.ticks.y = element_blank())
    } else if (i < j) {
      corr_val <- round(cor(df[[x]], df[[y]], use = "complete.obs"), 2)
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = corr_val, size = 6) +
        ggplot2::theme_void()
    } else {
      ggplot(df, aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        xlab(label_x) + ylab(label_y) +
        geom_point(alpha = 0.6, size = 1.5, color = var_colors[[x]]) +
        theme_minimal()
    }
  }

  panels <- vector("list", n * n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      panels[[(i - 1) * n + j]] <- make_panel(i, j)
    }
  }

  structure(list(panels = panels, layout_dim = n, title = title),
            class = "util_pairs_ggplot_panels")
}

#' Pairwise numeric variable visualization using plotly
#'
#' This function creates an interactive grid of plots showing pairwise
#' relationships between numeric columns in a dataset using `plotly`. The
#' diagonal contains histograms or density curves, the upper triangle shows
#' correlation coefficients, and the lower triangle shows scatterplots.
#'
#' @inheritParams util_pairs_ggplot
#'
#' @return A `plotly` object representing the interactive subplot matrix.
#'
#' @noRd
util_pairs_plotly <- function(data, columns = NULL, bins = 30, title = NULL,
                              columnLabels = NULL, diag = c("density", "histogram"),
                              correlation_method = c("pearson", "spearman")) {

  util_ensure_suggested("plotly", "plot interactive figures")

  util_expect_scalar(correlation_method,
                     check_type = is.character,
                     allow_more_than_one = TRUE)
  correlation_method <- util_match_arg(correlation_method)

  util_expect_scalar(diag,
                     check_type = is.character,
                     allow_more_than_one = TRUE)
  diag <- util_match_arg(diag)

  cols <- if (is.null(columns)) names(data) else intersect(columns, names(data))
  df <- data[, cols, drop = FALSE]
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  n <- ncol(df)
  if (n < 2) util_error("At least two numeric columns required.",
                        applicability_problem = TRUE)

  if (correlation_method == "spearman") {
    df <- as.data.frame(lapply(df, function(x) rank(x, na.last = "keep")))
  }

  palette <- c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#CC79A7", "#56B4E9", "#E69F00", "#999999")
  palette <- rep(palette, length.out = n)

  var_colors <- setNames(palette[seq_len(n)], names(df))

  panels <- vector("list", n * n)
  cnt <- 1
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      xname <- names(df)[j]
      yname <- names(df)[i]
      xlab <- if (!is.null(columnLabels)) columnLabels[j] else xname
      ylab <- if (!is.null(columnLabels)) columnLabels[i] else yname

      if (i == j) {
        if (diag == "histogram") {
          p <- plotly::plot_ly(x = df[[xname]], type = "histogram",
                               nbinsx = bins,
                               marker = list(color = var_colors[[xname]],
                                             line = list(color = "white")))
        } else {
          dens <- stats::density(df[[xname]], na.rm = TRUE)
          p <- plotly::plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines",
                               fillcolor = paste0(palette[j], "B3"), # B3 = 0.7 for alpha
                               fill = 'tozeroy', line = list(color = var_colors[[xname]]))
        }
        p <- plotly::layout(p,
                            xaxis = list(visible = TRUE,
                                         showgrid = TRUE,
                                         zeroline = TRUE,
                                         showticklabels = TRUE),
                            yaxis = list(visible = TRUE,
                                         showgrid = TRUE,
                                         zeroline = TRUE,
                                         showticklabels = TRUE))
      } else if (i < j) {
        corr_val <- round(cor(df[[xname]], df[[yname]], use = "complete.obs"), 2)
        axis_id <- paste0("corr_", i, "_", j)
        p <- plotly::plot_ly(showlegend = FALSE, type = "scatter", mode = "text",
                             xaxis = paste0("x", axis_id), yaxis = paste0("y", axis_id)) %>%
          plotly::add_annotations(
            text = corr_val,
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 20)
          ) %>%
          plotly::layout(xaxis = list(visible = FALSE),
                         yaxis = list(visible = FALSE),
                         margin = list(l = 20, b = 20))

        p <- p %>% plotly::layout(
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          margin = list(l = 20, b = 20)
        )

      } else {
        p <- plotly::plot_ly(x = df[[xname]], y = df[[yname]],
                             type = "scatter", mode = "markers",
                             marker = list(opacity = 0.6, size = 6, color = var_colors[[xname]]),
                             showlegend = FALSE)
        p <- plotly::layout(p, xaxis = list(visible = TRUE,
                                            showgrid = TRUE,
                                            zeroline = TRUE,
                                            showticklabels = TRUE),
                            yaxis = list(visible = TRUE,
                                         showgrid = TRUE,
                                         zeroline = TRUE,
                                         showticklabels = TRUE))
      }

      panels[[cnt]] <- p
      cnt <- cnt + 1
    }
  }

  fig <- plotly::subplot(panels,
                         nrows = n,
                         shareX = TRUE,
                         shareY = !TRUE)

  if (!is.null(title)) {
    fig <- fig %>% plotly::layout(title = list(text = title, x = 0.5))
  }

  fig <- fig %>% plotly::layout(showlegend = FALSE)

  if (is.null(columnLabels)) {
    columnLabels <- names(df)
  }

  if (!is.null(columnLabels)) {
    annotations <- list()
    for (i in seq_len(n)) {
      label <- columnLabels[i]
      annotations[[length(annotations) + 1]] <- list(
        text = label,
        x = i / n - 1 / (2 * n),
        y = 0,
        yshift = -50,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 12),
        xanchor = "center",
        yanchor = "bottom",
        cliponaxis = FALSE
      )
      annotations[[length(annotations) + 1]] <- list(
        text = label,
        x = 0,
        xshift = -50,
        y = 1 - (i / n - 1 / (2 * n)),
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 12),
        textangle = -90,
        xanchor = "left",
        yanchor = "middle",
        cliponaxis = FALSE
      )
    }

    fig <- fig %>% plotly::layout(annotations = annotations)
  }

  fig <- plotly::layout(fig,
                        margin = list(l = 60, b = 80, t = 80, r = 40)
  )

  fig
}

#' Print method for `util_pairs_ggplot_panels` objects
#'
#' @param x An object of class `util_pairs_ggplot_panels`.
#' @param ... Ignored.
#' @return The input object, invisibly.
#' @export
print.util_pairs_ggplot_panels <- function(x, ...) {
  n <- x$layout_dim
  panels <- x$panels
  title <- x$title

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, n)))
  vplayout <- function(row, col) grid::viewport(layout.pos.row = row, layout.pos.col = col)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      idx <- (i - 1) * n + j
      print(panels[[idx]], vp = vplayout(i, j))
    }
  }

  if (!is.null(title)) {
    grid::grid.text(title,
                    y = grid::unit(1, "npc") - grid::unit(2, "mm"),
                    gp = grid::gpar(fontsize = 15, fontface = "bold"))
  }
  invisible(x)
}

#' `grid.draw` method for `util_pairs_ggplot_panels` objects
#'
#' @param x An object of class `util_pairs_ggplot_panels`.
#' @param ... Ignored.
#' @exportS3Method grid::grid.draw
grid.draw.util_pairs_ggplot_panels <- function(x, ...) {
  print.util_pairs_ggplot_panels(x)
}
