#' Utility function to create bar plots
#'
#' A helper function for simple bar plots. The layout is intended for data with
#' positive numbers only (e.g., counts/frequencies).
#'
#' @param plot_data the data for the plot. It should consist of one column
#'                  specifying the categories, and a second column giving the
#'                  respective numbers / counts per category. It may contain
#'                  another column to specify the coloring of the bars
#'                  (`fill_var`).
#' @param cat_var column name of the categorical variable in `plot_data`
#' @param num_var column name of the numerical variable in `plot_data`
#' @param relative if `TRUE`, numbers will be interpreted as percentages
#'                 (values in `num_var` should lie within `[0,1]`)
#' @param show_numbers if `TRUE`, numbers will be displayed on top of the bars
#' @param fill_var column name of the variable in `plot_data` which will be used
#'                 to color the bars individually
#' @param colors vector of colors, or a single color
#' @param show_color_legend if `TRUE`, a legend for the colors will be displayed
#' @param flip if `TRUE`, bars will be oriented horizontally
#'
#' @return a bar plot
#'
util_bar_plot <- function(plot_data, cat_var, num_var, relative = FALSE,
                          show_numbers = TRUE,
                          fill_var = NULL, colors = "#2166AC",
                          show_color_legend = FALSE,
                          flip = FALSE) {

  # base plot
  bar_plot <-
    ggplot(plot_data, aes(x = .data[[cat_var]], y = .data[[num_var]])) +
    theme_minimal() +
    xlab("") +
    ylab("")
  # color setup, adding the bars
  if (!is.null(fill_var)) {
    if (is.numeric(plot_data[[fill_var]])) {
      if (relative) {
        scale_fill <- scale_fill_gradientn(colors = colors,
                                           labels = scales::percent,
                                           name = "")
      } else {
        scale_fill <- scale_fill_gradientn(colors = colors,
                                           name = "")
      }
    } else {
      scale_fill <- scale_fill_manual(values = colors, name = "")
    }
    bar_plot <- bar_plot +
      geom_bar(aes(fill = .data[[fill_var]]), stat = "identity", width = 0.8) +
      scale_fill
  } else {
    bar_plot <- bar_plot +
      geom_bar(fill = colors[1], stat = "identity", width = 0.8)
  }
  # y-axis setup, show numbers as percentages if needed
  if (relative) {
    scale_y <- scale_y_continuous(labels = scales::percent,
                                  expand = expansion(mult = c(0, 0.05)))
    num_labels <- paste0(round(plot_data[[num_var]] * 100, digits = 2), "%")
  } else {
    scale_y <- scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    num_labels <- as.character(plot_data[[num_var]])
  }
  bar_plot <- bar_plot +
    scale_y
  # some layout options must be adapted to the orientation of the plot:
  # - larger font size for category labels
  # - no grid lines parallel to the bars
  # - position of numbers above/below or next to the bars
  if (!flip) {
    scale_x <- scale_x_discrete(drop = FALSE,
                                expand = expansion(add = 0.5, mult = 0.1))
    ly <- theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
    if (show_color_legend) {
      gui <- guides(fill = guide_legend(position = "right"))
    } else {
      gui <- guides(fill = "none")
    }
    # set up label positions
    vert_just <- ifelse(plot_data[[num_var]] / max(plot_data[[num_var]]) < 0.4, # include potential threshold line here in max
                        -0.5, # above the bar
                        1.5) # within the bar
    horiz_just <- 0.5
    pos_labels <- ifelse(vert_just <= 0, "outside", "within")
  } else {
    bar_plot <- bar_plot + coord_flip()
    scale_x <- scale_x_discrete(limits = rev,
                                drop = FALSE,
                                expand = expansion(add = 0.5, mult = 0.1))
    ly <- theme(axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    if (show_color_legend) {
      gui <- guides(fill = guide_legend(position = "bottom", byrow = TRUE))
    } else {
      gui <- guides(fill = "none")
    }
    # set up label positions
    vert_just <- 0.5
    horiz_just <- ifelse(plot_data[[num_var]] / max(plot_data[[num_var]]) < 0.4, # include potential threshold line here in max
                         0, # on the right side next to the bar
                         1) # within the bar
    pos_labels <- ifelse(horiz_just == 0, "outside", "within")
    num_labels <- paste0(" ", num_labels, " ")
  }
  bar_plot <- bar_plot +
    scale_x +
    ly +
    gui

  # color setup for labels on/next to the bars
  if (show_numbers) {
    ld <- ggplot2::layer_data(bar_plot)
    col_bars <- ld$fill # hex code
    brightness <- apply(col2rgb(col_bars), 2, function(rgb_col) {
      (299 * rgb_col[1] +
         587 * rgb_col[2] +
         114 * rgb_col[3]) / 1000
    })
    # show label text in black or white, depending on the brightness of the background, https://stackoverflow.com/questions/11867545/change-text-color-based-on-brightness-of-the-covered-background-area, https://www.w3.org/TR/AERT/#color-contrast
    col_labels <- ifelse(pos_labels == "within" & brightness < 130,
                         "white", "black") # this is referred to by util_as_plotly_util_plot_categorical_vars
    bar_plot <- bar_plot +
      geom_text(label = num_labels,
                vjust = vert_just,
                hjust = horiz_just,
                color = col_labels,
                size = 3.5)
  }
  return(bar_plot)
}
