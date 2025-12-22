util_ggplot_text <- function(text,
                        fontsize = 12,
                        fontfamily = "",
                        hjust = 0.5,
                        vjust = 0.5) {
  text <- paste(text, collapse = "\n");
  if (suppressWarnings(util_ensure_suggested("cli", err = FALSE)))
    text <- cli::ansi_strip(text)

  ggplot() +
    ggplot2::theme_void() +
    annotate("text",
             x = 0.5, y = 0.5,
             label = text,
             size = fontsize / ggplot2::.pt,  # ggplot2 fontsize uses mm, convert from pt
             family = fontfamily,
             hjust = hjust,
             vjust = vjust
    ) +
    xlim(0, 1) + ggplot2::ylim(0, 1)
}
