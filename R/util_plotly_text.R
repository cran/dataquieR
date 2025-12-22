util_plotly_text <- function(text) {
  util_ensure_suggested("plotly")
  util_expect_scalar(text, check_type = is.character)
  text <- paste(text, collapse = "\n");
  if (suppressWarnings(util_ensure_suggested("cli", err = FALSE)))
    text <- cli::ansi_strip(text)

  plotly::plot_ly(
    type = "scatter",
    mode = "text",
    x = 0.5,
    y = 0.5,
    text = text,
    textposition = "middle center"
  ) %>%
    plotly::layout(
      xaxis = list(visible = FALSE, range = c(0, 1)),
      yaxis = list(visible = FALSE, range = c(0, 1)),
      showlegend = FALSE
    )
}
