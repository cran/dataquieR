test_that("util_plot_figure_no_plotly works", {
  skip_on_cran()
  # Create a test plot
  test_plot <- plot(cars)
  result <- util_plot_figure_no_plotly(test_plot)

  # Check that the result is an HTML plot tag
  expect_is(result, "shiny.tag")

})
