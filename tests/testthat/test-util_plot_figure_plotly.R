test_that("util_plot_figure_plotly works", {
  skip_on_cran()
  # Create test ggplot
  p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))

  result <- util_plot_figure_plotly(p1)

  # Check that the result is a plotly object
  expect_is(result, "plotly")

  # Create a test patchwork
  p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
  p3 <- p1 + p2

  result_2 <- util_plot_figure_plotly(p3)

  # Check that the result is an HTML plot tag
  expect_is(result_2, "shiny.tag")
})
