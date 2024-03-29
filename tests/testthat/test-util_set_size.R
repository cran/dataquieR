test_that("util_set_size works", {
  skip_on_cran()

  expect_error(util_set_size(plot(mtcars$wt, mtcars$mpg),
                             width_em =  6, height_em =  4))

  # Example ggplot object
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  # Set width and height using util_set_size
  p_resized <- util_set_size(p, width_em =  6, height_em =  4)

  # Check that the attributes have been set correctly
  expect_equal(attr(p_resized, "width_em"),  6)
  expect_equal(attr(p_resized, "height_em"),  4)
})
