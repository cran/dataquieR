#' Integer breaks for `ggplot2`
#'
#' creates integer-only breaks
#'
#' @param x the values
#' @param n integer giving the desired number of intervals. Non-integer values are rounded down.
#'
#' @return breaks suitable for `scale_*_continuous`' `breaks` argument
#'
#' @author [Sarah](https://stackoverflow.com/users/2738526/sarah)
#'
#' @seealso [StackOverflow](https://stackoverflow.com/a/57086284)
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' big_numbers1 <- data.frame(x = 1:5, y = c(0:1, 0, 1, 0))
#' big_numbers2 <- data.frame(x = 1:5, y = c(0:1, 0, 1, 0) + 1000000)
#'
#' big_numbers_plot1 <- ggplot(big_numbers1, aes(x = x, y = y)) +
#'   geom_point()
#'
#' big_numbers_plot2 <- ggplot(big_numbers2, aes(x = x, y = y)) +
#'   geom_point()
#'
#' big_numbers_plot1 + scale_y_continuous()
#' big_numbers_plot1 + scale_y_continuous(breaks = util_int_breaks_rounded)
#'
#' big_numbers_plot2 + scale_y_continuous()
#' big_numbers_plot2 + scale_y_continuous(breaks = util_int_breaks_rounded)
#' }
#'
util_int_breaks_rounded <- function(x, n = 5) {
  unique(pretty(x, n)[round(pretty(x, n),1) %% 1 == 0])
}
