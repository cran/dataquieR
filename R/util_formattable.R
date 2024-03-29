#' Plots simple HTML tables with background color scale
#'
#' @param tb [data.frame] the table as [data.frame] with mostly numbers
#' @param min_val [numeric] minimum value for the numbers in `tb`
#' @param max_val [numeric] maximum value for the numbers in `tb`
#' @param min_color [numeric] vector with the RGB color values for the
#'                            minimum color, values between 0 and 255
#' @param max_color [numeric] vector with the RGB color values for the
#'                            maximum color, values between 0 and 255
#' @param soften [function] to be applied to the relative values between 0
#'                          and 1 before mapping them to a color
#' @param style_header [character] to be applied to style the HTML header of
#'                                 the table
#' @param text_color_mode [enum] bw | gs. Should the text be displayed in black
#'                               and white or using a grey scale? In both cases,
#'                               the color will be adapted to the background.
#'
#' @seealso [util_html_table()]
#'
#' @return `htmltools` compatible object
#' @examples
#' \dontrun{
#'
#' tb <- as.data.frame(matrix(ncol = 5, nrow = 5))
#' tb[] <- sample(1:100, prod(dim(tb)), replace = TRUE)
#' tb[, 1] <- paste("case", 1:nrow(tb))
#' htmltools::browsable(util_formattable(tb))
#' htmltools::browsable(util_formattable(tb[, -1]))
#' }
#'
#' @keywords internal
util_formattable <- function(tb,
                             min_val = min(tb, na.rm = TRUE),
                             max_val = max(tb, na.rm = TRUE),
                             min_color = c(0, 0, 255),
                             max_color = c(255, 0, 0),
                             soften =
                               function(x) stats::plogis(x,
                                                  location = 0.5,
                                                  scale = 0.1),
                             style_header = "font-weight: bold;",
                             text_color_mode = c("bw", "gs")) {
  util_ensure_suggested("htmltools", "create colored tables")
  text_color_mode <- match.arg(text_color_mode)
  tb_val <- as.matrix(as.data.frame(suppressWarnings(lapply(tb, as.numeric))))
  if (missing(min_val)) {
    min_val <- min(tb_val, na.rm = TRUE)
  }
  if (missing(max_val)) {
    max_val <- max(tb_val, na.rm = TRUE)
  }
  rel_points <- ((tb_val - min_val) / abs(max_val - min_val))
  colors <- apply(soften(rel_points),
                  1:2,
                  function(x)
                    setNames((min_color + x * (max_color - min_color)) / 255,
                             c("red", "green", "blue")),
                  simplify = FALSE)
  htmltools::withTags(table(
    if (length(style_header) == ncol(tb)) {
      # style defined for each entry
      tr(lapply(seq_along(colnames(tb)), function(cc) {
        th(style = style_header[cc], colnames(tb)[cc])
      }))
    } else {
      if (length(style_header) > 1) {
        # style not well defined
        style_header <- style_header[1]
      }
      tr(lapply(colnames(tb), function(header) {
        th(style = style_header, header)
      }))
    },
    lapply(1:nrow(tb),
           function(rw)
             tr(
               lapply(1:ncol(tb),
                      function(cl) {
                        color <- try(do.call(rgb, as.list(colors[rw, cl][[1]])),
                                     silent = TRUE)
                        if (inherits(color, "try-error")) {
                          color <- "white"#"#bbbbbb"
                        }
                        if (text_color_mode == "gs") {
                          # invert
                          txtcolor <- 1 - colors[rw, cl][[1]]
                          if (suppressWarnings(max(abs(0.5 - txtcolor),
                                                   na.rm = TRUE) < 0.1)) {
                            txtcolor <- c(0, 0, 0)
                          }
                          #greyscale, https://www.baeldung.com/cs/convert-rgb-to-grayscale#3-luminosity-method
                          gs <-
                            0.3 * txtcolor[[1]] +
                            0.59 * txtcolor[[2]] +
                            0.11 * txtcolor[[3]]
                          txtcolor <- rep(gs, 3)
                        } else {
                          # text in black or white, depending on the brightness of the background, https://stackoverflow.com/questions/11867545/change-text-color-based-on-brightness-of-the-covered-background-area, https://www.w3.org/TR/AERT/#color-contrast
                          if (any(is.na(colors[rw, cl][[1]]))) {
                            txtcolor <- c(0, 0, 0)
                          } else {
                            brightness <- (299 * 255 * colors[rw, cl][[1]][1] +
                                             587 * 255 * colors[rw, cl][[1]][2] + 114 * 255 * colors[rw, cl][[1]][3]) / 1000
                            if (brightness > 125) {
                              txtcolor <- c(0, 0, 0)
                            } else {
                              txtcolor <- c(1, 1, 1)
                            }
                          }
                        }
                        txtcolor <- try(do.call(rgb, as.list(txtcolor)),
                          silent = TRUE)
                        if (inherits(txtcolor, "try-error")) {
                          txtcolor <- "#222222"
                        }
                        td(
                          style =
                            sprintf("background-color: %s; color: %s; text-align: right;",
                                    color, txtcolor),
                          tb[rw, cl]
                        )
                      })
             )
  )))
}
