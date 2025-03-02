% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_formattable.R
\name{util_formattable}
\alias{util_formattable}
\title{Plots simple HTML tables with background color scale}
\usage{
util_formattable(
  tb,
  min_val = min(tb, na.rm = TRUE),
  max_val = max(tb, na.rm = TRUE),
  min_color = c(0, 0, 255),
  max_color = c(255, 0, 0),
  soften = function(x) stats::plogis(x, location = 0.5, scale = 0.1),
  style_header = "font-weight: bold;",
  text_color_mode = c("bw", "gs"),
  hover_texts = NULL,
  escape_all_content = TRUE
)
}
\arguments{
\item{tb}{\link{data.frame} the table as \link{data.frame} with mostly numbers}

\item{min_val}{\link{numeric} minimum value for the numbers in \code{tb}}

\item{max_val}{\link{numeric} maximum value for the numbers in \code{tb}}

\item{min_color}{\link{numeric} vector with the RGB color values for the
minimum color, values between 0 and 255}

\item{max_color}{\link{numeric} vector with the RGB color values for the
maximum color, values between 0 and 255}

\item{soften}{\link{function} to be applied to the relative values between 0
and 1 before mapping them to a color}

\item{style_header}{\link{character} to be applied to style the HTML header of
the table}

\item{text_color_mode}{\link{enum} bw | gs. Should the text be displayed in black
and white or using a grey scale? In both cases,
the color will be adapted to the background.}

\item{hover_texts}{\link{data.frame} if not \code{NULL}, this data frame contains html
code displayed when the user's mouse pointer
moves inside corresponding cells from
\code{tb}. Can contain \code{HTML} code.}

\item{escape_all_content}{\link{logical} if \code{TRUE}, treat \code{tb} and \code{hover_texts}
using some \code{HTML} escaping function}
}
\value{
\code{htmltools} compatible object
}
\description{
Plots simple HTML tables with background color scale
}
\examples{
\dontrun{

tb <- as.data.frame(matrix(ncol = 5, nrow = 5))
tb[] <- sample(1:100, prod(dim(tb)), replace = TRUE)
tb[, 1] <- paste("case", 1:nrow(tb))
htmltools::browsable(util_formattable(tb))
htmltools::browsable(util_formattable(tb[, -1]))
}

}
\seealso{
\code{\link[=util_html_table]{util_html_table()}}
}
\keyword{internal}
