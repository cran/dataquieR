% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_sort_by_order.R
\name{util_order_by_order}
\alias{util_order_by_order}
\title{Get the order of a vector with general order given in some other vector}
\usage{
util_order_by_order(x, order, ...)
}
\arguments{
\item{x}{the vector}

\item{order}{the "order vector}

\item{...}{additional arguments passed to \code{order}}
}
\description{
Get the order of a vector with general order given in some other vector
}
\examples{
\dontrun{
util_order_by_order(c("a", "b", "a", "c", "d"), letters)
}
}
