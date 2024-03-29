% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_coord_flip.R
\name{util_coord_flip}
\alias{util_coord_flip}
\title{return a flip term for \code{ggplot2} plots, if desired.}
\usage{
util_coord_flip(w, h, p, ref_env, ...)
}
\arguments{
\item{w}{width of the plot to determine its aspect ratio}

\item{h}{height of the plot to determine its aspect ratio}

\item{p}{the \code{ggplot2} object, so far. If \code{w} or \code{h} are missing, \code{p} is used
for an estimate on \code{w} and \code{h}, if both axes are discrete.}

\item{ref_env}{environment of the actual entry function, so that the correct
formals can be detected.}

\item{...}{additional arguments for \code{coord_flip} or \code{coord_cartesian}}
}
\value{
\code{coord_flip} or \code{coord_cartesian}
}
\description{
return a flip term for \code{ggplot2} plots, if desired.
}
