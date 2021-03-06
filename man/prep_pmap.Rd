% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_pmap.R
\name{prep_pmap}
\alias{prep_pmap}
\title{Support function for a parallel \code{pmap}}
\usage{
prep_pmap(.l, .f, ..., cores = 0)
}
\arguments{
\item{.l}{\link{data.frame} with one call per line and one function argument
per column}

\item{.f}{\code{\link{function}} to call with the arguments from \code{.l}}

\item{...}{additional, static arguments for calling \code{.f}}

\item{cores}{number of cpu cores to use or a (named) list with arguments for
\link[parallelMap:parallelStart]{parallelMap::parallelStart} or NULL, if parallel has already
been started by the caller. Set to 0 to run without
parallelization.}
}
\value{
\link{list} of results of the function calls
}
\description{
parallel version of \code{purrr::pmap}
}
\seealso{
\code{purrr::pmap}

\href{https://stackoverflow.com/a/47575143}{Stack Overflow post}
}
\author{
\href{https://stackoverflow.com/users/6197649}{Aurèle}

S Struckmann
}
