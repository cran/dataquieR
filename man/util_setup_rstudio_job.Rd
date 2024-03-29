% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_setup_rstudio_job.R
\name{util_setup_rstudio_job}
\alias{util_setup_rstudio_job}
\title{Set up an RStudio job}
\usage{
util_setup_rstudio_job(job_name = "Job")
}
\arguments{
\item{job_name}{a name for the job}
}
\value{
list: the \code{progress} function and the \code{progress_msg} function
}
\description{
Also defines a progress function and a \code{progress_msg} function in
the caller's environment.
}
\details{
In \code{RStudio} its job system will be used, for \code{shiny::withProgress}
based calls, this will require min and max being set to 0 and 1 (defaults).
If \code{cli} is available, it will be used, in all other cases, just \code{message}s
will be created.
}
\examples{
\dontrun{
  test <- function() {
    util_setup_rstudio_job("xx")
    Sys.sleep(5)
    progress(50)
    progress_msg("halfway through")
    Sys.sleep(5)
    progress(100)
    Sys.sleep(1)
  }
  test()
}
}
