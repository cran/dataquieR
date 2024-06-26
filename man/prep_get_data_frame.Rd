% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_get_data_frame.R
\name{prep_get_data_frame}
\alias{prep_get_data_frame}
\title{Read data from files/URLs}
\usage{
prep_get_data_frame(
  data_frame_name,
  .data_frame_list = .dataframe_environment,
  keep_types = FALSE
)
}
\arguments{
\item{data_frame_name}{\link{character} name of the data frame to read, see
details}

\item{.data_frame_list}{\link{environment} cache for loaded data frames}

\item{keep_types}{\link{logical} keep types as possibly defined in a file, if the
data frame is loaded from one. set \code{TRUE} for
study data.}
}
\value{
\link{data.frame} a data frame
}
\description{
data_frame_name can be a file path or an URL you can append a pipe and a
sheet name for Excel files or object name e.g. for \code{RData} files. Numbers
may also work. All file formats supported by your \code{rio} installation will
work.
}
\details{
The data frames will be cached automatically, you can define an alternative
environment for this using the argument \code{.data_frame_list}, and you can purge
the cache using \link{prep_purge_data_frame_cache}.

Use \link{prep_add_data_frames} to manually add data frames to the
cache, e.g., if you have loaded them from more complex sources, before.
}
\examples{
\dontrun{
bl <- as.factor(prep_get_data_frame(
  paste0("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus",
    "/Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
    "publicationFile|COVID_Todesfälle_BL|Bundesland"))[[1]])

n <- as.numeric(prep_get_data_frame(paste0(
  "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/",
  "Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
  "publicationFile|COVID_Todesfälle_BL|Anzahl verstorbene",
  " COVID-19 Fälle"))[[1]])
plot(bl, n)
# Working names would be to date (2022-10-21), e.g.:
#
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \\
#    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/  \\
#    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|2
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \\
#    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|name
# study_data
# ship
# meta_data
# ship_meta
#
prep_get_data_frame("meta_data | meta_data")
prep_get_data_frame(file.path(system.file(package = "dataquieR"),
  "extdata", "meta_data.RData"))
prep_get_data_frame(file.path(system.file(package = "dataquieR"),
  "extdata", "meta_data.RData|meta_data"))
}
}
\seealso{
\link{prep_add_data_frames}

\link{prep_load_workbook_like_file}

Other data-frame-cache: 
\code{\link{prep_add_data_frames}()},
\code{\link{prep_list_dataframes}()},
\code{\link{prep_load_folder_with_metadata}()},
\code{\link{prep_load_workbook_like_file}()},
\code{\link{prep_purge_data_frame_cache}()}
}
\concept{data-frame-cache}
