% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_create_meta.R
\name{prep_create_meta}
\alias{prep_create_meta}
\title{Support function to create \link{data.frame}s of metadata}
\usage{
prep_create_meta(..., stringsAsFactors = FALSE, level, character.only = FALSE)
}
\arguments{
\item{...}{named column vectors, names will be mapped using
\link{WELL_KNOWN_META_VARIABLE_NAMES},
if included in \link{WELL_KNOWN_META_VARIABLE_NAMES}
can also be a data frame, then its column names will be mapped
using \link{WELL_KNOWN_META_VARIABLE_NAMES}}

\item{stringsAsFactors}{\link{logical} if the argument is a list of vectors, a
data frame will be
created. In this case, \code{stringsAsFactors} controls,
whether characters will be auto-converted to Factors,
which defaults here always to false independent from
the \link{default.stringsAsFactors}.}

\item{level}{\link{enum} level of requirement (see also \link{VARATT_REQUIRE_LEVELS})
set to \code{NULL}, if not a complete metadata frame is
created.}

\item{character.only}{\link{logical} a logical indicating whether level can be
assumed to be character strings.}
}
\value{
a data frame with:
\itemize{
\item metadata attribute names mapped and
\item metadata checked using \link{prep_check_meta_names} and do some more
verification about conventions, such as check for valid intervals
in limits)
}
}
\description{
Create a metadata data frame and map names.
Generally, this function only creates a \link{data.frame}, but using
this constructor instead of calling
\code{data.frame(..., stringsAsFactors = FALSE)}, it becomes possible, to adapt
the metadata \link{data.frame} in later developments, e.g. if we decide to use
classes for the metadata, or if certain standard names of variable attributes
change. Also, a validity check is possible to implement here.
}
\details{
For now, this calls \link{data.frame}, but it already renames variable attributes,
if they have a different name assigned in \link{WELL_KNOWN_META_VARIABLE_NAMES},
e.g. \code{WELL_KNOWN_META_VARIABLE_NAMES$RECODE} maps to \code{recode} in lower case.

NB: \code{dataquieR} exports all names from WELL_KNOWN_META_VARIABLE_NAME as
symbols, so \code{RECODE} also contains \code{"recode"}.
}
\seealso{
\link{WELL_KNOWN_META_VARIABLE_NAMES}
}
