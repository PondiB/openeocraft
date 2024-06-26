% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wellknown.R
\name{wellknown_functions}
\alias{wellknown_functions}
\alias{add_wellknown_version}
\alias{update_wellknown_version}
\title{\code{Wellknown} helper functions}
\usage{
add_wellknown_version(doc, api_version, url, production = FALSE, ...)

update_wellknown_version(doc, api_version, url, production = FALSE, ...)
}
\arguments{
\item{doc}{The document to which the version entry will be added/modified.}

\item{api_version}{Version number of the openEO specification the
back-end's entry implements.}

\item{url}{Absolute URLs to the service.}

\item{production}{Specifies whether the implementation is ready to be used
in production (\code{TRUE}) or not (\code{FALSE}, default).}

\item{...}{Additional attributes to include in the version entry.}
}
\value{
For API creation functions, returns a api object.
For API handling functions, returns the document to return
as response.
}
\description{
These functions modify and creates the API wellknown versions document.
}
\details{
\itemize{

\item \code{add_wellknown_version}: This function adds a new version entry
to the document by appending it to the existing list.

\item \code{update_wellknown_version}: This function updates an existing
entry in the \code{wellknown} versions document by replacing it with a
new entry. The existing entry is identified by its \code{api_version}.
If multiple entries with the same version exist, all of them are
replaced.

}
}
