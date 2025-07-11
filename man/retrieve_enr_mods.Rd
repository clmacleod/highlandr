% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enr functions.R
\name{retrieve_enr_mods}
\alias{retrieve_enr_mods}
\title{retrieve_enr_mods function}
\usage{
retrieve_enr_mods(
  directory,
  list_of_files = NULL,
  all_files = FALSE,
  patterns = ""
)
}
\arguments{
\item{directory}{directory to look for the models}

\item{list_of_files}{output from 'run_all_enr_models' containing the file names}

\item{all_files}{boolean indicating if all fiels should be read into memory}

\item{patterns}{a regex style pattern to identify which files to load into memory, all files should be false if supplying an argument here}
}
\description{
retrieves ENR models from a directiory using the output from run_all_enr_fit_mets function (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
}
\examples{
retrieve_enr_mods()
}
\keyword{enr}
\keyword{retrieve_enr_mods}
