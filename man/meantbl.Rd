% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{meantbl}
\alias{meantbl}
\title{meantbl function}
\usage{
meantbl(
  x,
  digs = 2,
  ftouse = mean,
  rwnames = "norowname",
  rwn_asvar = FALSE,
  rwn_asvar_lab = "variable",
  perc = FALSE,
  cnt = "none"
)
}
\arguments{
\item{x}{dataset containing only binary variables}

\item{digs}{digits to round the result to. default is 2}

\item{ftouse}{function to use for calculations. default is mean}

\item{rwnames}{vector of row names to use. default is none}

\item{rwn_asvar}{boolean indicating whether to make the row names the first variable in the returned dataset. default is FALSE}

\item{rwn_asvar_lab}{the variable name for the row name variable. to see this rwn_asvar should be TRUE. default is 'variable'}

\item{perc}{boolean indicating if the results should be formatted as a percent}

\item{cnt}{should an extra column be added that provides the counts as well as the percentages}
}
\description{
creates mean tables from binary suites
}
\examples{
meantbl_function()
}
\keyword{binary}
\keyword{dich}
\keyword{dichotomous}
\keyword{meantbl}
