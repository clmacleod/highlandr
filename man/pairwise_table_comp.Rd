% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{pairwise_table_comp}
\alias{pairwise_table_comp}
\title{pairwise_table_comp function}
\usage{
pairwise_table_comp(x = x, v = v, diagonal = TRUE)
}
\arguments{
\item{x}{the data frame that the variables are to be pulled from}

\item{v}{vector of variable names. all elements in the vector will have a pairwise comparison to all others}

\item{diagonal}{boolean argument that returns the full symmetric matrix if FALSE and only the lower triangle if TRUE. default is TRUE}
}
\description{
creates a list of information for pairwise comparisons of categorical or binomial variables. returns the individual tables, the individual the chi square models, a matrix of the chi square values, a matrix of the chi square p values, a matrix of the effect sizes, and a matrix of the starred pvalues
}
\examples{
pairwise_table_comp_function()
}
\keyword{chisquare}
\keyword{pairwise}
\keyword{pairwise_table_comp}
\keyword{x2}
