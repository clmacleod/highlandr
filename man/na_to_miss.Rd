% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{na_to_miss}
\alias{na_to_miss}
\title{na to miss format function}
\usage{
na_to_miss(x, fill = "Missing", na_null = "na")
}
\arguments{
\item{x}{vector containing the na values}

\item{fill}{character or numeric value to replace na with. default is 'Missing'.}

\item{na_null}{string determining what type of value to replace (i.e. na, null, nan). Options are "na", "null", and "nan", any other value will replace all of these. default is "na".}
}
\description{
converts na values in vector to "Missing" or any other value. if the variable is a factor then the value is added as a factor level. if 'missing' already exists as a factor level then it simply adds more 'missing'.
}
\examples{
na_to_miss_function()
}
\keyword{miss}
\keyword{na}
\keyword{na_to_miss}
