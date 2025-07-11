% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{clustplot_old}
\alias{clustplot_old}
\title{clustplot_old function}
\usage{
clustplot_old(dframe, clusters, varlist)
}
\arguments{
\item{dframe}{the data frame you wish to create the plot for. the dataframe must contain 3 elements. 1.) first column must contain the unique identifiers. 2.) there must be one or more of the raw data variables. 3.) there must be at least one cluster variable of the format 'hkclust' followed by a number (e.g. 'hkclust5' would be a 5 cluster solution).}

\item{clusters}{number of clusters you want to create the plot for. there must be a variable corresponding to this number with the format hkclust (e.g. if you put '5' here there must be a variable named 'hkclust5'- element 3 above)}

\item{varlist}{the list of variable used for the clustering (this is the raw data-element 2 above)}
}
\description{
multicluster barplot for summarizing cluster solutions (deprecated I believe)
}
\examples{
clustplot_old_function()
}
\keyword{barchart}
\keyword{bargraph}
\keyword{cluster}
\keyword{clustplot}
\keyword{plot}
