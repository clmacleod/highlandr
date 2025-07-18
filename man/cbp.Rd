% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{cbp}
\alias{cbp}
\title{cbp function}
\usage{
cbp(x, txt = 0.5, ttext = FALSE, code = FALSE)
}
\arguments{
\item{x}{the dataset you wish to summarize}

\item{txt}{the text scale you wish to use}

\item{ttext}{boolean argument determining label orientation and overlap. this argument forces the labels to appear if TRUE even if they overlap. defaults to FALSE}

\item{code}{boolean argument determining if a numeric code of 1:nrow should be used as the names in the plot (TRUE) or if the values in the first column should be used as the name argument (FALSE). defaults to FALSE}
}
\description{
Colin boxplot using regular data (i.e. cases as rows)
}
\examples{
cbp_function()
}
\keyword{boxplot}
\keyword{cbp}
