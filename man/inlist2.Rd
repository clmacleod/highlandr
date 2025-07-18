% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{inlist2}
\alias{inlist2}
\title{inlist 2 function}
\usage{
inlist2(
  srch_var,
  match_var,
  delimiter = ", ",
  extra_vecs_aslist = list(rep(0, length(srch_var)))
)
}
\arguments{
\item{srch_var}{a vector of characters each a delimited list of numeric items, see inlist. This is common output to mutliple choice questions.}

\item{match_var}{an atomic or vector or values to match against each element. for example if you wanted to know who selected options 1, 3, and 5 then this argument would be c(1, 3, 5)}

\item{delimiter}{the delimiter used for the data. default is set to ", "}

\item{extra_vecs_aslist}{a list of extra vectors that will be incorporated into the data. for example, if searching c("1, 2","2, 3, 4","0, 1","1, 9") for '2' it would restult in 1, 1, 0, 0 however, if we wanted to inforporate some other variable in an 'or' format like age over 45 saved as a binary variable then incorporating an extra vector that will turn the result to 1, if not already, for the case. each vector in the list must be the same length as the search variable.}
}
\description{
Works the same as inlist but allows for the incorporation of additional binary vectors which can change the 0/1 designation. e.g. inlist2(c("1, 2","2, 3, 4","0, 1","1, 9"),c(2),extra_vecs_aslist=list(c(1,0,0,1),c(0,0,1,0))) would return 1, 1, 1, 1 because the main search would find '2' in the 1st and 2nd elements, the first additional vector indicates that the 1st and 4th, and the second additional vector indicates the third.
}
\examples{
inlist2_function()
}
\keyword{binary}
\keyword{choice}
\keyword{dichotomous}
\keyword{extra}
\keyword{in}
\keyword{inlist}
\keyword{inlist2}
\keyword{list}
\keyword{multiple}
\keyword{vectors}
