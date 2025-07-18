% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{class_by_dich_fp}
\alias{class_by_dich_fp}
\title{class_by_dich_fp function}
\usage{
class_by_dich_fp(
  aggvars,
  byvar,
  data_to_use,
  cntfunc = sum,
  pctfunc = mean,
  catname = "Class",
  fieldnames = "none",
  cols = TRUE,
  pct_t = "pct",
  cnt_t = "none",
  transpose_result = TRUE,
  varlist_name = "Variable",
  total = TRUE,
  digs = 2
)
}
\arguments{
\item{aggvars}{a vector of variable names for those variables that you want aggregated. these must all be 0/1 binary variables}

\item{byvar}{the class variable you want to use to split the data. the unique values of this variable will be the dataframe columns}

\item{data_to_use}{the data frame than contains the aggvars and byvar}

\item{cntfunc}{the function to use for the calculation outside of the parentheses. default is sum}

\item{pctfunc}{the function to use inside the parentheses. default is mean}

\item{catname}{the name given to the first column which contains the names of the variables supplied. default is 'Class'}

\item{fieldnames}{vector of names to replace variable names supplied above in aggvars. if agvars = c("var1","var2","var3") and fiednames = c("do","ray","me") then the first variable in the returned dataset will be "do","ray","me"}

\item{cols}{boolean make the rows the same name as aggvars. default is TRUE}

\item{pct_t}{switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'}

\item{cnt_t}{switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'}

\item{transpose_result}{transposes the result so that rows are variables and columns are class variabe values. default is TRUE}

\item{varlist_name}{the name given to the first column of the data frame which contains the list of variables in the aggvars binary suite. default is 'variable'}

\item{total}{boolean indicating if a total column should be produced. default is TRUE}

\item{digs}{the number of digits to which the results will be rounded}
}
\description{
given a suite of binary variables and a class variable creates a data frame of frequencies and percents formatted as n(pct) with rows as variables and columns as class variable elements using the function supplied
}
\examples{
class_by_dich_fp_function()
}
\keyword{binary}
\keyword{class_by_dich}
\keyword{class_by_dich_fp}
\keyword{dich}
\keyword{dichotomous}
\keyword{fp}
