\name{plot2x2}
\alias{plot2x2}
\title{Plot bioequivalence variable of a 2x2 study}
\description{
  It plots two 2x2 plots for a variable.
}
\usage{
plot2x2(bedata, Var)
}
\arguments{
  \item{bedata}{Data table name. This should have at least the following columns and a variable column to be plotted.
\preformatted{  
 GRP : Group or Sequence, 'RT' or 'TR'
 PRD : Period, 1 or 2
 SUBJ : Subject ID
 TRT : Treatment or Drug, 'R' or 'T'
}
  }
  \item{Var}{Variable to be plotted. This should be one of the column names in \code{bedata} table.}
}
\details{
  It plots \code{Var} column values according to GRP, PRD, TRT.
}
\value{
  It just draws two 2x2 plots for equivalence exploration.
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
plot2x2(NCAResult4BE, "AUClast")
plot2x2(NCAResult4BE, "Cmax")
plot2x2(NCAResult4BE, "Tmax")
}
\keyword{Plot}
