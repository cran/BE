\name{hodges}
\alias{hodges}
\title{Hodges-Lehmann estimation for a variable of a 2x2 study}
\description{
  It performs Hodges-Lehmann estimation for 2x2 study. This is usually for Tmax variable.
}
\usage{
hodges(bedata, Var)
}
\arguments{
  \item{bedata}{Data table name. This should have at least the following columns and a variable column to be tested.
\preformatted{  
 GRP : Group or Sequence, 'RT' or 'TR'
 PRD : Period, 1 or 2
 SUBJ : Subject ID
 TRT : Treatment or Drug, 'R' or 'T'
}
  }
  \item{Var}{Variable to be estimated. This should be one of the column names in \code{bedata} table. Usually 'Tmax'}
}
\details{
  It nonparametrically tests \code{Var} variable equivalency from a 2x2 study. This is done for a variable which we cannot assume log-normal distribution. 
}
\value{
  \item{Wilcoxon Signed-Rank Test}{A kind of nonparametric test}
  \item{Hodges-Lehmann Estimate}{90\% confidence interval in the original scale and the percent scale}
}
\author{Kyun-Seop Bae k@acr.kr}

\examples{
hodges(NCAResult4BE, "Tmax") 
}

\keyword{Statistical Test}
