\name{test2x2}
\alias{test2x2}
\title{Bioequivalence test for a variable of a 2x2 study}
\description{
  It performs conventional bioequivalence test for 2x2 study. Basic assumption is that the variable is distributed as a log-normal distribution. This is SAS PROC GLM style. If you want PROC MIXED style use \code{nlme::lme}.
}
\usage{
test2x2(bedata, Var)
}
\arguments{
  \item{bedata}{Data table name. This should have at least the following columns and a variable column to be tested. \code{Var} column values should be all positive values.
\preformatted{  
 GRP : Group or Sequence, 'RT' or 'TR'
 PRD : Period, 1 or 2
 SUBJ : Subject ID
 TRT : Treatment or Drug, 'R' or 'T'
}
  }
  \item{Var}{Variable to be tested. This should be one of the column names in \code{bedata} table. Usually 'AUClast' or 'Cmax'}
}
\details{
  It tests \code{Var} variable equivalency from a 2x2 study. Current regulatory requirement is that the 90\% confidence interval of geometric mean ratio (Test/Reference) should be within [0.8, 1.25]. 
}
\value{
  \item{Analysis of Variance (log scale)}{Analysis of Variance in log scale}
  \item{Between and Within Subject Variability}{Variance in log scale and coefficient of variance in original scale}
  \item{Least Square Means}{Geometric means}
  \item{90\% Confidence Interval}{90\% confidence interval of geometric mean ratio (T/R)}
  \item{Sample Size}{Sample size for the replication of this study}
}
\author{Kyun-Seop Bae k@acr.kr}

\examples{
  test2x2(NCAResult4BE, "AUClast")
  test2x2(NCAResult4BE, "Cmax")
}

\keyword{Statistical Test}
