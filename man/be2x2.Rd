\name{be2x2}
\alias{be2x2}
\title{Bioequivalence test of a 2x2 study}
\description{
  It performs conventional bioequivalence test for 2x2 study. Input is a file. Basic assumption is that the variable is distributed as log-normal distribution. This is SAS PROC GLM style. If you want PROC MIXED style, use \code{nlme:lme}.
}
\usage{
be2x2(filename, Columns = c("AUClast", "Cmax", "Tmax"), Plot = TRUE)
}
\arguments{
  \item{filename}{Data file name. This should have at least the following columns and variable columns to be tested. AUC ans Cmax should be all positive values.
\preformatted{  
 GRP : Group or Sequence, 'RT' or 'TR'
 PRD : Period, 1 or 2
 SUBJ : Subject ID
 TRT : Treatment or Drug, 'R' or 'T'
}
  }
  \item{Columns}{Column names of variables to be tested. This is usaully c("AUClast", "Cmax", "Tmax") or c("AUClast", "AUCinf", "Cmax", "Tmax")}
  \item{Plot}{Whether plot on screen is to be done or not.}
}
\details{
  It performs bioequivalency tests for severval variables of a 2x2 study in a data file. 
}
\value{
  Returns text output of equivalence test result.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\seealso{\code{\link{test2x2}}, \code{\link{plot2x2}}}
\examples{
  write.csv(NCAResult4BE, "temp.csv", quote=FALSE, row.names=FALSE)
  print(be2x2("temp.csv", c("AUClast", "Cmax", "Tmax")), na.print="") 
}

\keyword{Report}
