\name{be2x2}
\alias{be2x2}
\title{Bioequivalence test of a 2x2 study}
\description{
  It performs conventional bioequivalence test for 2x2 study. Input is a file. Basic assumption is that the variable is distributed as a log-normal distribution. This is SAS PROC GLM style. If you want PROC MIXED style, use \code{nlme::lme}.
}
\usage{
be2x2(Data, Columns = c("AUClast", "Cmax", "Tmax"), rtfName="")
}
\arguments{
  \item{Data}{A \code{data.frame} or a file name. This should have at least the following columns and variable column(s) to be tested. AUC and Cmax should be all positive values.
\preformatted{  
 GRP : Group or Sequence, 'RT' or 'TR'
 PRD : Period, 1 or 2
 SUBJ : Subject ID
 TRT : Treatment or Drug, 'R' or 'T'
}
  }
  \item{Columns}{Column names of variables to be tested. This is usaully c("AUClast", "Cmax", "Tmax") or c("AUClast", "AUCinf", "Cmax", "Tmax")}
  \item{rtfName}{Output filename of rich text format(rtf)}
}
\details{
  It performs bioequivalency tests for several variables of a 2x2 study in a data file. If you specify output filename in \code{rtfName}, the output will be saved in the file. 
}
\value{
  Returns text output of equivalence test result.
}
\author{Kyun-Seop Bae k@acr.kr}
\seealso{\code{\link{test2x2}}, \code{\link{plot2x2}}}
\examples{
  be2x2(NCAResult4BE, c("AUClast", "Cmax", "Tmax"))
}
\keyword{Report}
