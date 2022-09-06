\name{NCAResult4BE}
\alias{NCAResult4BE}
\docType{data}
\title{
  An Example of Noncompartmental Analysis Result for Bioequivalence Test
}
\description{
  Contains a noncompartmental analysis result table from a concentration simulated bioequivalence study. 
}
\usage{
NCAResult4BE
}
\format{
  A data frame with 66 observations on the following 7variables.
  \describe{
    \item{\code{SUBJ}}{Subject ID}
    \item{\code{GRP}}{Group or Sequence character code: 'RT' or 'TR"}
    \item{\code{PRD}}{Period numeric value: 1 or 2}
    \item{\code{TRT}}{Treatment or Drug code: 'R' or 'T'}
    \item{\code{AUClast}}{AUClast positive numeric value}
    \item{\code{Cmax}}{Cmax positive numeric value}
    \item{\code{Tmax}}{Tmax positive numeric value}
  }
}
\details{
  This contains a simulated data for 2x2 bioequivalence study data analysis. Noncompartmental analysis results are from the \code{NonCompart} package. 
}
\keyword{datasets}
