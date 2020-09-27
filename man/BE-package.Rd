\name{BE-package}
\alias{BE-package}
\alias{BE}
\docType{package}
\title{Bioequivalence Study Data Analysis}
\description{
  Analyze bioequivalence study data with industrial strength. Sample size could be determined for various crossover designs, such as 2x2 design, 2x4 design, 4x4 design, Balaam design, Two-sequence dual design, and William design.
  Basic assumption is that the variable is distributed as a log-normal distribution. This is SAS PROC GLM style. If you want PROC MIXED style, use \code{nlme::lme}.
}
\details{
  It performs bioequivalency tests for several variables of a 2x2 study in a data file. 
}
\author{Kyun-Seop Bae k@acr.kr}
\references{
  \enumerate{
  \item Chow SC, Liu JP. Design and Analysis of Bioavailability and Bioequivalence Studies. 3rd ed. (2009, ISBN:978-1-58488-668-6)
  \item Hauschke D, Steinijans V, Pigeot I. Bioequivalence Studies in Drug Development. (2007, ISBN:978-0-470-09475-4)
  \item Diletti E, Hauschke D, Steinijans VW. Sample size determination for bioequivalence assessment by means of confidence intervals. Int J Clinical Pharmacol Ther Tox. 1991;29(1):1-8
  }
}
\examples{
# write.csv(NCAResult4BE, "temp.csv", quote=FALSE, row.names=FALSE)
# be2x2("temp.csv", c("AUClast", "Cmax", "Tmax"))

## 'nlme' or SAS PROC MIXED is preferred for the confidence interval

## SAS PROC MIXED equivalent
# require(nlme)
# r2 = lme(log(Cmax) ~ GRP + PRD + TRT, random=~1|SUBJ, data=BEdata)
# summary(r2)
# VarCorr(r2)
# ci = intervals(r2, 0.90) ; ci 
# exp(ci$fixed["TRTT",])

## SAS PROC GLM equivalent
# require(sasLM) # includes 'BEdata' which is a real dataset
# BEdata = af(BEdata, c("SEQ", "SUBJ", "PRD", "TRT")) # Columns as factor
# formula1 = log(CMAX) ~ SEQ/SUBJ + PRD + TRT # Model
# GLM(formula1, BEdata) # ANOVA tables of Type I, II, III SS
# T3MS(formula1, BEdata) # EMS table
# T3test(formula1, BEdata, Error="SEQ:SUBJ") # Hypothesis test
}
