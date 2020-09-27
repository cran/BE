test2x2 = function(bedata, Var)
{
  if(!assert(bedata)) {
    cat("\n Drop-outed subjects should not be included!\n");
    return(NULL);
  }

  Yijk = log(bedata[, Var])
  Yi11 = log(bedata[bedata$GRP=="RT" & bedata$PRD==1, Var])
  Yi21 = log(bedata[bedata$GRP=="RT" & bedata$PRD==2, Var])
  Yi12 = log(bedata[bedata$GRP=="TR" & bedata$PRD==1, Var])
  Yi22 = log(bedata[bedata$GRP=="TR" & bedata$PRD==2, Var])

  n1 = length(Yi11)
  n2 = length(Yi12)

  Y... = mean(Yijk)
  SStotal = sum((Yijk - Y...)^2)

  Y.11 = mean(Yi11)
  Y.21 = mean(Yi21)
  Y.12 = mean(Yi12)
  Y.22 = mean(Yi22)

  Yi.1 = (Yi11 + Yi21) / 2
  Yi.2 = (Yi12 + Yi22) / 2

  Y..1 = mean(Yi.1)
  Y..2 = mean(Yi.2)

  mu.r = (Y.11 + Y.22) / 2
  mu.t = (Y.21 + Y.12) / 2

  di1 = (Yi21 - Yi11) / 2
  di2 = (Yi22 - Yi12) / 2

  d.1 = mean(di1)
  d.2 = mean(di2)

#  Chat = Y.12 + Y.22 - Y.11 - Y.21
#  Fhat = mu.t - mu.r
#  Phat = (Y.21 - Y.11 - Y.12 + Y.22)/2

  SScarry   = 2*n1*n2/(n1 + n2)*(Y.12 + Y.22 - Y.11 - Y.21)^2 / 4
  SSinter   = (sum((Yi.1 - Y..1)^2) + sum((Yi.2 - Y..2)^2)) * 2
  SSbetween = SScarry + SSinter

  SSperiod  = 2*n1*n2/(n1+n2)*(Y.21 + Y.22 - Y.11 - Y.12)^2 / 4
  SSdrug    = 2*n1*n2/(n1+n2)*(Y.21 + Y.12 - Y.11 - Y.22)^2 / 4

  SSintra   = 2*(sum((di1 - d.1)^2) + sum((di2 - d.2)^2))
#  SSmodel = SStotal - SSintra

  Source = c("SUBJECT", "GROUP", "SUBJECT(GROUP)", "PERIOD", "DRUG", "ERROR", "TOTAL");
  SS     = c(SSbetween, SScarry, SSinter, SSperiod, SSdrug, SSintra, SStotal);
  DF     = c(n1 + n2 - 1, 1, n1 + n2 - 2, 1, 1, n1 + n2 - 2, 2*n1 + 2*n2 - 1);
  MS     = SS / DF
  mse    = SSintra/(n1 + n2 - 2)
  Fv      = MS / c(mse, MS[3], mse, mse, mse, mse, mse);
  p1 = 1 - pf(Fv[1], n1 + n2 - 1, n1 + n2 - 2)
  p2 = 1 - pf(Fv[2], 1, n1 + n2 - 2);
  p3 = 1 - pf(Fv[3], n1 + n2 - 2, n1 + n2 - 2);
  p4 = 1 - pf(Fv[4], 1, n1 + n2 - 2);
  p5 = 1 - pf(Fv[5], 1, n1 + n2 - 2);
  p  = c(p1, p2, p3, p4, p5, NA, NA)
  Fv[6] = Fv[7] = MS[7] = NA

  ANOVA = cbind(DF, SS, MS, Fv, p)
  dimnames(ANOVA) = list(Source, c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
  class(ANOVA) = "anova"

  pe = mu.t - mu.r
  se = sqrt(mse/2 * (1/n1 + 1/n2))   # See pp 62-63 of Chow & Liu
  t0 = qt(0.95, n1 + n2 - 2);
  ci0 = cbind(pe - t0 * se, pe, pe + t0 * se)

  sig2b = (MS[3] - MS[6])/2
  sig2w = MS[6]

  cvs = cbind(sqrt(exp(sig2b) - 1), sqrt(exp(sig2w) - 1)) * 100

  lsm = cbind(exp(mu.r), exp(mu.t))
  dimnames(lsm) = list("Geometric Means", cbind("Reference Drug", "Test Drug"))

  ci = exp(ci0);
  dimnames(ci) = list("90% CI for Ratio", c("Lower Limit", "Point Estimate", "Upper Limit"));

  sampsize1 = ssmse(mse);
  sampsize2 = ssmse(mse, True.R=exp(pe));
  ss = cbind(sampsize1, sampsize2)
  dimnames(ss) = list("80% Power Sample Size", c("True Ratio=1", "True Ratio=Point Estimate"));

  cvs = rbind(cbind(sig2b, sig2w), cvs)
  dimnames(cvs) = list(cbind("Variance Estimate", "Coefficient of Variation, CV(%)"), cbind("Between Subject", "Within Subject"))

  result = list(ANOVA, cvs, lsm, ci, ss);
  names(result) = c("Analysis of Variance (log scale)", "Between and Within Subject Variability", "Least Square Means (geometric mean)", "90% Confidence Interval of Geometric Mean Ratio (T/R)", "Sample Size")

  return(result);
}
