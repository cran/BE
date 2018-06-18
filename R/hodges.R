hodges = function(bedata, Var)
{
  if(!assert(bedata)) {
    cat("\n Subject count should be balanced!\n");
    return(NULL);
  }

  Yi11 = bedata[bedata$GRP=="RT" & bedata$PRD==1, Var]
  Yi21 = bedata[bedata$GRP=="RT" & bedata$PRD==2, Var]
  Yi12 = bedata[bedata$GRP=="TR" & bedata$PRD==1, Var]
  Yi22 = bedata[bedata$GRP=="TR" & bedata$PRD==2, Var]

  n1 = length(Yi11)
  n2 = length(Yi12)

  if(n1 * n2 < 12) {
    cat("\n Too Small Sample Size for 90% Confidence Interval !\n");
    return(NULL);
  }

  mu.r = (mean(Yi11) + mean(Yi22)) / 2;

  G1D = (Yi21 - Yi11) / 2
  G2D = (Yi22 - Yi12) / 2
  D = sort(outer(G1D, G2D, "-"));

  pval = pwilcox(min(length(D[D>0]), length(D[D<0])), n1, n2)
  w05 = qwilcox(0.05, n1, n2)
  w95 = qwilcox(0.95, n1, n2)

  names(pval) = list(c("p-value"));

  est1 = cbind(D[w05 - 1], median(D), D[w95])
  est2 = (1 + est1 / mu.r ) * 100
  est.a = rbind(est1, est2)
  dimnames(est.a) = list(c("90% Confidence Interval", "90% Confidence Interval(%)"), c("Lower Limit", "Point Estimate", "Upper Limit"));

  result = list(pval, est.a);
  names(result) = c("Wilcoxon Signed-Rank Test", "Hodges-Lehmann Estimate")
  return(result);
}

