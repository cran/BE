ss2x2ci = function(n1, n2, LL, UL, Alpha=0.1)
{
  pe = exp((log(UL) + log(LL))/2)
  mse = ci2mse(n1, n2, LL, UL, Alpha)
  s1 = ssmse(mse)
  s2 = ssmse(mse, True.R=pe)
  sampsize = cbind(s1, s2)
  p1 = round(100 * pow2x2mse(n1, n2, mse))
  p2 = round(100 * pow2x2mse(n1, n2, mse, True.R=pe))
  Power = cbind(p1, p2)
  result = rbind(sampsize, Power)
  dimnames(result) = list(c("80% Power Sample Size", paste("Power at n1 =", n1, ", n2 =", n2)), c("True Ratio=1", sprintf("True Ratio=%.4f", pe)))
  return(result)
}
