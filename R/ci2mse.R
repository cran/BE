ci2mse = function(n1, n2, LL, UL, Alpha=0.1)
{
  pe = exp((log(UL) + log(LL))/2)
  t0 = qt(1 - Alpha/2, n1 + n2 - 2)
  SD = (log(UL) - log(LL))/(2*t0)
  mse = 2*SD^2/(1/n1 + 1/n2)
  return(mse)
}
