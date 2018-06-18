pow2x2ci = function(n1, n2, LL, UL, Alpha=0.1)
{
  mse = ci2mse(n1, n2, LL, UL, Alpha)
  Power = pow2x2mse(n1, n2, mse)
  return(Power)
}
