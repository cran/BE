ci2cv = function(n1, n2, LL, UL, Alpha=0.1)
{
  mse = ci2mse(n1, n2, LL, UL, Alpha)
  return(mse2cv(mse))
}
