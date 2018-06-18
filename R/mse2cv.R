mse2cv = function(mse)
{
  cv = sqrt(exp(mse) - 1)
  return(100*cv)
}
