scaledBound = function(CV = 40, k=0.76, digits=4)
{
  if (CV <= 30) return(c(0.8, 1.25))
  if (CV > 50) CV = 50

# SD0 = sqrt(log(1 + (30/100)^2)) # SD0 = 0.2935604
# k = log(1.25)/SD0
# format(k, digits=22) = 0.76012829768047396 # When CV=30%, bound becomes 0.8 - 1.25

  SD0 = sqrt(log(1 + (CV/100)^2))
  return(round(exp(c(-k, k)*SD0), digits))
}
