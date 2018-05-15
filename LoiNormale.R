LoiNormale <- function(m,s)
{
  X <- sqrt(-log(runif(1)))*cos(2*pi*runif(1))
  X <- m + sqrt(s)*X
  return (X)
}

PlotNormale <- function(m,s,precision=1000)
{
  vec = rep(0,precision)
  for (i in 1:precision)
  {
    vec[i] <- LoiNormale(m,s)
  }
  hist(vec)
}