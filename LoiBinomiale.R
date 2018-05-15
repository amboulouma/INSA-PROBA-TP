LoiBinomiale <- function(n,p)
{
  #set.seed(215,kind="Mersenne-Twister")
  succes <- 0
  for (i in 1:n)
  {
    if (runif(1) <= p)
    {
      succes <- succes + 1
    }
  }
  return (succes)
}

PlotBinomiale <- function(n,p,precision=1000)
{
  vec = rep(0,precision)
  for (i in 1:precision)
  {
    vec[i] <- LoiBinomiale(n,p)
  }
  plot(table(vec))
}