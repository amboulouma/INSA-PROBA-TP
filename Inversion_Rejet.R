Inversion <- function()
{
  return (exp(sqrt(runif(1)*log(2)))-1)
}

plotInversion <- function()
{
  vec = rep(0,10000)
  for (i in 1:10000)
  {
    vec[i] <- Inversion()
  }
  hist(vec)
}

Rejet <- function()
{
  c <- 2/(log(2)*log(2))
  y <- runif(1)
  x <- runif(1)
  while(x <= (log(1+y)/(1+y))/c)
  {
    x <- runif(1)
    y <- runif(1)
  }
  x <- y
  return (x)
}

plotRejet <- function()
{
  vec = rep(0,1000)
  for (i in 1:1000)
  {
    vec[i] <- Rejet()
  }
  hist(vec)
}