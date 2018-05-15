testStandardMinimal <- function(k, graine)
{
  vec <- rep(0,k)
  g <- graine
  for (i in 1:k)
  {
    r <- StandardMinimal(g)
    g <- r
    vec[i] <- r
  }
  return (vec)
}

testStandardMinimal2 <- function(n)
{
  u <- testStandardMinimal(n, 215)
  plot(u[1:(n-1)], u[2:n])
}

testStandardMinimal3 <- function(n)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testStandardMinimal(1000, vec[i])
    res[i] <- Frequency(u, 10)
  }
  return (res)
}

testStandardMinimal4 <- function(n)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testStandardMinimal(1000, vec[i])
    res[i] <- Runs(u, 10)
  }
  return (res)
}

testStandardMinimal5 <- function(n)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testStandardMinimal(1000, vec[i])
    res[i] <- order.test(u, d=4, echo=FALSE)$p.value
  }
  return (res)
}