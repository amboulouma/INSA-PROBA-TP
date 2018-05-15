testRANDU <- function(k, graine)
{
  vec <- rep(0,k)
  g <- graine
  for (i in 1:k)
  {
    r <- RANDU(g)
    g <- r
    vec[i] <- r
  }
  return (vec)
}

testRANDU2 <- function(n, graine)
{
  u <- testRANDU(n, graine)
  plot(u[1:(n-1)], u[2:n])
}

testRANDU3 <- function(n)
{
  res = rep(0,n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testRANDU(1000, vec[i])
    res[i] <- Frequency(u, 10)
  }
  cat(mean(res))
  return (res)
}

testRANDU4 <- function(n)
{
  res = rep(0,n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testRANDU(1000, vec[i])
    res[i] <- Runs(u, 10)
  }
  return (res)
}

testRANDU5 <- function(n)
{
  res = rep(0,n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- testRANDU(1000, vec[i])
    res[i] <- order.test(u, d=4, echo=FALSE)$p.value
  }
  return (res)
}