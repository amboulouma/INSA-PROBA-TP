MersenneTwister <- function(n,p=1,graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(list(x=x,s=x[n]))
}

testMersenneTwister <- function(n,p=1,graine)
{
  u <- (MersenneTwister(n, p, graine))$x
  plot(u[1:(n-1)], u[2:n])
}

testMersenneTwister2 <- function(n,p=1)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- (MersenneTwister(1000, p, vec[i]))$x
    res[i] <- Frequency(u, 10)
  }
  return (res)
}

testMersenneTwister3 <- function(n,p=1)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- (MersenneTwister(1000, p, vec[i]))$x
    res[i] <- Runs(u, 10)
  }
  return (res)
}

testMersenneTwister4 <- function(n,p=1)
{
  res <- rep(0, n)
  vec <- sample(1:1000, n)
  for (i in 1:n)
  {
    u <- (MersenneTwister(1000, p, vec[i]))$x
    res[i] <- order.test(u[,1], d=4, echo=FALSE)$p.value
  }
  return (res)
}


Sobol <- function(n,p)
{
  return(round(sobol(n,p)*(2^31-1)))
}

testSobol <- function(n,p=1)
{
  u <- Sobol(n, p)
  plot(u[1:(n-1)], u[2:n])
}

testSobol2 <- function(n,p=1)
{
  u <- Sobol(n, p)
  return (Frequency(u, 10))
}

testSobol3 <- function(n,p=1)
{
  u <- Sobol(n, p)
  return (Runs(u, 10))
}

testSobol4 <- function(n,p=1)
{
  u <- Sobol(n, p)
  return (order.test(u, d=4, echo=FALSE)$p.value)
}

binary <- function(x)
{
  if((x<2^31)&(x>0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}
