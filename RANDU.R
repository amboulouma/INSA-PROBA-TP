RANDU <- function(graine)
{
  a <- 65539
  b <- 0
  m <- 2^31
  S <- (a*graine+b)%%m
  return (S)
}