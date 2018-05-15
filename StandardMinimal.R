StandardMinimal <- function(graine)
{
  a <- 16807
  b <- 0
  m <- 2^31-1
  S <- (a*graine+b)%%m
  return (S)
}