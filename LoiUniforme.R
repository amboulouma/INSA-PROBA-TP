LoiUniforme <- function(p)
{
  k <- 0
  somme <- 0
  while(runif(1) > somme)
  {
    k <- k + 1
    somme <- somme + p
  }
  return (k)
}