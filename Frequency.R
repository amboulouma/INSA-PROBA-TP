Frequency <- function(x, nb)
{
  S <- 0
  for (a in 1:length(x))
  {
    vecBin <- binary(x[a])
    taille <- length(vecBin)
    init <- taille-nb+1
    for (i in init:taille)
    {
      if (vecBin[i] == 1)
      {
        S <- S + 1
      }
      else
      {
        S <- S - 1
      }
    }
  }
  Sobs <- abs(S)/(sqrt(nb*length(x)))
  Pvaleur <- 2*(1 - pnorm(Sobs))
  return (Pvaleur)
}

