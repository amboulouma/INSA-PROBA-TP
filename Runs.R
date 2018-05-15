Runs <- function(x,nb)
{
  Vnobs <- 0
  somme <- 0
  for (i in 1:length(x))
  {
    vec <- binary(x[i])
    init <- length(vec)-nb+1
    for (j in init:length(vec))
    {
      somme <- somme + vec[j]
    }
  }
  p <- somme/(nb*length(x))
  t <- 2/sqrt(nb*length(x))
  if (abs(p-0.5) >= t)
  {
    return (0.0)
  }
  for (i in 1:length(x))
  {
    vec <- binary(x[i])
    init <- length(vec)-nb+1
    for (j in init:length(vec)-1)
    {
      if (vec[j] != vec[j+1])
      {
        Vnobs <- Vnobs + 1
      }
    }
  }
  res <- 2*(1-pnorm(abs(Vnobs-2*nb*length(x)*p*(1-p))/(2*sqrt(nb*length(x))*p*(1-p))))
  return (res)
}