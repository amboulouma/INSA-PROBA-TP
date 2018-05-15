FileMM1Plot <- function(m,s,precision=1000)
{
  vec = rep(0,precision)
  for (i in 1:precision)
  {
    vec[i] <- LoiNormale(m,s)
  }
  hist(vec)
}