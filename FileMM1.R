FileMM1 <- function(lambda, mu, D)c c c
{
  arrivee <- c()
  depart <- c()
  
  time <- rexp(1, lambda)
  
  while (time < D)
  {
    arrivee <- c(arrivee, time)
    time <- time + rexp(1, lambda)
  }
  
  if (length(arrivee) > 0)
  {
    depart <- c(depart, arrivee[1] + rexp(1, mu))
    for (i in 2:length(arrivee))
    {
      if (arrivee[i] > depart [i-1])
        time = arrivee[i] + rexp(1, mu)
      else
        time = depart[i-1] + rexp(1, mu)
      
      if (time < D)
        depart[i] = time
      else
        break
    }
    
  }
  return(list(arrivee,depart))
}

