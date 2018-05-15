---
title: "Compte Rendu"
author: "Amine Boulouma / Clément Florant"
date: "15 mai 2018"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(randtoolbox)
source('generateurs.R')
source('RANDU.R')
source('StandardMinimal.R')
source('testRANDU.R')
source('testStandardMinimal.R')
source('Frequency.R')
source('LoiBinomiale.R')
source('LoiNormale.R')
source('LoiUniforme.R')
source('Runs.R')
set.seed(215,kind="Mersenne-Twister")
```

# Compte Rendu TP Probabilités

## Question 1

### RANDU

Implémentation de l'algorithme RANDU

```{r eval=FALSE}
RANDU <- function(graine)
{
  a <- 65539
  b <- 0
  m <- 2^31
  S <- (a*graine+b)%%m
  return (S)
}
```

Génération de k valeurs avec RANDU

```{r eval=FALSE}
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
```

Affichage de 10 valeurs générées par RANDU pour une graine de 215

```{r}
testRANDU(k=10, graine=215)
```
### Standard Minimal

Implémentation de l'algorithme Standard Minimal

```{r eval=FALSE}
StandardMinimal <- function(graine)
{
  a <- 16807
  b <- 0
  m <- 2^31-1
  S <- (a*graine+b)%%m
  return (S)
}
```
Génération de k valeurs avec Standard Minimal

```{r eval=FALSE}
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
```

Affichage de 10 valeurs générées par Standard Minimal pour une graine de 215

```{r}
testStandardMinimal(k=10, graine=215)
```

## Question 2.1

Remarque générale : Dans tous les diagrammes, on remarque que la dernière série de valeurs apparait moins que les autres, tout simplement car cette plage de valeurs est moins large que les autres (Largeur normale : 2.0e08, largeur dernière plage : 5.25e07). Ces valeurs ont donc logiquement moins de chances d'apparaître.

### RANDU

Histogramme de 1000 valeurs générées par RANDU avec une graine de 215

```{r}
hist(testRANDU(1000, 215))
```

On remarque que la répartition des valeurs est plutôt équilibrée sauf pour la deuxième plage de valeurs qui n'apparaissent que 60 fois environ. Cela ne nous permet pas de conclure mais on peut supposer qu'il y a un possible désequilibre dans la répartition des valeurs.

### Standard Minimal

Histogramme de 1000 valeurs générées par Standard Minimal avec une graine de 215

```{r}
hist(testStandardMinimal(1000, 215))
```

La répartition des valeurs est équlibrée et ne traduit pas de problème particulier.

### Mersenne-Twister

Histogramme de 1000 valeurs générées par Mersenne-Twister avec une graine de 215

```{r}
hist((MersenneTwister(1000, 1, 215))$x)
```

La répartition des valeurs est équlibrée et ne traduit pas de problème particulier.

### Sobol

Histogramme de 1000 valeurs générées par Sobol

```{r}
hist(Sobol(1000, 1))
```

La répartition des valeurs est parfaitement équilibrée et semble même "trop" régulière.

## Question 2.2

Affichage de la valeur obtenue en fonction de la valeur précédente

### RANDU

```{r}
n <- 1000
u <- testRANDU(n, 215)
plot(u[1:(n-1)], u[2:n])
```

### Standard Minimal

```{r}
n <- 1000
u <- testStandardMinimal(n, 215)
plot(u[1:(n-1)], u[2:n])
```

### Mersenne-Twister

```{r}
n <- 1000
p <- 1
u <- (MersenneTwister(n, p, 215))$x
plot(u[1:(n-1)], u[2:n])
```

### Sobol

```{r}
n <- 1000
p <- 1
u <- Sobol(n, p)
plot(u[1:(n-1)], u[2:n])
```

### Commentaires

Pour les trois premiers algorithmes, il n'y a pas de remarque particulière si ce n'est que les valeurs consécutives semblent indépendantes contrairement à la suite de Sobol où l'on observe une corrélation entre les valeurs obtenues. Il semble que celle-ci ne soit pas adaptée pour générer des valeurs imprévisibles.

## Question 3 : Test de Fréquence Monobit

### RANDU

```{r}
n <- 100
res <- rep(0,n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testRANDU(1000, vec[i])
  res[i] <- Frequency(u, 10)
}
hist(res)
```

### Standard Minimal

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testStandardMinimal(1000, vec[i])
  res[i] <- Frequency(u, 10)
}
hist(res)
```

### Mersenne-Twister

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- (MersenneTwister(1000, p, vec[i]))$x
  res[i] <- Frequency(u, 10)
}
hist(res)
```

### Sobol

```{r}
n <- 100
p <- 1
u <- Sobol(n, p)
return (Frequency(u, 10))
```

## Question 4 Test des Runs


### RANDU

```{r}
n <- 100
res = rep(0,n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testRANDU(1000, vec[i])
  res[i] <- Runs(u, 10)
}
hist(res)
```

### Standard Minimal

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testStandardMinimal(1000, vec[i])
  res[i] <- Runs(u, 10)
}
hist(res)
```

### Mersenne-Twister

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- (MersenneTwister(1000, p, vec[i]))$x
  res[i] <- Runs(u, 10)
}
hist(res)
```

### Sobol

```{r}
n <- 100
p <- 1
u <- Sobol(n, p)
return (Runs(u, 10))
```

## Question 5 Test d'ordre


### RANDU

```{r}
n <- 100
res = rep(0,n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testRANDU(1000, vec[i])
  res[i] <- order.test(u, d=4, echo=FALSE)$p.value
}
hist(res)
```

### Standard Minimal

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- testStandardMinimal(1000, vec[i])
  res[i] <- order.test(u, d=4, echo=FALSE)$p.value
}
hist(res)
```

### Mersenne-Twister

```{r}
n <- 100
res <- rep(0, n)
vec <- sample(1:1000, n)
for (i in 1:n)
{
  u <- (MersenneTwister(1000, p, vec[i]))$x
  res[i] <- order.test(u[,1], d=4, echo=FALSE)$p.value
}
hist(res)
```

### Sobol

```{r}
n <- 100
p <- 1
u <- Sobol(n, p)
return (order.test(u, d=4, echo=FALSE)$p.value)
```

## Question 6

```{r eval=FALSE}
LoiBinomiale <- function(n,p)
{
  succes <- 0
  for (i in 1:n)
  {
    if (runif(1) <= p)
    {
      succes <- succes + 1
    }
  }
  return (succes)
}
```

```{r eval=FALSE}
PlotBinomiale <- function(n,p,precision=1000)
{
  vec = rep(0,precision)
  for (i in 1:precision)
  {
    vec[i] <- LoiBinomiale(n,p)
  }
  plot(table(vec))
}
```

```{r}
PlotBinomiale(n=100, p=0.5)
```

## Question 7

```{r eval=FALSE}
LoiNormale <- function(m,s)
{
  X <- sqrt(-log(runif(1)))*cos(2*pi*runif(1))
  X <- m + sqrt(s)*X
  return (X)
}
```

```{r eval=FALSE}
PlotNormale <- function(m,s,precision=1000)
{
  vec = rep(0,precision)
  for (i in 1:precision)
  {
    vec[i] <- LoiNormale(m,s)
  }
  hist(vec)
}
```

```{r}
PlotNormale(10,2)
```