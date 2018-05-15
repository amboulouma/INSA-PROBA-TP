main <- function()
{
  # Test du generateur RANDU
  testRANDU(10, 215)
  # Histogramme RANDU
  hist(testRANDU(1000, 215))
  #fonction de la valeur precedente RANDU (question 2.2.1 : 2.2)
  testRANDU2(1000, 215)
  #Test de frequence monobit RANDU
  testRANDU3(100)
  #Test des Runs RANDU
  testRANDU4(100)
  #Test d'ordre RANDU
  testRANDU5(100)
  
  
  #Test du generateur Standard minimal
  testStandardMinimal(10, 215)
  # Histogramme Standard Minimal
  hist(testStandardMinimal(1000, 215))
  #fonction de la valeur precedente Standard minimal (question 2.2.1 : 2.2)
  testStandardMinimal2(1000)
  #Test de frequence monobit Standard minimal
  testStandardMinimal3(100)
  #Test des Runs Standard minimal
  testStandardMinimal4(100)
  #Test d'ordre Standard minimal
  testStandardMinimal5(100)
  
  #Test du generateur Mersenne-Twister
  #histogramme Mersenne-Twister
  hist((MersenneTwister(1000, 1, 215))$x)
  #fonction de la valeur precedente Mersenne-Twister (question 2.2.1 : 2.2)
  testMersenneTwister(1000, 1, 215)
  #Test de frequence monobit Mersenne-Twister
  testMersenneTwister2(100)
  #Test des Runs Mersenne-Twister
  testMersenneTwister3(100)
  #Test d'ordre Mersenne-Twister
  testMersenneTwister4(100)
  
  #Test de la suite de Sobol
  #histogramme Sobol
  hist(Sobol(1000, 1))
  #fonction de la valeur precedente Sobol (question 2.2.1 : 2.2)
  testSobol(1000, 1)
  #Test de frequence monobit Sobol
  testSobol2(1000)
  #Test des Runs Sobol
  testSobol3(1000)
  #Test d'ordre Sobol
  testSobol4(1000)
  
  LoiBinomiale(10000, 0.05)
}

