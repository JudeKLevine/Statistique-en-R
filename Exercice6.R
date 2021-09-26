# Exercice 6:
# 1- Simulation de la loi normale de parametre mu = -10 et sigma=1.5
  M=5000; mu=-10; sigma=1.5
  Xn=rnorm(M, mu, sigma)
# 2- Trie des resultats
  Y=Xn[Xn<=-10 & Xn>-12]
  hist(Y, col='red')
# 3- Comparaison entre proportion et probabilite
  prop =length(Y)/M
  prob =pnorm(-10, mu, sigma) - pnorm(-12, mu, sigma)
  prop; prob
  # Les resultats sont presque les memes
# 4- Superposition de la fonction densite
  x = seq(-16, -4, length=100) # pts d'abscisses,
  y =dnorm(x, mu, sigma)       # pts d'ordonn�es
  plot(x, y, type="l", col="red")
  u=seq(-12,-10, length=100); v=dnorm(u, mu, sigma)
  polygon(c(-12, u,-10), c(0, v, 0), col="grey")
