#Exercice 5:
# 1- Histogramme des realisations
  lambda=1/2; Ms=5000
  U=runif(Ms)
  Y=-log(U)/lambda
  m=floor(min(Y)); M=ceiling(max(Y)); brk=c(seq(m, M, 0.3))
  hist(Y, probability=TRUE, col='light blue', breaks=brk)
# 2- Graphique de la fonction densite
  u=seq(0,15,length=500); v=dexp(u, rate=lambda)
  lines(u,v, col='red', type='l', lwd=1)
# 3- Proportion des donnees comprises entre 8 et 12
  Proportion = length(Y[Y<12 & Y>8])/Ms
# 4- Probabilite F(12)-F(8)
  Proba = pexp(12,rate=lambda)-pexp(8,rate=lambda)
# 5- Integration
  f=function(x)exp(-x*lambda)*lambda
  proba_Int = integrate(f, 8, 12)
# 6- Representation graphique
  x=seq(8, 12, length=300); y=dexp(x, rate=lambda)
  plot(x, y, col='red', type='l', lwd=7,ylim=c(0,max(y)))
  s=seq(8,12,length=200); t=dexp(s, rate=lambda)
  polygon(c(8,s,12), c(0,t,0), col='gray')
  

  
