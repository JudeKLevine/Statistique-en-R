# Exercice 8: Loi normale
# 1- Estimateur du maximal de vraisemblance
# 2- Simulation et evaluation
  M=5000; n=15; mu=8; sigma=sqrt(0.25)
  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()
  for(i in 1:M){
    Realisation=rnorm(n, mu, sigma)
    a = mean(Realisation)
    Estimateur1[i]=1/n*sum((Realisation - a)*(Realisation - a))
    Estimateur2[i]=var(Realisation)
    Estimateur3[i]=sqrt(sum((Realisation - 8)**4)/(3*n))
  }
  mean(Estimateur1); mean(Estimateur2); mean(Estimateur3)
  var(Estimateur1); var(Estimateur2); var(Estimateur3)

  # l'estimateur 2 semble ne pas etre biaise, car so moyenne se rapproche assez
  # de sigma, mais sa variance est plus grande que les autres
  # les estimateurs 1 et 3 sont biaises, ils ont des moyennes assez eloignees de sigma
