# Exercice 7: Esteimateur de vraisemblance
# 1- Simulation
  M=5000; n=10; theta=0.1
  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()
  Estimateur4=c()
  for(i in 1:M){
    Real=runif(n, 0, theta)
    Estimateur1[i]=max(Real)
    Estimateur2[i]=(n/(n+1))*max(Real)
    Estimateur3[i]=((n+1)/n)*max(Real)
    Estimateur4[i]=2*mean(Real)
  }
# 2- Evaluation des esperances et des variances
  mean(Estimateur1); mean(Estimateur2); mean(Estimateur3); mean(Estimateur4)
  var(Estimateur1); var(Estimateur2); var(Estimateur3); var(Estimateur4)
  
  #l'estimateur 1 et 2 ont des moyennes assez eloignees la theta, ils sont biaisees
  #les estimateurs 3 et 4 ne sont pas biaises, ils ont des moyennes qui sont
  # sensiblement egales ) theta et des variances assez petites.
