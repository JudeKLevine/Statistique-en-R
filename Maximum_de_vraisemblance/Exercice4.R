# Exercice 4: comparaison de 4 estimateurs du parametre d'une loi de poisson
# 1- Simulation des reslisation
  M=10000; lambda=2; n=15
  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()
  Estimateur4=c()
  for (i in 1:M){
    Realisation = rpois(n, lambda)
    Estimateur1[i]=mean(Realisation)
    Estimateur2[i]=var(Realisation)
    Estimateur3[i]=median(Realisation)
    Estimateur4[i]=0.5*mean(Realisation)+0.5*var(Realisation)
  }
# 2- Evaluation de l'esperance et de la variance
  mean(Estimateur1); mean(Estimateur2); mean(Estimateur3); mean(Estimateur4); 
  var(Estimateur1); var(Estimateur2); var(Estimateur3); var(Estimateur4)
  
  #l'estimateur 3 est biaise car sa moyenne est << lambda,
  #l'estimateur 2 est biaise car sa variance est grande 
  #les estimateurs 1 et 4 ne sont pas biaises
