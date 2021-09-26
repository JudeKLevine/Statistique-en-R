# Exercice 3
# 1- Caracteristiques de Population:
  Population=treering
  summary(Population)
# 2- Simulation et calcul:
  M=500; n=65
  Realisation=matrix(nrow=M, ncol=n)
  Moyenne=c()
  for(i in 1:M){
    Realisation[i,]=sample(Population, 65, replace=FALSE, prob=NULL)
    Moyenne[i]=mean(Realisation[i,])
  }
# 3- Comparaison des moyennes
  # Moyenne theorique
  Moy.Theo=mean(Population)
  # Moyenne des moyennes
  Moy.Moy=mean(Moyenne)
  # Moyenne observee
  Moy.Obs=mean(Realisation)
  # la difference entre les natures des moyennes 
  hist(Moyenne, col='red', breaks=50)
