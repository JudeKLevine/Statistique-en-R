# Exercice 6: Comparaison des estimateurs du parametre de la loi exponentielle
# 1- Simulation
  M=1000; n=18; lambda=0.1
  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()
  Estimateur4=c()
  for (i in 1:M){
    Realisation=rexp(n, lambda)
    Estimateur1[i]=1/mean(Realisation)
    Estimateur2[i]=(n - 1)/(n*mean(Realisation))
    Estimateur3[i]=4*n/((4*n + 11)*sqrt(var(Realisation)))
    Estimateur4[i]=log(2)*(n - 4)/((n - 3)*median(Realisation))
   
  }
# 2- Evaluation des variances et des esperances
 mean(Estimateur1); mean(Estimateur2); mean(Estimateur3); mean(Estimateur4)
 var(Estimateur1); var(Estimateur2); var(Estimateur3); var(Estimateur4)
 
 # les estimateurs 3 et 4 sont biaises, ils ont les plus grandes variances comparé aux deux autres
 # les estimateurs 1 et 2 ne sont pas biaises, ils ont les plus petites variances
