# Exercice 5: determination des estimateurs de lambda
# 1- Simulation
  M=10000; theta=100; n=20 # lambda = 1/theta
  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()  
  Estimateur4=c()
  for (i in 1:M){
    Realisation=rexp(n, theta)
    Estimateur1[i]=mean(Realisation)
    Estimateur2[i]=(1 + 1/n)*sqrt(sum(Realisation*Realisation)/(2*n))
    Estimateur3[i]=(1 + 1/n)*sqrt(var(Realisation))
    Estimateur4[i]=median(Realisation)/(log(2)*(1 + 1/n))
  }
  
# 2- Evaluation de l'esperance et de la variance: loi des grands nombres
  mean(Estimateur1); mean(Estimateur2); mean(Estimateur3); mean(Estimateur4)
  var(Estimateur1); var(Estimateur2); var(Estimateur3); var(Estimateur4)
  
  
  # l'estimateur 1 n'est pas biaise, il a la plus petite variance, c'est 
  # l'estimateur du maximum de vraisemblance, le 2 aussi n'est pas biaise
  # les estimateurs 3 et 4 sont biaises, ils ont les plus grandes variances
