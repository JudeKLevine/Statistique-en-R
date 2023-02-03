# Exercice 2:
  # 1- Calcul de la moyenne, de la variance et representation graphique
  M=5000; n=15; lambda=1
  Realisation=matrix(ncol=n, nrow=M)
  Moyenne = c()
  Variance = c()
  for (i in 1:M){
    Realisation[i,]=rexp(n,lambda)
    Moyenne[i] = mean(Realisation[i, ])
    Variance[i] = var(Realisation[i,])
  }

  hist(Moyenne, col='red', breaks=50, probability = TRUE)
  # le graphique ressemble a un gaussienne

  # 2- calcul des proportions
  A = Moyenne[Moyenne <= 1.5 & Moyenne >= 0.8]
  B = Variance[Variance <= 3 & Variance >= 1]
  C = Variance[Variance <= 3 & Variance >= 1 & Moyenne <= 1.5 & Moyenne >= 0.8]
  (P_A = length(A)/M); (P_B = length(B)/M); (P_C = length(C)/M); (P_B*P_A)
  
# 3- Commentaire
  # P_C != P_A*P_B les evenements A te B ne sont pas independants
  
# 4- Coefficient de correlation
  cor(Moyenne, Variance)
# ce coefficient est grand
  
# 5- On definit T
  T = sqrt(n)*(Moyenne - 1)/(sqrt(Variance))
  hist(T, col='red', breaks=50, proba=TRUE)
  curve(dt(x, df=14),col="black",lwd = 2,add=TRUE)
  # le graphe de T ressemble a celui de la densite de la 
  # loi exponentielle
  
# 6- On definit la variable
  K_K=(n - 1)*Variance/(lambda*lambda)
  hist(K_K, col='blue', breaks=50, proba=TRUE)
  curve(dchisq(x, df=14),col="red",lwd = 2,add=TRUE)
  # le graphique ressemble plus ou moins à celui de la densite de la 
  # loi de Khi-deux
# 7- calcul de P(X > 1,5+3 | X > 1,5) = P(X> 1,5+3)/P(X> 1,5)
  N=75000;
  Test = rbind(Realisation[,1:15]); length(Test)
  length(Test[(Test> 4.5)])/length(Test[(Test>1.5)]) #   0.05188369
  length(Test[(Test> 3)])/N #  0.04990667
  
  
  (1-pexp(4.5, rate=1))/(1-pexp(1.5, rate=1))  #  0.04978707
  (1-pexp(3, rate=1))  #  0.04978707 
