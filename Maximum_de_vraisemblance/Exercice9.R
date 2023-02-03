
#Exercice 9: Estimateur sans biais et de variance minimale
  # 1- Que representent les moyennes: 
  # - la moyenne theorique est une constante connue,
  # - la moyenne de l'echantillon represente la moyenne des 
  #   des observations, c'est une variable aleatoire

  Estimateur1=c()
  Estimateur2=c()
  Estimateur3=c()
  Estimateur4=c()
  C=sqrt(2/49)*(gamma(25)/gamma(24.5))
  # 2- Simulaiton
  M=3000; n=50; mu=5
  for(i in 1:M){
    Real=rnorm(n, mu, 4*mu*mu)
    Estimateur1[i]=(Real[1] + Real[n])/2
    Estimateur2[i]=median(Real)
    Estimateur3[i]=(1/n)*sum(Real)
    Estimateur4[i]=sqrt(sum((Real-mean(Real))**2))*sqrt(1/(n-1))/(2*C)
  }
  # 3- Superposition des histogrammes
  # 4- Biais et variances des estimateurs
    mean(Estimateur1); mean(Estimateur2); mean(Estimateur3); mean(Estimateur4)
    var(Estimateur1); var(Estimateur2); var(Estimateur3); var(Estimateur4);
    
    # l'estimateur 4 a la plus petite variance mais est biais. Le 1 et le 2 
    # sont biaises, le 3 semble etre correct avec une variable assez petite
