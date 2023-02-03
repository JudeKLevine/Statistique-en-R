# Exercice 1 : Maximum de vraisemblance

  # Question 1 : Sur la copie
  # Question 2 : Simulation et superposition avec la loi de densité
    M = 1000; gamma = 5
    x = abs((rnorm(M, 0, sqrt(gamma))))
    hist(x, prob=TRUE, breaks = 50)
    curve(2*dnorm(x, 0, sqrt(5)), col="red", add=T) # densité = 2*dnorm
    
  # Question 3 : Test des estimateur 
    M2 = 5000; n = 15
    E1 = c(); E2 = c(); E3 = c(); E4 = c(); E5 = c(); Xbar = c()
    for(i in 1:M2){
      Echant = rnorm(n, 0, sqrt(5))
      Xbar[i] = mean(Echant)
      E1[i] = (1/n)*sum((Echant - Xbar[i])*(Echant - Xbar[i])) + Xbar[i]*Xbar[i]
      E2[i] = (1/n)*sum(Echant*Echant)
      E3[i] = (1/(n-1))*sum(Echant*Echant)
      E4[i] = sqrt(pi/2)*Xbar[i]*Xbar[i]
      E5[i] = Xbar[i]*Xbar[i]*n*n/(n + 2*n*(n-1)/pi)
    }
    
    # Test d'estimateurs biaisés
    mean(E1) # 5.015986 # les estimateurs 1 et 2 sont les 
    mean(E2) # 5.015986 # mêmes et son non biaisés
    mean(E3) # 5.374271
    mean(E4) # 0.408028
    mean(E5) # 0.4926408
                          # les autres sont biaisés, le 4 et le 5 donnent des 
                          # valeurs tres eloignées de la valeur attendue
    # Test de la stabilité
    var(E1) # 3.298077    # assez grande pour un estimateur sans biais
    var(E3) # 3.786058    # Ils ne sont pas tres stables! 
    
# Exercice 2 : Intervalle de confiance
    
    n = 25; mu1 = 15; sigma = sqrt(8); Binf1; Bsup1; Cq = qt(0.02, n-1)
    # Question 1 : 
    Echant = rnorm(n, mu1, sigma)
    Binf1 = mean(Echant) + Cq*sd(Echant)/sqrt(n)
    Bsup1 = mean(Echant) - Cq*sd(Echant)/sqrt(n)
    Binf1 < mu1 & Bsup1 > mu1   # TRUE
    
    # Question 2 : 
    M = 3000; Binf11 = c(); Bsup11 = c(); count1 = 0
    for(i in 1:M){
      Echant = rnorm(n, mu1, sigma)
      Binf11[i] = mean(Echant) + Cq*sd(Echant)/sqrt(n)
      Bsup11[i] = mean(Echant) - Cq*sd(Echant)/sqrt(n)
      if(Binf11[i] < mu1 & Bsup11[i]>mu1){count1 = count1 + 1} 
    }
    count1/M # 0.954  bon!!!
    
    # Question 3 : Intervalle de confiance de la variance
    mu2 = 10; sigma2 = 9; n2 = 37; Binf2; Bsup2
    C1 = qchisq(0.025, n2 - 1); C2 = qchisq(0.975, n2 - 1)
    Echant2 = rnorm(n, mu2, sigma2)
    Binf2 = (n2 - 1)*var(Echant2)/C2
    Bsup2 = (n2 - 1)*var(Echant2)/C1
    Binf2 < sigma2*sigma2 & Bsup2 > sigma2*sigma2   #TRUE
    
    # Question 4 et 5: 
    M2 = 8000; Binf22 = c(); Bsup22 = c(); count2 = 0
    Binf3 = c(); Bsup3 = c(); count3 = 0
    for(i in 1:M2){
      Echant22 = rnorm(n2, mu2, sigma2)
      Binf22[i] = (n2 - 1)*var(Echant22)/C2
      Bsup22[i] = (n2 - 1)*var(Echant22)/C1
      Binf3[i] = 36*var(Echant22)/59.04
      Bsup3[i] = 36*var(Echant22)/22.67
      if(Binf22[i] < 81 & Bsup22[i] > 81){count2 = count2 + 1} 
      if(Binf3[i] < 81 & Bsup3[i] > 81){count3 = count3 + 1} 
    }
    count2/M2 # 0.951 bon!!!
    count3/M2 # 0.951875 bon!!
    
    # Question 6 : 
    Long1 = mean(Bsup22 - Binf22) # 83.4021
    Long2 = mean(Bsup3 - Binf3) # 79.52122
    #on constate que la longueure moyenne des intevalles de la question 5
    #est plus petite que celle de la qustion 4, la methode de la question 5 
    #donne donc de meilleurs intervalles que celle de la question 4
