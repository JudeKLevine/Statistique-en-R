# Exercice 1 : Estimateurs
  theta = 2
  # 2 - Simulation
    x=rchisq(500,1)/theta
    f = function(x){1/sqrt(pi*x)*exp(-x)}
    hist(x, probability=TRUE, breaks=50)
    curve(f(x),0.0000001, 6, add=T, type='l', lwd=2)
  # 3 - Simulations estimateurs
    M = 5000; n = 10
    E1 = c(); E2 = c(); E3 = c(); E4 = c()
    for(i in 1:M){
      Echant = rchisq(n, 1)/theta
      E1[i] = sqrt(3*n/(2*sum(Echant*Echant)))
      E2[i] = sqrt(8/(9*var(Echant)))
      E3[i] = n/sum(Echant)
      E4[i] = (n - 2)/sum(Echant)
    }
    # les estimateurs biaisés
    mean(E1) # 2.016286
    mean(E2) # 2.030236
    mean(E3) # 2.522167, il est biaisé
    mean(E4) # 2.017734

    # Teste de la stabilité
    var(E1) # 1.36113     ils ne sont pas tres
    var(E2) # 1.721824,   stables car leurs variances 
    var(E3) # 1.890824    sont assez grandes
    
# Exercice 2 : Intervalle de confiance
    M = 1000; n2 = 200; Binf1 = c(); Bsup1 = c(); Cq = qnorm(0.95)
    C1 = qchisq(0.05, n2 - 1); C2 = qchisq(0.95, n2 - 1); Binf2 = c() 
    Bsup2 = c(); Binf3 = c(); Bsup3 = c(); Binf4 = c(); Bsup4 = c()
    Binf5 = c(); Bsup5 = c();
    c1 = 0; c2 = 0; c3 = 0; c4 = 0
    for(i in 1:M){
      Echant = rchisq(n2, 1)/theta
      Binf1[i] = 1/mean(Echant)*(1 - sqrt(2/n2)*Cq)
      Bsup1[i] = 1/mean(Echant)*(1 + sqrt(2/n2)*Cq)
      if(Binf1[i] <= theta & Bsup1[i] >= theta){c1 = c1 + 1}
      Binf2[i] = C1/(sum(Echant))
      Bsup2[i] = C2/(sum(Echant))
      if(Binf2[i] <= theta & Bsup2[i] >= theta){c2 = c2 + 1}
      Binf3[i] = 1/(mean(Echant)*(1 + Cq*sqrt(2/n2)))
      Bsup3[i] = 1/(mean(Echant)*(1 - Cq*sqrt(2/n2)))
      if(Binf3[i] <= theta & Bsup3[i] >= theta){c3 = c3 + 1}
      Binf4[i] = sqrt(2/(var(Echant) + Cq*mean(Echant)*mean(Echant)*sqrt(56/n2)))
      Bsup4[i] = sqrt(2/(var(Echant) - Cq*mean(Echant)*mean(Echant)*sqrt(56/n2)))
      if(Binf4[i] <= theta & Bsup4[i] >= theta){c4 = c4 + 1}
    }
  c1/M # 0.891
  c2/M # 0.889
  c3/M # 0.895
  c4/M # 0.898
  
  mean(Bsup1 - Binf1) #  0.663232
  mean(Bsup2 - Binf2) #  0.6607792
  mean(Bsup3 - Binf3) #  0.681675
  mean(Bsup4 - Binf4) #  1.103905
