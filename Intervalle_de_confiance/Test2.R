# Exercice 2 : Loi gaussienne
  # Question 1 et 2
  M = 1000; mu = -10; n = 20; sigma = 1.5; Binf = c(); Bsup = c()
  Xbar = c(); Sc = c(); count = 0; Cq = qt(0.95, n-1)
  
  # definir le cadre du graphique type="n": sans graphique
  plot(range(c(mu-1.5*sigma,mu+1.5*sigma)), range(c(0,M)), type="n", 
  xlab="", ylab="" ) 
  abline(v=mu, col="red") # tracer le seqment Vertical representant 
  
  for(i in 1:M){
    Echant = rnorm(n, mu, sigma)
    Xbar[i] = mean(Echant)
    Sc[i] = var(Echant)
    Binf[i] = Xbar[i] - sqrt(Sc[i])*Cq/sqrt(n)
    Bsup[i] = Xbar[i] + sqrt(Sc[i])*Cq/sqrt(n)
    if(Binf[i] <= mu & Bsup[i] >= mu){count = count + 1}
    lines(c(Binf[i],Bsup[i]), c(i,i), col="blue") # trace les intervalles
  }
  count/M  #0.91 assez proche de 1-alpha
  
  # Question 3, 4 et 5: intervalle de confiance de la variance
  C1 = qchisq(0.025, n-1); C2 = qchisq(0.975, n-1)
  C11 = qchisq(0.04461, n-1); C22 = qchisq(1-0.00539, n-1)
  Binf_var1 = c(); Bsup_var1 = c(); count_var1 = 0
  Binf_var2 = c(); Bsup_var2 = c(); count_var2 = 0
  c_lg = 0
  for(i in 1:M){
    Echant = rnorm(n, mu, sigma)
    Sc[i] = var(Echant)
    Binf_var1[i] = (n - 1)*Sc[i]/C2
    Bsup_var1[i] = (n - 1)*Sc[i]/C1
    Binf_var2[i] = (n - 1)*Sc[i]/C22
    Bsup_var2[i] = (n - 1)*Sc[i]/C11
    if(Binf_var2[i] <= sigma & Bsup_var2[i] >= sigma){count_var2 = count_var2 + 1}
    if(Binf_var1[i] <= sigma & Bsup_var1[i] >= sigma){count_var1 = count_var1 + 1}
    if(Bsup_var2[i] - Binf_var2[i] < Bsup_var1[i] - Binf_var1[i]){c_lg = c_lg + 1}
    # les differences des longueurs des intervalles
  }
  count_var1/M # 0.79
  count_var2/M # 0.9, assez proche de 0.95
  c_lg/M # 1 les les longueurs des ces intervalles sont plus petites que  
         #   celles avec les quantiles alpha/2
