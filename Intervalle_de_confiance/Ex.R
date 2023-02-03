# install.packages("MASS")
library(MASS)

n = 28; Population = SP500
summary(SP500); length(SP500)
hist(SP500, breaks=500, prob=T)Exercice_1.R

Echantillon = sample(Population, 28, replace=T)
# intarvalle de confiance

Cq = qt(0.95, 27)
Binf = mean(Echantillon) - Cq * var(Echantillon)/sqrt(n)
Bsup = mean(Echantillon) + Cq * var(Echantillon)/sqrt(n)

Binf < mean(Population) & mean(Population) < Bsup 
# La moyenne est bien dans l'intervalle de confiance
#Inter_conf = matrix(nrow = 100, ncol = 2)
Binf=c(); Bsup=c(); M=1000
compteur = 0?barplot
for(i in 1:M){ 
  Echantillon = sample(Population, 28, replace=T)
  Cq = qt(0.975, 27)
  Binf[i] = mean(Echantillon) - Cq * var(Echantillon)/sqrt(n)
  Bsup[i] = mean(Echantillon) + Cq * var(Echantillon)/sqrt(n)
  #Inter_conf[i,1] = mean(Echantillon) - Cq * var(Echantillon)/sqrt(n)
  #Inter_conf[i,2] = mean(Echantillon) + Cq * var(Echantillon)/sqrt(n)
  if(mean(Population) <= Bsup[i] & mean(Population) >= Binf[i]){ compteur = compteur + 1}
}
compteur/M
#mean(Inter_conf[i,1]) < mean(Population) & mean(Population) > mean(Inter_conf)
#barplot(Inter_conf, beside=TRUE, type='l')

# variance
Vinf=c(); Vsup=c(); M=100; n=15
C1 = qchisq(0.05, n-1); C2 = qchisq(0.95, n-1)
compteur = 0
for(i in 1:M){ 
  Echantillon = sample(Population, 28, replace=T)
  Cq = qt(0.975, 27)
  Vinf[i] = (n-1)*var(Echantillon) /C2
  Bsup[i] = (n-1)*var(Echantillon) /C1
  #Inter_conf[i,1] = mean(Echantillon) - Cq * var(Echantillon)/sqrt(n)
  #Inter_conf[i,2] = mean(Echantillon) + Cq * var(Echantillon)/sqrt(n)
  if(var(Population) <= Bsup[i] & var(Population) >= Binf[i]){ compteur = compteur + 1}
}
compteur/M
plot(Vinf, Vsup, type='l')
abline(h=0.5, col='red')
