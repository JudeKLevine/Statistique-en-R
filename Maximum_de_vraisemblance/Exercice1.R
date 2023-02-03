# Exercice 1:
# 1- Calcul de la moyenne, de la variance et representation graphique
M=5000; n=15; mu=-10; sigma=1.5
Realisation=matrix(ncol=n, nrow=M)
Moyenne = c()
Variance = c()
for (i in 1:M){
  Realisation[i,]=rnorm(n, mu, sigma)
  Moyenne[i] = mean(Realisation[i, ])
  Variance[i] = var(Realisation[i,])
}

hist(Moyenne, col='red', breaks=50, probability = TRUE)
x = seq(-12, -8, length=500); y=dnorm(x, mu, sigma/sqrt(n))
lines(x, y, col='yellow', lwd=4)
# la variable Moyenne suit la loi normale de parametre
# sigma_Moy = sigma/sqrt(n)  et mu_Moy = mu
# 2- calcul des proportions
A = Moyenne[Moyenne <= -9.5 & Moyenne >= -10.5]
B = Variance[Variance <= 4 & Variance >= 2]
C = Variance[Variance <= 4 & Variance >= 2 & Moyenne <= -9.5 & Moyenne >= -10.5]
(P_A = length(A)/M); (P_B = length(B)/M); (P_C = length(C)/M)
# 3- Calcul des probabilites
prob_A = pnorm(-9.5, mu, sqrt(sigma^2/n))- pnorm(-10.5, mu, sigma/sqrt(n))
prob_B = P_B; prob_C = P_C; (prob_A_B = prob_A*prob_B)
# les evenments sont independants
# 4- Coefficient de correlation entre Xbar et Sc²
cor(Moyenne, Variance)

# 5- On definit Ts
Ts = sqrt(n)*(Moyenne + 10)/sqrt(Variance)
hist(Ts, col='red', breaks=50)
# Ts suit une loi normale
# 6- On definit la variable
K_K=(n - 1)*Variance/(sigma*sigma)
hist(K_K, col='blue', breaks=50)
# le graphique ressemble plus ou moins à celui de la densite d'une 
# loi normale
