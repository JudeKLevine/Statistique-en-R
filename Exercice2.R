# Exercice 2
# 1- loi de X : X suit la loi binomiale de parametre n = 5 et p = 1/3
# 2- Simulation et comparaison des frequences
  M=1000; n=5; p=1/3
  Simul=rbinom(M,n,p)
  # Effectif
  Eff=table(Simul)
  # Frequences Theoriques
  Freq.Theo=dbinom((0:5), n, p)
  # Frequences Observees
  Freq.Obs=Eff/M
  # elles sont sensiblement egales
# 3- Diagramme à batons
  Matrice = matrix(c(Freq.Theo, Freq.Obs), nrow=2, byrow=TRUE)
  barplot(Matrice, beside=TRUE, col=c('red', 'yellow'))
# 4- Comparaison des moyennes
  Moy.Theo=sum((0:5)*Freq.Theo)
  Moy.Obs=mean(Simul)
  (ecart=Moy.Theo-Moy.Obs)
  #la difference tend vers 0, les moyennes sont sensiblement egales
