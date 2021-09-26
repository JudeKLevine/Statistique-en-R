# Exercice 1
  # 1- On peche simultanement 3 poissons parmi 10, les possibilites sont :
  combn(10, 3)
# 2- calcul de probabilite
  # K  = 4 poissons marques parmi les 10, 
  # P(k = 2) = (Combinaison(4,2) * combinaison(6,1)) / combinaison(10,3)
  Proba=(choose(4,2)*choose(6,1))/choose(10,3) 
# 3- Simulation
  M=500; N=10; K=4; k=3
  Simul=rhyper(M, K, N-K, k)
  # Effectifs observes
  Effec=table(Simul)
  # Frequence theorique
  Freq.Theo=dhyper(0:3,K,N-K, k)
  # Frequence Observe
  Freq.Obs=Effec/M
# 4- Graphique
  Matrice=matrix(c(Freq.Obs, Freq.Theo), nrow=2, byrow=TRUE)
  barplot(
    Matrice, beside=TRUE, col=c('red','black')
  )
# 5- Comparaison des moyennes 
  Moy.Theo=sum(Freq.Theo*(0:3))
  Moy.Obs=mean(Simul)
  ecart = Moy.Theo-Moy.Obs
  
