# Exercice 4
# 1- Loi de X: X suit la loi geometrique de parametre p=1/3
# 2- Simulation et comparaison des frequences
  M=1000; p=1/3
  Simul=rgeom(M, p) + 1
  # Effectifs observes detaille
  Eff=table(Simul)
  Eff.Obs=table(Simul[Simul<15])
  Eff.Obs[15]=M-sum(table(Simul[Simul<15]))
  names(Eff.Obs)=c(1:14,">=15")
  # Frequences Theoriques
  Freq.Theo=c(dgeom((0:13), p), 1-pgeom(13, p))
  # Frequences Observees
  Freq.Obs=Eff.Obs/M
  # les frequences sont presques les memes
# 3- Diagramme en baton
  Realisation=matrix(c(Freq.Obs, Freq.Theo), nrow=2, byrow=TRUE)
  barplot(Realisation, beside=TRUE, col=c('red', 'yellow'))
# 4- Comparaison des moyennes
  Moy.Theo=sum((0:55 + 1)*dgeom(0:55, 1/3))
  Moy.Obs=mean(Simul)
  ecart=Moy.Theo-Moy.Obs
  # Les moyennes sont sensiblement egales
