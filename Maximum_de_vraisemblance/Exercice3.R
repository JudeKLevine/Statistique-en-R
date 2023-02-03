# Exercice 3 : illustrastion de la loi forte des grands nombres
# 1- loi uniforme
  n=10000; theta=2
  U=runif(n, 0,theta)
  Test = cumsum(U)/(1:n)
  x=1:n
  plot(Test, col='red', cex=0.2)
  lines(1:n, rep(theta/2, n), col='black')

# 2- loi de poisson
  lambda=4; # E[X]=lambda
  variable=rpois(n,lambda)
  Test=cumsum(variable)/(1:n)
  plot(Test, col='red', cex=0.2)
  lines((1:n), rep(lambda, n), col='black')
  # on a bien la convergence de la suite moyenne vers l'esperance
# Exercice 3 : illustrastion de la loi forte des grands nombres
# 1- loi uniforme
  n=10000; theta=2
  U=runif(n, 0,theta)
  Test = cumsum(U)/(1:n)
  x=1:n
  plot(Test, col='red', cex=0.2)
  lines(1:n, rep(theta/2, n), col='black')

# 2- loi de poisson
  lambda=4; # E[X]=lambda
  variable=rpois(n,lambda)
  Test=cumsum(variable)/(1:n)
  plot(Test, col='red', cex=0.2)
  lines((1:n), rep(lambda, n), col='black')
  # on a bien la convergence de la suite moyenne vers l'esperance
