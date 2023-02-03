# Exercice 3: Illustration du theoreme central limite
  M = 1000; n =50; N = M*n; x=rbeta(N, 1/2, 1/2)
  # Question 1 et 2 : histogramme b et courbe de densité
  
    mx=min(x); Mx=max(x); brk=seq(mx, Mx, length=101)
    hist(x, probability=TRUE, col='blue', breaks=brk)
    curve(dbeta(x, 0.5, 0.5),col='yellow', lwd=4, add=TRUE)
    
  # Question 3 et 4: Histogramme de z densité de la loi gaussienne
  
    Echant=matrix(x, nrow=1000, byrow=TRUE)
    z = numeric(M)
    for(i in 1:M){
      z[i] = sqrt(n)*(mean(Echant[i,]) - 0.5)/sqrt(1/8)
    }
    hist(z, prob=T, breaks=0.1*(-55:55), xlim=c(-5,5), main='loi beta(0.50.5)')
    curve(dnorm(x), col='red', ldw=2, add=TRUE)
    # Z suit le loi normale centrée reduite
