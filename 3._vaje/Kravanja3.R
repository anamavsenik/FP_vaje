library(combinat)
library(Rlab)

#1.naloga
S0<-50
u<-1.05
d<-0.95
T<-5
R <-0.03
W<-c(1,2,3,4,5,6)


#a.)
pot1 <-c(50.00,52.50,49.88,47.38,45.01,47.26)
pot2 <-c(50.00,52.50,55.12,57.88,60.78,63.81)
pot3 <- c(50.00,47.50,49.88,47.38,45.01,42.76)
pot4 <- c(50.00,47.50,45.12,47.38,45.01,47.26)
pot5 <-c(50.00,52.50,49.88,52.37,54.99,52.24)

K1<-sum(pot1*W)/sum(W)
K2<-sum(pot2*W)/sum(W)
K3<-sum(pot3*W)/sum(W)
K4 <-sum(pot4*W)/sum(W)
K5<-sum(pot5*W)/sum(W)

izplaciloX<-c(max(47.26-K1,0),max(63.81-K2,0),max(42.76-K3,0),max(47.26-K4,0),max(52.24-K5,0))
izplaciloY <- c(max(-47.26+K1,0),max(-63.81+K2,0),max(42.76-K3,0),max(-47.26+K4,0),max(-52.24+K5,0))

#b.) 
izplacilo <- function(vrsta,W,type){
  if (type == 'call'){
    m <- max(tail(vrsta,n=1)-(sum(vrsta*W)/sum(W)),0)
    return(m)
  } else if (type == 'put'){
    m <- max(-tail(vrsta,n=1)+(sum(vrsta*W)/sum(W)),0)
    return(m)
  }
}

#2.naloga
#a.)
binomski <- function(S0,u,d,R,T,W,type){
  q<-(1+R-d)/(u-d)
  ponovitev <- (rep(2,T))
  kocka <- hcube(ponovitev)
  gor <- kocka-1
  dol <- 2-kocka
  matrika <- u^gor*d^dol
  zacetek <- rep(S0,nrow(matrika))
  skupaj <- cbind(zacetek,matrika)
  kumulativni_produkt <-t(apply(skupaj,1,cumprod))
  premija <- apply(kumulativni_produkt,1,izplacilo, W=W, type=type)
  Q <- q^(rowSums(gor))*(1-q)^(T-rowSums(gor))
  E <- sum(Q*premija)/(1+R)^T
  return(E)
}

#b.) simulacija N poti cene delnic z M.C.metodo
monte <- function(S0,u,d,R,T,W,type, N){
  q <- (1+R-d)/(u-d)
  binfunkcija <- matrix(rbinom(N*T,1,q),nrow = N, ncol = T)
  zacetni <- rep(S0, nrow(binfunkcija))
  matrika <- u^(binfunkcija)*d^(1-binfunkcija)
  skupaj <- cbind(zacetni,matrika)
  kumulativni_produkti <- t(apply(skupaj, 1, cumprod))
  X <- apply(kumulativni_produkti, 1, izplacilo, W=W, type=type)
  Q <- q^(rowSums(binfunkcija))*(1-q)^(T-rowSums(binfunkcija))
  prièakovano <- sum(X)/N
  E <- prièakovano/(1+R)^T
  return(E)
  
}

#dani podatki:
S0 = 60
u = 1.05
d =0.95
R=0.01
T =15
W = rep(1,16)
type = "put"
N1=10
N2=100
N3=1000

#3.a) doloèimo 100 ponovitev za vsak N in v b delu narišemo histograme, za vsako simulacijo posebej ustvarimo vektor vrednosti
M=100

sim1 <- c()
sim2 <- c()
sim3 <- c()

for(i in 1:M){
  sim1 <- c(sim1,monte(S0,u,d,R,T,W,type, N1))
  sim2 <- c(sim2,monte(S0,u,d,R,T,W,type, N2))
  sim3 <- c(sim3,monte(S0,u,d,R,T,W,type, N3))
}


#3.b) na vse tri histograme dodamo navpiènice, ki oznaèujejo obe povpreèji in pušèice standardnega odklona

povprecje0 <- binomski(S0,u,d,R,T,W,type)
povprecje1 <- mean(sim1)
povprecje2 <- mean(sim2)
povprecje3 <- mean(sim3)
odklon1 <- sd(sim1)
odklon2 <- sd(sim2)
odklon3 <- sd(sim3)

hist(sim1, col="red", main="Histogram prve porazdelitve N=10",
     xlim=c(0,4),
     xlab="cena premije",
     ylab="gostota v porazdelitvi")
abline(v=c(povprecje0, povprecje1), col=c("yellow", "blue"), lty=c(1,2), lwd=c(2,3))
arrows(povprecje1, 0, c(povprecje1 - odklon1, povprecje1 + odklon1), 0, col="green", lwd=3)
legend("topright", legend=c("binomsko", "monte carlo", "standardni odklon"), col=c("yellow","blue", "green"),lty=c(1,2,1), lwd=c(2,3,3))

hist(sim2, col="blue", main="Histogram druge porazdelitve N=100",
     xlim=c(0,4),
     xlab="cena premije",
     ylab="gostota v porazdelitvi")
abline(v=c(povprecje0, povprecje2), col=c("yellow", "red"), lty=c(1,2), lwd=c(2,3))
arrows(povprecje2, 0, c(povprecje2 - odklon2, povprecje2 + odklon2), 0, col="green", lwd=3)
legend("topright", legend=c("binomsko", "monte carlo", "standardni odklon"), col=c("yellow","red", "green"),lty=c(1,2,1), lwd=c(2,3,3))

hist(sim3, col="green", main="Histogram tretje porazdelitve N=1000",
     xlim=c(0,4),
     xlab="cena premije",
     ylab="gostota v porazdelitvi")
abline(v=c(povprecje0, povprecje3), col=c("blue", "red"), lty=c(1,2), lwd=c(2,3))
arrows(povprecje3, 0, c(povprecje3 - odklon3, povprecje3 + odklon3), 0, col="yellow", lwd=3)
legend("topright", legend=c("binomsko", "monte carlo", "standardni odklon"), col=c("blue","red", "yellow"),lty=c(1,2,1), lwd=c(2,3,3))


