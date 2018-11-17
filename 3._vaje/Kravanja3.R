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

#b.)
N1 <- 10
N2 <- 100
N3 <- 1000 

vrednostN1 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N1)
vrednostN2 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N2)
vrednostN3 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N3)
