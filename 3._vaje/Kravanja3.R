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
monte <- function(S_0,u,d,R,t,W,type, N){
  q = (1+R-d) / (u-d)
  binomskaf <- matrix(rbinom(N*t,1,q),nrow = N, ncol = t)
  vS_0 <- rep(S_0, N)
  nova_binomskaf <- cbind(vS_0,u^binomskaf * d^(1-binomskaf))
  produkt <- t(apply(nova_binomskaf,1, cumprod))
  izplacilo_vrstice <- apply(produkt, 1, izplacilo, W = W, type = type)
  Q <- q^rowSums(binomskaf) * (1-q)^(t - rowSums(binomskaf) )
  E_Q <- sum(izplacilo_vrstice) / N
  premija <- E_Q / (1+R)^t
  return (premija)
}

S_0 <- 60
u <- 1.05
d <- 0.95
R <- 0.01
t <- 15
W <- rep(1,16)
type <- "put"
N1 <- 10
N2 <- 100
N3 <- 1000

bin <- binomski(S_0,u,d,R,t,W,type)

#3. naloga
#3.a
M = 100
simulacija1 <- c()
for(i in 1:M ){
  simulacija1 <- c(simulacija1,monte(S_0,u,d,R,t,W,type, N1))
}

simulacija2 <- c()
for(i in 1:M ){
  simulacija2 <- c(simulacija2,monte(S_0,u,d,R,t,W,type, N2))
}

simulacija3 <- c()
for(i in 1:M ){
  simulacija3 <- c(simulacija3,monte(S_0,u,d,R,t,W,type, N3))
}

hist(simulacija1,col="yellow",main="histogram simulacije 1",
     xlab="porazdelitev ocen premije",ylab= "pogostost")
hist(simulacija2,col="green",main="histogram simulacije 2",
     xlab="porazdelitev ocen premije",ylab= "pogostost")
hist(simulacija3,col="blue",main="histogram simulacije 3",
     xlab="porazdelitev ocen premije",ylab= "pogostost")


#3.b
hist(simulacija1)
abline(v = c(mean(simulacija1),bin ),col="red")
arrows(mean(simulacija1) , 0, mean(simulacija1) + sd(simulacija1),0) #sd raèuna standardni odklon
arrows(mean(simulacija1) , 0, mean(simulacija1) - sd(simulacija1),0)
