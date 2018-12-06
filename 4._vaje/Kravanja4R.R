library(readr)
library(dplyr)
#1.naloga
#a.)
tabela <- read_csv("srebro.csv")

tabela <- tabela[0:123,]
tabela <- tabela[123:0,]

#b.)

casovna_vrsta <- ts(tabela$Close)
casovna_vrsta <- as.numeric(gsub("\\$", "", casovna_vrsta))


graf <- ts.plot(casovna_vrsta,xlab="Èas",
                ylab="Vrednost v €", 
                main="Vrednost zlata") + points(casovna_vrsta,cex = 0.5)

#2.naloga:GLAJENJE Z DRSEÈIM POVPREÈJEM REDA K
#a.)
G <- function(vrsta,k){
    konec <- length(vrsta)
    ze_zglajene <- c()
    for (i in (1+k):(konec+1)){
      ze_zglajene[i] <- sum(vrsta[(i-1):(i-k)])/k
      }
    return(ze_zglajene)
}

#b.)
napoved1 <- G(casovna_vrsta,5)
napoved_naprej <- function(vrsta,k){
  konec <- length(vrsta)
  return(sum(vrsta[(konec-k+1):konec])/k)
}
napovedovanje<- c(napoved1, napoved_naprej(casovna_vrsta,5))

#c.)

zglajen_graf <- ts.plot(ts(napoved1),ts(casovna_vrsta),
                        xlab='Èas', ylab ='Vrednost v €',
                        main = 'Drseèe povpreèje reda 5', 
                        col=c('orange', 'black'),lwd = 2)

#d.) 
skn <- function(vrsta,k){
  konec <- length(vrsta)
  napaka <- 0
  for (i in (k):(konec-1)){
    napaka <- napaka + (vrsta[i+1] - G(vrsta,k)[i+1])^2
  }
  return(napaka/(konec - k))
}

#e.)
napoved15 <- G(casovna_vrsta,15)
napovedovanje15<- c(napoved15, napoved_naprej(casovna_vrsta,15))
napoved30 <- G(casovna_vrsta,30)
napovedovanje30<- c(napoved30, napoved_naprej(casovna_vrsta,30))

zglajen_graf2 <- ts.plot(ts(napoved15),ts(casovna_vrsta),ts(napoved30),
                        xlab='Èas', ylab ='Vrednost v €',
                        main = 'Drseèe povpreèje reda 15,30', 
                        col=c('orange', 'black','green'),lwd = 2)
srednja_kvadraticna_15 <- skn(casovna_vrsta,15)
srednja_kvadraticna_15 <- skn(casovna_vrsta,30)


#3.naloga: EKSPONENTNO GLAJENJE
#3.a)
EG <- function(vrsta,alpha){
  konec <- length(vrsta)
  zglajene_vrednosti <- vrsta[1]
  for (i in 2:konec){
    zglajene_vrednosti[i] <- alpha * vrsta[i] +(1-alpha)*zglajene_vrednosti[i-1]
  }
  zglajena_vrsta <- ts(zglajene_vrednosti)
  return(zglajena_vrsta)
}

#b.)
eks <-ts(EG(casovna_vrsta,0.3))
zglajen_graf3 <- ts.plot(eks,ts(casovna_vrsta), 
                         col=c('blue','red'),
                         xlab="Äasovno zaporedje",
                         ylab="cene",
                         main="Graf èasovne vrste ") +
  points(casovna_vrsta, cex=0.7) 
legend("topright",c("Èasovna vrsta", "zglajena vrsta ekspon. glajenja"), lty=1:1, col=c("red", "blue"))
naslednji<- tail(eks,n=1)

#c.)
skn_e <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina-1))
}

opt_alpha <- optimize(skn_e, c(0,1), vrsta = casovna_vrsta)


#d.)
eks2 <-ts(EG(casovna_vrsta,opt_alpha$minimum))

zglajen_graf4 <- ts.plot(eks2,ts(casovna_vrsta), 
                         col=c("green", "red"),
                         xlab="Èasovno zaporedje",
                         ylab="cene",
                         main="Graf èasovne vrste 5",
                         lwd=2:1) +
  points(casovna_vrsta, cex=0.5) 
legend("topright",c("Èasovna vrsta", "zglajena vrsta optimizirana"),lty=1:1, lwd=2:1, col=c("green", "red"))


