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
View(casovna_vrsta)

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



