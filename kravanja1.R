library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(tidyr)
library(actuar)
library(combinat)
library(shiny)

#1.NALOGA
tabela2008 <- read.csv2("Copy of hist_EURIBOR_2008.csv",header = TRUE, sep = ";", quote = "\"", dec = ",", nrows = 15,row.names = 1,
                        fill = TRUE, comment.char = "",na = c("-", ""))
nova2008 <- tabela2008[,c( "X2.01.2008","X1.02.2008","X3.03.2008","X1.04.2008","X2.05.2008","X2.06.2008","X1.07.2008","X1.08.2008","X1.09.2008","X1.10.2008","X3.11.2008","X1.12.2008")]
nova2008 <- t(nova2008[1:15,])

tabela2009 <- read.csv2("Copy of hist_EURIBOR_2009.csv",sep = ";", quote = "\"", dec = ",", nrows = 15,
                        fill = TRUE,na = c("-", ""))

nova2009 <- tabela2009[,c("X2.01.2009","X2.02.2009","X2.03.2009","X1.04.2009","X4.05.2009","X1.06.2009","X1.07.2009","X3.08.2009","X1.09.2009","X1.10.2009","X2.11.2009","X1.12.2009")]
nova2009 <- t(nova2009[1:15,])

tabela2010 <- read.csv2("Copy of hist_EURIBOR_2010.csv",sep = ";", quote = "\"", dec = ",", nrows = 15,
                        fill = TRUE,na = c("-", ""))

nova2010 <- tabela2010[,c("X04.01.2010","X01.02.2010","X01.03.2010","X01.04.2010","X03.05.2010","X01.06.2010","X01.07.2010","X02.08.2010","X01.09.2010","X01.10.2010","X01.11.2010","X01.12.2010")]
nova2010 <- t(nova2010[1:15,])


zdruzeno<-rbind(nova2008,nova2009,nova2010) %>%
  subset(elect=c(1,2,4,5,6,9,12,15))%>%
  as.data.frame()
#zdruzeno <- zdruzeno[,c(9,15)]
View(zdruzeno)

zdruzeno[] <- lapply(zdruzeno,as.character) %>% lapply(as.numeric)

graf_6m <- ts(zdruzeno$'9', start=c(2008,1),frequency =12)
graf_12m <- ts(zdruzeno$'15',start=c(2008,1),frequency = 12)

ts.plot(graf_6m ,graf_12m, xlab='obdobje',ylab='obrestna mera (%)', main = 'Euribor 2008-2010', col=c('green','red'), lwd=2)
legend("topright",legend=c('6m', '9m'), col=c('green','red'),lty=1:1,cex=1.0)



#2.naloga
#a.) Izbrala sem si datume : 1.10.2008, 2.3.2009 in 1.4.2010

#b.) 
sesti_mesec <- zdruzeno$'6m'
dvanajsti_mesec <- zdruzeno$'12m'

zdruzeno <- t(zdruzeno)
obrestna_mera <- as.data.frame(zdruzeno) %>%
  subset(select=c('X1.10.2008', 'X2.03.2009', 'X01.04.2010'))

obrestna_mera$dospetje <- c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,11,12)
obrestna_mera <- melt(obrestna_mera,id.vars='dospetje',variable.name = 'datum', value.name = 'obrestna_mera' )

drugi_graf <- ggplot(obrestna_mera, aes(x=dospetje, y=obrestna_mera,group=datum, color = datum)) +
      geom_line() + geom_point() +
  labs(title='Èasovna struktura obrestnih mer', y='obrestna mera (%)',x='Dospetje(v mesecih)')

#opazke: Pri vseh grafih je obrestna mera za daljša obdobja višja kot za krajša. 
# Krivulji za leto 2008 in 2009 sta konkavni, medtem ko je za leto 2010 krivulja konveksna. 
#Za tedenska obdobja je strmina seveda veèja, za meseèna manjša oziroma v nekaterih delih skoraj ravna. 
#Z leti so se obresne mere, sodeè po grafu, nižale.


#3.naloga

#3a) Terminske obrestne mere tipa TxU
terminske_obr_mere <- subset(t(zdruzeno), select = c('6m','12m'))
napovedi<- ((1/(1-1/12))*(((1+1*dvanajsti_mesec/100)/(1+0.5*sesti_mesec/100))-1))*100
tabela <-cbind(terminske_obr_mere,napovedi)


#3b) Terminska obrestna mera meseca 12-6= 6 
napovedi2 <- c(c(NA, NA, NA, NA, NA, NA), napovedi[-c(31:36)])
tabela2 <-cbind(terminske_obr_mere,napovedi2)

#3c) Razsevni grafikon, regresijska premica in simetrala kvadrantov




