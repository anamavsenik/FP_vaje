require(graphics)
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
sl <- locale(encoding = "Windows-1250", decimal_mark = ",", grouping_mark = ".")

tabela2008 <- read_csv2("Copy of hist_EURIBOR_2008.csv",locale = sl, trim_ws = TRUE,
                        na = c("-", ""))
nova2008 <- tabela2008[,c("X1", "2.01.2008","1.02.2008","3.03.2008","1.04.2008","2.05.2008","2.06.2008","1.07.2008","1.08.2008","1.09.2008","1.10.2008","3.11.2008","1.12.2008")]
nova2008 <- t(nova2008[1:15,])
imena <- as.character(unlist(nova2008[1,]))
nova2008 <- nova2008[-1,]
colnames(nova2008) <- imena

tabela2009 <- read_csv2("Copy of hist_EURIBOR_2009.csv",locale = sl, trim_ws = TRUE,
                        na = c("-", ""))
#View(tabela2009)
#View(nova2008)
nova2009 <- tabela2009[,c("2.01.2009","2.02.2009","2.03.2009","1.04.2009","4.05.2009","1.06.2009","1.07.2009","3.08.2009","1.09.2009","1.10.2009","2.11.2009","1.12.2009")]
nova2009 <- t(nova2009[1:15,])

tabela2010 <- read_csv2("Copy of hist_EURIBOR_2010.csv",locale = sl, trim_ws = TRUE,
                        na = c("-", ""))
#View(tabela2010)
#View(nova2009)
nova2010 <- tabela2010[,c("04/01/2010","01/02/2010","01/03/2010","01/04/2010","03/05/2010","01/06/2010","01/07/2010","02/08/2010","01/09/2010","01/10/2010","01/11/2010","01/12/2010")]
nova2010 <- t(nova2010[1:15,])
#View(nova2010)

zdruzeno <-rbind(nova2008,nova2009,nova2010) %>%
  subset(elect=c(1,2,4,5,6,9,12,15))%>%
  as.data.frame()
zdruzeno <- zdruzeno[,c(9,15)]
View(zdruzeno)

zdruzeno[] <- lapply(zdruzeno,as.character) %>% lapply(as.numeric)


graf_6m <- ts(zdruzeno$'6m', start=c(2008,1),frequency =12)
graf_12m <- ts(zdruzeno$'12m',start=c(2008,1),frequency = 12)

ts.plot(zdruzeno , xlab='dan',ylab='obrestna mera (%)', main = 'Euribor 2008-2010', col=c('green','red'), lwd=2)
legend("topright",legend=c('6m', '9m'), col=c('green','red'),lty=1:1,cex=1.0)



#2.naloga


