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
tabela2008 <- read_csv2("Copy of hist_EURIBOR_2008.csv") %>%
  subset(select = c("X1", "2.01.2008","1.02.2008","3.03.2008","1.04.2008","2.05.2008","2.06.2008","1.07.2008","1.08.2008","1.09.2008","1.10.2008","3.11.2008","1.12.2008")) %>%
  t()
imena <- as.character(unlist(tabela2008[1,]))
tabela2008 <- tabela2008[-1,]
colnames(tabela2008) <- imena
#View(tabela2008)

tabela2009 <- read_csv2("Copy of hist_EURIBOR_2009.csv") %>% 
  subset(select= c("2.01.2009","2.02.2009","2.03.2009","1.04.2009","4.05.2009","1.06.2009","1.07.2009","3.08.2009","1.09.2009","1.10.2009","2.11.2009","1.12.2009"))
  t()
tabela2009 <- tabela2009[-1,]
#View(tabela2009)

tabela2010 <- read_csv2("Copy of hist_EURIBOR_2010.csv") %>% 
  subset(select=c("2.01.2009","2.02.2009","2.03.2009","1.04.2009","4.05.2009","1.06.2009","1.07.2009","3.08.2009","1.09.2009","1.10.2009","2.11.2009","1.12.2009"))
  t()
tabela2010 <- tabela2010[-1,]
#View(tabela2010)

zdruzeno <- rbind(tabela2008,tabela2009,tabela2010) 
ocisceno <- zdruzeno[,c(9,15)]
#ocisceno[] <- lapply(ocisceno,as.character) %>% lapply(as.numeric)

#graf_6m <- ts(ocisceno$'6m', start=c(2008,1),frequency =12)
#graf_12m <- ts(ocisceno$'12m',start=c(2008,1),frequency = 12)

ts.plot(ocisceno , xlab='dan',ylab='obrestna mera (%)', main = 'Euribor 2008-2010', col=c('green','red'), lwd=2)
legend("topright",legend=c('6m', '9m'), col=c('green','red'),lty=1:1,cex=1.0)


#2.naloga


