library(actuar)

#KOMULATIVNI MODEL TVEGANJA IN PANGERJEV ALGORITEM

# 1.)
#a.)

vzorec <- read.delim("vzorec2.txt",header = FALSE)

hist(vzorec$V1, main="histogram",
     xlab="vzorec",ylab= "pogostost pojavljanja",
     border="green", col="yellow")



#b.) 
parametri <- mde(vzorec$V1,fun = pweibull, 
                 start = list(shape= 1.5, scale= 1),
                 measure = 'CvM')

#c.)
k <- as.numeric(parametri$estimate[1])
lambda <- as.numeric(parametri$estimate[2])

hist(vzorec$V1, main="histogram",
     xlab="vzorec",ylab= "pogostost pojavljanja",
     border="green", col="yellow")

curve(dweibull(x, shape=k, scale = lambda),
      from = 0, 
      to = 7,
      col = "red",
      lwd = 2)

plot(ecdf(vzorec$V1),
     main = 'Porazdelitvena funkcija odškodnin',
     ylab = 'porazdelitvena funkcija',
     xlab = 'višina odškodnin')
 
curve(pweibull(x, shape=k, scale = lambda),
      from = 0, 
      to = 7, 
      add = TRUE,
      col = "red",
      lwd = 2) #komulativna je pweibull

#d.) (porazdelitev števila odškodninskih zahtevkov je porazdeljena binomsko)
n <-20
p <- 1/2
upanjey <- lambda * gamma(1 + (1/k))
upanjen <- n*p
upanjeS <- upanjey * upanjen

variancay <- lambda^2 * (gamma(1+ (2/k)) - gamma (1+ (1/k))^2)
variancan <- n*p* (1-p)
upanjeyna2 <- varianca_y + upanjey^2
variancaS <- variancay * upanjen + variancan * upanjeyna2

# 2.) PANJERJEV ALGORITEM
# a.)
h <- 0.25
n <- 38
diskretno <- discretize(pweibull (x,shape=k, scale = lambda),
                        step = 0.25,
                        from = 0,
                        to = 10,
                        method = 'rounding')

#b.) 
graf1 <- curve(dweibull(x, shape=k, scale = lambda),
              from = 0, 
              to = 10,
              main = 'Weibullova porazdelitev',
              ylab = 'porazdelitvena funkcija',
              lwd = 2,
              col= 'orange')

plot(stepfun(seq.int(0,n*h,by=h),y = diskretno), 
     add=TRUE)
legend("topright", legend=c('diskretizacija', 'Weibullova porazdelitev'), col=c('black','orange'),lty=1:1, cex=0.7)

#c.) porazdelitvena funkcija komulativne škode S
  
FS <- aggregateDist(method = 'recursive',
                    model.freq = 'poisson',
                    model.sev = diskretno,
                    lambda = 10,
                    x.scale = 0.25,
                    maxit = 1000000,
                    tol = 0.025)
plot(FS,
     main = 'Porazdelitvena funkcija odškodnin',
     xlab = 'Višina odškodnine')

#d.) upanje in disperzija komulativne škode
upanjedrugoS <- mean(FS)
variancadrugaS <- sum(knots(FS)^2 * diff(FS)) - upanjedrugoS^2


#3.) METODA MONTE CARLO
#a.) 
simulacijaN <- rpois(10000,10) #simuliranje spr. N, rpois generira nakljuèno s pomoèjo Poissiona
simulacijaS <- c(0) #simuliranje neodvisnih realizacije spremenljivke Y
for (i in 1:10000){
  simulacijaS[i] <- sum(rweibull(simulacijaN[i], k, lambda))
}

#b.) ocena upanja in disperzije iz simulacije
upanjetretjeS <- mean(simulacijaS)
variancatretjaS<- var(simulacijaS)

#c.) slika simulirane porazdelitvene funkcije in primerjava s porazdelitvijo iz 2c
plot(ecdf(simulacijaS),
     col = 'green',
     add = TRUE )

legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'green'),
       lty=1:1, cex=0.9)

