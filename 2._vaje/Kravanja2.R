library(actuar)

# 1.)
#a.)

vzorec <- read.delim("vzorec2.txt",header = FALSE)
View(vzorec)

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

#d.) 
n <-20
p <- 1/2
upanjey <- lambda * gamma(1 + (1/k))
upanjen <- n*p
upanjeS <- upanjey * upanjen

variancay <- lambda^2 * (gamma(1+ (2/k)) - gamma (1+ (1/k))^2)
variancan <- n*p* (1-p)
upanjeyna2 <- varianca_y + upanjey^2
variancaS <- variancay * upanjen + variancan * upanjeyna2

# 2.)
# a.)
h <- 0.25
n <- 38
diskretno <- discretize(pweibull (x,shape=k, scale = lambda),
                        step = 0.25,
                        from = 0,
                        to = 10,
                        method = 'rounding')

#b.) 
graf1 <- curve(pweibull(x, shape=k, scale = lambda),
              from = 0, 
              to = 10,
              main = 'Weibullova porazdelitev',
              ylab = 'porazdelitvena funkcija',
              lwd = 2)

plot(stepfun(seq.int(0,n*h,by=h),y = diskretno), add=TRUE)

