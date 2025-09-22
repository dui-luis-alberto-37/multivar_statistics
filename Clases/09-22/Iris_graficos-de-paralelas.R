library(lattice)
data(iris)
iris

install.packages("andrews")
library(aplpack)
library(andrews)


faces(iris[c(1,51,101),1:4],
      nrow.plot = 1, 
      ncol.plot = 3, 
      main = "La primer flor de cada especie", print.info = TRUE)

faces(iris[c(1:10,51:60,101:110),1:4],
      nrow.plot = 3, 
      ncol.plot = 10, 
      main = "Las primeras 10 flores de cada especie", print.info = FALSE)

faces(cbind(iris[1:15,1:4],rep(1:5,rep(3,1)), rep(c(2,4,6),rep(5,3))),
      nrow.plot = 3, 
      ncol.plot = 5, 
      main = "¿Sonrisas?", print.info = FALSE)

apply(iris[ ,1:4],2,max)

apply(iris[ ,1:4],2,min)

chico<-c(4,2,1,.05) # Valores más chicos de las 4 variables.
grande<-c(8,5,7,3) # Valores más grandes de las 4 variables.
faces(rbind(iris[c(1:10,51:60,101:110),1:4],chico,grande), nrow.plot=5, ncol.plot=10,main="Primeros 10 de cada especie + chico + grande" )

par(mfrow=c(2,2))
andrews(iris, type = 1, clr = 5, ymax = 3, main = "Curva tipo 1")
andrews(iris, type = 2, clr = 5, ymax = 3, main = "Curva tipo 2")
andrews(iris, type = 3, clr = 5, ymax = 3, main = "Curva tipo 3")
andrews(iris, type = 4, clr = 5, ymax = 3, main = "Curva tipo 4")

parallelplot(~iris|iris$Species, main="Plot de paralelas por Cilindros")
parallelplot(~iris, col=as.numeric(iris$Species),main="Plot de paralelas por Cilindros" )

palette(rainbow(12, s = 0.6, v = 0.75))

stars(iris[, 1:4], len = 0.8, key.loc = c(15, 0),
      main = "Motor Trend Cars", draw.segments = TRUE)

stars(iris[, 1:4], locations = c(0, 0), radius = FALSE,
      key.loc = c(0, 0), main = "Motor Trend Cars", lty = 2)

iris

