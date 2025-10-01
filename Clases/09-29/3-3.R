getwd()
setwd("Tic\'s/5nto/multivar_statistics/Clases/09-29")
library(factoextra)
library(readxl)
records <- read_excel("../../data/NationalTrackRecords2.xlsx")
records2 <- records[,2:8]
records
records2
rownames(records2) <- records$...1

var(records2)
cor(records2)
library(corrplot)
res1 <- cor.mtest(records2, conf.level= .95)
res2 <- cor.mtest(records2, conf.level= .99)
res2
cor.mat <- cor(records2, use="complete.obs")

corrplot(cor.mat)

cp1 <- princomp(records2, cor = TRUE)
summary(cp1)

cp1$loadings

cp1$sdev

varianza.cp1 <- (cp1$sdev)^2
varianza.cp1

porcentaje_varianza <- (cp1$sdev)^2*100/sum((cp1$sdev)^2)
porcentaje_varianza

proporcion_acumulada_varianza <- cumsum(porcentaje_varianza)
proporcion_acumulada_varianza

eig_corr = eigen(cor.mat)
eig_corr

