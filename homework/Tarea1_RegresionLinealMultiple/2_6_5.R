setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')
datos = read.csv('../../data/Liga_nacional_de_futbol.csv')

m1 = lm(y~.-Equipo, datos)

# a)
fs = forward_stepwise(m1)
bac

summary(fs)
