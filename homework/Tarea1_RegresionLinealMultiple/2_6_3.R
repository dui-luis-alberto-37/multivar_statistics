setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')
datos = read.csv('../../data/Precios_de_viviendas.csv')

#a)
lm_coefficients(datos, 'y', colnames(datos)[2:10])

m1 = lm('y ~ .', datos)
m1

#b)
F0_test_values(m1)
summary(m1)

#c)
t0_test_values(m1)

