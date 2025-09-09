setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')



datos = read.csv('../../data/Liga_nacional_de_futbol.csv')


lm_coefficients(datos, 'y', 'x2', 'x7', 'x8')

plot_gg(datos, 'y', 'x2', 'x7', 'x8')

m1 = lm('y ~ x2 + x7 + x8', datos)
m1

summary(m1)

anova(m1)


