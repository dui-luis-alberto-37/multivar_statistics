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

#d)
R2_test(m1)

#e)
t0_test_values(m1)[,'x3']

#f)
var_corr(m1)

#g)
qq_residuals(m1)

#h)
res_vs_fitt(m1)

#i)
res_vs_vars_plot(m1)
