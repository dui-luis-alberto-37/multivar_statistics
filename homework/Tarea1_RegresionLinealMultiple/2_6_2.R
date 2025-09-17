setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')
datos = read.csv('../../data/Rendimiento_de_gasolina.csv')

#a)
lm_coefficients(datos, 'y', c('x1','x6'))

m1 = lm('y ~ x1 + x6', datos)
m1

#b)
anova_table(m1)

F0_test_values(m1)

#c)
R2_test(m1)

m2 = lm('y ~ x1', datos)
m2

R2_test(m2)

#d)
intervalos_conf_beta(m1)
intervalos_conf_beta(m2)

#e)
intervalos_conf_media_y(c(255, 2),m1)

#f)
intervalos_pred_y(c(255, 2),m1)

#g)
intervalos_conf_media_y(c(255),m2)
intervalos_pred_y(c(255),m2)

#h)
qq_residuals(m1)
qq_residuals(m2)

#i)
res_vs_fitt(m1)
res_vs_fitt(m2)

#j)
res_vs_vars_plot(m1)
res_vs_vars_plot(m2)
