# git/TIC'S
setwd("Tic's/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')

datos = read.csv('../../data/Liga_nacional_de_futbol.csv')

# a)

lm_coefficients(datos, 'y', c('x2', 'x7', 'x8'))

m1 = lm('y ~ x2 + x7 + x8', datos)
m1

plot_y_vs_xvars(m1)

#b)
anova_table(m1)

anova(m1)

F0_test_values(m1)

#c)
t0_test_values(m1)

summary(m1)

#d)
R2_test(m1)

#e)
qq_residuals(m1)

#f)
res_vs_fitt(m1)

#g)
res_vs_vars_plot(m1)

#h)
intervalos_conf_beta(m1)

intervalos_conf_media_y(c(2300, 56, 2100), m1)

#i)

m2 = lm('y ~ x7 + x8', datos)
m2

lm_coefficients(datos, 'y', c('x7', 'x8'))

F0_test_values(m2)

plot_y_vs_xvars(m1)

#j)
R2_test(m2)
R2_test(m1)

#k)
intervalos_conf_beta(m2)

intervalos_conf_media_y(c(56, 2100), m2)

#l) conclusiones