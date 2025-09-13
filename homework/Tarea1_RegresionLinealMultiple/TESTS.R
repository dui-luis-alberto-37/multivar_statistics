setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
datos = read.csv('../../data/Liga_nacional_de_futbol.csv')

m1 = lm('y ~ x2 + x7 + x8', datos)

t_0_values(m1)
datos$y

inter = intervalos_conf(m1)

inter
intervalos_conf(m1)
anova_table(m1)
t_test = t0_test_values(m1)
tt = t_test[3]
tt

source('../../mylibrary/RegresionMultiple.R')
intervalos_conf_media_y(c(2300, 56, 2100), m1)
