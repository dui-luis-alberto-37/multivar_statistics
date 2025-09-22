setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
setwd("Tic's/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')
datos = read.csv('../../data/Liga_nacional_de_futbol.csv')

m1 = lm(y~.-Equipo, datos)

# a)
fs = forward_stepwise(m1)
fs

# b)
bs = backward_stepwise(m1)
bs

# c)

as = all_models_step(m1)
as
