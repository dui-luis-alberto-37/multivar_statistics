setwd('git/tics/5nto/multivar_statistics/homework/validacion_supuestos/')
source('../../mylibrary/RegresionMultiple.R')
library(datarium)
data("marketing")

m1 = lm('sales ~ .', marketing)
m1

var_corr(m1, v = T)

qq_residuals(m1)

res_vs_fitt(m1)

dw_test(m1)
