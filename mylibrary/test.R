setwd('git/tics/5nto/multivar_statistics/mylibrary')
source('RegresionMultiple.R')

library(datarium)
data("marketing")

{datos <- data.frame(
  Paciente = 1:32,
  Infarc = c(
    0.119, 0.190, 0.395, 0.469, 0.130, 0.311, 0.418, 0.480,
    0.687, 0.847, 0.062, 0.122, 0.033, 0.102, 0.206, 0.249,
    0.220, 0.299, 0.350, 0.350, 0.588, 0.379, 0.149, 0.316,
    0.390, 0.429, 0.477, 0.439, 0.446, 0.538, 0.625, 0.974
  ),
  Area = c(
    0.34, 0.64, 0.76, 0.83, 0.73, 0.82, 0.95, 1.06,
    1.20, 1.47, 0.44, 0.77, 0.90, 1.07, 1.01, 1.03,
    1.16, 1.21, 1.20, 1.22, 0.99, 0.77, 1.05, 1.06,
    1.02, 0.99, 0.97, 1.12, 1.23, 1.19, 1.22, 1.40
  ),
  X2 = c(
    0,0,0,0,0,0,0,0,
    0,0,1,1,1,1,1,1,
    1,1,1,1,1,0,0,0,
    0,0,0,0,0,0,0,0
  ),
  X3 = c(
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,1,
    1,1,1,1,1,1,1,1
  )
)}
datos
mc = lm(Infarc~ Area + X2 + X3, data = datos)
m1 = lm(Infarc~ X2 + X3, data = datos)
a = anova(mc)
a0 = anova_table(mc)
a1 = anova_table(m1)

sum(a$`Sum Sq`[1:3]) 
SSR0 = a0$`Suma de cuadrados`[1]
SSR1 = a1$`Suma de cuadrados`[1]
SSR0 - SSR1
a$`Sum Sq`[1] 
a

a0$ANOVA_global
a0$ANOVA_parcial


mc0 = lm(Infarc~ X2 + X3 + Area, data = datos)
anova(mc0)
a0

anova(mc)
mv = lm('Infarc ~ 1', datos)
res_vs_vars_plot(mv)
