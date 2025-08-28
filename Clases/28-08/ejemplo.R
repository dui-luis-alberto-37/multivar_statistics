datos <- data.frame(
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
)
datos

modelo1 = lm(Infarc ~ Area + X2 + X3 , data = datos)
summary(modelo1)


anov = anova(modelo1)

#X3 y b0 son no son relevantes

SCE_area = anov$'Sum Sq'[1]
SSE_comp = anov$'Sum Sq'[4]
df2 = anov$Df[4]
df1 = nrow(datos) - 3 -df2

f_g = (SCE_area / df1) / (SSE_comp / df2)
f_g # el mismo que anov$'F value'[1]


SCE_all = sum(anov$'Sum Sq'[1:3])
df1 = nrow(datos) - 1 - df2

f_all = (SCE_all / df1) / (SSE_comp / df2)
f_all

pf(f_all, df = 3, df2 = 28, lower.tail = FALSE) 
# same as F-statistic in summary(modelo1) e indica que hay almenos un valor 
# significante entre las 3 variables

summary(modelo1)

SCE_x2_x3 = sum(anov$'Sum Sq'[2:3])
df1 = nrow(datos) - 2 - df2

f_dos = (SCE_x2_x3 / df1) / (SSE_comp / df2)
f_dos

pf(f_dos, df = 2, df2 = 28, lower.tail = FALSE) 
