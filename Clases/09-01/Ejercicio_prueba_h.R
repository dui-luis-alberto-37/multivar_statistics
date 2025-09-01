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

## 1) Pruebas sobre uno de los parametros

# H_0: β_1=0, modelo reducido y_i= β_0 + β_2 * x_2i + β_3 * x_3i + ϵ_i. 
# Vamos a denotar a la suma de los cuadrados de los errores asociada a 
# este modelo SSE(x_1), la cual tiene n − 3 grados de libertad asociados 
# (tenemos 3 parámetros a estimar β_0, β_2, β_3). Como el p-valor es muy 
# pequeño, se rechaza a H0 y se concluye que hay suficiente evidencia para
# decir que el tamaño del infarto está significativamente relacionado con el
# tamaño del área de riesgo.

modelo_c <- lm(Infarc~ Area + X2 + X3, data = datos)
summary(modelo_c)

anov <- anova(modelo_c)
anov

SCE_X1 <- anov$`Sum Sq`[1]
SSE_C <- anov$`Sum Sq`[4]
df2 <- anov$Df[4]
df1 <- nrow(datos) - 3 - df2
F_g <- (SCE_X1/df1) / (SSE_C/df2) 
F_g

SCE_X1X2X3 <- sum(anov$`Sum Sq`[1:3]) 
df1 <- nrow(datos) -1 - df2 

F_all <- (SCE_X1X2X3 / df1) / (SSE_C / df2)
F_all


# h0 : B_3 = 0
SCE_X_3 <- anov$`Sum Sq`[3]
SSE_C <- anov$`Sum Sq`[4]
df2 <- anov$Df[4]
df1 <- nrow(datos) - 3 - df2
F_3 <- (SCE_X_3/df1) / (SSE_C/df2) 
F_3

p-value = pf(F_3, df1, df2, lower.tail = FALSE)

# p-value es mayor a nuestro valor de significancia
# por lo tanto acptamos la hipotesis de que B_3 no tiene un valor significante
# para nuestro modelo