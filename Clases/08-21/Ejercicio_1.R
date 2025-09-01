library(datarium)
data('marketing')

str(marketing)


modelo = lm(sales ~ youtube + facebook + newspaper, marketing)
modelo

summary(modelo)

# eliminamos newsparer por falta de significancia
modelo2 = lm(sales ~ youtube + facebook, marketing)

summary(modelo2)


## ejercicio 1:Realizar las pruebas de hipótesis sobre la significancia 
## de la regresión y sobre los coeficientes. Encontrar los intervalos de
## confianza respectivos del 95%. Para una tienda con presupuestos: youtube=150,
## facebook=30, newspaper=20 (en miles de USD): (a) Calcula el intervalo de
## confianza del 95% para la media de ventas E(sales|X0). (b) Calcula el 
## intervalo de predicción del 95% para una nueva observación de ventas.
## (c) Comenta la diferencia entre ambos intervalos. Subir respuesta y
## explicación de sus resultados a github.


Y = marketing$sales
n = length(Y)
n
p = 3

idv <- rep(1, n)
idv

X <- matrix(c(idv,marketing$youtube,marketing$facebook),nrow=n,ncol=p)
X

beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta


b0 = beta[1]
b1 = beta[2]
b2 = beta[3]

SCT <- t(Y) %*% Y -  sum(Y)**2 / nrow(marketing) ### calculo más eficiente de SCT
SCT

SCE <- t(beta) %*% t(X) %*% Y - sum(Y)**2 / nrow(marketing) 
SCE

SSE <- SCT - SCE
SSE

varest = SSE / (n-p-1)

F0 <- (SCE / (p - 1)) / (SSE / (n - p - 1))
F0
## tabular de f_0.05, 3, 200  = 2.650. semjante a necesitado f_0.05, 3, 196
## como F0 es muy grande se rechaza H0 por lo q sales depende de youtube o facebook

alpha <- 0.05; df1 <- p; df2 <- n-p-1
F_crit <- qf(1 - alpha, df1, df2)
F_crit

# confirmamos el rechaso de H0


# Hipotesis de coeficientes 

tX = t(X)
inv_tx_x = solve(tX %*% X)

  # youtube dado facebook

C11 = inv_tx_x[2,2]
C11

t0 <- b1 / sqrt(varest * C11)
t0

tt <- qt(p = 0.95 + 0.05/2, df = df2, lower.tail = TRUE)
tt

summary(modelo2)
## por lo tanto el valor b1 es significativo y rechazamos H0

  # facebook dado youtube

C22 = inv_tx_x[3,3]
C22

t0 <- b2 / sqrt(varest * C22)
t0

tt

summary(modelo2)

## por lo tanto el valor b2 es significativo y rechazamos H0

# intervalos de confianza
  
  #b1

izq <- b1 - tt * sqrt(varest*C11)
izq

der <- b1 + tt * sqrt(varest*C11)
der

  #b2

izq <- b2 - tt * sqrt(varest*C22)
izq

der <- b2 + tt * sqrt(varest*C22)
der


# a) Intervalo de confianza del 95% para la media de ventas E(sales|X0).

X0 = matrix(c(1, 150,30), nrow = 3)
X0

Y0 = t(X0)%*% beta
Y0

var_y0 = varest * t(X0)%*%solve(t(X)%*%X)%*%X0
var_y0

l_izq_0 = Y0 -tt*sqrt(var_y0)
l_izq_0

l_der_0 = Y0 +tt*sqrt(var_y0)
l_der_0


# b) youtube = 30, facebook = 150

X0 = matrix(c(1, 30,150), nrow = 3)
X0

Y0 = t(X0)%*% beta
Y0

var_y0 = varest * t(X0)%*%solve(t(X)%*%X)%*%X0
var_y0

l_izq_1 = Y0 -tt*sqrt(var_y0)
l_izq_1

l_der_1 = Y0 +tt*sqrt(var_y0)
l_der_1


c(l_izq_0,l_der_0)
c(l_izq_1,l_der_1)


# observaciones: 