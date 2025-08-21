datos <- data.frame( # refrescos
  Observacion = 1:25,
  y = c(16.68, 11.50, 12.03, 14.88, 13.75,
        18.11, 8.00, 17.83, 79.24, 21.50,
        40.33, 21.00, 13.50, 19.75, 24.00,
        29.00, 15.35, 19.00, 9.50, 35.10,
        17.90, 52.32, 18.75, 19.83, 10.75),
  x1 = c(7, 3, 3, 4, 6,
         7, 2, 7, 30, 5,
         16, 10, 4, 6, 9,
         10, 6, 7, 3, 17,
         10, 26, 9, 8, 4),
  x2 = c(560, 220, 340, 80, 150,
         330, 110, 210, 1460, 605,
         688, 215, 255, 462, 448,
         776, 200, 132, 36, 770,
         140, 810, 450, 635, 150)
)

pairs(datos[-1])


n = length(datos$y)
p = length(datos)-1


idv <- rep(1, n)
idv

X <- matrix(c(idv,datos$x1,datos$x2),nrow=n,ncol=p)
X


y <- matrix(datos$y, nrow = n, ncol = 1)
y

# solucion calculada
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta

M1 = lm(y ~ x1 + x2, data=datos)
M1

beta_0 <- M1$coefficients[1]
beta_1 <- M1$coefficients[2]
beta_2 <- M1$coefficients[3]

# Prueva de hipótesis
SCT <- t(y) %*% y -  sum(y)**2 / nrow(datos) ### calculo más eficiente de SCT
SCT

SCE <- t(beta) %*% t(X) %*% y - sum(y)**2 / nrow(datos) 
SCE

SSE <- SCT - SCE
# alternativa_ejemplo1: SSE = t(y) %*% y - t(beta) %*% t(X) %*% y 
SSE

# estadistico F
F0 <- (SCE / (ncol(X) - 1)) / (SSE / (nrow(X) - (ncol(X) -1) - 1))
F0

# Usuando modelo calculado M1
  
  # Suma de Cuadrados

SCT.m =sum((datos$y-mean(datos$y))^2)
SCT.m

SCE.m = sum((M1$fitted-mean(datos$y))^2)
SCE.m

SSE.m = sum(M1$residuals^2)
SSE.m

  # Grados de libertad

GLT = n-1
GLT

GLRes = df.residual(M1)
GLRes

GLR<- GLT-GLRes
GLR

  # Cuadrados Medios

CMR <- SCE /GLR
CMR

CMRes <- SSE / GLRes
CMRes

  # F

F0 <- CMR/CMRes
F0

  # p-value

pv <- 1 - pf(F0, GLR,GLRes)
pv

  # tabulado de F

alpha <- 0.05 
df1 <- GLR
df2 <- GLRes
F_crit <- qf(1 - alpha, df1, df2)
F_crit


#Pruebas sobre coeficientes individuales de regresión

  #Estadístico t0

C22 <- solve(t(X) %*% X)[3,3]
C22

t0 <- beta_2 / sqrt(varest * C22)
t0

## alfa = 0.05, p = 2, n = 25
## buscamos el t_{\alfa/2, n-p-1} en una tabla de distribuciones 
## que nos da 2.0687 por lo q se rechasa h0 que es Bj = 0


## t tabulado con confianza 95% y 22 grados de libertad
tt <- qt(p = 0.95 + 0.05/2, df = df2, lower.tail = TRUE)
tt

#Con M1:

summary(M1)
