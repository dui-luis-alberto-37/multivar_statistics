install.packages('datasets')
library(datasets)
data('trees')
datos = trees
datos
n = length(datos$Volume)
p = length(datos)

idv <- rep(1, n)
idv

X <- matrix(c(idv,datos$Girth,datos$Height),nrow=n,ncol=p)
X

y <- matrix(datos$Volume, nrow = n, ncol = 1)
y

beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta


M1 <- lm(Volume ~ Girth + Height, datos)
summary(M1)

SSE <- t(y)%*% y - t(beta) %*% t(X) %*% y
SSE

varest <- SSE / (n -p) 
varest
