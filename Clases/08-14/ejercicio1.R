datos <- data.frame(
  Experimento = 1:17,
  x1 = c(41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57.0, 63.5, 64.3, 71.1, 77.0, 77.8),
  x2 = c(29.1, 29.3, 29.5, 29.7, 29.9, 30.3, 30.5, 30.7, 30.8, 30.9, 31.5, 31.7, 31.9, 32.0, 32.1, 32.5, 32.9),
  y  = c(251.3, 251.3, 248.3, 267.5, 273.0, 276.5, 270.3, 274.9, 285.0, 290.0, 297.0, 302.5, 304.5, 309.3, 321.7, 330.7, 349.0)
)

n = length(datos$y)
p = length(datos)-1


idv <- rep(1, n)
idv

X <- matrix(c(idv,datos$x1,datos$x2),nrow=n,ncol=p)
X


y <- matrix(datos$y, nrow = n, ncol = 1)
y

beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta


M1 <- lm(y ~ x1 + x2, datos)

SSE <- t(y)%*% y - t(beta) %*% t(X) %*% y
SSE

varest <- SSE / (n - p) 
varest
