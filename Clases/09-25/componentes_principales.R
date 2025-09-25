Sigma <- matrix(c(1, -2, 0,
                  -2, 5, 0,
                  0,  0, 2),
                nrow = 3, byrow = TRUE)

Sigma
eig = eigen(Sigma)
eig
lamda = eig$values
lamda
E = eig$vectors
E
D = t(E) %*% Sigma %*%E
D
print(round(D,8))


vars_pc = sapply(1:3, function(i) as.numeric(t(E[,i]) %*% Sigma %*% E[,i]))
vars_pc

covs_pc = matrix(0, 3, 3)
for(i in 1:3) for(j in 1:3) covs_pc[i,j] = as.numeric(t(E[,i]) %*% Sigma %*% E[,j])

print(round(covs_pc, 8))

traza_sigma = sum(diag(Sigma))
traza_sigma
suma_autovalores = sum(lamda)
suma_autovalores


## Y_1 = -0.38*X_11 + 0.92*X_12 |||||  e1 = (-0.38, 0.92, 0) 
## Y_2 = 1*X_23                 |||||  e2 = (0, 0, 1) 
## Y_3 = 0.92*X_31 + 0.38*X_32  |||||  e3 = (0.92, 0.38, 0)


total_var = traza_sigma
prop_explicada = lamda / total_var
prop_explicada

cum_prop = cumsum(prop_explicada)
cum_prop

## correlaciones
sigma_diagonal = diag(Sigma)
sigma_diagonal
ncomp = length(lamda)
rho = matrix(NA, nrow = ncomp, ncol = ncol(E), dimnames = list(paste0('Y', 1:ncomp),
                                                               paste0('X', 1:ncol(E))))
for(i in 1:ncomp){
  for(j in 1:ncol(E)){
    rho[i,j] = E[j,i]*sqrt(lamda[i]) / sqrt(sigma_diagonal[j])
  }
}
rho
