Sigma <- matrix(c(1, 4,
                  4, 100), nrow = 2, byrow = TRUE)

Sigma

sd_vec <- sqrt(diag(Sigma))
sd_vec

R_mat <- diag(1/ sd_vec) %*% Sigma %*% diag(1/ sd_vec)
round(R_mat, 6)

eig_Sigma <- eigen(Sigma)
eig_Sigma

lambda_S <- eig_Sigma$values
P_S <- eig_Sigma$vectors

cat("Eigen Sigma:\n"); print(round(lambda_S,6))
cat("Autovectores Sigma (columnas):\n"); print(round(P_S,6))

cat("\nFormas lineales (Sigma): Yi = a1 * X1 + a2 * X2  (autovectores col)\n")
for(i in 1:2){
  coefs <- round(P_S[,i], 6)
  cat(sprintf("Y%d = %6.6f * X1 + %6.6f * X2\n", i, coefs[1], coefs[2]))
}

eig_R <- eigen(R_mat)
lambda_r <- eig_R$values
P_r <- eig_R$vectors

lambda_r
P_r

cat("\nEigen R:\n"); print(round(lambda_r,6))

cat("Autovectores R (columnas):\n"); print(round(P_r,6))

cat("\nFormas lineales (R, sobre Z estandarizadas): Yi = b1 * Z1 + b2 * Z2\n")

for(i in 1:2){
  coefs <- round(P_r[,i], 6)
  cat(sprintf("Y%d = %6.6f * Z1 + %6.6f * Z2\n", i, coefs[1], coefs[2]))
}

total_var_S <- sum(diag(Sigma))
total_var_S

prop_S <- lambda_S / total_var_S
prop_S

cat("\nProporciones (Sigma):\n")
for(i in 1:2) cat(sprintf("PC%d: lambda=%.6f prop=%.4f cumul=%.4f\n",
                          i, lambda_S[i], prop_S[i], cumsum(prop_S)[i]))

total_var_r <- sum(diag(R_mat))  # = 2
total_var_r
prop_r <- lambda_r / total_var_r
prop_r
cat("\nProporciones (R - variables estandarizadas):\n")

for(i in 1:2) cat(sprintf("PC%d: lambda=%.6f prop=%.4f cumul=%.4f\n",
                          i, lambda_r[i], prop_r[i], cumsum(prop_r)[i]))

sigma_diag <- diag(Sigma)

rho_YX_Sigma <- matrix(NA, 2, 2, dimnames=list(paste0("Y",1:2), paste0("X",1:2)))

for(i in 1:2) for(j in 1:2){
  rho_YX_Sigma[i,j] <- P_S[j,i] * sqrt(lambda_S[i]) / sqrt(sigma_diag[j])
}
cat("\nCorrelaciones entre Yi (de Sigma) y Xj:\n")
print(round(rho_YX_Sigma,6))

R_YZ_R <- matrix(NA, 2, 2, dimnames=list(paste0("Y",1:2), paste0("Z",1:2)))

for(i in 1:2) for(j in 1:2){
  R_YZ_R[i,j] <- P_r[j,i] * sqrt(lambda_r[i])    # / sqrt(1) = 1
}

cat("\nCorrelaciones entre Yi (de R) y Zj (variables estandarizadas):\n")
print(round(R_YZ_R,6))

cat("\nP_S' Sigma P_S  (debe ser diagonal = autovalores de Sigma)\n")
print(round(t(P_S) %*% Sigma %*% P_S, 8))

cat("\nP_r' R P_r (debe ser diagonal = autovalores de R)\n")
print(round(t(P_r) %*% R_mat %*% P_r, 8))
