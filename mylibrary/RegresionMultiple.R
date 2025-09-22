library(ggplot2)
library(gridExtra)
library(corrplot)
library(lmtest)
library(olsrr)
library(MASS)
lm_coefficients = function(Data, y, x){
  n = length(Data[[y]])
  p = length(x)+1
  X <- cbind(1, as.matrix(Data[, x]))
  Y <- Data[[y]]
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  beta = cbind(matrix(beta, nrow =1))
  colnames(beta) = c("Intercept", x)
  return(beta)
}

extraer_datos_lm <- function(modelo) {
  
  mf <- model.frame(modelo)
  y <- model.response(mf)
  
  X <- model.matrix(modelo)[1:length(y),2:ncol(model.matrix(modelo))]
  
  nombres_y <- names(mf)[1]

  nombres_x <- colnames(mf)[2:ncol(mf)]
  
  return(list(
    y = y,
    X = X,
    nombre_y = nombres_y,
    nombres_x = nombres_x
  ))
}

plot_y_vs_xvars = function(modelo) {
  Data = model.frame(modelo)
  info = extraer_datos_lm(modelo)
  y = info$nombre_y
  x = info$nombres_x
  
  colors = c("red", "blue", "green","cyan", "magenta", "yellow", "black",
             
             "gray")
  n = length(Data[[y]])
  p = length(x)
  colors = rep(colors, length.out = n)
  
  x_vars = x
  gg = list()
  print(p)
  if (FALSE) {
    print('if')
    ggplot(data = Data, mapping = aes(x = Data$x_vars, y = Data[[y]])) +
      geom_point(color = colors[i], size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else{
  for(i in 1:p){
    print('sym')
    print(i)
    gg[[i]] = ggplot(data = Data, mapping = aes(x = !!sym(x_vars[i]), y = !!sym(y))) +
      geom_point(color = colors[i], size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  do.call(grid.arrange, c(gg, list(ncol = min(p, 3))))
}}

anova_table = function(modelo){
  
  Data = model.frame(modelo)
  info = extraer_datos_lm(modelo)
  y = info$nombre_y
  x = info$nombres_x
  Y = info$y
  X = info$X
  
  n = length(Y)
  p = length(x)+1
  
  
  X <- cbind(1, as.matrix(X))
  
  Y <- model.response(Data)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  anov = data.frame(
    'Fuente de Variación' = c('Regresión', 'Residuales', 'Total'),
    stringsAsFactors = FALSE
  )
  scr = t(beta) %*% t(X) %*% Y - sum(Y)**2 / n
  sce = t(Y) %*% Y -  t(beta) %*% t(X) %*% Y
  sct = t(Y) %*% Y -  sum(Y)**2 / n
  anov$'Suma de cuadrados' = c(scr,sce,sct)
  
  glr = p - 1
  gle = n - p
  glt = n - 1
  anov$'Grados de libertad' = c(glr, gle, glt)
  
  cmr = scr/glr
  cme = sce/gle
  anov[1, 'Cuadrados medios'] = cmr
  anov[2, 'Cuadrados medios'] = cme
  
  f0 = cmr/cme
  anov[1, 'F_0'] = f0
  return(anov)
}

F0_test_values = function(modelo){
  anov_t = anova_table(modelo)
  anov_t
  F0 = anov_t[1,'F_0']
  F0
  df = anov_t$'Grados de libertad'
  
  pvlue = 1 - pf(F0,df[1],df[2])
  pvlue
  
  alpha = 0.05
  F1 = qf(1-alpha,df[1],df[2])
  F1
  
  F_table = cbind(matrix(c(F0, pvlue, F1), nrow = 1))
  colnames(F_table) = c('F0', 'p-value', 'F1')
  return(F_table)
}

t0_test_values = function(modelo){
  
  Data = model.frame(modelo)
  info = extraer_datos_lm(modelo)
  y = info$nombre_y
  x = info$nombres_x
  
  Y = info$y
  X = info$X
  
  n = length(Data$y)
  p = length(x)+1
  X <- cbind(1, as.matrix(Data[, x]))
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  C = solve(t(X) %*% X)
  Cii = matrix(nrow = p)
  for(i in 1:p){
    Cii[i,1] = C[i,i]
  }
  
  anov_t = anova_table(modelo)
  cme = anov_t$'Cuadrados medios'[2]
  
  t0 = c(beta / sqrt(rep(cme,length.out = p) * Cii))
  df = anov_t$'Grados de libertad'
  
  p_values = 2 * pt(-abs(t0), df[2])
  
  alpha = 0.05
  tt = qt(1 - alpha/2, df[2], lower.tail = TRUE)
  
  t_values_table = cbind(matrix(ncol = p ,nrow = 3))
  colnames(t_values_table) = c('Intercept', x)
  
  t_values_table[1,] = t0
  t_values_table[2,] = p_values
  t_values_table[3,] = rep(tt,length.out = p)
  
  rownames(t_values_table) = c('t0', 'p-value', 'tt')
  
  return(t_values_table)
}

R2_test = function(modelo){

  anov_t = anova_table(modelo)
  scr = anov_t[1,'Suma de cuadrados']
  sct = anov_t[3,'Suma de cuadrados']
  R2 = (scr/sct)
  cme = anov_t[2,'Cuadrados medios']
  cmt = anov_t[3,'Suma de cuadrados'] / anov_t[3,'Grados de libertad']
  R2_adj = (1 - (cme/cmt))
  
  R2_table = cbind(matrix(c(R2, R2_adj), nrow = 1))
  colnames(R2_table) = c('R2', 'R2 ajustada')
  return(R2_table)
}

qq_residuals = function(modelo){
  gplot = ggplot(datos, aes(sample = modelo$residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(x = "Cuantiles Teóricos",
         y = "Cuantiles Muestrales") +
    theme_minimal()
  
  print(shapiro.test(modelo$residuals))
  return(gplot)
}

res_vs_fitt = function(modelo){
  g = ggplot(m1, aes(x=modelo$fitted.values, y=modelo$residuals)) +
    geom_point(alpha=0.7,color='blue',size=2) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(
      x = 'y predricha',
      y = 'Residuales'
    ) +
    theme_minimal()
  print(bptest(modelo))
  return(g)
}

res_vs_vars_plot = function(modelo) {
  colors = c("red", "blue", "green","cyan", "magenta", "yellow", "black",
             
             "gray")
  
  
  info = extraer_datos_lm(modelo)
  
  n = length(info$nombres_x)
  colors = rep(colors, length.out = n)
  
  x_vars = info$nombres_x
  gg = list()
  
  Data = model.frame(modelo)
  for(i in 1:length((x_vars))){
    gg[[i]] = ggplot(data = Data, mapping = aes(x = !!sym(x_vars[i]), y = modelo$residuals)) +
      geom_point(color = colors[i], size = 2) +
      labs(y = 'Residuales', x  = x_vars[i] ) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  do.call(grid.arrange, c(gg, list(ncol = min(n, 3))))
}

intervalos_conf_beta = function(modelo){
  anov_t = anova_table(modelo)
  
  info = extraer_datos_lm(modelo)
  Data = model.frame(modelo)
  x = info$nombres_x
  y = info$nombres_y
  X = info$X
  Y = info$y
  n = length(Y)
  p = length(x)+1
  
  
  X = cbind(1, as.matrix(X))
  
  C = solve(t(X) %*% X)
  Cii = matrix(nrow = p)
  for(i in 1:p){
    Cii[i,1] = C[i,i]
  }
  
  t_test = t0_test_values(modelo)
  tt = t_test[3,]
  
  betas = solve(t(X) %*% X) %*% t(X) %*% Y

  cme = anov_t$'Cuadrados medios'[2]
  cme = rep(cme,length.out = p)
  
  izq = betas-tt*sqrt(cme*Cii)
  der = betas+tt*sqrt(cme*Cii)
  
  interval_table = cbind(matrix(ncol = p, nrow = 4))
  colnames(interval_table) = c("Intercept", x)
  interval_table[1,] = izq
  interval_table[2,] = betas
  interval_table[3,] = der
  interval_table[4,] = der - izq
  rownames(interval_table) = c('izq', 'beta_value', 'der', 'long')
  
  return(interval_table)
}

intervalos_conf_media_y = function(X0, modelo){
  anov_t = anova_table(modelo)
  
  info = extraer_datos_lm(modelo)
  Data = model.frame(modelo)
  x = info$nombres_x
  y = info$nombre_y
  n = nrow(Data)
  p = length(x)+1
  X = cbind(1, as.matrix(info$X))
  Y = info$y
  betas = solve(t(X) %*% X) %*% t(X) %*% Y
  cme = anov_t$'Cuadrados medios'[2]
  t_test = t0_test_values(modelo)
  tt = t_test[3]
  
  X0 = matrix(c(1,X0), ncol = length(p))
  #print(dim(t(X0)))
  y0 = t(X0) %*% (betas)
  var_y0 = cme * t(X0) %*% solve(t(X) %*% X) %*% X0
  
  izq = y0 - tt * sqrt(var_y0)
  der = y0 + tt * sqrt(var_y0)
  interval_table = cbind(matrix(c(izq, der, der-izq), ncol = 3))
  colnames(interval_table) = c('izq', 'der', 'long')
  rownames(interval_table) = 'y0'
  
  return(interval_table)
}

intervalos_pred_y = function(X0, modelo){
  anov_t = anova_table(modelo)
  
  info = extraer_datos_lm(modelo)
  Data = model.frame(modelo)
  x = info$nombres_x
  y = info$nombre_y
  n = nrow(Data)
  p = length(x)+1
  X = cbind(1, as.matrix(info$X))
  Y = info$y
  betas = solve(t(X) %*% X) %*% t(X) %*% Y
  cme = anov_t$'Cuadrados medios'[2]
  
  t_test = t0_test_values(modelo)
  tt = t_test[3]
  
  X0 = matrix(c(1,X0), ncol = length(p))
  #print(dim(t(X0)))
  y0 = t(X0) %*% (betas)
  var_y0 = cme * (1 + t(X0) %*% solve(t(X) %*% X) %*% X0)
  
  izq = y0 - tt * sqrt(var_y0)
  der = y0 + tt * sqrt(var_y0)
  
  interval_table = cbind(matrix(c(izq, der, der - izq), ncol = 3))
  colnames(interval_table) = c('izq', 'der', 'long')
  rownames(interval_table) = 'y0'
  
  return(interval_table)
}

var_corr = function(modelo){
  info = extraer_datos_lm(modelo)
  X = info$X
  corr = cor(X,method = "pearson")
  return(corrplot(corr))
}

forward_stepwise = function(modelo){
  al = ols_step_forward_aic(modelo, details = T)
  
  return(plot(al))
}

backward_stepwise = function(modelo){
  return(ols_step_backward_aic(modelo, details = T))
}

all_models_step = function(modelo){
  return(ols_step_both_aic(modelo, details = T))
}
