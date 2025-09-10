lm_coefficients = function(Data, y, ...){
  n = length(Data$y)
  p = length(c(...))+1
  X <- cbind(1, as.matrix(Data[, c(...)]))
  X = matrix(X, nrow=n,ncol=p)
  Y <- matrix(Data$y)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  beta = cbind(matrix(beta, nrow =1))
  colnames(beta) = c("Intercept", ...)
  return(beta)
}

library(ggplot2)
library(gridExtra)

plot_gg = function(Data, y, ...) {
  colors = c("red", "blue", "green","cyan", "magenta", "yellow", "black",
             
             "gray")
  n = length(Data$y)
  colors = rep(colors, length.out = n)
  
  x_vars = list(...)
  gg = list()
  
  for(i in 1:length((x_vars))){
    gg[[i]] = ggplot(data = Data, mapping = aes(x = !!sym(x_vars[[i]]), y = !!sym(y))) +
      geom_point(color = colors[i], size = 2) +
      labs(title  =  paste(y, '~', x_vars[[i]]), x  = x_vars[[i]] ) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  do.call(grid.arrange, c(gg, list(ncol = min(n, 3))))
}

anova_table = function(Data, y, ...){
  n = length(Data$y)
  p = length(c(...))+1
  X <- cbind(1, as.matrix(Data[, c(...)]))
  X = matrix(X, nrow=n,ncol=p)
  Y <- matrix(Data$y)
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

t_0_values = function(Data, y, ...){
  n = length(Data$y)
  p = length(c(...))+1
  X <- cbind(1, as.matrix(Data[, c(...)]))
  X = matrix(X, nrow=n,ncol=p)
  Y <- matrix(Data$y)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  C = solve(t(X) %*% X)
  Cii = matrix(nrow = p)
  for(i in 1:p){
    Cii[i,1] = C[i,i]
  }
  
  sce = t(Y) %*% Y -  t(beta) %*% t(X) %*% Y
  gle = n - p
  cme = sce/gle
  
  return(c(beta / sqrt(rep(cme,length.out = p) * Cii)))
}

man_R2 = function(Data, y, ...){
  AnovTab = anova_table(Data, y, ...)
  scr = AnovTab[1,'Suma de cuadrados']
  sct = AnovTab[3,'Suma de cuadrados']
  return(scr/sct)
}

man_R2_adj = function(Data, y, ...){
  AnovTab = anova_table(Data, y, ...)
  cme = AnovTab[2,'Cuadrados medios']
  cmt = AnovTab[3,'Suma de cuadrados'] / AnovTab[3,'Grados de libertad']
  return(1 - (cme/cmt))
}


#t_0_values(datos, 'y', 'x2', 'x7', 'x8')

anova_table(datos, 'y', 'x2', 'x7', 'x8')
