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
