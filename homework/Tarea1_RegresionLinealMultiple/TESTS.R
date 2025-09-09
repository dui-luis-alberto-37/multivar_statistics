setwd("git/TIC'S/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
datos = read.csv('../../data/Liga_nacional_de_futbol.csv')



g1 <- ggplot(data = datos, mapping = aes(x = x1, y = y)) +
  geom_point(color = "forestgreen", size = 2) +
  labs(title  =  'yp ~ x1', x  =  'x1') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

g2 <- ggplot(data = datos, mapping = aes(x = x2, y = y)) +
  geom_point(color = "orange", size = 2) +
  labs(title  =  'yp ~ x2', x  =  'x2') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

install.packages('patchwork')
library(patchwork)
g1/g2

install.packages('gridExtra')
library(gridExtra)
grid.arrange(g1, g2, ncol = 2)




datos <- data.frame(
  Experimento = 1:17,
  x1 = c(41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57.0, 63.5, 64.3, 71.1, 77.0, 77.8),
  x2 = c(29.1, 29.3, 29.5, 29.7, 29.9, 30.3, 30.5, 30.7, 30.8, 30.9, 31.5, 31.7, 31.9, 32.0, 32.1, 32.5, 32.9),
  y  = c(251.3, 251.3, 248.3, 267.5, 273.0, 276.5, 270.3, 274.9, 285.0, 290.0, 297.0, 302.5, 304.5, 309.3, 321.7, 330.7, 349.0)
)


lm_coefficients(datos, 'y', 'x1', 'x2')


for(i in 1:5){
  print(i)
}


rep(c(1,2), length.out = 3)

plot_gg(datos, 'y', 'x1', 'x2')
