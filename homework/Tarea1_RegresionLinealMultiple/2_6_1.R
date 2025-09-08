#setwd('5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/')
datos = read.csv('../../data/Liga_nacional_de_futbol.csv')

m1 = lm('y ~ x2 + x7 + x8', datos)
m1

plot(datos$x2, datos$y,
     main = "m1",
     xlab = "yardas por aire",
     ylab = "juegos ganados",
     pch = 19, col = "blue")
abline(m1, col = "red", lwd = 4)

library(ggplot2)

ggplot(datos, aes(x = x2, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "m1",
    x = "yardas por aire",
    y = "juegos ganados"
  ) +
  theme_minimal()

ggplot(datos, aes(x = x7, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "m1",
    x = "% jugadas por tierra",
    y = "juegos ganados"
  ) +
  theme_minimal()

ggplot(datos, aes(x = x8, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "m1",
    x = "% jugadas por tierra del contrario",
    y = "juegos ganados"
  ) +
  theme_minimal()

summary(m1)
# el valor de x7 parece aportar poco al modelo, pero sigue siendo 
# suficientemente significante con un p-value de 0.037815, en el t-test
anova(m1)


