datos <- data.frame(
  Fuerza_enlace = c(
    2158.70, 1678.15, 2316.00, 2061.30, 2207.50,
    1708.30, 1784.70, 2575.00, 2357.90, 2256.70,
    2165.20, 2399.55, 1779.80, 2336.75, 1765.30,
    2053.50, 2414.40, 2200.50, 2654.20, 1753.70
  ),
  Edad_lote = c(
    15.50, 23.75, 8.00, 17.00, 5.50,
    19.00, 24.00, 2.50, 7.50, 11.00,
    13.00, 3.75, 25.00, 9.75, 22.00,
    18.00, 6.00, 12.50, 2.00, 21.50
  )
)

datos

modelo_completo <- lm(Fuerza_enlace ~ Edad_lote, data = datos)

plot(datos$Edad_lote, datos$Fuerza_enlace,
     main = "Modelo completo",
     xlab = "Edad del lote (semanas)",
     ylab = "Fuerza del enlace (psi)",
     pch = 19, col = "blue")

# Agregar la recta de regresión
abline(modelo_completo, col = "red", lwd = 2)


ggplot(datos, aes(x = Edad_lote, y = Fuerza_enlace)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Modelo completo",
    x = "Edad del lote (semanas)",
    y = "Fuerza del enlace (psi)"
  ) +
  theme_minimal()

modelo_reducido <- lm(Fuerza_enlace ~ 1, data = datos)

plot(datos$Edad_lote, datos$Fuerza_enlace,
     main = "Modelo reducido",
     xlab = "Edad del lote (semanas)",
     ylab = "Fuerza del enlace (psi)",
     pch = 19, col = "blue")

# Agregar la recta de regresión
abline(modelo_reducido, col = "red", lwd = 2)

ggplot(datos, aes(x = Edad_lote, y = Fuerza_enlace)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ 1, se = TRUE, fullrange = TRUE) +
  labs(title = "Modelo reducido",
       x = "Edad del lote (semanas)", y = "Fuerza del enlace (psi)") +
  theme_minimal()


anova(modelo_completo)


## con el p-value rechazamos h0 : B1 = 0, por lo tanto x1 es relevante para y