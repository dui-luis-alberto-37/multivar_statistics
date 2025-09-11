# git/TIC'S
setwd("Tic's/5nto/multivar_statistics/homework/Tarea1_RegresionLinealMultiple/")
source('../../mylibrary/RegresionMultiple.R')



datos = read.csv('../../data/Liga_nacional_de_futbol.csv')


lm_coefficients(datos, 'y', 'x2', 'x7', 'x8')

plot_gg(datos, 'y', 'x2', 'x7', 'x8')

m1 = lm('y ~ x2 + x7 + x8', datos)
m1

anov_t = anova_table(datos, 'y', 'x2', 'x7', 'x8')
anov_t
F0 = anov_t[1,'F_0']
F0
df = anov_t$'Grados de libertad'

pvlue = pf(F0,df[2],df[1])
pvlue

alpha = 0.05
F1 = qf(1-alpha,df[1],df[2])
F1

anova(m1)


t0 = t_0_values(datos, 'y', 'x2', 'x7', 'x8')
t0

tt <- qt(1 - alpha/2, df[2], lower.tail = TRUE)
tt

p_values = 2 * pt(-abs(t0), df[2])
p_values

summary(m1)

library(broom)
R2 = glance(m1)$r.squared
R2adj = glance(m1)$adj.r.squared

R2
R2adj


man_R2(datos , 'y' , 'x2' , 'x7' , 'x8')
man_R2_adj(datos , 'y' , 'x2' , 'x7' , 'x8')

qqnorm(m1$residuals)
qqline(m1$residuals)

ggplot(datos, aes(sample = m1$residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales") +
  theme_minimal()

shapiro.test(m1$residuals)

ggplot(m1, aes(x=m1$fitted.values, y=m1$residuals)) +
  geom_point(alpha=0.7,color='blue',size=2) +
  labs(
    title = 'Grafico Residuales vs Respuesta Predica',
    x = 'Juegos ganados',
    y = 'Residuales'
  ) +
  theme_minimal()
