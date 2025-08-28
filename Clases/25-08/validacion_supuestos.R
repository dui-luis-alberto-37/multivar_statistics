install.packages('ggplot2')
library(ggplot2)

# tiempo de coccion
yp <-c(6.40, 15.05, 18.75, 30.25, 44.85, 48.85, 51.55, 61.50, 100.44, 111.42)

# ancho del horno
x1 <-c(1.32, 2.69, 3.56, 4.41, 5.35, 6.20, 7.12, 8.87, 9.80, 10.65)

# temperatura
x2 <-c(1.15, 3.40, 4.10, 8.75, 14.82, 15.15, 15.32, 18.18, 35.19, 40.40)
datos<-data.frame(yp, x1, x2)
datos

plot(datos)
g1 = ggplot(data=datos, mapping = aes(x=x1, y= yp)) + geom_point(color = 'blue', size=2) + 
  labs(title = 'yp-x1', x = 'x1', y = 'yp')+ 
  geom_smooth(method='lm', se=FALSE, color='black')+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))
g1

##MULTICOLINEALIDAD 

variables = data.frame(x1, x2)
m_cor=cor(variables, method = 'pearson')
m_cor

install.packages('corrplot')
library(corrplot)

corrplot(m_cor)

modelo1 = lm(formula = yp~x1+x2, data = datos)
summary(modelo1)

modelo2 = lm(formula = yp~x2, data = datos)
summary(modelo2)


# anova
anova(modelo1,modelo2)

modelo3 = lm(formula = yp~x1+x2 -1, data = datos)
summary(modelo3)

# normalidad

residuales = modelo3$residuals
qqnorm(residuales)
qqline(residuales)
plot(modelo3)

# H_0 = la distribucion es normal
# H_1 = no son normales

shapiro.test(residuales)
# alpha = 0.05, el p value es mayor a alpha no se rechaza H_0 

# Homocedasticidad
par(mfrow = c(2, 2))
plot(modelo3)

install.packages('lmtest')
library(lmtest)

bptest(modelo3)

modelo4 <- lm(formula = yp ~ log(x1) + x2 -1, data = datos)

summary(modelo4)

bptest(modelo4)


par(mfrow = c(2, 2))
plot(modelo4)

## no-autocorrelacion

dwtest(modelo4)


# Predictions

nuevo.dato <- data.frame(x1 = 2.10, x2 = 3.10)

prediccion <- predict(modelo4, newdata = nuevo.dato)

paste("La cantidad estimada de tiempo de coccion es:", round(prediccion, 2))


