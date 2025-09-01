library(ggplot2)
install.packages(c("dplyr", "tidyr", "ggplot2", "broom", "car", "mvnormtest"))
install.packages("sass")
install.packages('olsrr' , dependencies = c("Depends", "Imports", "LinkingTo"))
library(olsrr)
library(leaps)
library(GGally)

cemento = read.table("../../data/cement.txt", harder= T, skip = 5)


ggpairs(cemento)

modelo_completo = lm(y~., cemento, x=T, y=T)
sumary(modelo_completo)

## calcula el AIC, CIS, etc sobre todos los subconjuntos posibles
outs = leaps(modelo_completo$x, cemento$y, int=FALSE)
plot(outs$size,outs$Cp, log="y",cex=0.3)
lines(outs$size,outs$size)
text(outs$size, outs$Cp, labels=row(outs$which), cex=0.5, pos=4)

#Mejor modelo por la regla Cp ≈ p  (p = número de predictores)

idx_best_rule <- which.min(abs(outs$Cp - outs$size))

#Variables que entran en cada mejor modelo
noms_x <- colnames(modelo_completo$x)            # nombres de los predictores
vars_rule  <- noms_x[ outs$which[idx_best_rule, ] ]

vars_rule

ejempl0 = ols_step_all_possible(modelo_completo)
plot(ejemplo)