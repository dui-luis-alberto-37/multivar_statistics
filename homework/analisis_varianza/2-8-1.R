setwd("Tic's/5nto/multivar_statistics/homework/analisis_varianza/")
source("../../mylibrary/RegresionMultiple.R")
datos = datasets::trees
datos

#1)
mc = lm('Volume ~ .', datos); mc

summary(mc)
anov_mc = anova_table(mc); anov_mc


m1 = lm('Volume ~ Girth', datos); m1

summary(m1)
anov_m1 = anova_table(m1); anov_m1

anov_mc;anov_m1

anova_table(mc,m1)

anova(m1,mc)
anova(mc,m1)
anova(mc)
anova(m1)

#2)
m2 = lm('Volume ~ 1', datos)
m2

anov$`F value`
