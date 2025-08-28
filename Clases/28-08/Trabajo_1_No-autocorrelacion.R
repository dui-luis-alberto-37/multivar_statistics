library(datarium)
data('marketing')

str(marketing)

modelo1 = lm(formula =sales ~ youtube + facebook - 1, data = marketing)
summary(modelo1)

dwtest(modelo1)
### DW almost 2 then there's no autocorelation
