library('MASS')
datasets::swiss
swiss

modelo_completo<-lm(Fertility~., swiss)
paso_a_paso<-stepAIC(modelo_completo, direction="both", trace=T)
summary(paso_a_paso)
ols_step_forward_aic(modelo_completo, details = T)
