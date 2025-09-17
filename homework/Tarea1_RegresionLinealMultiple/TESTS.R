m2 = lm('y ~ x1', datos)
plot_y_vs_xvars(m1)
source('../../mylibrary/RegresionMultiple.R')
m3 = lm('y ~ x6', datos)
plot_y_vs_xvars(m3)

R2_test(m1)
R2_test(m2)

summary(m1)
t0_test_values(m1)
anova(m1)
anova_table(m1)
anova(m1, m2)


intervalos_conf_beta(m2)
intervalos_conf_media_y(c(225, 2), m1)
intervalos_pred_y(c(225, 2), m1)
intervalos_conf_media_y(c(225), m2)
intervalos_pred_y(c(225), m2)
anova(m1)
anova_table(m1)


m2 = lm('y ~ x1 + x2', datos)

F0_test_values(m1)
summary(m1)
source('../../mylibrary/RegresionMultiple.R')
var_corr(m1)
