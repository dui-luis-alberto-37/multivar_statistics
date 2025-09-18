x_names = extraer_datos_lm(m1)$nombres_x
x_names
lm_coefficients(datos, 'Calidad', c(x_names))

plot_y_vs_xvars(m1)

F0_test_values(m1)

qq_residuals(m1)
res_vs_fitt(m1)
res_vs_vars_plot(m1)

intervalos_conf_media_y(c(1.0, 3.3, 2.8, 3.1, 4.1, 1), m1)
source('../../mylibrary/RegresionMultiple.R')
intervalos_pred_y(c(1.0, 3.3, 2.8, 3.1, 4.1, 1), m1)

var_corr(m1)
