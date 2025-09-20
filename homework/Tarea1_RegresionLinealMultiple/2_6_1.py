from importador import LinearModel

m1 = LinearModel('y', ['x2', 'x7', 'x8'], 'Liga_nacional_de_futbol.csv')
print('Resultados del modelo 1:')
print('------------------------')
print('Parametros estimados:')
print(m1.params, '\n')
print('Tabla ANOVA:')
print(m1.anov_t, '\n')
print('¿El modelo es significativo?')
print(m1.is_significant, '\n')
print('Resultado de la prueba F:')
print(m1.Ftest(), '\n')
print('Resultado de la prueba t para cada parámetro:')
print(m1.Ttest(), '\n')
print('R^2_values:')
print(m1.R2_values(), '\n')
print('QQ plot de los residuales:')
print(m1.qq_plot(show = False), '\n') #cambie show = True si quiere ver el grafico
print('Residuales contra valores ajustados:')
print(m1.residuals_vs_fitted(show = False), '\n') #cambie show = True si quiere ver el grafico
print('Residuales contra cada variable predictora:')
print(m1.residuals_vs_variables(show = False), '\n') #cambie show = True si quiere ver el grafico
print('Intervalos de 95% para los coeficientes de regresión:')
print(m1.IC_params())
print('Intervalos de 95% para la media de Y dado X = [2300, 56, 2100]:')
print(m1.IC_mean_y([2300, 56, 2100]))
print('Intervalos de 95% para la predicción de Y dado X = [2300, 56, 2100]:')
print(m1.IC_pred_y([2300, 56, 2100]))