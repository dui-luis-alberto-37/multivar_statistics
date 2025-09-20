import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats 
# import matplotlib
# matplotlib.use('Agg') 


def read_data(name):
    path = './data/' + name
    if path[-3:] == 'csv':
        df = pd.read_csv(path)
        return df

class LinearModel():
    def __init__(self, y, x, name):
        
        self.rawdata = read_data(name)
        
        self.Y = self.rawdata[y]
        
        self.n = len(self.Y)
        self.p = len(x) + 1
        
        ones = np.ones((self.n,self.p))
        ones[:,1:] = self.rawdata[x]
        self.X = ones
        
        self.y_name = y
        self.x_names = x
        
        betas = self.betas()
        self.params = betas
        self.fitted_values = self.X@self.betas
        self.residuals = self.Y - self.fitted_values
        anov_t = self.anova_table()
        self.anov_t = anov_t

        self.t_critical = stats.t.ppf(0.975, self.GL['E'])

        self.is_significant = self.Ftest(call = False)
        
    def betas(self):
        X = self.X
        Y = self.Y
        betas = np.linalg.inv(X.T@X)@X.T@Y
        df_betas = {'Intercept' : [betas[0]]}
        
        for i, x in enumerate(self.x_names, 1):
            df_betas[x] = [betas[i]]
            
        self.betas = betas

        return pd.DataFrame(df_betas)
    
    def anova_table(self):
        df_anova = {'Fuente de variación' : ['Regresión', 'Residuales', 'Total']}
        
        betas = self.betas
        Y = self.Y
        X = self.X
        n = self.n
        p = self.p
        
        SCR = betas@X.T@Y - sum(Y)**2/n
        sce = Y.T@Y - betas@X.T@Y
        SCT = Y.T@Y - sum(Y)**2/n
        
        self.SC =  {'R': SCR,
                    'E': sce,
                    'T': SCT}
        
        df_anova['Suma de cuadrados'] = self.SC.values()
        
        self.GL =  {'R': p-1,
                    'E': n-p,
                    'T': n-1}
        
        df_anova['Grados de libertad'] = self.GL.values()
        
        self.CM = {k:self.SC[k]/self.GL[k] for k in ('R','E','T')}
        
        df_anova['Cuadrados medios'] = self.CM.values()
        
        F0 = self.CM['R']/self.CM['E']
        
        df_anova['F_0'] = [F0, pd.NA, pd.NA]

        self.F0 = F0
        
        df_anova = pd.DataFrame(df_anova)
        
        return df_anova
    
    def Ftest(self, call = True):
        F0 = self.F0
        glr = self.GL['R']
        gle = self.GL['E']

        p_value = 1 - stats.f.cdf(F0, glr, gle)
        
        f_critical = stats.f.ppf(0.95, glr, gle)

        if call:
            df_result = pd.DataFrame({'F0': [F0],
                                    'p_value': [p_value],
                                    'f_critical': [f_critical],
                                    'is_significant': [p_value < 0.05]})
            print(df_result)
        
        return p_value < 0.05

    def Ttest(self, call = True):
        betas = self.betas
        Y = self.Y
        X = self.X
        n = self.n
        p = self.p
        
        var_e = self.CM['E']
        var_betas = var_e * np.linalg.inv(X.T@X).diagonal()
        se_betas = np.sqrt(var_betas)
        
        gle = self.GL['E']
        
        t_values = betas / se_betas
        p_values = (1 - stats.t.cdf(np.abs(t_values), gle)) * 2
        t_critical = self.t_critical
        
        is_significant = p_values < 0.05
        
        df_ttest = {'Beta': betas,
                    't_value': t_values,
                    't_critical': [t_critical]*p,
                    'p_value': p_values,
                    'is_significant': is_significant}
        
        df_ttest = pd.DataFrame(df_ttest, index=['Intercept'] + self.x_names)
        
        return df_ttest
    
    def R2_values(self):
        SCR = self.SC['R']
        SCT = self.SC['T']
        
        R2 = SCR / SCT
        R2_adj = 1 - (1 - R2) * (self.n - 1) / (self.n - self.p)
        
        df_r2 = pd.DataFrame({'R2': [R2],
                            'R2_adjusted': [R2_adj]})
        
        return df_r2

    def qq_plot(self, show = True):
        sm.qqplot(self.residuals, line ='45')
        if show:
            plt.show()
        plt.close()
        shap = self.shapiro_test(self.residuals)
        
        return shap

    def shapiro_test(self, data):
        stat, p_value = stats.shapiro(data)
        df_result = pd.DataFrame({'W': [stat],
                                'p_value': [p_value],
                                'is_normal': [p_value > 0.05]})
        return df_result
    
    def residuals_vs_fitted(self, show = True):
        plt.scatter(self.fitted_values, self.residuals)
        plt.axhline(0, color ='red', linestyle ='--')
        plt.xlabel('Fitted values')
        plt.ylabel('Residuals')
        if show:
            plt.show()
        
        plt.close()

        shap = self.shapiro_test(self.residuals)

        exog = sm.add_constant(self.fitted_values)
        
        breusch_pagan = sm.stats.diagnostic.het_breuschpagan(self.residuals, exog, robust = True)

        lm_stat, lm_pval, f_stat, f_pval = breusch_pagan

        df_breusch = pd.DataFrame({'LM_stat': [lm_stat],
                                'LM_p_value': [lm_pval],
                                'F_stat': [f_stat],
                                'F_p_value': [f_pval],
                                'is_heteroscedastic': [lm_pval < 0.05]})
        return df_breusch

    def residuals_vs_variables(self, show = True):
        n_vars = len(self.x_names)
        n_cols = 2
        n_rows = (n_vars + 1) // n_cols
        
        fig, axes = plt.subplots(n_rows, n_cols, figsize=(10, 5 * n_rows))
        axes = axes.flatten()
        
        bp_tests_list = []

        for i, x in enumerate(self.x_names):
            axes[i].scatter(self.rawdata[x], self.residuals)
            axes[i].axhline(0, color='red', linestyle='--')
            axes[i].set_xlabel(x)
            axes[i].set_ylabel('Residuals')

            exog = sm.add_constant(self.rawdata[x])
            breusch_pagan = sm.stats.diagnostic.het_breuschpagan(self.residuals, exog, robust = True)   
            breusch_pagan = list(breusch_pagan)
            breusch_pagan.append(breusch_pagan[1] < 0.05)
            bp_tests_list.append(breusch_pagan)
        
        for j in range(i + 1, len(axes)):
            fig.delaxes(axes[j])
        
        bp_tests = pd.DataFrame(bp_tests_list, columns=['LM_stat', 'LM_p_value', 'F_stat', 'F_p_value', 'is_heteroscedastic'], index=self.x_names)

        plt.tight_layout()
        if show:
            plt.show()
        plt.close()
        return bp_tests

    def IC_params(self):
        betas = self.betas
        X = self.X

        var_e = self.CM['E']
        var_betas = var_e * np.linalg.inv(X.T@X).diagonal()
        se_betas = np.sqrt(var_betas)

        t_critical = stats.t.ppf(0.975, var_e)
        left = betas - t_critical * se_betas
        right = betas + t_critical * se_betas
        df_ic = pd.DataFrame({'left': left,
                            'betas': betas,
                            'right': right,
                            'length': right - left}, index=['Intercept'] + self.x_names)
        return df_ic
    
    def IC_mean_y(self, X0):
        if len(X0) != self.p - 1:
            raise ValueError(f'X0 debe tener {self.p - 1} elementos.')
        
        X0 = np.array([1] + X0)
        betas = self.betas
        Y0 = X0 @ betas

        var_e = self.CM['E']
        var_Y0 = var_e * (X0 @ np.linalg.inv(self.X.T @ self.X) @ X0.T)
        se_Y0 = np.sqrt(var_Y0)

        t_critical = self.t_critical
        left = Y0 - t_critical * se_Y0
        right = Y0 + t_critical * se_Y0

        df_ic_mean = pd.DataFrame({'left': [left],
                                'Y0': [Y0],
                                'right': [right],
                                'length': [right - left]})
        
        return df_ic_mean
    
    def IC_pred_y(self, X0):
        if len(X0) != self.p - 1:
            raise ValueError(f'X0 debe tener {self.p - 1} elementos.')
        
        X0 = np.array([1] + X0)
        betas = self.betas
        Y0 = X0 @ betas

        var_e = self.CM['E']
        var_Y0 = var_e * (1 + X0 @ np.linalg.inv(self.X.T @ self.X) @ X0.T)
        se_Y0 = np.sqrt(var_Y0)

        t_critical = self.t_critical
        left = Y0 - t_critical * se_Y0
        right = Y0 + t_critical * se_Y0

        df_ic = pd.DataFrame({'left': [left],
                                'Y0': [Y0],
                                'right': [right],
                                'length': [right - left]})
        
        return df_ic
    
    def complete_summary(self, X0 = None, show = True):
        print('Resultados del modelo 1:')
        print('------------------------')
        print('Parametros estimados:')
        print(self.params, '\n')
        print('Tabla ANOVA:')
        print(self.anov_t, '\n')
        print('¿El modelo es significativo?')
        print(self.is_significant, '\n')
        print('Resultado de la prueba F:')
        print(self.Ftest(), '\n')
        print('Resultado de la prueba t para cada parámetro:')
        print(self.Ttest(), '\n')
        print('R^2_values:')
        print(self.R2_values(), '\n')
        print('QQ plot de los residuales:')
        print(self.qq_plot(show = show), '\n') #cambie show = True si quiere ver el grafico
        print('Residuales contra valores ajustados:')
        print(self.residuals_vs_fitted(show = show), '\n') #cambie show = True si quiere ver el grafico
        print('Residuales contra cada variable predictora:')
        print(self.residuals_vs_variables(show = show), '\n') #cambie show = True si quiere ver el grafico
        print('Intervalos de 95% para los coeficientes de regresión:')
        print(self.IC_params())
        if X0:
            print(f'Intervalos de 95% para la media de Y dado X = {X0}:')
            print(self.IC_mean_y(X0))
            print(f'Intervalos de 95% para la predicción de Y dado X = {X0}:')
            print(self.IC_pred_y(X0))
        
        return None




