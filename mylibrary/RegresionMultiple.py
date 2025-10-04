import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats 
import seaborn as sns
from copy import deepcopy
# import matplotlib
# matplotlib.use('Agg') 


def read_data(name, test = False):
    if type(name) == pd.DataFrame:
        for col in name.columns:
            name[col] = pd.to_numeric(name[col])
        return name
    path = './data/' + name
    if test: path = '.' + path
    if path[-3:] == 'csv':
        df = pd.read_csv(path)
    elif path[-4:] == 'xlsx':
        df = pd.read_excel(path)
    return df

def anova_between_lm(lm_0, lm_1):
    if lm_0.p > lm_1.p:
        lm_0, lm_1 = lm_1, lm_0
    scr_0 = lm_0.SC['E']
    scr_1 = lm_1.SC['E']
    glr_0 = lm_0.GL['E']
    glr_1 = lm_1.GL['E']
    F_0 = ((scr_0 - scr_1)/(glr_0 - glr_1)) / (scr_1/glr_1)
    df = {'Modelo': ['Completo', 'Reducido'],
            'Grados de libertad': [glr_0, glr_1],
            'Suma de cuadrados': [scr_0, scr_1],
            'Suma de Cuadrados': [pd.NA, scr_0 - scr_1],
            'F_0': [pd.NA, F_0],
            'p_value': [pd.NA, 1 - stats.f.cdf(F_0, glr_0 - glr_1, glr_1)]
            }
    return pd.DataFrame(df)

class LinearModel():
    def __init__(self, y, x, name, minus = None):
        self.name = name
        df = deepcopy(read_data(name))
        self.rawdata = df
        
        self.Y = np.array(self.rawdata[y])
        
        self.n = len(self.Y)
        if x == '.':
            x = list(self.rawdata.columns)
            x.remove(y)

        if minus:
            if type(minus) == str:
                minus = [minus]
            for m in minus:
                x.remove(m)
            
        self.p = len(x) + 1
        
        ones = np.ones((self.n,self.p))
        ones[:,1:] = self.rawdata[x]
        self.X = np.array(ones)
        
        self.y_name = y
        self.x_names = x
        betas = self.betas()
        self.params = betas
        self.fitted_values = self.X@self.betas_
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
            
        self.betas_ = betas

        return pd.DataFrame(df_betas)
    
    def anova_table(self):
        df_anova = {'Fuente de variación' : ['Regresión', 'Residuales', 'Total']}
        
        betas = self.betas_
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
        betas = self.betas_
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
        betas = self.betas_
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
        betas = self.betas_
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
        betas = self.betas_
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

    def coorplot(self, show = True):
        coor = self.rawdata[self.x_names].corr(method='pearson')
        plt.figure(figsize=(8, 6))
        sns.heatmap(coor, 
                    annot=True, 
                    cmap='coolwarm_r', 
                    center=0,
                    square=True,
                    vmax=1, vmin=-1)
        if show:
            plt.show()
        plt.close()
        return None
    
    def forward_stepwise(self, alpha=0.05):
        selected = []
        remaining = deepcopy(list(self.x_names))
        best_score = float('inf')
        while remaining:
            best_candidate = None
            for x in remaining:
                model = LinearModel(self.y_name, selected + [x], self.name)
                pvals = model.Ttest()['p_value']
                min_pval = pvals[1:].min()
                if min_pval < best_score:
                    best_score = min_pval
                    best_candidate = x
            if best_score >= alpha:
                break
            if best_candidate is None:
                break
            selected.append(best_candidate)
            remaining.remove(best_candidate)
                

            # scores_with_candidates = []
            # for candidate in remaining:
            #     pvals = self.Ttest()['p_value']
            #     max_pval = pvals[1:].max()
            #     scores_with_candidates.append((pvals[candidate], candidate))
            #     print(scores_with_candidates)
            # scores_with_candidates.sort()
            # best_pval, best_candidate = scores_with_candidates[0]
            # if best_pval < alpha:
            #     selected.append(best_candidate)
            #     remaining.remove(best_candidate)
            # else:
            #     break
        return selected
    
    def backward_stepwise(self, alpha=0.05):
        selected = deepcopy(list(self.x_names))
        while len(selected) > 0:
            model = LinearModel(self.y_name, selected, self.name)
            pvals = model.Ttest()['p_value']
            worst_pval = pvals[1:].max()

            if worst_pval > alpha:
                worst_var = pvals[1:].idxmax()
                selected.remove(worst_var)
            else:
                break
        return selected
    
    def both_stepwise(self, alpha=0.05):
        selected = []
        remaining = deepcopy(list(self.x_names))
        while True:
            # Forward step
            changed = False
            scores_with_candidates = []
            for candidate in remaining:
                    # print(selected, remaining)
                model = LinearModel(self.y_name, selected + [candidate], self.name)
                pvals = model.Ttest()['p_value']
                max_pval = pvals[1:].max()
                scores_with_candidates.append((max_pval, candidate))
            scores_with_candidates.sort()
            if scores_with_candidates and scores_with_candidates[0][0] < alpha:
                best_candidate = scores_with_candidates[0][1]
                selected.append(best_candidate)
                remaining.remove(best_candidate)
                changed = True
            # Backward step
            new_model = LinearModel(self.y_name, selected, self.name)
            pvals = new_model.Ttest()['p_value']
            worst_pval = pvals[1:].max()
            if worst_pval > alpha:
                worst_var = pvals[1:].idxmax()
                selected.remove(worst_var)
                remaining.append(worst_var)
                changed = True
            if not changed:
                break
        return selected