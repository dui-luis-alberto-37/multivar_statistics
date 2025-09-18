import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf


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
        
        Ftest = self.Ftest()
        
        
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
        SCE = Y.T@Y - betas@X.T@Y
        SCT = Y.T@Y - sum(Y)**2/n
        
        self.SC =  {'R': SCR,
                    'E': SCE,
                    'T': SCT}
        
        df_anova['Suma de cuadrados'] = self.SC.values()
        
        self.GL =  {'R': p-1,
                    'E': n-p,
                    'T': n-1}
        
        df_anova['Grados de libertad'] = self.GL.values()
        
        self.CM = {k:self.SC[k]/self.GL[k] for k in ('R','E','T')}
        
        df_anova['Cuadrados medios'] = self.CM.values()
        
        df_anova['F_0'] = [self.CM['R']/self.CM['E'], pd.NA, pd.NA]
        
        return pd.DataFrame(df_anova)
    
    
    def Ftest(self):
        pass

m1 = LinearModel('y', ['x2', 'x7', 'x8'], 'Liga_nacional_de_futbol.csv')
print(m1.anova_table())


