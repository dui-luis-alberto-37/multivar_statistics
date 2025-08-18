import pandas as pd
import numpy as np

class RM():
    def __init__(self, data:pd.DataFrame, yname:str):
        self.data = data
        self.y = data[yname]
        self.x = data.drop(yname, axis = 1)
        
        self.varset = None
        self.beta = self._calculate_params()

        return None


    def _calculate_params(self):
        Y = np.array(self.y).T
        X = self.x
        # X = np.array(self.x)
        n = len(X)



        nX = [[1] for _ in range(n)]
        for i in range(n):
            nX[i] += list(X.iloc[i])
        
        X = np.array(nX)
        Xt = X.T                        # * Matrices transpuestas
        Yt = Y.T

        beta = np.linalg.inv(Xt@X)@Xt@Y
        SSE = Yt@Y -beta.T@X.T@Y

        p = len(beta)
        gl = n-p

        self.varest = SSE / gl

        return beta