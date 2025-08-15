import pandas as pd
class RM():
    def __innit__(self, data:pd.DataFrame, yname:str):
        self.data = data
        self.y = data[yname]
        self.x = data.drop(yname, axis = 1)
        
        self.beta = self._calculate_params()

        return self.beta


    def _calculate_params(self):
        Y = np.array(self.y)
        X = np.array(self.x)
