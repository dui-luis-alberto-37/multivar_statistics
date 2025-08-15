x1 = (41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57.0, 63.5, 64.3, 71.1, 77.0, 77.8)
x2 = (29.1, 29.3, 29.5, 29.7, 29.9, 30.3, 30.5, 30.7, 30.8, 30.9, 31.5, 31.7, 31.9, 32.0, 32.1, 32.5, 32.9)
y  = (251.3, 251.3, 248.3, 267.5, 273.0, 276.5, 270.3, 274.9, 285.0, 290.0, 297.0, 302.5, 304.5, 309.3, 321.7, 330.7, 349.0)

import numpy as np

X = []

for x_1, x_2 in zip(x1,x2):     # * Creacion de la matriz X
    X.append([1,x_1,x_2])

X = np.array(X)
print(X, X.shape)

Y = np.array(y).T               # * Creacion de la matriz Y

print(Y, Y.shape)


Xt = X.T                        # * Matrices transpuestas
Yt = Y.T

beta = np.linalg.inv(Xt@X)@Xt@Y
print(beta)

SSE = Yt@Y -beta.T@X.T@Y
print(SSE)

n = len(Y)
print(n)

p = len(beta)
print(p)

gl = n-p

varest = SSE / gl
print(varest)