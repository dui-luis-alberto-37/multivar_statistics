import numpy as np
y = [10.3, 10.3, 10.2, 16.4, 18.8, 19.7, 15.6, 18.2, 22.6, 19.9, 24.2, 21.0, 21.4, 21.3, 19.1, 22.2, 33.8, 27.4, 25.7, 24.9, 34.5, 31.7, 36.3, 38.3, 42.6, 55.4, 55.7, 58.3, 51.5, 51.0, 77.0]
x2 = '70 65 63 72 81 83 66 75 80 75 79 76 76 69 75 74 85 86 71 64 78 80 74 72 77 81 82 80 80 80 87'
x1 = '8.3  8.6  8.8 10.5 10.7 10.8 11.0 11.0 11.1 11.2 11.3 11.4 11.4 11.7 12.0 12.9 12.9 13.3 13.7 13.8 14.0 14.2 14.5 16.0 16.3 17.3 17.5 17.9 18.0 18.0 20.6'


x1 = list(map(float, x1.split()))
x2 = list(map(float, x2.split()))

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