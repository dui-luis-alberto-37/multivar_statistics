y = (16.68, 11.50, 12.03, 14.88, 13.75,
        18.11, 8.00, 17.83, 79.24, 21.50,
        40.33, 21.00, 13.50, 19.75, 24.00,
        29.00, 15.35, 19.00, 9.50, 35.10,
        17.90, 52.32, 18.75, 19.83, 10.75),
x1 = [7, 3, 3, 4, 6,
        7, 2, 7, 30, 5,
        16, 10, 4, 6, 9,
        10, 6, 7, 3, 17,
        10, 26, 9, 8, 4]
x2 = [560, 220, 340, 80, 150,
        330, 110, 210, 1460, 605,
        688, 215, 255, 462, 448,
        776, 200, 132, 36, 770,
        140, 810, 450, 635, 150]


import numpy as np

X = []

for x_1, x_2 in zip(x1,x2):
    X.append([1,x_1,x_2])

X = np.array(X)
print(X, X.shape)

Y = np.array(y).T

print(Y, Y.shape)


Xt = X.T
Yt = Y.T
beta = np.linalg.inv(Xt@X)@Xt@Y
print(beta)

SSE = Yt@Y -beta.T@X.T@Y
print(SSE)

n = len(Y)
print(n)

p = len(beta)
print(p)

gl = n-p-1

varest = SSE / gl
print(varest)
#import matplotlib.pyplot as plt




#fig, ax = plt.subplots(3)
#ax[0].scatter(y,x1)
#ax[1].scatter(y,x2)
#ax[2].scatter(x1,x2)
#plt.savefig('nose.png')


