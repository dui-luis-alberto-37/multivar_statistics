from importador import LinearModel

m1 = LinearModel('y', '.', 'Liga_nacional_de_futbol.csv', minus = 'Equipo')
print('Variables selecionadas en el forward stepwise:')
print(m1.forward_stepwise(),'\n')
print('Variables selecionadas en el bacward stepwise:')
print(m1.backward_stepwise(), '\n')
print('Variables selecionadas en el both stepwise:')
print(m1.both_stepwise())