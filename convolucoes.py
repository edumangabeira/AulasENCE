# exercicio 2, cap 7.1, Snell e Grinstead

stock = {-1:0.25,0:0.5,1:0.125,2:0.125}
soma = 0

print("Distribuicão de Z = X + Y \n\n")
# [-2,4] é o intervalo de Z
for z in range(-2, 5):
    f_z = 0
    for change in stock:
        try:
            f_z = f_z + stock[change] * stock[z-change]
        except KeyError:
            f_z = f_z + 0
    print("f(Z={}) = {}".format(z, f_z))
    print("\n")
    soma = soma + f_z
    z +=1
print("soma = {}".format(soma))