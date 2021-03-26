n = int(input())
older = 0
habitantes = 0
arianas = 0
while n != -1:
    sexo, cabelo, olho = input().split()
    if n > older:
        older = n
    if n >= 18 and n <= 35 and sexo == 'f' and cabelo == 'l' and olho == 'v':
        arianas += 1
    habitantes += 1
    n = int(input())
print ('Mais velho: {}'.format(older))
print ('Mulheres com olhos verdes, loiras com 18 a 35 anos: {:.2f}%'.format(arianas*100/habitantes))