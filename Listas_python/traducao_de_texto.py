n = int(input())
dic = {}
for i in range(0,n):
    a, _, b = str(input()).split()
    dic[a] = b
line = str(input())

while line != '*':
    words = line.split()
    print(dic[words[0]],end='')

    for i in range(1, len (words)):
        l = words[i]
        print(' ' + dic[l], end='')
    print()
    line = str(input())