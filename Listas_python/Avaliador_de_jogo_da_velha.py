def play():
    mat = []
    for i in range(0,3):
        mat.append(str(input()).upper().split())
    xwinner = owinner = False
    xnumber = onumber = 0
    ilegal = False
    for i in range(0,3):
        line = []
        col = []
        for j in range(0,3):
            if mat[i][j] == 'O':
                onumber += 1
            elif mat[i][j] == 'X':
                xnumber += 1
            line.append(mat[i][j])
            col.append(mat[j][i])
        if line == ['X','X','X'] or col == ['X','X','X']:
            xwinner = True
        if line == ['O','O','O'] or col == ['O','O','O']:
            owinner = True
    diagonal = [mat[0][0],mat[1][1],mat[2][2]]
    invDiagonal = [mat[0][2],mat[1][1],mat[2][0]]
    if diagonal == ['X','X','X'] or invDiagonal == ['X','X','X']:
        xwinner = True
    if diagonal == ['O','O','O'] or invDiagonal == ['O','O','O']:
        owinner = True
    if xwinner == True and owinner == True:
        print('ILEGAL')
    elif xnumber - onumber < 0 or xnumber - onumber > 1:
        print('ILEGAL')
    elif xwinner == True and xnumber - onumber != 1:
        print('ILEGAL')
    elif owinner == True and xnumber - onumber != 0:
        print('ILEGAL')    
    elif xwinner == True:
        print('X_VENCEU')
    elif owinner == True:
        print('O_VENCEU')
    else:
        print('EM_ANDAMENTO')


def main():
    t = int(input())
    for i in range(0,t):
        play()

if __name__ == "__main__":
    main()