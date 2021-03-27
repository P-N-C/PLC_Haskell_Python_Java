def main():
    line = str(input())
    while(line != 'NAO QUERO MAIS'):
        line = line.lower()
        
        white = len(list(filter(lambda x: x == ' ', line)))
        print(white)

        aA = len(list(filter(lambda x: x == 'a', line)))
        print(aA)

        dic = {}
        for i in range(1, len(line)):
            if line[i].isalpha() and line[i-1].isalpha():
                pair = (line[i-1],line[i])
                if pair in dic:
                    dic[pair] += 1
                else:
                    dic[pair] = 1
        if len(dic) == 0:
            print('NENHUM PAR')
        else:
            resp_p = (':)',':(')
            maxi = 0
            for entry in dic.items():
                if entry[1] > maxi:
                    maxi = entry[1]
                    resp_p = entry[0]
            print(maxi)
            print(resp_p[0]+resp_p[1])
    
        line = str(input())

if __name__ == "__main__":
    main()