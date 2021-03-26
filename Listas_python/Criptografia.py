def main():
    n = int(input())
    for i in range(0,n):
        text = str(input())[::-1]
        text = ''.join(map(lambda x: chr(ord(x)+3) if x.isalpha() else x, text))
        pivo = int(len(text) / 2)
        revt = text[pivo:]
        text = text[:pivo] 
        revt = ''.join(map(lambda x: chr(ord(x)-1), revt))
        print(text + revt)

if __name__ == "__main__":
    main()