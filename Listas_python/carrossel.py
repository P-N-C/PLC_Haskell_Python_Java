q = int(input())
arr = list(map (lambda x: int(x),input().split()))
mlis1 = 1
lis1 = 1
for h in range(0,q+1):
    for it in range(1,q):
        x = (h+it) % q
        if(arr[x] > arr[x-1]):
            lis1 += 1
        else:
            mlis1 = max(mlis1,lis1)
            lis1 = 1
    mlis1 = max(mlis1,lis1)       
    lis1 = 1 
print(max(mlis1,lis1))