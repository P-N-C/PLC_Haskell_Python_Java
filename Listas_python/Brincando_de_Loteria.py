class Player:
    def __init__(self, name, numbers):
        self.name = name
        self.numbers = set()
        self.numbers.update(numbers)
    def calcPoints(self, gold):
        self.points = 0
        for g in gold:
            if g in self.numbers:
                self.points += 1

def main():
    line = str(input())
    players = {}
    counter = 0
    while(line != 'FIM'):
        args = line.split()
        player = Player(args[0], list(map(lambda x:int(x),args[1:])))
        players[args[0]] = player
        line = str(input())
    
    gold = list(map(lambda x: int(x), input().split('-')))
    truePlayers = list(players.values())
    for i in range(0,len(players)):
        truePlayers[i].calcPoints(gold)
    truePlayers.sort(key = lambda x: x.name)
    truePlayers.sort(key = lambda x: x.points)
    for p in truePlayers:
        print (p.name,end=' ')
        for i in range (0,p.points):
            print('+',end='')
        print()

if __name__ == "__main__":
    main()