import random


wins = 0
losses = 0
ties = 0
print('ROCK, PAPER, SCISSORS')

while True:
    num = random.randint(0,2)
    if num == 0:
        hand = 'r'
    elif num == 1:
        hand = 's'
    else:
        hand = 'p'

    move = input('Enter your move: (r)ock, (p)aper, (s)cissors, or (q)uit: ')
    if move == 'q':
        break
    elif hand == 'r':
        if move == 'r':
            print('ROCK versus..')
            print('ROCK')
            print('It is a tie!')
            ties = ties + 1
        elif move == 's':
            print('SCISSORS versus..')
            print('ROCK')
            print('It is a loss!')
            losses = losses + 1
        else:
            print('PAPER versus..')
            print('ROCK')
            print('It is a win!')
            wins = wins + 1
    elif hand == 's':
        if move == 'r':
            print('ROCK versus..')
            print('SCISSORS')
            print('It is a win!')
            wins = wins + 1
        elif move == 's':
            print('SCISSORS versus..')
            print('SCISSORS')
            print('It is a tie!')
            ties = ties + 1
        else:
            print('PAPER versus..')
            print('SCISSORS')
            print('It is a loss!')
            losses = losses + 1
    else:
        if move == 'r':
            print('ROCK versus..')
            print('PAPER')
            print('It is a loss!')
            losses = losses + 1
        elif move == 's':
            print('SCISSORS versus..')
            print('PAPER')
            print('It is a win!')
            wins = wins + 1
        else:
            print('PAPER versus..')
            print('PAPER')
            print('It is a tie!')
            ties = ties + 1
    print(str(wins) + ' wins,' + str(losses) + ' losses,' + str(ties) + ' ties')


