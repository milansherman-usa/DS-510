

temperatures =[]
cont = True
end = False
i = 0
print('This program will take any number of temperatures and return the maximum and minimum temperature and the number of temperatures entered.')

while cont:
    while True:
        try:
            temp = float(input('Please input a temperature as a number or any letter to exit: '))
            break
        except:
            end = True
            break
    if end == True:
        cont = False
    else:
        temperatures.insert(i, temp)
        i = i + 1
        print(temperatures)

max = max(temperatures)
min = min(temperatures)
length = len(temperatures)
print(f'The maximum temperature is: {max}')
print(f'The minimum temperature is: {min}')
print(f'The number of temperatures is: {length}')

