#DSC510
#Week 6
#Programming Assignment 6
#Author: Milan Sherman
#7-17-21

temperatures =[] # initialize temperatures list
i = 0 # set index
print('This program will take any number of temperatures and return the maximum and minimum temperature and the number of temperatures entered.')

while True:
    user_input = input('Please input a temperature as a number or EXIT to end: ')
    if user_input.upper() == 'EXIT':  # designate sentinel value
        break
    else:
        while True:  # check user input is numeric
            try:
                temp = float(user_input)
                break
            except:
                user_input = input('Please input a temperature as a number or EXIT to end: ')
                if user_input.upper() == 'EXIT':
                    break
    temperatures.insert(i, temp) # add input value to list
    i += 1 # increment index

# find max, min, and number of temperatures input and provide output to user
if i > 0:  # skip output if user's first input is exit
    max = max(temperatures)
    min = min(temperatures)
    length = len(temperatures)
    print(f'The maximum temperature is: {max}')
    print(f'The minimum temperature is: {min}')
    print(f'The number of temperatures is: {length}')
else:
    pass


# Change#: 1
# Change made: file created
# Date of change: 7-17-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
