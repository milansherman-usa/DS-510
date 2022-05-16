#DSC510
#Week 5
#Programming Assignment 5
#Author: Milan Sherman
#6-30-21

# this function takes an operation as input and returns the result of that operation on two numbers
def performCalculation(operation):
    if operation == '+':
        sum = num1 + num2
        print(f'The sum is {sum}')
    elif operation == '-':
        diff = num1 - num2
        print(f'The difference is {diff}')
    elif operation == '*':
        prod = num1*num2
        print(f'The product is {prod}')
    elif operation == '/':
        quo = num1/num2
        print(f'The quotient is {quo}')
    else:
        print(f'Invalid operation')

# this function finds the average of a set numbers defined by the user
def calculateAverage():
    iter = int(input('How many numbers do you want to average?'))
    sum = float(0)
    for i in range(iter):
        num = float(input('Please enter a number:'))
        sum = sum + num
    avg = sum/iter
    return avg


print('Welcome to the python calculator.')
print('This program returns the result of a basic arithmetic operation (+, -, *, /) on two numbers,')
print('or calculates the average for a specified set of numbers.')

# the user is prompted to choose a calculator (basic arithmetic or average)
repeat = True
while repeat:
# exceptional handling for user input
    while True:
        try:
            user_input = int(input('Enter 1 to find the result of a basic arithmetic operation, or 2 to find the average a group of numbers: '))
            break
        except:
            print('Invalid Entry')
    if user_input == 1:
        num1 = float(input('Please input a number: '))
        num2 = float(input('Please input another number: '))
        operation = input('Please input a mathematical operation (+, -, *, /): ')
        performCalculation(operation)
    elif user_input == 2:
        average = calculateAverage()
        print(f'The average is {average}')
    else:
        print('Invalid entry')
# prompt user to continue or exit
    response = input('Would you like to continue? (Y/N): ')
    if response == 'N' or response == 'n' or response == 'No' or response == 'no':
        repeat = False

# Change#: 1
# Change made: initial creation
# Date of change: 7-11-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
