#DSC510
#Week 3
#Programming Assignment 3
#Author: Milan Sherman
#6-22-21

# welcome message
print('Welcome to the Fiber Optics Installation cost calculator tool')

# include a while loop around the whole program to prompt users to get
# another estimate before quitting
repeat = True

while repeat:
    print('')
    company_name = input('What is the name of your company?')

    # make sure that the user provides a number for length of cable to be installed
    while True:
        try:
            cable_length = int(input('How many feet of cable would you like to have installed?'))
            break
        except:
            print('Please input a number')

    # compute cost based on the amount of cable to be installed
    # 100 feet or less: $0.87/ft
    # 100 - 250 feet: $0.80/ft
    # 251 - 500 feet: $0.70/ft
    # more than 500 feet: $0.50/ft

    if cable_length > 100 and cable_length <= 250:
        installation_cost = cable_length*0.80
    elif 250 < cable_length <= 500:
        installation_cost = cable_length*0.70
    elif cable_length > 500:
        installation_cost = cable_length*0.50
    else:
        installation_cost = cable_length*0.87

    # format cost to two decimal places for currency
    installation_cost_two_decimal = "{:.2f}".format(installation_cost)

    # print receipt
    print('')
    print('****************************************')
    print('RECEIPT')
    print(f'Company:{company_name}')
    print(f'Number of feet of cable to be installed: {cable_length}')
    print(f'Total cost: ${installation_cost_two_decimal}')
    print('****************************************')
    print('')
    response = input('Would you like to get another estimate? (Y/N)')
    if response == 'N' or response == 'n':
        repeat = False

# Change#: 1
# Change made: added conditional for bulk discount on installation cost (lines 19-27)
# Date of change: 6-22-21
# Author: Milan Sherman
# Change approved by: Milan Sherman

# Change#: 2
# Fixed typos (lines 8 and 21)
# Date of change: 6-23-21
# Author: Milan Sherman
# Change approved by: Milan Sherman

# Change#: 3
# Added While loop to continue providing estimates until the user chooses to escape (lines 12, 14, 48-50)
# Date of change: 6-23-21
# Author: Milan Sherman
# Change approved by: Milan Sherman

# Change#: 4
# Fixed formatting of cost to two decimal places (line 37)
# Date of change: 6-25-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
