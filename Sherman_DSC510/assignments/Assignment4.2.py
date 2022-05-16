#DSC510
#Week 4
#Programming Assignment 4
#Author: Milan Sherman
#6-25-21



# this function determines the cost per foot for fiber optic
# cable installation depending on the number of feet to be installed
# 100 feet or less: $0.87/ft
# 100 - 250 feet: $0.80/ft
# 251 - 500 feet: $0.70/ft
# more than 500 feet: $0.50/ft

def cost(cable_length):
    if cable_length > 100 and cable_length <= 250:
        cost = 0.80
    elif 250 < cable_length <= 500:
        cost = 0.70
    elif cable_length > 500:
        cost = 0.50
    else:
        cost = 0.87
        return cost

# this function determines the total installation cost based on
# the number of feet of cable to be installed
# it calls the cost() function to determine the price per foot
def cost_calculator(cable_length):
    price_per_foot = cost(cable_length)
    installation_cost = cable_length*price_per_foot
    return installation_cost

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

# call the cost_calculator function to determine the total cost of installation
    installation_cost = cost_calculator(cable_length)
# format installation cost to two decimal places for currency
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
