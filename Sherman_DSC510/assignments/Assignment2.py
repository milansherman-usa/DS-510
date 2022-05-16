#DSC510
#Week 2
#Programming Assignment 2
#Author: Milan Sherman
#6-20-21

# welcome message
print('Welcome the Fiber Optics Installation cost calculator tool')
company_name = input('What is the name of your company?')

# make sure that the user provides a number for length of cable to be installed
while True:
    try:
        cable_length = int(input('How many feet of cable would you like to have installed?'))
        break
    except:
        print('Please input a number')

# compute cost
installation_cost = cable_length*0.87

# print receipt
print('****************************************')
print('RECEIPT')
print(f'Company:{company_name}')
print(f'Number of feet of cable to be installed: {cable_length}')
print(f'Total cost: ${round(installation_cost,2)}')
print('****************************************')
