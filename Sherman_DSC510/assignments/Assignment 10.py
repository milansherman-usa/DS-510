#DSC510
#Week 10
#Programming Assignment 10.2
#Author: Milan Sherman
#8-14-21

import locale

locale.setlocale(locale.LC_ALL, '')

class CashRegister:
    """A simple attempt to model a cash register"""
    items = []

    def __init__(self):
        """Initialize price attribute"""

    def getTotal(self):
        return sum(self.items)

    def getCount(self):
        return len(self.items)

    def additem(self, price):
        self.items.append(price)

cart = CashRegister()

while True:
    user_input = input('Please input an item price as a number or EXIT to end: ')
    if user_input.upper() == 'EXIT':  # designate sentinel value
        break
    else:
        try:
            price = float(user_input)
            cart.additem(price)
        except:
            print('Invalid price')
            break

total = locale.currency(cart.getTotal())
print(f'The total cost of your cart is',total)
print('The total number of items in your cart is', cart.getCount())

# Change#: 1
# Change made: file created
# Date of change: 8-14-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
