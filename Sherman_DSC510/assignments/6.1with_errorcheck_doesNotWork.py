# DSC 510
# Week 6
# Programming Assignment Week 6
# Author Arjun Varma
# 07/12/2021

# CHANGE CONTROL LOG
# Change#:1
# Change(s) Made:
# Change(s) Made:
# Change(s) Made:
# Change(s) Made:
# Change(s) Made:
# Date of Change: 07/12/2021
# Author: Arjun Varma
# Change Approved by: Michael Eller
# Date Moved to Production: 07/12/2021


# Create empty List called temperatures
temperatures = []
# setting variable for user Input
value = input("Enter temperature values in numbers only. Enter DONE to stop: ")
# Implementing error checking for sentinel value, checking for float values
while value.upper() != "DONE":
    try:
        float(value)
    except ValueError:
        print("Not a valid value, try again.")
    value = input("Enter temperature values in numbers only. Enter DONE to stop: ")
# appending values from user input into List
temperatures.append(value)
# starting with first element
index = 0
# While loop to count user values entered and index entry less than count of temp values entered
while index < len(temperatures):
    print(temperatures[index])
# updating loop variable to increment index count by 1
index = index + 1
# setting variable for count of temperature values entered
readings_entered = len(temperatures)
max = max(temperatures)
min = min(temperatures)

# Adding suffix of Degrees as unit to results
txt = "Number of temperature readings entered: = {} Degrees"
print(txt.format(readings_entered))
#print("Number of temperature readings entered: ", readings_entered)
txt = "MAX Temperature Reading Entered: = {} Degrees"
print(txt.format(max))
#print("MAX Temperature Reading Entered: ", max)
txt = "MIN Temperature Reading Entered: = {} Degrees"
print(txt.format(min))
#print("MIN Temperature Reading Entered: ", min)
