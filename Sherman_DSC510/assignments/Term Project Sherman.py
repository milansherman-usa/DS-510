#DSC510
#Week 10
#Term Project
#Author: Milan Sherman
#7-22-21
#
import requests

url = "http://api.openweathermap.org/data/2.5/weather"

# user_input() obtains and validates user input for how to look up weather (city or zip)
# and the units to use for temperatures
def user_input():
    isLookupValid = False
    isUnitsValid = False

    while isLookupValid == False:
        try:
            user_input.lookup = int(input("""
Would you like to look up weather by US city or zip code? 
Enter 1 for US City or 2 for zip code: 
"""))
        except:
            print('Invalid Entry.')
        if user_input.lookup == 1 or user_input.lookup == 2:
            isLookupValid = True
        else:
            print('Invalid look up entry.  Try again.')

    while isUnitsValid == False:
        user_input.units = input("""
Would you like to view temps in Fahrenheit, Celsius, or Kelvin?
Enter F for Fahrenheit, C for Celsius, or K for Kelvin: 
""")
        if user_input.units.upper() == 'F' or user_input.units.upper() == 'C' or user_input.units.upper() == 'K':
            isUnitsValid = True
        else:
            print('Invalid units entry.  Try again.')

# weather() parses the main json file to get the different weather conditions
def weather(responseJson):
    mainJson = responseJson["main"]
    weather.city = responseJson["name"]
    weather.current_temp = float(mainJson["temp"])
    weather.high_temp = float(mainJson["temp_max"])
    weather.low_temp = float(mainJson["temp_min"])
    weather.pressure = mainJson["pressure"]
    weather.humidity = mainJson["humidity"]
    weather.clouds = responseJson["weather"][0]["description"]

# pretty_print() formats and prints the parsed elements of the main json file
def pretty_print(city, clouds, current_temp, high_temp, low_temp, pressure, humidity):
    current_temp_no_decimal = "{:.0f}".format(current_temp)
    high_temp_no_decimal = "{:.0f}".format(high_temp)
    low_temp_no_decimal = "{:.0f}".format(low_temp)
    print('')
    print(f'Current weather conditions for {city}:')
    print(f'Skies: {clouds}')
    print(f'Current Temp: {current_temp_no_decimal} degrees')
    print(f'High Temp: {high_temp_no_decimal} degrees')
    print(f'Low Temp: {low_temp_no_decimal} degrees')
    print(f'Atmospheric pressure: {pressure} hPa')
    print(f'Humidity: {humidity}%')
    print('')

# weather_dict() creates a dictionary for input to the API request based on user input
def weather_dict(lookup, units):
    if lookup == 1:
        cityname = input("""
Please enter the name of a city: 
""")
        state = input("""
Please enter the state abbreviation: 
""")
        city_name = str(cityname + ',' + state + ',US')
        if units.upper() == 'F':
            weather_dict.querystring = {"q":city_name, "APPID":"492c7062e1a348ca21245f3beb564ce5", "units":"imperial"}
        elif units.upper() == 'C':
            weather_dict.querystring = {"q":city_name, "APPID":"492c7062e1a348ca21245f3beb564ce5", "units":"metric"}
        else:
            weather_dict.querystring = {"q":city_name, "APPID":"492c7062e1a348ca21245f3beb564ce5"}
    elif lookup == 2:
        try:
            zip = int(input("""
Please enter a zip code: 
"""))
            if units.upper() == 'F':
                weather_dict.querystring = {"zip":zip, "APPID":"492c7062e1a348ca21245f3beb564ce5", "units":"imperial"}
            elif units.upper() == 'C':
                weather_dict.querystring = {"zip":zip, "APPID":"492c7062e1a348ca21245f3beb564ce5", "units":"metric"}
            else:
                weather_dict.querystring = {"zip":zip, "APPID":"492c7062e1a348ca21245f3beb564ce5"}
        except:
            print('Not a valid zip code.')

# request_api() takes the dictionary from weather_dict(), checks for various errors,
# provides feedback to the user depending on the error code,
# and returns a response to be parsed by the weather() function if no error occurs
# and calls pretty_print() to provide output to the user
def request_api(lookup, units):
    weather_dict(lookup, units)
    response = requests.request("GET", url, params= weather_dict.querystring)
    responseJson = response.json()
    if responseJson["cod"] == 400:
        print('Looks like a bad request.  Try again.')
    elif responseJson["cod"] == 401:
        print('Unauthorized Access.  Check your API key and try again')
    elif responseJson["cod"] == 403:
        print('Access Denied')
    elif responseJson["cod"] == 404:
        print('Data not Found')
    elif responseJson["cod"] == 500:
        print('Internal Server Error')
    elif responseJson["cod"] == 503:
        print('Service Unavailable')
    elif responseJson["cod"] == 200:
        weather(responseJson)
        pretty_print(weather.city, weather.clouds, weather.current_temp, weather.high_temp, weather.low_temp, weather.pressure, weather.humidity)
    else:
        print("""
Please check your input and try again.
""")

# main() welcomes the user to the program, calls user_input() and request_api()
# and prompts the user to do a weather lookup for another city
def main():
    repeat = True
    print('Welcome the the Weather app!  Find current weather conditions in any US city using city name or zip code')
    while repeat:
        user_input()
        request_api(user_input.lookup, user_input.units)
        exit = input("""
Would you like to look up weather in another city? (Y/N) 
""")
        if exit.upper() == 'N':
            repeat = False

if __name__ == "__main__":
    main()

# Change#: 1
# Change made: file created
# Date of change: 7-22-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
