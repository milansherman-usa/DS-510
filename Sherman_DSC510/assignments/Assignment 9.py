#DSC510
#Week 9
#Programming Assignment 9
#Author: Milan Sherman
#7-05-21

import requests
print('Welcome to the Chuck Norris joke generator. We\'ve got more jokes than Chuck Norris has muscles'
      ' (just kidding, that\'s not possible)')
repeat = True
while repeat:
        # joke = input ('Wanna hear a Chuck Norris joke? (Y/N) ')
        # if joke.upper() == 'YES' or joke.upper() == 'Y':
        url = "https://api.chucknorris.io/jokes/random"
        querystring = {"APPID":"Z_iPV61QSta15P-hBu10Mw"}
        headers = {'cache-control':'no-cache'}
        response = requests.request("GET", url, headers = headers, params=querystring)
        responseJson = response.json()
        print(responseJson["value"])

        joke = input('Wanna hear another joke? (Y/N): ')
        if joke.upper() == 'NO' or joke.upper() == 'N':
            repeat = False
print('Chuck Norris thanks you for stopping by!')

# Change#: 1
# Change made: file created
# Date of change: 7-05-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
