import requests

#repeat = True
#while repeat:
        # joke = input ('Wanna hear a Chuck Norris joke? (Y/N) ')
        # if joke.upper() == 'YES' or joke.upper() == 'Y':
url = "http://www.BikeReg.com/api/search"
querystring = {"eventtype":"cyclocross", "year":"2020", 'EventName': 'Sunday Cyclocross Practice'}
headers = {'cache-control':'no-cache'}
response = requests.request("GET", url, headers = headers, params=querystring)
responseJson = response.json()
print(responseJson)
