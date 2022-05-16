import requests
url = "https://api.chucknorris.io/jokes/random"
#querystring = {"APPID":"Z_iPV61QSta15P-hBu10Mw"}
#headers = {'cache-control':'no-cache'}
response = requests.request("GET", url)
responseJson = response.json()

print(responseJson)
print(responseJson["value"])
