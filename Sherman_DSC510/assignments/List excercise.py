

def pretty_print(x):
    spam_list = input("Enter the elements of a list separated by a space: ")
    spam = spam_list.split()
    string = [0]*len(x)
    print(string)
    list_len = len(x)
    for i in range(list_len):
        if i != (list_len-1):
            string[i] = (str(x[i]))
        else:
            string[i] = ('and ' + str(x[i]))
    print (*string, sep= ", ")

def main():
    repeat = True
    print('Welcome the list generator!')
    while repeat:
        pretty_print(spam)
        exit = input("""
Would you like to generate another list? (Y/N) 
""")
        if exit.upper() == 'N':
            repeat = False

if __name__ == "__main__":
    main()
