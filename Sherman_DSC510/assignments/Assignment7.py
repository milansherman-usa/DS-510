#DSC510
#Week 7
#Programming Assignment 7
#Author: Milan Sherman
#7-22-21

word_count_dict = {}

# this function checks to see if a word is in a dictionary
# if it's not in the dictionary it is added with count = 1
# if it is in the dictionary it increments the count
def add_word(word, dict):
    if word.lower() not in dict:
        dict[word.lower()] = 1
    else:
        dict[word.lower()] +=1

# this function takes a line of text and separates it into distinct words, removing punctuation
# it then calls the add_word() function to create a dictionary of words and associated frequency
def process_line(line):
    import regex as re
    res = re.findall(r'\w+', line)
    for word in res:
        add_word(word, word_count_dict)

# this function provides easy to read output from the word count dictionary
def pretty_print(dict):
    length = len(dict.keys())
    print(f'Length of the dictionary: {length} ')
    print('Word' + '                 ' + 'Count')
    print('----------------------------')
    sort_orders = sorted(dict.items(), key=lambda x: x[1], reverse=True)
    for key, value in sort_orders:
        print("{0:20}{1:5d}".format(key,value))

def main():
# read the file containing the speech
    speechFile = open('C:/Users/misherman/source/DSC510Summer2021/Sherman_DSC510/assignments/speech.txt')
    speech = speechFile.readlines()  # separate the speech into lines
    for line in speech:
        process_line(line)  # for each line, call process_line()
        for word in word_count_dict:
            count = word_count_dict[word]
    pretty_print(word_count_dict)  # call pretty_print() function to generate output

if __name__ == "__main__":
    main()

# Change#: 1
# Change made: file created
# Date of change: 7-22-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
