#DSC510
#Week 8
#Programming Assignment 8
#Author: Milan Sherman
#7-29-21



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
def process_line(line, dict):
    import regex as re
    res = re.findall(r'\w+', line)
    for word in res:
        add_word(word, dict)

# this function provides easy to read output from the word count dictionary
def process_file(fname2, dict):
    with open(fname2, "w") as f:
        f.write('Length of the dictionary:' + str(len(dict)))
        f.write('\nWord' + '                 ' + 'Count')
        f.write('\n----------------------------')
        sort_orders = sorted(dict.items(), key=lambda x: x[1], reverse=True)
        for key, value in sort_orders:
            f.write("\n{0:20}{1:5d}".format(key,value))

def main():
    word_count_dict = {}
    fname = str(input('What would you like to name the file? '))
    fname2 = fname +".txt"
# read the file containing the speech
    try:
        with open("speech.txt", "r") as speechFile:
            speech = speechFile.readlines()
            for line in speech:
                process_line(line, word_count_dict)  # for each line, call process_line()
                # for word in word_count_dict:
                #     count = word_count_dict[word]
            # with open(fname2, "w") as output_file:

            process_file(fname2, word_count_dict)
    except FileNotFoundError as n:
        print(n)

if __name__ == "__main__":
    main()

# Change#: 2
# Change made: updated to write to file instead of print to screen
# Date of change: 7-31-21
# Author: Milan Sherman
# Change approved by: Milan Sherman
