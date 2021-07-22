#!/usr/bin/env python3
import sys
import string
import re
from os import path
#from collections import Counter

#*******************************************************************
#*******************************************************************
class txtproccess():
    '''
    this program gives an text file name and gives frequencies of letters, words,
    and successors of a given word from a textfile
    '''
    def __init__(self):
        self.freq = dict() 

    def __FreqSort__(self):
        sorted_key = sorted(self.freq, key=self.freq.get, reverse=True)
        self.FrqSort = [(k, self.freq[k]) for k in sorted_key]
        return sorted_key  
    def first_x(self, x):
        sorted_keys = self.__FreqSort__()
        return [(k, self.freq[k]) for k in sorted_keys[:x]]
    def output_lines(self):
        sorted_keys = self.__FreqSort__()
        return(["{:2}: {}".format(k, self.freq[k]) for k in sorted_keys])
#*******************************************************************
    def add_item(self, itm):
        if itm in self.freq.keys():
            # if it is already in the dictionary increment it
            self.freq[itm] += 1
        else:
            # if it is not in the keys create new key
            self.freq[itm] = 1

    def __sorted_keys_by_freq__(self):
        return(sorted(self.freq, key=self.freq.get, reverse=True))
    def sorted(self):
        return({k: self.freq[k] for k in self.__sorted_keys_by_freq__()})
    def first_x(self, x):
        sorted_keys = self.__sorted_keys_by_freq__()
        return [(k, self.freq[k]) for k in sorted_keys[:x]]
    def output_lines(self):
        sorted_keys = self.__sorted_keys_by_freq__()
        return(["{:2}: {}".format(k, self.freq[k]) for k in sorted_keys])
#*******************************************************************
class Successor():
    def __init__(self):
        #self={} 
        self.succFreq = {'a':0}       
    def succ_Freq(self,t,word):

        sec_word_counter= Counter()        
        for i,w in enumerate(t): 
            if  w == word:
                succ = t[i+1]
                sec_word_counter[succ]+=1
        self.succFreq[word] = dict(sec_word_counter)
    def first_XSuccessor(self,x):
        self.first_XSuccessor={}
        for word in self.succFreq.keys():
            sorted_key = sorted(self.succFreq[word],key=self.succFreq[word].get,reverse=True)
            temp= [(k, self.succFreq[word][k])for k in sorted_key]
            self.first_XSuccessor[word]=temp[:x]
#*####******************************************************************
class Successors:
    def __init__(self):

        self.succFreq = dict()

    def add_successor(self, word, succ):
        if word in self.succFreq.keys():
            # if the word already exists in the dictionary only add the successor
            if succ in self.succFreq[word].keys():
                # if the successor already exists in the successors dictionary
                # increment the occurance count
                self.succFreq[word][succ] += 1
            else:
                # otherwise create successor
                self.succFreq[word][succ] = 1
        else:
            # if the word is not exist in dictionary create word and
            # add the first successor
            self.succFreq[word] = {succ: 1}

        
    def __sorted_keys_by_freq__(self, dic):
        return(sorted(dic, key=dic.get, reverse=True))
    def first_x_successors(self, x):
        self.first_XSuccessor={}
        for word in self.succFreq.keys():
            sorted_key = sorted(self.succFreq[word],key=self.succFreq[word].get,reverse=True)
            temp= [(k, self.succFreq[word][k])for k in sorted_key]
            self.first_XSuccessor[word]=temp[:x]
#*******************************************************************
def custom_split(str):
    seps = '[' + string.whitespace + string.punctuation + ']+'
    return(re.split(seps, str))

def read_all_text(filename):
    with open(filename, "r") as text_file:
        all = text_file.read()
    return(all)

def is_word(str):
    return(str.isalpha())
#*******************************************************************
#*******************************************************************
def return_stats(filename):
    letters = txtproccess()
    words = txtproccess()
    successor = Successors()

    with open(filename, "r") as text_file:
        all_text = custom_split(text_file.read())
        all_text = [w.lower() for w in all_text if is_word(w)]
        letters.add_item(all_text[0])
        words.add_item(all_text[0])
        for i in range(1, len(all_text)):
            prev_word = all_text[i-1]
            w = all_text[i]
            
            words.add_item(w)
            successor.add_successor(prev_word, w)
    
            for c in w:
                letters.add_item(c)
    '''print('number of successor.succFreq.keys()',len(successor.succFreq.keys())) 
    print('successor.succFreq.{"frush"})',successor.succFreq['frush'])'''
    successor.first_x_successors(3)
    return (letters, words,successor)

#*******************************************************************
#*******************************************************************
def output_generator(letters, words, successors, in_fname):
    letter_outs = letters.output_lines()
    first_5_word = words.first_x(5)
    word_succs_outs = []
    word_padding = max(first_5_word, key=lambda x:len(x[0]))[0].__len__() + 1
  
    for w,f in first_5_word:
        word_succs_outs.append("{:{width}} ({} occurances)".format(w, f, width = word_padding))
        succs = list(successors.first_XSuccessor[w])
        succs_padding = max(succs, key=lambda x:len(x[0]))[0].__len__() + 1
        for sw, sf in succs:
            word_succs_outs.append("\t{:{width}} : {}".format(sw, sf, width = succs_padding))
   
    unique_words = []
    Total_words = 0
    unique_words_num = 0
    for itm in words.freq:
        Total_words += words.freq[itm]
        if words.freq[itm] == 1:
            unique_words_num += 1
            unique_words.append(itm)
        

    header = ["Text Statistics of \""+in_fname+"\" File"]
    seperator_line = ["*"*60]
    letter_header = ["."*60,'Letter Frequencies',"."*60]
    word_header = ["."*60,'Words and Successors', "."*60]
    unique_header=["."*60,'Unique Words ', "."*60]
    words_Numbers=["."*60,'Unique Words & Total words numbers ', "."*60]
    Num = ['Unique Words Number = ',unique_words_num, 'Total words Number  = ',Total_words]
    out_list = seperator_line + header + seperator_line + \
                letter_header + letter_outs + \
                word_header + word_succs_outs +\
                 words_Numbers + Num+ seperator_line
    return out_list

def print_stdout(out):
    for line in out:
        print(line)

def print_fout(out_fname, out):
    org_stdout = sys.stdout
    sys.stdout = open("./" + out_fname, "w")
    print_stdout(out)
    sys.stdout = org_stdout

def run_from_pkg(in_fname, out_fname = None):
    # get the statistics about the text file
    letters, words, successors = return_stats(in_fname)
    output_lines = output_generator(letters, words, successors, in_fname)
    print_stdout(output_lines)
    if out_fname:
        print_fout(out_fname, output_lines)

def run_terminal():
    if len(sys.argv)==1:
        print("It need a text file name as the second argument !")
        sys.exit()
    elif len(sys.argv) > 3:
        print("Max three arguments are expected! ")
        sys.exit()
    elif not path.isfile(sys.argv[1]):
        print(f"There is no such a file named : {sys.argv[1]}")
        sys.exit()
    elif len(sys.argv) == 3:
        out_fname = sys.argv[2]
        out_flag = 1
    else:
        out_flag = 0

    in_fname = sys.argv[1]

    # get the statistics about the text file
    letters, words, successors = return_stats(in_fname)

    output_lines = output_generator(letters, words, successors, in_fname)

    print_stdout(output_lines)

    if out_flag:
        print_fout(out_fname, output_lines)


if __name__ == '__main__':
    run_terminal()
    #run_from_pkg('shakespeare.txt', out_fname = 'out_fname.txt')
    
