#!/usr/bin/env python3
from m_text3 import *
import random as rnd
#import numpy as np

class Text_Generator:
    """
        The class which generates text by using successors.
        Attributes:
            letters : <Letters>
                Letters object prom text_stats package that we created
            words : <Words>
                Words object prom text_stats package that we created
            successors : <Successors>
                Successors object prom text_stats package that we created
            text : <str>
                Generated string.
        Functions:
            probs_successors
                calls succ_Freq(word) from M_text . it needs to evaluate probabilities
                of every successor to 
            next_word
                extract the next word of output text.next_word(word)
                implement a kind of roulette-wheel selection to give word.
            generate_text
                takes starting_word and max_word count and generates as many words as
                it can without passing the max_word condition
            write_file
                directs the stdout to a file and write the current generated text to a file
            print_stdout
                print the current text to terminal.
       """
     
   
    def __init__(self, in_fname):
        self.beta=2
        self.letters, self.words, self.successors = return_stats(in_fname)
        with open(in_fname, "r") as text_file:
            text = custom_split(text_file.read())
            text = [w.lower() for w in text if is_word(w)]
            self.file = text
        self.text = ""
    def probs_successors(self, word):
        #self.successors.succ_Freq(self.file,word)
        succs = self.successors.succFreq[word] #it contains the successors of word with relative frequncies
        worst_freq = min(list(succs.values()))
        
        word_freq = self.words.freq[word]
        if word_freq and succs:
            probs = {k: v/word_freq for k,v in succs.items()}
            return probs
        else:
            return None
        
    
    def next_word(self, word):
        probs = self.probs_successors(word)
        if probs:
            threshold = rnd.random()
            cum_prob = 0
            for word, prob in probs.items():
                if cum_prob + prob > threshold:
                    return word
                else:
                    cum_prob += prob
        else:
            return None
        return word
    def generate_text(self, start_word, max_word=100):
        text = start_word
        curr_word = start_word
        while max_word>0:
            succ = self.next_word(curr_word)
            if succ:
                text = text + " " + succ
                curr_word = succ
            else:
                break
            max_word -= 1
        if text != start_word:
            self.text = text
        else:
            self.text = "No successor of the word '{}'".format(start_word)
            print("No successor of the word '{}'".format(start_word))
    def write_file(self, out_file):
        org_stdout = sys.stdout
        sys.stdout = open("./" + out_file, "w")
        self.print_stdout()
        sys.stdout = org_stdout
    def print_stdout(self):
        print(self.text)

def run_from_pkg(in_fname,start_word, max_word_count=100, out_fname = False):
    text_gen = Text_Generator(in_fname)
    text_gen.generate_text(start_word, max_word_count)
    text_gen.print_stdout()
    if out_fname:
        text_gen.write_file(out_fname)

def run_terminal():
    if len(sys.argv)==1:
        print("There is no argument!")
        sys.exit()
    elif len(sys.argv) > 5:
        print("Max five arguments are expected! ")
        sys.exit()
    elif not path.isfile(sys.argv[1]):
        print("There is no such a file named", sys.argv[1])
        sys.exit()
    elif len(sys.argv) == 5:
        out_fname = sys.argv[4]
        out_flag = 1
    else:
        out_flag = 0

    in_fname = sys.argv[1]
    start_word = sys.argv[2]
    max_word_count = int(sys.argv[3])

    text_gen = Text_Generator(in_fname)
    text_gen.generate_text(start_word, max_word_count)
    text_gen.print_stdout()
    if out_flag:
        text_gen.write_file(out_fname)

if __name__ == '__main__':
    run_terminal()
    #run_from_pkg("shakespeare.txt",'i',600)
    
