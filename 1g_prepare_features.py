from __future__ import division

import csv
import os
import zlib
import random
import string

random.seed(1)

from lib.essayutils import *

essays = Essays()
essays.load_text_from_file("data/essay_texts/proc.txt","proc")

def load_ngrams(path):
    ngrams_file = open(path)
    ngrams = {}
    for ind, line in enumerate(ngrams_file.xreadlines()):
        line_split = line.split("\t")
        ngrams[" ".join(line_split[1:])[:-2]] = int(line_split[0])
        if ind % 100000 == 0:
            print ind
    return ngrams



# loads word frequency
def prepare_word_dict_features():
    word_freq = {}
    for ind,line in enumerate(open("data/external/combined_wordfreq.txt").xreadlines()):
        line_split = line.split("\t")
        try:
            word_freq[line_split[0]] = int(line_split[1][:-1])
        except:
            pass

    # calculates word frequency statistics
    # min frequency

    for essay_num in range(1,10+1):
        print essay_num
        output = open("features/essay_%d/word_frequency" % (essay_num,),"w")
        for essay in essays.get_essay_set(essay_num):
            text = essay.get_text("proc")

            essay_word_freq = [word_freq.get(word) for word in text.split() if word in word_freq]
            if len(essay_word_freq) > 0:
                essay_word_num = len(text.split())

                min_frequency = min(essay_word_freq)
                min_log_frequency = min([log(k) for k in essay_word_freq])
                coverage = len(essay_word_freq) / essay_word_num
                average_frequency = sum(essay_word_freq) / len(essay_word_freq)
                average_log_frequency = sum([log(k) for k in essay_word_freq]) / len(essay_word_freq)

                features = [min_frequency, min_log_frequency, coverage, average_frequency, average_log_frequency]
            else:
                features = [0,0,0,0,0]
            output.write(",".join([str(k) for k in features]) + "\n")
        output.close()



def word_generator(size=6, chars=string.ascii_uppercase):
    return ''.join(random.choice(chars) for x in range(size))

def calculate_description_similarities():
    for essay_num in range(1,10+1):
        essay_description = open("data/descriptions/essay_%d_description.txt" % (essay_num,)).read()
        description_infovalue = len(zlib.compress(essay_description,9))
        print essay_num
        output = open("features/essay_%d/description_similarity" % (essay_num,),"w")
        for essay in essays.get_essay_set(essay_num):
            
            essay_text = essay.get_text("proc")
            random_text = word_generator(len(essay_text))
            if len(essay_text) > 0:
                essay_infovalue = len(zlib.compress(essay_description + essay_text,9))
                essay_infovalue_dummy = len(zlib.compress(essay_description + random_text,9))

                essay_infovalue_length_raw = essay_infovalue - description_infovalue
                if len(essay_text) != 0:
                    essay_infovalue_length_norm = (essay_infovalue - description_infovalue) / len(essay_text)
                else:
                    essay_infovalue_length_norm = 0

                if (description_infovalue - essay_infovalue_dummy) != 0:
                    essay_infovalue_length_norm2 = (description_infovalue - essay_infovalue) / (description_infovalue - essay_infovalue_dummy)
                else:
                    essay_infovalue_length_norm2 = 0
            else:
                essay_infovalue_length_raw = -1
                essay_infovalue_length_norm = -1
                essay_infovalue_length_norm2 = -1
                
            output.write("%.6f,%.6f,%.6f\n" % (essay_infovalue_length_raw, essay_infovalue_length_norm, essay_infovalue_length_norm2))
        output.close()


def calculate_essay_uniqueness():
    for essay_num in range(1,10+1):
        print essay_num
        output = open("features/essay_%d/essay_uniqueness" % (essay_num,),"w")
        
        for this_essay in essays.get_essay_set(essay_num):
            essay_text = this_essay.get_text("proc")
            random_text = word_generator(len(essay_text))
            print this_essay.id
            if len(essay_text) > 0:
                other_essays = "".join([essay.get_text("proc") for essay in essays.get_essay_set(essay_num) if essay.id != this_essay.id])
                
                essays_infovalue = len(zlib.compress(other_essays))
                
                essay_infovalue_dummy = len(zlib.compress(other_essays + random_text,9))
                essay_infovalue_real  = len(zlib.compress(other_essays + essay_text,9))

                essay_infovalue_length_norm = (essays_infovalue - essay_infovalue_real) / (essays_infovalue - essay_infovalue_dummy)
            else:
                essay_infovalue_length_norm = -1
                
            output.write("%.6f\n" % (essay_infovalue_length_norm))
        output.close()

def calculate_ngram_coverage(freqdict, n, outputfile):
    for essay_num in range(1,10+1):
        print essay_num
        output = open("features/essay_%d/%s" % (essay_num,outputfile), "w")
        for this_essay in essays.get_essay_set(essay_num):
            essay_text = this_essay.get_text("proc")
            all_ngrams = 0
            ngram_found = 0
            frequencies = []
            for ngram in get_ngrams(essay_text,n):
                all_ngrams += 1
                ngram_text = " ".join(ngram)
                if ngram_text in freqdict:
                    ngram_found += 1
                    frequencies.append(freqdict[ngram_text])
            if all_ngrams != 0:
                percentage = ngram_found / all_ngrams
            else:
                percentage = 0

            if len(frequencies)!=0:
                min_frequencies = log(min(frequencies))
                avg_frequency = sum([log(k) for k in frequencies]) / len(frequencies)
            else:
                min_frequencies = 0
                avg_frequency = 0
            output.write("%.5f,%.5f,%.5f\n" % (percentage,min_frequencies,avg_frequency))
        output.close()

def calculate_key_coverage():
    for essay_num in [1,5,6]:
        print essay_num
        output = open("features/essay_%d/key_coverage" % (essay_num,), "w")
        for this_essay in essays.get_essay_set(essay_num):
            essay_text = this_essay.get_text("proc")
            random_text = word_generator(len(essay_text))
            keys_concordance = []
            for key in open("data/descriptions/essay_%d_key.txt" % (essay_num,)).readlines():
                key_info = zlib.compress(key)
                key_plus_essay_text = zlib.compress(key + essay_text)
                key_plus_random_text = zlib.compress(key + random_text)
                keys_concordance.append((len(key_plus_essay_text) - len(key_info)) / (len(key_plus_random_text) - len(key_info)))
            output.write("%s,%.5f,%.5f\n" % (",".join([str(k) for k in keys_concordance]), sum(keys_concordance), min(keys_concordance)),)
        output.close()                

def calculate_additional_texts_similarity():
    for essay_num in range(1,10+1):
        # opens additional texts
        print essay_num

        additional_texts = []
        filelist = os.listdir("data/external/additional_texts/essay_%d" % (essay_num,))
        for filename in filelist:
            text = open("data/external/additional_texts/essay_%d/%s" % (essay_num,filename)).read()
            additional_texts.append(text)

        if len(additional_texts) > 0:
            output = open("features/essay_%d/additional_text_similarity" % (essay_num,), "w")
            for ind, this_essay in enumerate(essays.get_essay_set(essay_num)):
                essay_text = this_essay.get_text("proc")
                random_text = word_generator(len(essay_text))
                similarities = []
                for additional_text in additional_texts:
                    key_info = zlib.compress(additional_text,9)
                    key_plus_essay_text = zlib.compress(additional_text + essay_text,9)
                    key_plus_random_text = zlib.compress(additional_text + random_text,9)
                    similarities.append((len(key_plus_essay_text) - len(key_info)) / (len(key_plus_random_text) - len(key_info)))
                output.write("%s\n" % (",".join([str(k) for k in similarities])))
                if ind % 100 == 0:
                    print ".",
            print
            output.close()
            
def calculate_text_information():
    for essay_num in range(1,10+1):
        print essay_num
        output = open("features/essay_%d/essay_info" % (essay_num,), "w")
        for this_essay in essays.get_essay_set(essay_num):
            essay_text = this_essay.get_text("proc")
            random_text = word_generator(len(essay_text))
            if len(essay_text) > 0:
                info_value = len(zlib.compress(essay_text,9)) / len(zlib.compress(random_text,9))
            else:
                info_value = 0
            output.write("%.5f\n" % (info_value,))
        output.close()
                
#uncomment all
"""
prepare_word_dict_features()

calculate_description_similarities()

calculate_essay_uniqueness()

NGRAM3 = load_ngrams("data/external/w3_.txt")
calculate_ngram_coverage(NGRAM3, 3, "ngram3_coverage")

NGRAM4 = load_ngrams("data/external/w4_.txt")
calculate_ngram_coverage(NGRAM4, 4, "ngram4_coverage")

NGRAM5 = load_ngrams("data/external/w5_.txt")
calculate_ngram_coverage(NGRAM5, 5, "ngram5_coverage")

calculate_key_coverage()

calculate_additional_texts_similarity()

calculate_text_information()
"""
