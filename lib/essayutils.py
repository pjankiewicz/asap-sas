import csv
import nltk
import re
import time
import random
import sys
from math import log

from string import lower, printable
from itertools import chain
from lib import pyaspell
import numpy as np
import nltk

from lib.utils import memoized

TRAIN_PATH = "data/raw/train.tsv"
TEST_PATH = "data/raw/public_leaderboard.tsv"
PRIVATE_PATH = "data/raw/private_leaderboard.tsv"

NA_STRING = "NA"
ESSAY_SETS = [1,2,3,4,5,6,7,8,9,10]

try:
    s = STOPWORDS
    s = words_stat 
except:
    STOPWORDS = nltk.corpus.stopwords.words("english")
    stopwords_set = set(STOPWORDS)
    spellchecker = pyaspell.Aspell(("lang", "en"))
    wn = nltk.corpus.wordnet
    brown = nltk.corpus.brown
    sentence_tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
    printable_characters = set([k for k in printable if k not in ("\n","\t","\r",";",'"',"'")])
    word_stat = nltk.FreqDist([w.lower() for w in brown.words()])

ESSAY_TRAIN = 1
ESSAY_PUBLIC = 2
ESSAY_PRIVATE = 3

class Essays():
    def __init__(self, train_path = TRAIN_PATH, test_path = TEST_PATH, private_path = PRIVATE_PATH):
        input = csv.reader(open(train_path),delimiter="\t")
        essays = [line for line in input][1:]
        self.essays = [Essay(int(line[0]), int(line[1]), int(line[2]), int(line[3]), line[4], ESSAY_TRAIN) for line in essays]

        if test_path:
            input = csv.reader(open(test_path),delimiter="\t")
            essays = [line for line in input][1:]
            self.essays.extend([Essay(int(line[0]), int(line[1]), None, None, line[2], ESSAY_PUBLIC) for line in essays])

        if private_path:
            input = csv.reader(open(private_path),delimiter="\t")
            essays = [line for line in input][1:]
            self.essays.extend([Essay(int(line[0]), int(line[1]), None, None, line[2], ESSAY_PRIVATE) for line in essays])


    def apply_text_function(self, source, destination, f, essay_set = None):
        #print time.asctime(time.localtime())
        for n in range(len(self.essays)):
            if essay_set == None or self.essays[n].essay_set == essay_set:
                processed_version = f(self.essays[n].get_text(source))
                self.essays[n].set_text(destination, processed_version)

    def add_feature(self, feature_name, feature_extract_f, filter_function = None):
	# todo add caching features - some of them take reaaaally long to calculate
	# cache key should be combined hash of essay versions + function code
        for n in range(len(self.essays)):
            if filter_function == None or filter_function(self.essays[n]):
                self.essays[n].features[feature_name] = feature_extract_f(self.essays[n])

    def get_essay_by_id(self,essay_id):
        return [essay for essay in self.essays if essay.id == essay_id][0]

    def get_essay_set(self,essay_set):
        return [essay for essay in self.essays if essay.essay_set == essay_set]

    def get_essays_by_type(essay_type):
        return [essay for essay in self.essays if essay.essay_type == essay_type]

    def get_essay_set_possible_scores(self,essay_set):
        return list(set([essay for essay in self.essays if essay.essay_set == essay_set]))

    def get_corpora_by_essay_set(self,essay_set,version):
        corpora = {}
        for essay in self.get_essay_set(essay_set):
            if essay.essay_type == ESSAY_TRAIN:
                for word in essay.get_text(version).split():
                    if word in corpora:
                        corpora[word] += 1
                    else:
                        corpora[word] = 1
        return corpora

    def get_data(self,essay_set = None):
        features_names = []
        for essay in self.essays:
            if essay_set == None or essay.essay_set == essay_set:
                features_names += essay.get_features_names()
        features_names = sorted(list(set(features_names)))

        data = []
        data.append(["id","essay_set","score_1","score_2"] + features_names)
        
        for essay in self.essays:
            if essay_set == None or essay.essay_set == essay_set:
                row = []
                for feature in features_names:
                    row.append(essay.get_feature(feature))
                data.append([essay.id, "Essay" + str(essay.essay_set), essay.score_1, essay.score_2] + row)
        return data

    def load_text_from_file(self, path, text_version):
        inp = open(path)
        for essay_ind, line in enumerate(inp.readlines()):
            line_split = line.split("\t")
            text = line_split[1].replace("\n","")
            self.essays[essay_ind].set_text(text_version, text)
        inp.close()
                        
		
class Essay():
    def __init__(self, id, essay_set, score_1, score_2, raw_text, essay_type):
        self.id = id
        self.essay_set = essay_set
        self.score_1 = score_1
        self.score_2 = score_2
        self.text_versions = {}
        self.set_text("raw",raw_text)
        self.essay_type = essay_type
        self.features = {}

    def get_tokens(self, version):
        return self.get_text(version).split()

    def set_text(self, version, text):
        self.text_versions[version] = text

    def get_text(self, version):
        return self.text_versions.get(version,"")

    def get_features(self):
        return self.features

    def get_feature(self,name):
        return self.features.get(name, NA_STRING)

    def get_features_names(self):
        return self.features.keys()

# gets the text off all essays
def get_combined_text(essays,version,filter_function = None):
    return [essay.get_text(version) for essay in essays if filter_function == None or filter_function(essay)]
        
# text functions
def remove_all_non_printable(text):
    return "".join([k for k in text if k in printable_characters])

def remove_all_non_characters(text):
    return re.sub("[^a-zA-Z\s]"," ",text)

def remove_non_number(text):
    return re.sub("[^0-9\s]"," ",text).strip()

def remove_multispaces(text):
    return re.sub("[\s]+"," ",text)

def remove_spaces(text):
    return re.sub("[\s]","",text)

def remove_stopwords(text):
    return " ".join([w for w in text.split() if w not in STOPWORDS])

def replace_consecutive_letters(text):
    letters = [k for k in text if k not in [" ",]]
    for letter in letters:
        text = re.sub("[%s]+" % (letter,), letter, text)
    return text

def count_words_with_len(words,lengths):
    return len([word for word in words if len(word) in lengths])

def count_words_with_geq_len(words,length):
    return len([word for word in words if len(word) >= length])

def porter_stemmer(text):
    return " ".join([nltk.stem.porter.PorterStemmer().stem(word) for word in text.split()])

def pos_tagger(text):
    text = text.split()
    return [word[1] for word in nltk.pos_tag(text)]

def mistaken_words(text):
    mistakes = []
    for word in text.split():
        if not spellchecker.check(word):
            mistakes.append(word)
    return mistakes
    
def remove_stopwords(text):
    return " ".join([k for k in text.split() if k not in STOPWORDS])

def spellcheck(text):
    correct_text = text
    for word in text.split():
        if not spellchecker.check(word):
            suggestion = spellchecker.suggest(word)
            if suggestion:
                correct_text = correct_text.replace(word,suggestion[0])
    return correct_text

def remove_rare_tokens(corpora,sparsity = 1):
    def process(text):
        tokens = text.split()
        for word in tokens:
            if corpora.get(word,0) <= sparsity:
                tokens.remove(word)
        return " ".join(tokens)
    return process

def tokenize_sentences(text):
    return sentence_tokenizer.tokenize(text)


def add_definitions(text,used_dictionary=None):
    rawtext_adj = ""
 
    # Print the information
    for word in text.split():
        rawtext_adj += " " + word
        if len(word) >= 4:
            synsets = wn.synsets(word)
            for synset in synsets:
                #print "-" * 10
                #print "Name:", synset.name
                #print "Lexical Type:", synset.lexname
                #print "Lemmas:", synset.lemma_names
                #print "Definition:", synset.definition
                #for example in synset.examples:
                #    print "Example:", example
                rawtext_adj += " " + " ".join(synset.lemma_names)
                rawtext_adj += " " + synset.definition
    return rawtext_adj


@memoized
def wup_similarity(a,b):
    return wn.wup_similarity(a,b)

# utility functions
def export_data_sep_files(essays):
    for essay_set in ESSAY_SETS:
        data = essays.get_data(essay_set)
        write_data(data,"data_%d.csv" % (essay_set,))

def export_data_one_file(essays):
    data = essays.get_data()
    write_data(data,"data.csv")
    
def write_data(data, path):
    output = open(path, "w")
    for row in data:
        output.write(";".join([str(k) for k in row]) + "\n")
    output.close()

def export_text(essays, version, path, save_id = True):
    output = open(path, "w")
    for essay in essays:
        if save_id:
            output.write("%s\t%s\n" % (essay.id, essay.get_text(version)))
        else:
            output.write("%s\n" % (essay.get_text(version)))
    output.close()

def export_feature(feature, path):
    output = open(path, "w")
    for elem in feature:
        output.write("%s\n" % (elem,))
    output.close()

# cuts the text into list of 10 tokens each
def cut_text(text,n=10):
    splited = text.split()
    return [' '.join(splited[x:x+n]) for x in xrange(0, len(splited), n)]
    
# calculates cosinus distance
def cosine_distance(v1, v2):
    return float(np.dot(v1,v2) / (np.linalg.norm(v1) * np.linalg.norm(v2)))

# returns word synonyms
def synonyms(word):
    return [l for s in wn.synsets(word) for l in s.lemmas]

# splits the text into ngrams
def get_ngrams(text,n):
    text_split = text.split()
    ngrams = []
    for ind in range(0,len(text_split)-n+1):
        ngrams.append(text_split[ind:ind+n])
    return ngrams

def sign(n):
    if n==0:
        return 0
    elif n < 0:
        return -1
    else:
        return 1

def hashingtrick(document,M):
    bow=[0]*M
    for word in document.split():
        h=word.__hash__()
        bow[abs(h) % M]+=sign(h)
    return bow

class NgramFreq():
    def __init__(self, essays, n):
        self.essays = essays
        self.n = n
        self.freq = self.get_ngrams_dict()

    def get_ngrams_dict(self):
        self.ngrams_dict = {}
        for essay in self.essays:
            ngrams = get_ngrams(essay,self.n)
            for ngram in ngrams:
                ngram_key = "###".join(ngram)
                if ngram_key in self.ngrams_dict:
                    self.ngrams_dict[ngram_key] += 1
                else:
                    self.ngrams_dict[ngram_key] = 1

    def get_frequency(self, key):
        return self.ngrams_dict.get("###".join(key), 0)

    def get_high_freq_ngrams(self, min_freq):
        return [k.split("###") for (k,v) in self.ngrams_dict.items() if v >= min_freq]

@memoized
def synsets(token):
    return wn.synsets(token)

# information value based on corpus
@memoized
def info_value(word, corpus=word_stat):
    n = corpus.get(word,1)
    N = len(corpus)
    return 1 - log(n + 1) / log(N + 1)

# sentence similarity based
def shallow_similarity(sentence_1, sentence_2, delta = 0.75):
    tokens_1 = sentence_1.split()
    tokens_2 = sentence_2.split()
    tokens_all = list(set(tokens_1 + tokens_2))

    sim = np.empty((len(tokens_1),len(tokens_all)))

    for a in range(len(tokens_1)):
        for b in range(len(tokens_all)):
            s1 = synsets(tokens_1[a])
            s2 = synsets(tokens_all[b])
            if len(s1) > 0 and len(s2) > 0:
                s1 = s1[0]
                s2 = s2[0]
                # TODO: memoize
                simi = wup_similarity(s1, s2)
                if simi is None or simi <= 0.25:
                    simi = 0
                #print tokens_1[a], tokens_all[b], simi, simi * info_value(s1) * info_value(s2)
                sim[(a,b)] = simi * info_value(s1) * info_value(s2)
            else:
                sim[(a,b)] = 0
                
    max_sim = dict(zip(tokens_all,map(max, zip(*sim))))

    s1 = [0 if k not in tokens_1 else v  for k,v in max_sim.items()]
    s2 = [0 if k not in tokens_2 else v  for k,v in max_sim.items()]

    Sd = cosine_distance(s1,s2)

    r1 = np.array([0 if token not in tokens_1 else tokens_1.index(token) for i,token in enumerate(tokens_all)])
    r2 = np.array([0 if token not in tokens_2 else tokens_2.index(token) for i,token in enumerate(tokens_all)])

    Sr = 1 - np.linalg.norm(r1 - r2) / np.linalg.norm(r1 + r2)

    #print Sd, Sr
    return delta * Sd + (1 - delta) * Sr

# sentence similarity based (with synonyms)
def deep_similarity(sentence_1, sentence_2, delta = 0.75):
    tokens_1 = sentence_1.split()
    tokens_2 = sentence_2.split()
    tokens_all = list(set(tokens_1 + tokens_2))

    sim = np.empty((len(tokens_1),len(tokens_all)))

    for a in range(len(tokens_1)):
        for b in range(len(tokens_all)):
            w1 = tokens_1[a]
            w2 = tokens_all[b]
            
            s1 = synonyms(w1)
            s2 = synonyms(w2)

            c1, c2 = w1, w2

            if w1 == w2:
                sim[(a,b)] = info_value(w1) * info_value(w2)
    
            elif len(s1) > 0 and len(s2) > 0:
                max_simi = 0
                for s1_synonym in s1:
                    for s2_synonym in s2:
                        # TODO: memoize
                        simi = wn.wup_similarity(s1_synonym.synset, s2_synonym.synset)
                        if simi > max_simi:
                            max_simi = simi
                        
                if max_simi is None or max_simi <= 0.25:
                    max_simi = 0

                sim[(a,b)] = max_simi * info_value(w1) * info_value(w2)
            else:
                sim[(a,b)] = 0

    max_sim = dict(zip(tokens_all,map(max, zip(*sim))))

    s1 = [0 if k not in tokens_1 else v  for k,v in max_sim.items()]
    s2 = [0 if k not in tokens_2 else v  for k,v in max_sim.items()]

    Sd = cosine_distance(s1,s2)

    r1 = np.array([0 if token not in tokens_1 else tokens_1.index(token) for i,token in enumerate(tokens_all)])
    r2 = np.array([0 if token not in tokens_2 else tokens_2.index(token) for i,token in enumerate(tokens_all)])

    Sr = 1 - np.linalg.norm(r1 - r2) / np.linalg.norm(r1 + r2)

    return delta * Sd + (1 - delta) * Sr


def sentence_similarity(essays, output, essay_set = None, essay_type = None):
    sentence_sim_output = open(output,"w")

    scores = []

    for index_essay, essay_1 in enumerate([essay for essay in essays.essays if \
                                           (essay_set is None or essay.essay_set == essay_set) and (essay_type is None or essay.essay_type == essay_type)]):
        scores_mul = 0
        scores_sum = 0

        print index_essay, essay_1.id, time.time(), essay_1.score_1, essay_1.score_2
        for sentence_1 in cut_text(essay_1.get_text("proc"),10):
            print sentence_1
            max_similarity = [0,0,0,0]
            max_similarity_sent = ["","","",""]
            essays_ids = [essay.id for essay in essays.essays if essay.id != essay_1.id and essay.essay_set == essay_1.essay_set and essay.score_1 is not None]
            
            for essay_num in random.sample(essays_ids, 300):
                essay_2 = essays.get_essay_by_id(essay_num)
                for sentence_2 in cut_text(essay_2.get_text("proc"),10):
                    same_words = set(sentence_1.split()).intersection(sentence_2.split()).difference(stopwords_set)
                    if len(same_words) == 0:
                        continue
                    similarity = shallow_similarity(sentence_1, sentence_2)
                    
                    if similarity > max_similarity[essay_2.score_1]:
                        max_similarity[essay_2.score_1] = similarity
                        max_similarity_sent[essay_2.score_1] = sentence_2

                # if very similar sentences found in 0-scored set and above
                # it means that it is redundant
                #if max_similarity[0] >= 0.75 and max_similarity[1] >= 0.75:
                #    max_similarity = [0,0,0,0]
                #    break
                        
            for sim_score, sent in zip(max_similarity,max_similarity_sent):
                print sim_score, sent

            sentence_sim_output.write("%s;%s\n" % (essay_1.id, ";".join([str(k) for k in max_similarity])))
                    
        print time.time()

    sentence_sim_output.close()
