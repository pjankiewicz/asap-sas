import csv

from lib.essayutils import *

essays = Essays()
essays.load_text_from_file("data/essay_texts/proc.txt","proc")
essays.load_text_from_file("data/essay_texts/number.txt","number")
essays.load_text_from_file("data/essay_texts/raw.txt","raw")
essays.load_text_from_file("data/essay_texts/pos.txt","pos")
essays.load_text_from_file("data/essay_texts/stem.txt","stem")
essays.load_text_from_file("data/essay_texts/stem2.txt","stem2")
essays.load_text_from_file("data/essay_texts/stem3.txt","stem3")

# unigram

ngram_num = 1
for text_version in ("proc","raw","stem","stem2","stem3","number","pos"): #"proc_def"
    for essay_num in range(1,10+1):
        print essay_num
        ngrams_dict = {}
        for essay in essays.get_essay_set(essay_num):
            ngrams = essay.get_text(text_version).split()
            for ngram in ngrams:
                if ngram in ngrams_dict:
                    ngrams_dict[ngram] += 1
                else:
                    ngrams_dict[ngram] = 1
        # filter out very sparse ngrams
        ngrams_dict = dict(filter(lambda x: x[1]>10, ngrams_dict.items()))
        ngrams_list = set(ngrams_dict.keys())
        if len(essays.get_essay_set(essay_num)[0].get_text(text_version)) > 0:
            output = open("features/essay_%d/%s_ngram_1" % (essay_num,text_version),"w")
            for essay in essays.get_essay_set(essay_num):
                ngram_in_essay = []
                ngrams = essay.get_text(text_version).split()
                for ngram_to_find in ngrams_list:
                    ngram_in_essay.append(str(ngrams.count(ngram_to_find)))
                output.write(",".join(ngram_in_essay) + "\n")
            output.close()

#bigrams, trigrams, ... 
#saves ngrams for each text version
#not used :(
"""
for text_version in ("raw","proc","stem","stem2","stem3"):
    for ngram_num in range(2,5+1):
        print "Ngram",ngram_num
        for essay_num in range(1,10+1):
            print essay_num
            ngrams_dict = {}
            for essay in essays.get_essay_set(essay_num):
                ngrams = get_ngrams(essay.get_text(text_version),ngram_num)
                for ngram in ngrams:
                    ngram_key = "###".join(ngram)
                    if ngram_key in ngrams_dict:
                        ngrams_dict[ngram_key] += 1
                    else:
                        ngrams_dict[ngram_key] = 1
            # filter out very sparse ngrams
            ngrams_dict = dict(filter(lambda x: x[1]>10, ngrams_dict.items()))
            ngrams_list = set(ngrams_dict.keys())
            print "ngrams",len(ngrams_list)

            output = open("features/essay_%d/%s_ngram_%d" % (essay_num,text_version,ngram_num),"w")
            for essay in essays.get_essay_set(essay_num):
                ngram_in_essay = []
                ngrams = ["###".join(ngram) for ngram in get_ngrams(essay.get_text("proc"),ngram_num)]
                for ngram_to_find in ngrams_list:
                    ngram_in_essay.append(str(ngrams.count(ngram_to_find)))
                output.write(",".join(ngram_in_essay) + "\n")
            output.close()
"""
