import csv

from lib.essayutils import *

essays = Essays()
essays.load_text_from_file("data/essay_texts/proc.txt","proc")
essays.load_text_from_file("data/essay_texts/stem.txt","stem")

for essay_num in range(1,10+1):
    print essay_num
    essays_this = essays.get_essay_set(essay_num)
    essays_text = [essay.get_text("stem") for essay in essays_this]
    essays_id = [essay.id for essay in essays_this]

    output = open("features/essay_%d/chunks" % (essay_num,),"w")
    output2 = open("features/essay_%d/chunks_stem" % (essay_num,),"w")
    for (id,text) in zip(essays_id, essays_text):
        
        for ngram in get_ngrams(text,7):
            hashed_bow = hashingtrick(" ".join(ngram),100)
            output.write("%d,%s\n" % (id, ",".join([str(k) for k in hashed_bow])))
            output2.write("%d\t%s\n" % (id, " ".join(ngram)))

    output.close()
    output2.close()
        
