import csv

from lib.essayutils import *

essays = Essays()

# raw
essays.apply_text_function("raw","raw",remove_all_non_printable)  

# processed
essays.apply_text_function("raw","proc",remove_all_non_characters)  
essays.apply_text_function("proc","proc",spellcheck)
essays.apply_text_function("proc","proc",lower)
essays.apply_text_function("proc","proc",remove_multispaces)

# stem
essays.apply_text_function("proc","stem",porter_stemmer)

# stem 2
essays.apply_text_function("stem","stem2",replace_consecutive_letters)

# stem 3
essays.apply_text_function("proc","stem3",remove_stopwords)
essays.apply_text_function("stem3","stem3",porter_stemmer)

# pos
essays.apply_text_function("proc","pos",pos_tagger)
essays.apply_text_function("pos","pos",lambda t: " ".join(t))

# numbers
essays.apply_text_function("raw","number",remove_non_number) 
essays.apply_text_function("number","number",remove_multispaces)

# processed with added definitions and synonyms - not tested :(
#essays.apply_text_function("proc_def","proc_def",lambda x: x.replace("_",""))
#essays.apply_text_function("proc_def","proc_def",remove_all_non_characters)
#essays.apply_text_function("proc_def","proc_def",lower)
#essays.apply_text_function("proc_def","proc_def",remove_multispaces)
#essays.apply_text_function("proc_def","proc_def",lambda x: " ".join(list(set(x.split()))))

# exporting 
export_text(essays.essays, "raw", "data/essay_texts/raw.txt")
export_text(essays.essays, "proc", "data/essay_texts/proc.txt")
export_text(essays.essays, "stem", "data/essay_texts/stem.txt")
export_text(essays.essays, "stem2", "data/essay_texts/stem2.txt")
export_text(essays.essays, "stem3", "data/essay_texts/stem3.txt")
export_text(essays.essays, "pos", "data/essay_texts/pos.txt")
export_text(essays.essays, "number", "data/essay_texts/number.txt")

"""
essays.load_text_from_file("data/essay_texts/proc.txt","proc")
essays.load_text_from_file("data/essay_texts/number.txt","number")
essays.load_text_from_file("data/essay_texts/raw.txt","raw")
essays.load_text_from_file("data/essay_texts/pos.txt","pos")
essays.load_text_from_file("data/essay_texts/stem.txt","stem")
essays.load_text_from_file("data/essay_texts/stem2.txt","stem2")
essays.load_text_from_file("data/essay_texts/stem3.txt","stem3")
"""

# exporting for each essay
for essay_num in range(1,10+1):
    export_text(essays.get_essay_set(essay_num), "raw", "features/essay_%d/text_raw" % (essay_num,))
    export_text(essays.get_essay_set(essay_num), "proc", "features/essay_%d/text_proc" % (essay_num,))
    export_text(essays.get_essay_set(essay_num), "stem", "features/essay_%d/text_stem" % (essay_num,))
    export_text(essays.get_essay_set(essay_num), "stem2", "features/essay_%d/text_stem2" % (essay_num,))
    export_text(essays.get_essay_set(essay_num), "stem3", "features/essay_%d/text_stem3" % (essay_num,))
    export_text(essays.get_essay_set(essay_num), "pos", "features/essay_%d/text_pos" % (essay_num,))
    if len(essays.get_essay_set(essay_num)[0].get_text("number")) > 0:
        export_text(essays.get_essay_set(essay_num), "number", "features/essay_%d/text_number" % (essay_num,))
