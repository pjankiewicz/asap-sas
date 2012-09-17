import csv

from lib.essayutils import *

# mistakes counts
essays = Essays()
essays.apply_text_function("raw","proc",remove_all_non_characters)  
essays.apply_text_function("proc","proc",lower)
essays.apply_text_function("proc","proc",remove_multispaces)

for essay_num in range(1,10+1):
    print essay_num
    this_essay_set = essays.get_essay_set(essay_num)
    mistakes_count = [len(mistaken_words(essay.get_text("proc"))) for essay in this_essay_set]
    mistakes_count_unique = [len(set(mistaken_words(essay.get_text("proc")))) for essay in this_essay_set]

    export_feature(mistakes_count, "features/essay_%d/mistakes_count" % (essay_num,))
    export_feature(mistakes_count_unique, "features/essay_%d/mistakes_count_unique" % (essay_num,))

# essay 10 - color changed to 1 variable
essays = Essays()
essays.load_text_from_file("data/essay_texts/proc.txt","proc")

essay_10 = essays.get_essay_set(10)
essay_10_raw = get_combined_text(essay_10,"raw")
colors_dict = {"white":0, "light gray":1, "dark gray":2, "black":3} 
essay_10_color = [colors_dict.get(t.split(" :: ")[0],1.5) for t in essay_10_raw]
export_feature(essay_10_color, "features/essay_10/color")

