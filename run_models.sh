#!/bin/sh

echo "1a_prepare_essays.py"
python 1a_prepare_essays.py

echo "1b_prepare_ngrams.py"
python 1b_prepare_ngrams.py

echo "1d_prepare_chunks.py"
python 1d_prepare_chunks.py

echo "1d_prepare_essay_specific_features.py"
python 1d_prepare_essay_specific_features.py

echo "1g_prepare_features.py"
python 1g_prepare_features.py

#echo "1c_prepare_features.r"
#R -f 1c_prepare_features.r

#echo "2_prepare_data.r"
#R -f 2_prepare_data.r

#echo "3_make_models.r"
#R -f 3_make_models.r

#echo "4_blend.r"
#R 4_blend.r
