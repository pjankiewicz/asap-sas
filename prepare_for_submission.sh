#!bin/sh

find features/. -type f -delete
find submissions/. -type f -delete
find data/. -name "data_*" -delete
find models/. -type f -delete
find models_meta/. -type f -delete
find data/essay_texts/. -type f -delete
