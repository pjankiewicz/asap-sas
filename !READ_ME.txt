If your reading this file that means that our team is in the top 5.
We hope that you'll find the process easy to replicate and we will be eligible for prices :)

Here it goes

1. Installation.
   - Python 2.6.6+ Libraries needed: numpy, nltk*, pyaspell
   - R 2.14.1+ Libraries needed (as in config.r file)
   - RStudio 0.95.262+

* for nltk there is a necessity to download some external data: 
  nltk.corpus.stopwords
  nltk.corpus.brown
  nltk.corpus.wordnet

  It is done very quickly using a command nltk.download() and selecting those 3 libraries
  
2. Place the private leaderboard data set as private_leaderboard.tsv file in data/raw/ folder
   The file that resides there is only a dummy file that helped us create a process.

3. Change the header to setwd(YOUR_CURRENT_PATH) in files: 
   - 1c_prepare_features.r
   - 2_prepare_data.r
   - 4_make_models.r
   - 5_blend.r

4. Run the files:
   - 1a_prepare_essays.py (python)
   - 1b_prepare_ngrams.py (python)
   - 1c_prepare_features.r (Rstudio)
   - 1d_prepare_chunks.py (python)
   - 1d_prepare_essay_specific_features.py (python)
   - 1g_prepare_features.py (python)
   - 2_prepare_data.r (Rstudio)
   - 3_make_models.r (Rstudio)
   - 4_blend.r (Rstudio)

  It will take 1-2 weeks to calculate all the models depending on how many cores you have.

5. The submission should be placed in submissions/private.csv
