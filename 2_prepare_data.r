setwd("~/Dropbox/asap-sas/submission.2")
source("config.r")

data_v1 = prepare_data(c("create_character_counts(text_raw.m[,2])",
                         "create_text_statistics_larkey(text_proc.m[,2])",
                         "mistakes_count.m",
                         "mistakes_count_unique.m",
                         "number_ngram_1.m",
                         "pos_ngram_1.m",
                         "proc_ngram_1.m",
                         "stringdot_wrapper(text_raw.m[,2], kpar = list(length = 4, lambda = 0.5), features = 0, th = 5e-4)"))
save(data_v1,file="data/data_v1")

data_v2 = prepare_data(c("create_character_counts(text_raw.m[,2])",
                         "create_text_statistics_larkey_v1(text_proc.m[,2])",
                         "mistakes_count.m",
                         "mistakes_count_unique.m",
                         "number_ngram_1.m",
                         "stem_ngram_1.m",
                         "lsa(stem_ngram_1.m)$tk",
                         "stringdot_wrapper(text_stem.m[,2], kpar = list(length = 3, lambda = 0.25), features = 0, th = 5e-4)",
                         "pos_ngram_1.m"))
save(data_v2,file="data/data_v2")

data_v3 = prepare_data(c("create_character_counts(text_raw.m[,2])",
                         "create_text_statistics_larkey(text_proc.m[,2])",
                         "mistakes_count.m",
                         "mistakes_count_unique.m",
                         "number_ngram_1.m",
                         "proc_ngram_1.m",
                         "lsa(stem_ngram_1.m)$tk",
                         "stringdot_wrapper(text_raw.m[,2], kpar = list(length = 4, lambda = 0.5), features = 0, th = 5e-4)"))
save(data_v3,file="data/data_v3")

data_v4 = prepare_data(c("create_character_counts(text_raw.m[,2])",
                         "create_text_statistics_larkey(text_proc.m[,2])",
                         "mistakes_count.m",
                         "mistakes_count_unique.m",
                         "number_ngram_1.m",
                         "essay_uniqueness.m",
                         "word_frequency.m",
                         "description_similarity.m",
                         "pos_ngram_1.m",
                         "proc_ngram_1.m",
                         "stringdot_wrapper(text_raw.m[,2], kpar = list(length = 4, lambda = 0.5), features = 0, th = 5e-4)"))
save(data_v4,file="data/data_v4")

data_v5 = prepare_data(c("create_character_counts(text_raw.m[,2])",
                         "create_text_statistics_larkey(text_proc.m[,2])",
                         "mistakes_count.m",
                         "mistakes_count_unique.m",
                         "number_ngram_1.m",
                         "essay_uniqueness.m",
                         "word_frequency.m",
                         "description_similarity.m",
                         "pos_ngram_1.m",
                         "proc_ngram_1.m",
                         "chunks_clustered.m",
                         "ngram3_coverage.m",
                         "ngram4_coverage.m",
                         "ngram5_coverage.m",
                         "key_coverage.m",
                         "additional_text_similarity.m",
                         "stringdot_wrapper(text_raw.m[,2], kpar = list(length = 4, lambda = 0.5), features = 0, th = 5e-4)"))
save(data_v5,file="data/data_v5")

data_v7 = prepare_data(c("stem_ngram_1.m","mistakes_count.m","mistakes_count_unique.m","pos_ngram_1.m","number_ngram_1.m"))
save(data_v7,file="data/data_v7")

data_v8 = prepare_data(c("stem_ngram_1.m","word_frequency.m","essay_uniqueness.m","ngram3_coverage.m","additional_text_similarity.m",
                         "mistakes_count.m","mistakes_count_unique.m","pos_ngram_1.m","number_ngram_1.m","color.m"))
save(data_v8,file="data/data_v8")

data_v9 = prepare_data(c("stem_ngram_1.m","word_frequency.m","essay_uniqueness.m","ngram3_coverage.m","additional_text_similarity.m","essay_info.m",
                         "description_similarity.m","mistakes_count.m","mistakes_count_unique.m","pos_ngram_1.m","number_ngram_1.m","color.m"))
save(data_v8,file="data/data_v9")


data_v10 = prepare_data(c("stem_ngram_1.m")); save(data_v10,file="data/data_v10")
data_v11 = prepare_data(c("proc_ngram_1.m")); save(data_v11,file="data/data_v11")
data_v12 = prepare_data(c("raw_ngram_1.m")); save(data_v12,file="data/data_v12")

data_v13 = prepare_data(c("stem_ngram_1.m","mistakes_count.m","mistakes_count_unique.m")); save(data_v13,file="data/data_v13")
data_v14 = prepare_data(c("proc_ngram_1.m","mistakes_count.m","mistakes_count_unique.m")); save(data_v14,file="data/data_v14")
data_v15 = prepare_data(c("raw_ngram_1.m","mistakes_count.m","mistakes_count_unique.m")); save(data_v15,file="data/data_v15")
