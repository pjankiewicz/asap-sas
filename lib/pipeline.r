
prepare_data <- function(features,parallel=T) {
  essays_technical_columns = load_technical_columns()
  
  if (parallel==T) {
    lapply = mclapply
  }
  
  data = lapply(1:10, FUN=function(essay_num){
    
    this.set = essays_technical_columns$EssaySet == essay_num
    
    # common operations
    technical_columns = essays_technical_columns[this.set,]
    
    data = list()
    data$id = technical_columns[,"Id"]
    data$y = technical_columns[,c("Score1","Score2")]
    data$x = technical_columns[,"Id"]
    
    features_num = 0
    for (feature in features) {
      print(feature)
      new_feature = load_f(feature,essay_num,cache=F)
      if (!is.na(new_feature)) {
        print(dim(new_feature))
        data$x = cbind(data$x, new_feature)  
        features_num = features_num + 1
      }
    }
    
    data$x = remove_duplicate_columns(data$x)
    data$x$new_feature = NULL
    data$x$id = NULL
    data$x$Id = NULL
    
    
    data$x = data$x[,-1]
    
    if (features_num > 1) {
      data$x = remove_constant_variables(data$x)  
    }
    
    return(data)  
  })
}

load_file <- function(path) {
  fname = load(path)
  eval(parse(text=sprintf("output = %s",fname)))
  return(output)
}

load_f <- function(formula,essay_num,verbose=F,cached=F) {

  if (verbose) {print(formula)}
  
  #md5_filename = str_c("tmp/cache/",digest(str_c(essay_num,formula)))
  
  #if (file.exists(md5_filename) & cached) {
  #  output = load_file(md5_filename)
  #  return(output)
  #}
  
  features = str_match_all(formula, "(\\w+).m")[[1]]
  featuresn = dim(features)[1]
  for (n in 1:featuresn) {
    
    featurepath = str_c("features/essay_",essay_num,"/",features[n,2])

    if (file.exists(featurepath)) {
      # load matrix into env
      if (str_detect(features[n,1],"^text_")) {
        
        eval(parse(text=sprintf("%s = read.csv('%s',sep='\t', fileEncoding='windows-1252',header=F)",features[n,1],featurepath)))  
      } else {
        eval(parse(text=sprintf("%s = read.table('%s',sep=',')",features[n,1],featurepath)))
      }
    }
  }
  
  output = NA
  try({output = eval(parse(text=formula))},silent=FALSE)
  
  #cached complicated formulas
  #loading_time = toc()
  #if (length(output)>0 & cached) {
  #  save(output,file=md5_filename)
  #}
  
  return(output)
}


create_tm_dataframe <- function (essays) {
  
	just.essays <- as.data.frame(essays)

	tm.data <- Corpus(DataframeSource(just.essays))

	tm.data <- tm_map(tm.data, tolower)
	tm.data <- tm_map(tm.data, removeNumbers)
	tm.data <- tm_map(tm.data, removePunctuation)

	tm.data <- tm_map(tm.data, function(x) gsub("[“]«»", " ", x)) 
	tm.data <- tm_map(tm.data, function(x) gsub("[”]«»", " ", x)) 

	#tm.data <- tm_map(tm.data, removeWords, stopwords_en)

	return(tm.data) 
}

create_character_counts  <- function (essays) {
  data <- data.frame(id=1:length(essays))
  for (char in str_split(c('!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'),"")[[1]]) {
    if (!(char %in% c("","^"))) {
      data[,char] <- str_count(essays, str_c("[",char,"]"))  
    }
  }

  data$balanced_parentheses = ifelse(data[,"("] == data[,")"],1,0)
  
  for (col in colnames(data)) {
    if (length(unique(data[,col])) == 1) {
      data[,col] = NULL
    }
  }
  
  data$id = NULL
  
  return(data)
}

create_text_statistics_normalized <- function(essays) {
  data <- data.frame(chars=str_length(essays))

  data$wrds <- str_count(essays , "[:space:]")
  data$nwords_len_1 = str_count(essays,"[[:alpha:]]{1}") / data$wrds
  data$nwords_len_2 = str_count(essays,"[[:alpha:]]{2}") / data$wrds
  data$nwords_len_3 = str_count(essays,"[[:alpha:]]{3}") / data$wrds
  data$nwords_len_4_5 = str_count(essays,"[[:alpha:]]{4,5}") / data$wrds
  data$nwords_len_6_7 = str_count(essays,"[[:alpha:]]{6,7}") / data$wrds
  data$nwords_len_8_9 = str_count(essays,"[[:alpha:]]{8,9}") / data$wrds
  data$nwords_len_10_more = str_count(essays,"[[:alpha:]]{10,100}") / data$wrds
  data$paragraph_count = str_count(essays,"^p")
  
  data$wrdsPerChars <- data$wrds / data$chars
  data$charsPerWrds <- data$chars / data$wrds
  
  data$unique_words = unlist(lapply(essays,FUN=function(d){length(unique(strsplit(as.character(d)," ")))}))

  #dtm.matrix$stopsPerWords <- training.stops / dtm.matrix$wrds
  #dtm.matrix$commasPerWords <- training.commas / dtm.matrix$wrds
  
  return(data) 
}

create_text_statistics <- function(essays) {

  data <- data.frame(chars=str_length(essays))
  
  data$wrds <- str_count(essays , "[:space:]")
  data$nwords_len_1 = str_count(essays,"[[:alpha:]]{1}")
  data$nwords_len_2 = str_count(essays,"[[:alpha:]]{2}")
  data$nwords_len_3 = str_count(essays,"[[:alpha:]]{3}")
  data$nwords_len_4_5 = str_count(essays,"[[:alpha:]]{4,5}")
  data$nwords_len_6_7 = str_count(essays,"[[:alpha:]]{6,7}")
  data$nwords_len_8_9 = str_count(essays,"[[:alpha:]]{8,9}")
  data$nwords_len_10_more = str_count(essays,"[[:alpha:]]{10,100}")
  data$paragraph_count = str_count(essays,"^p")
  
  data$wrdsPerChars <- data$wrds / data$chars
  data$charsPerWrds <- data$chars / data$wrds
  
  data$unique_words = unlist(lapply(essays,FUN=function(d){length(unique(strsplit(as.character(d)," ")))}))
  
  #dtm.matrix$stopsPerWords <- training.stops / dtm.matrix$wrds
  #dtm.matrix$commasPerWords <- training.commas / dtm.matrix$wrds
  
  return(data) 
}

create_text_statistics_larkey_v1 <- function(essays) {

  data <- data.frame(chars=str_length(essays))
  
  data$wrds <- str_count(essays , "[:space:]")
  #data$unique_words = unlist(lapply(essays,FUN=function(d){length(unique(strsplit(as.character(d)," ")))}))
  data$unique_words = unlist(lapply(str_split(essays," "),FUN=function(d){length(unique(d))}))
  
  data$unique_words_4th_root = data$unique_words^(1/4)
  data$sentences_count = pmax(str_count(essays,"[.]"),str_count(essays,"^p"),1)
  data$avg_words_length = data$chars / data$wrds 
  data$avg_sentence_length = data$chars / data$sentences_count

  data$nwords_len_eq_3_norm = str_count(essays,"[[:alpha:]]{3}") / data$wrds
  data$nwords_len_eq_4_5_norm = str_count(essays,"[[:alpha:]]{4,5}") / data$wrds
  data$nwords_len_eq_6_7_norm = str_count(essays,"[[:alpha:]]{6,7}") / data$wrds
  data$nwords_len_eq_8_9_norm = str_count(essays,"[[:alpha:]]{8,9}") / data$wrds
  data$nwords_len_eq_10_more_norm = str_count(essays,"[[:alpha:]]{10,100}") / data$wrds
  
  data$nwords_len_5_more_norm = str_count(essays,"[[:alpha:]]{5,100}") / data$wrds
  data$nwords_len_6_more_norm = str_count(essays,"[[:alpha:]]{6,100}") / data$wrds
  data$nwords_len_7_more_norm = str_count(essays,"[[:alpha:]]{7,100}") / data$wrds
  data$nwords_len_8_more_norm = str_count(essays,"[[:alpha:]]{8,100}") / data$wrds
  data$nwords_len_9_more_norm = str_count(essays,"[[:alpha:]]{9,100}") / data$wrds
  
  data$wrdsPerChars <- data$wrds / data$chars
  data$charsPerWrds <- data$chars / data$wrds
  
  return(data) 
}

create_text_statistics_larkey <- function(essays) {
  
  data <- data.frame(chars=str_length(essays))
  
  data$wrds <- str_count(essays , "[:space:]")
  #data$unique_words = unlist(lapply(essays,FUN=function(d){length(unique(strsplit(as.character(d)," ")))}))
  data$unique_words = unlist(lapply(str_split(essays," "),FUN=function(d){length(unique(d))}))
  
  data$unique_words_4th_root = data$unique_words^(1/4)
  data$sentences_count = pmax(str_count(essays,"[.]"),str_count(essays,"^p"),1)
  data$avg_words_length = data$chars / data$wrds 
  data$avg_sentence_length = data$chars / data$sentences_count
  
  #data$nwords_len_5_more = str_count(essays,"[[:alpha:]]{5,100}")
  #data$nwords_len_6_more = str_count(essays,"[[:alpha:]]{6,100}")
  #data$nwords_len_7_more = str_count(essays,"[[:alpha:]]{7,100}")
  #data$nwords_len_8_more = str_count(essays,"[[:alpha:]]{8,100}")
  #data$nwords_len_9_more = str_count(essays,"[[:alpha:]]{9,100}")
  
  data$nwords_len_5_more_norm = str_count(essays,"[[:alpha:]]{5,100}") / data$wrds
  data$nwords_len_6_more_norm = str_count(essays,"[[:alpha:]]{6,100}") / data$wrds
  data$nwords_len_7_more_norm = str_count(essays,"[[:alpha:]]{7,100}") / data$wrds
  data$nwords_len_8_more_norm = str_count(essays,"[[:alpha:]]{8,100}") / data$wrds
  data$nwords_len_9_more_norm = str_count(essays,"[[:alpha:]]{9,100}") / data$wrds
  
  data$wrdsPerChars <- data$wrds / data$chars
  data$charsPerWrds <- data$chars / data$wrds
  
  return(data) 
}


create_text_statistics <- function(essays) {
  
  data <- data.frame(chars=str_length(essays))
  
  data$wrds <- str_count(essays , "[:space:]")
  data$nwords_len_1 = str_count(essays,"[[:alpha:]]{1}")
  data$nwords_len_2 = str_count(essays,"[[:alpha:]]{2}")
  data$nwords_len_3 = str_count(essays,"[[:alpha:]]{3}")
  data$nwords_len_4_5 = str_count(essays,"[[:alpha:]]{4,5}")
  data$nwords_len_6_7 = str_count(essays,"[[:alpha:]]{6,7}")
  data$nwords_len_8_9 = str_count(essays,"[[:alpha:]]{8,9}")
  data$nwords_len_10_more = str_count(essays,"[[:alpha:]]{10,100}")
  data$paragraph_count = str_count(essays,"^p")
  
  data$wrdsPerChars <- data$wrds / data$chars
  data$charsPerWrds <- data$chars / data$wrds
  
  data$unique_words = unlist(lapply(essays,FUN=function(d){length(unique(strsplit(as.character(d)," ")))}))
  
  #dtm.matrix$stopsPerWords <- training.stops / dtm.matrix$wrds
  #dtm.matrix$commasPerWords <- training.commas / dtm.matrix$wrds
  
  return(data) 
}

create_lsa_vars  <- function (dtm) {
  space = lsa(dtm)
  space.tm = as.textmatrix(space)
  #space.tm = lw_logtf(space.tm)# * gw_entropy(space.tm) # weighting
  space = lsa(space.tm)
  return (data.frame(space$tk))          
}

create_dtm <- function(text) {
  dtm <- DocumentTermMatrix(tm.data.stemmed)
  dtm <- removeSparseTerms(dtm, 0.99)
  dtm.matrix <- as.data.frame(as.matrix(dtm))
  return(dtm.matrix)  
}

create_spelling_mistakes <- function(text) {
  SpellingMistakes <- create_spelling_mistake_counts(text)
  return(SpellingMistakes)
}

create_kernel_data <- function(dtm, chars, minlength=10) {
  tm.data.forKern <- tm.data
  tm.data.forKern[chars < minlength] <- paste(tm.data.forKern[chars < minlength] ,
                                              paste0(rep(" ",10), collapse=""))
}

create_kernel_spectral_cluster <- function(dtm, centers=36) {
  SpecCluster <-  specc(dtm, centers, kernel = "stringdot", kpar = list(length=4, lambda=0.5),
                        nystrom.red = FALSE, #nystrom.sample = length(x)/6, 
                        iterations = 500,
                        mod.sample =  0.75, na.action = na.omit)
  SpecCluster <- as.factor(SpecCluster)
  SpecCluster <- as.data.frame(SpecCluster)
  return(SpecCluster)
}

create_kernel_pca <- function(dtm) {
  kernPCA <- kpca(tm.data.forKern, kernel = "stringdot", kpar = list(length = 4, lambda = 0.5),
                  features = 0, th = 5e-4, na.action = na.omit)
  kernPCA.dframe  <-as.data.frame(kernPCA@pcv)
  names(kernPCA.dframe) <-  paste("kPCA", names(kernPCA.dframe) , sep="")
  return(kernPCA.dframe)
}

get_sentence_similarity_features <- function() {
  all.essays = load_technical_columns()
  
  sentence_similarity_raw_data <- read.csv("features/sentence_similarity_raw.txt",sep=";",header=F)
  colnames(sentence_similarity_raw_data)[1] = "id"
  
  sentence_similarity_raw_data$ind1 = sentence_similarity_raw_data$V3 / sentence_similarity_raw_data$V2
  sentence_similarity_raw_data$ind2 = sentence_similarity_raw_data$V4 / sentence_similarity_raw_data$V2
  sentence_similarity_raw_data$ind3 = sentence_similarity_raw_data$V5 / sentence_similarity_raw_data$V2
  
  sm1 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind1")
  sm1[is.infinite(sm1[,2]),2] <- 1
  sm2 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind2")
  sm2[is.infinite(sm2[,2]),2] <- 1
  sm3 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind3")
  sm3[is.infinite(sm3[,2]),2] <- 1
  
  sentence_similarity_raw_data$ind4 = ifelse((sentence_similarity_raw_data$V3 / sentence_similarity_raw_data$V2) > 1.1,1,0)
  sentence_similarity_raw_data$ind5 = ifelse((sentence_similarity_raw_data$V4 / sentence_similarity_raw_data$V2) > 1.1,1,0)
  sentence_similarity_raw_data$ind6 = ifelse((sentence_similarity_raw_data$V5 / sentence_similarity_raw_data$V2) > 1.1,1,0)
  
  sm4 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind4")
  sm4[is.infinite(sm4[,2]),2] <- 1
  sm5 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind5")
  sm5[is.infinite(sm5[,2]),2] <- 1
  sm6 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind6")
  sm6[is.infinite(sm6[,2]),2] <- 1
  
  sentence_similarity_raw_data$ind7 = ifelse((sentence_similarity_raw_data$V3 / sentence_similarity_raw_data$V2) > 1.2,1,0)
  sentence_similarity_raw_data$ind8 = ifelse((sentence_similarity_raw_data$V4 / sentence_similarity_raw_data$V2) > 1.2,1,0)
  sentence_similarity_raw_data$ind9 = ifelse((sentence_similarity_raw_data$V5 / sentence_similarity_raw_data$V2) > 1.2,1,0)
  
  sm7 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind7")
  sm7[is.infinite(sm7[,2]),2] <- 1
  sm8 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind8")
  sm8[is.infinite(sm8[,2]),2] <- 1
  sm9 <- cast(data=sentence_similarity_raw_data, id ~ ., function(x){mean(x,na.rm=T)}, value="ind9")
  sm9[is.infinite(sm9[,2]),2] <- 1
  
  colnames(sm1) = c("id","sm1")
  colnames(sm2) = c("id","sm2")
  colnames(sm3) = c("id","sm3")
  colnames(sm4) = c("id","sm4")
  colnames(sm5) = c("id","sm5")
  colnames(sm6) = c("id","sm6")
  colnames(sm7) = c("id","sm7")
  colnames(sm8) = c("id","sm8")
  colnames(sm9) = c("id","sm9")
  
  sentence_similarity <- data.frame(id=all.essays$Id)
  sentence_similarity <- merge(sentence_similarity, sm1, by="id")
  sentence_similarity <- merge(sentence_similarity, sm2, by="id")
  sentence_similarity <- merge(sentence_similarity, sm3, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm4, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm5, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm6, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm7, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm8, by="id")
  #sentence_similarity <- merge(sentence_similarity, sm9, by="id")
  
  #colnames(sentence_similarity)[2:10] = paste("sm",1:9,sep="")
  
  return(sentence_similarity)
}
remove_duplicate_columns <- function(data) {
  non.duplicated <- !duplicated(as.matrix(data),MARGIN=2)
  return(data[,non.duplicated])
}


remove_data_with_strong_dominant <- function(data,dominancy=0.99) {
  for (col in colnames(data)) {
    freq_table = table(data[,col])
    if (max(freq_table) >dominancy * sum(freq_table)) {
      data = data[,colnames(data) != col]
    }
  }
  return(data)  
}

stringdot_wrapper <- function(text, ...) {
  essay_text.this = Corpus(DataframeSource(as.data.frame(text)))
  essay_text.this[nchar(essay_text.this) < 10] <- paste(essay_text.this[nchar(essay_text.this) < 10], paste(rep(" ",10,sep=""), collapse=""))
    
  bow_kpca = kpca(essay_text.this, kernel = "stringdot", na.action = na.omit, ...)
  output = as.data.frame(bow_kpca@pcv)
  return(output)
}

remove_constant_variables <- function(data) {
  columns_with_1_unique_value = unlist(lapply(colnames(data),FUN=function(col){length(unique(data[,col]))==1}))
  data = data[,!columns_with_1_unique_value]
  return(data)
}

constant_variables <- function(data) {
  return(unlist(lapply(colnames(data),FUN=function(col){length(unique(data[,col]))==1})))
}
