
save_submission <- function(predictions,prediction.type="public") {
  predictions_final = data.frame()
  for (essay_num in 1:10) {
    predictions.this_essay = predictions[predictions$essay_num==essay_num,]
    
    predictions_final = rbind(predictions_final,data.frame(id=predictions.this_essay$id[predictions.this_essay$type==prediction.type], 
                                                           essay_score=prediction_score(predictions.this_essay$score_1,
                                                                                        predictions.this_essay$score_2,
                                                                                        essay_num,
                                                                                        prediction.type)$score_goc))
  }
  write.csv(predictions_final,file=str_c("submissions/",prediction.type,".csv"),row.names=F)
}

get_models <- function(modelspath, pattern, quantile.min = 0, quantile.max = 1, normalize.predictions = T) {
  
  technical_columns.all = load_technical_columns()
  predictions_by_quantile = c()
  
  final_scores = c()
  public_predictions = data.frame()
  
  data.blend = vector(mode="list",length=10)
  
  for (essay_num in 1:10) {
    
    technical_columns.this = technical_columns.all[technical_columns.all$EssaySet==essay_num,]
    trainset = !is.na(technical_columns.this$Score1)
    testset = is.na(technical_columns.this$Score1)
    publicset = technical_columns.this$Id %in% technical_columns.this$Id[technical_columns.this$type=="public"]
    
    all.predictions = c() #vector(length=sum(trainset) + sum(testset))
    all.predictions.goc = c() #vector(length=sum(trainset) + sum(testset))
    
    all.predictions.names = c()
    performance.vector = c()
    
    for (modelfile in list.files(str_c(modelspath,"/"))) {
      if (str_detect(modelfile,pattern)) {
        load(file=str_c(modelspath,"/",modelfile))
        #print(modelfile)
        
        if (class(predictions)=="matrix_predictions" & !str_detect(modelfile,"svm")) {
          best_predictions = choose_best_prediction_vec(predictions[[essay_num]],essay_num)
          predictions = data.frame(score_1=best_predictions$rater_1, score_2=best_predictions$rater_2)
        } else {
          predictions = predictions[technical_columns.all$EssaySet==essay_num,]
        }
        
        performance = prediction_score((predictions$score_1), (predictions$score_2), essay_num)
        performance.vector = c(performance.vector,performance$score_avg)
        model.predictions = ((predictions$score_1) + (predictions$score_2))/2
        
        #print(performance$score_avg)
        
        all.predictions = cbind(all.predictions, model.predictions)
        all.predictions.names = c(all.predictions.names, modelfile)
        all.predictions.goc = cbind(all.predictions.goc, predictions$score_avg_goc)
      }
    }
    
    #print(performance.vector)
    #print(head(all.predictions))
    
    # best value 0.7
    chosen_predictions = which(performance.vector >= quantile(performance.vector,quantile.min) & performance.vector <= quantile(performance.vector,quantile.max))
    #print(chosen_predictions)
    
    if (normalize.predictions) {
      all.predictions = apply(all.predictions,2,normalize01)  
    }
    all.predictions = all.predictions[,chosen_predictions]
    
    data.blend[[essay_num]]$y = cbind(technical_columns.this$Score1,technical_columns.this$Score2)
    data.blend[[essay_num]]$x = as.data.frame(all.predictions)
    colnames(data.blend[[essay_num]]$x) = all.predictions.names[chosen_predictions]
    
    if (!is.vector(all.predictions)) {
      blend.prediction.raw = apply(all.predictions,1,mean)
    } else {
      blend.prediction.raw = all.predictions
    }
    
    # performance
    blend.prediction.goc = grade_on_a_curve(c(technical_columns.this$Score1[trainset],technical_columns.this$Score2[trainset]), blend.prediction.raw)
    public_predictions = rbind(public_predictions, data.frame(id=technical_columns.this$Id[publicset],essay_score=blend.prediction.goc[publicset]))
    final_score = 0.5*ScoreQuadraticWeightedKappa(blend.prediction.goc[trainset], technical_columns.this$Score2[trainset]) + 0.5*ScoreQuadraticWeightedKappa(blend.prediction.goc[trainset], technical_columns.this$Score1[trainset])
    final_scores = c(final_scores, final_score)
    
    print(sprintf("%.4f - %.4f inc %.4f",max(performance.vector),final_score, final_score - max(performance.vector)))
    print(all.predictions.names[chosen_predictions])
  }
  
  
  return(list(data=data.blend,score=MeanQuadraticWeightedKappa(final_scores)))
}


load_essay_text <- function(name) {
  return(read.csv(sprintf("data/essay_texts/%s.txt",name),sep="\t", fileEncoding="windows-1252",header=F))
}

load_technical_columns <- function () {
  training.essays <- read.csv('data/raw/train.tsv',header=TRUE, sep = "\t", fileEncoding="windows-1252")
  training.essays$type = "training"

  validation.essays <- read.csv('data/raw/public_leaderboard.tsv',header=TRUE,sep = "\t",fileEncoding="windows-1252")
  validation.essays$Score1 <- NA
  validation.essays$Score2 <- NA
  validation.essays$type <- "public"

  private.essays <- read.csv('data/raw/private_leaderboard.tsv',header=TRUE,sep = "\t",fileEncoding="windows-1252")
  private.essays$Score1 <- NA
  private.essays$Score2 <- NA
  private.essays$type <- "private"
  
  all.essays <- rbind(training.essays, validation.essays, private.essays)
  all.essays$EssayText <- NULL
  return(all.essays) 
}


grade_on_a_curve <- function(real_grades, predicted_scores) {
  grade.distribution <- table(real_grades) / length(real_grades)
  grade.distribution <- cumsum(grade.distribution)
  
  predicted_outputs_df <- data.frame(id=1:length(predicted_scores),outputs=predicted_scores)
  predicted_outputs_df <- predicted_outputs_df[order(predicted_outputs_df$outputs),]
  predicted_outputs_df$cumulative <- 1:length(predicted_scores) / length(predicted_scores)
  predicted_outputs_df$grade <- cut(predicted_outputs_df$cumulative,breaks=c(0,grade.distribution),labels=F)- 1
  predicted_outputs_df <- predicted_outputs_df[order(predicted_outputs_df$id),]
  
  return(predicted_outputs_df$grade)
}

grade_on_a_curve_auto <- function(predicted_scores,essay_num,indices=NA) {
  this.essay_set = ESSAY_TECHNICAL_COLUMNS[ESSAY_TECHNICAL_COLUMNS$type=="training" & ESSAY_TECHNICAL_COLUMNS$EssaySet==essay_num,]
  real_grades = c(this.essay_set$Score1, this.essay_set$Score2)
                  
  grade.distribution <- table(real_grades) / length(real_grades)
  grade.distribution <- cumsum(grade.distribution)
  
  predicted_outputs_df <- data.frame(id=1:length(predicted_scores),outputs=predicted_scores)
  predicted_outputs_df <- predicted_outputs_df[order(predicted_outputs_df$outputs),]
  predicted_outputs_df$cumulative <- 1:length(predicted_scores) / length(predicted_scores)
  predicted_outputs_df$grade <- cut(predicted_outputs_df$cumulative,breaks=c(0,grade.distribution),labels=F)- 1
  predicted_outputs_df <- predicted_outputs_df[order(predicted_outputs_df$id),]
  
  return(predicted_outputs_df$grade)
}

score_predictions <- function(predicted_scores,real_scores,essay_num) {
  this.essay_set = ESSAY_TECHNICAL_COLUMNS[ESSAY_TECHNICAL_COLUMNS$type=="training" & ESSAY_TECHNICAL_COLUMNS$EssaySet==essay_num,]
  real_grades = c(this.essay_set$Score1, this.essay_set$Score2)
  
  grade.distribution <- table(real_grades) / length(real_grades)
  grade.distribution <- cumsum(grade.distribution)
  
  score.goc = grade_on_a_curve(real_grades,predicted_scores)
 
  
  return( ScoreQuadraticWeightedKappa(real_scores, score.goc))
}

generate_cv_sets <- function(Nobs,K=5,dataset=NA, predictionset=NA){
  set.seed(10)
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d) 
    list(trainset=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         testset=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
}

remove_na_nan_inf <- function(data,replacement) {
  for (col in colnames(data)) {
    data[is.nan(data[,col]),col] = replacement
    data[is.infinite(data[,col]),col] = replacement
    data[is.na(data[,col]),col] = replacement
  }
  return(data)
}

replace_na_in_factors <- function(data) {
  for (col in colnames(data)) {
    if (is.factor(data[,col])) {
      d_as_char <- as.character(data[,col])
      data[,col] <- as.factor(ifelse(is.na(data[,col]),"NA",d_as_char))
    }
  }
  return(data)
}

convert_factors_to_bits <- function(data,columns,delete_column=F) {
  
  for (col in columns) {
    if (is.factor(data[,col])) {
      if (nlevels(data[,col])>2) {
        for (flevel in sort(unique(data[,col]))) {
          if (!is.na(flevel)) {
            newcolname = paste(col,"_",flevel,sep="")
            newcol = ifelse(data[,col]==flevel,1,0)
            data = add_column(data, newcol, newcolname)  
          }
        }
        if (delete_column) {
          data[,col] = NULL  
        }
      }
    }
  }
  return(data)
}

convert_2_level_factors_to_numeric <- function(data) {
  for (col in colnames(data)) {
    if (is.factor(data[,col])) {
      if (nlevels(data[,col])==2) {
        data[,col] = as.numeric(data[,col])
      }
    }
  }
  return(data)
}


normalize01 <- function(v) {
  v.max = max(v)
  v.min = min(v)
  return ((v - min(v)) / (max(v) - min(v)))
}

normalize.mean <- function(v) {
  return((v - mean(v)) / sd(v))
}

normalize01_ext_omit <- function(v,alpha=0.10) {
  v.max = quantile(v,1-alpha)
  v.min = quantile(v,alpha)
  return ((v - v.min) / (v.max - v.min))
}

normalizeQ <- function(v) {
  return(as.vector(normalize.quantiles(as.matrix(v))))
}

dominant <- function(v) {
  freq <- table(v)  
  return(as.integer(names(freq[order(-freq)][1])))
}

geometric_mean <- function(v) {
  return(exp(mean(log(v))))
}

normalize_scores <- function(scores_distribution, scores) {
  scores_distribution = c(technical_columns.this$Score1, technical_columns.this$Score2)
  scores = all.predictions[,1]
  
  scores_distribution = scores_distribution[!is.na(scores_distribution)]
  distribution = table(scores_distribution) / sum(table(scores_distribution))
  cum_distribution = cumsum(distribution)
  
  new_scores = c()
  for (score_break_num in 1:length(cum_distribution)) {
    score_break_min = ifelse(score_break_num==1,0,cum_distribution[score_break_num-1])
    score_break_max = as.vector(cum_distribution[score_break_num])
    score_break_q = as.vector(quantile(scores,score_break_max))
  
    cat(score_break_min,score_break_max,score_break_q,"\n")
    
    #score_break_q = 0.50  
    score_range = scores[scores < score_break_max & scores > score_break_min]
    new_scores = c(new_scores, score_range * (score_break_q / score_break_max))
  }
  hist(scores,50)
  hist(new_scores,50)
  
  quantile(scores,0.18)
}

Gini <- function(a, p) {
  if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
  temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
  temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
  population.delta <- 1 / length(a)
  total.losses <- sum(a)
  null.losses <- rep(population.delta, length(a)) 
  accum.losses <- temp.df$actual / total.losses
  gini.sum <- cumsum(accum.losses - null.losses)
  sum(gini.sum) / length(a)
}

filter_na <- function(v) {
  return(v[!is.na(v)])
}

summarize_predictions <- function(predictions,verbose=T) {
  essays_technical_columns = ESSAY_TECHNICAL_COLUMNS
  final_scores = c()
  for (essay_num in 1:10) {
      this_essay_set_trainset = essays_technical_columns$EssaySet == essay_num & essays_technical_columns$type == "training"
      
      average_score = (predictions[this_essay_set_trainset,]$score_1 + predictions[this_essay_set_trainset,]$score_2)/2
      score_goc = grade_on_a_curve(c(essays_technical_columns$Score1[this_essay_set_trainset],
                                     essays_technical_columns$Score2[this_essay_set_trainset]),
                                   average_score)
      kappa_1 = ScoreQuadraticWeightedKappa(essays_technical_columns$Score1[this_essay_set_trainset],score_goc)
      kappa_2 = ScoreQuadraticWeightedKappa(essays_technical_columns$Score2[this_essay_set_trainset],score_goc)
      final_scores = c(final_scores, 0.5*kappa_1 + 0.5*kappa_2)
      if (verbose) print(sprintf("Essay %.0f kappa %.4f",essay_num, 0.5*kappa_1 + 0.5*kappa_2))
  }
  mean_score = MeanQuadraticWeightedKappa(final_scores)
  if (verbose) print(sprintf("Mean Kappa: %.4f",mean_score))
  
  return(list(scores=final_scores, mean_score=mean_score))
}

prediction_score <- function(prediction_rater_a,prediction_rater_b,essay_num,prediction_type="training") {
  essays_technical_columns = ESSAY_TECHNICAL_COLUMNS
  essays_technical_columns.this = essays_technical_columns[essays_technical_columns$EssaySet == essay_num,]
  this_essay_set_trainset = essays_technical_columns.this$type == "training"
  this_essay_set_trainset_c = essays_technical_columns.this$type == prediction_type
  
  average_score = (prediction_rater_a + prediction_rater_b)/2
  score_goc = grade_on_a_curve(c(essays_technical_columns.this$Score1[this_essay_set_trainset],
                                 essays_technical_columns.this$Score2[this_essay_set_trainset]),
                               average_score)
  
  if (prediction_type=="training") {
    kappa_1 = ScoreQuadraticWeightedKappa(essays_technical_columns.this$Score1[this_essay_set_trainset],score_goc[this_essay_set_trainset])
    kappa_2 = ScoreQuadraticWeightedKappa(essays_technical_columns.this$Score2[this_essay_set_trainset],score_goc[this_essay_set_trainset])  
  } else {
    kappa_1 = NA
    kappa_2 = NA
  }
  
  if (prediction_type!="training") {
    score_goc = score_goc[this_essay_set_trainset_c]  
  }

  return(list(score_1=kappa_1, score_2=kappa_2, score_avg=(kappa_1 + kappa_2)/2,average_score=average_score, score_goc=score_goc))
}

blend_predictions <- function(...) {
  all.predictions.list <- list(...)
  technical_columns.all = ESSAY_TECHNICAL_COLUMNS
  final_scores = c()
  
  for (essay_num in 1:10) {
    technical_columns.this = technical_columns.all[technical_columns.all$EssaySet==essay_num,]
    trainset = !is.na(technical_columns.this$Score1)
    testset = is.na(technical_columns.this$Score1)
    publicset = technical_columns.this$Id %in% technical_columns.this$Id[technical_columns.this$type=="public"]
    
    all.predictions = vector(length=sum(trainset) + sum(testset))
    all.predictions.goc = vector(length=sum(trainset) + sum(testset))
    
    performance.vector = c()
    
    for (prediction_num in 1:length(all.predictions.list)) {
        predictions = all.predictions.list[[prediction_num]]
        predictions = predictions[technical_columns.all$EssaySet==essay_num,]
        all.predictions = cbind(all.predictions, (normalize01(predictions$score_1) + normalize01(predictions$score_2))/2)
        all.predictions = cbind(all.predictions, (normalize01(predictions$score_1) + normalize01(predictions$score_2))/2)
        all.predictions.goc = cbind(all.predictions.goc, predictions$score_avg_goc)
    }
    
    all.predictions = all.predictions[,-1]
    all.predictions.goc = all.predictions.goc[,-1]
    
    all.predictions = apply(all.predictions,2,normalize01)
    
    if (!is.vector(all.predictions)) {
      blend.prediction.raw = apply(all.predictions,1,mean)
    } else {
      blend.prediction.raw = all.predictions
    }

    blend.prediction.goc = grade_on_a_curve(c(technical_columns.this$Score1[trainset],technical_columns.this$Score2[trainset]), blend.prediction.raw)
    #public_predictions = rbind(public_predictions, data.frame(id=technical_columns.this$Id[publicset],essay_score=blend.prediction.goc[publicset]))
    final_score = 0.5*ScoreQuadraticWeightedKappa(blend.prediction.goc[trainset], technical_columns.this$Score2[trainset]) + 0.5*ScoreQuadraticWeightedKappa(blend.prediction.goc[trainset], technical_columns.this$Score1[trainset])
    final_scores = c(final_scores, final_score)
    print(sprintf("%.4f - %.4f inc %.4f",max(performance.vector),final_score, final_score - max(performance.vector)))
  }
  
  print(sprintf("Mean Kappa: %.4f",MeanQuadraticWeightedKappa(final_scores)))
  return(all.predictions)
}

get_scores <- function(essay_num) {
  return(ESSAY_TECHNICAL_COLUMNS[ESSAY_TECHNICAL_COLUMNS$type=="training" & ESSAY_TECHNICAL_COLUMNS$EssaySet==essay_num,]$Score1)
}

get_scores_freq <- function(essay_num) {
  return(table(get_scores(essay_num)) / sum(get_scores(essay_num)))
}

get_scores_breaks <- function(essay_num) {
  return(cumsum(get_scores_freq(essay_num))/ sum(get_scores_freq(essay_num)))
}

prediction_matrix_scores <- function(predictions) {
  scores = c()
  best_indices = c()
  for (essay_num in 1:10) {
    optim = choose_best_prediction_vec(predictions[[essay_num]], essay_num)
    scores = c(scores,optim$best_score)
    print(str(optim))
    best_indices = c(best_indices,optim$best_index*100)
  }
  print(scores)
  print(MeanQuadraticWeightedKappa(scores))
  return(best_indices)
}

id = function(n) diag(c(1),nrow=n,ncol=n)
jd = function(n,m) matrix(c(1),nrow=n,ncol=m)

tfidf <- function(bow) {
  cs <- colSums(bow)
  rs <- rowSums(bow)
  mdocs <- dim(bow)[1]
  
  tf <- bow / rs
  wcount <- apply(bow, 2, FUN=function(wv){sum(ifelse(wv>0,1,0))})
  idf <- log(mdocs / wcount)
  idfm <- as.vector(idf) * jd(length(idf),length(idf))  
  
  return(tf * idfm)
}

scale_predictions_data <- function(data) {
  for (essay_num in 1:10) {
    breaks.data = data.frame(avg_prediction_breaks = rowMeans(apply(data[[essay_num]]$x,2,FUN=function(d){quantile(d,get_scores_breaks(essay_num))})), 
                             scores_breaks=get_scores_breaks(essay_num))
    
    data[[essay_num]]$x = apply(data[[essay_num]]$x,2,FUN=function(d){scale_predictions(d,breaks.data)})
  }
  return(data)
}

scale_predictions <- function(prediction_vector, breaks.data) {
  #breaks.data = df
  #prediction_vector = predictions_matrix_1[,1]
  
  data = data.frame(prediction_vector=prediction_vector,scaled=prediction_vector)
  
  num.breaks = dim(breaks.data)[1]
  
  for (break.num in 1:num.breaks) {
    #break.num = 1
    if (break.num == 1) {
      quantile.min = 0
      quantile.min.align = 0
    } else {
      quantile.min = breaks.data[break.num - 1,]$scores_breaks
      quantile.min.align = breaks.data[break.num - 1,]$avg_prediction_breaks
    }
    
    quantile.max = breaks.data[break.num,]$scores_breaks - ifelse(break.num < num.breaks,0.000001,0) # open range
    quantile.max.align = breaks.data[break.num,]$avg_prediction_breaks - ifelse(break.num < num.breaks,0.000001,0) # open range
    
    quantile.min.real = quantile(data$prediction_vector, quantile.min)
    quantile.max.real = quantile(data$prediction_vector, quantile.max)
    
    which.to.scale = which(data$prediction_vector >= quantile.min.real & data$prediction_vector <= quantile.max.real)
    data.to.scale = data$prediction_vector[which.to.scale]
    data.to.scale = normalize01(data.to.scale)
    data.to.scale = data.to.scale * (quantile.max.align - quantile.min.align)
    data.to.scale = data.to.scale + quantile.min.align
    
    data[which.to.scale,]$scaled = data.to.scale
    #cat("scale", quantile.min.real, quantile.max.real, " to ", quantile.min.align, quantile.max.align,"\n")
  }
  
  return(data$scaled)
}

get_optimal_ntrees <- function(modelname) {
  optim.ntrees = read.table("ntrees_optimal.txt",sep=",",header=T)[,-1]
  modelname = "data_v1_gbm_interaction_depth_1_shrinkage_0.05_ntree_matrix_bag.fraction_0.5_minobs_10"
  return(as.vector(optim.ntrees[optim.ntrees$V1==modelname,-1]))
}