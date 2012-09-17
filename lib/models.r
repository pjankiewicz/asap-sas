
#
# data
# - data (x, y)
# - modelf
# - cv_num - number of cross validation 
# 

model_essays <- function(all.data, modelf, cv_num, essays = 1:10, cv.parallel=T) {
  essays_technical_columns = ESSAY_TECHNICAL_COLUMNS
  
  final_predictions = data.frame(id=essays_technical_columns$Id)
  final_predictions$score_1 = NA
  final_predictions$score_2 = NA
  final_predictions$score_1_goc = NA
  final_predictions$score_2_goc = NA
  final_predictions$score_avg_goc = NA
  
  for (essay_num in essays) {
    print(Sys.time())
    
    data = all.data[[essay_num]]
    technical_columns = essays_technical_columns[essays_technical_columns$EssaySet == essay_num,]
    technical_columns = essays_technical_columns[essays_technical_columns$Id %in% data$id,]
    
    train_set = !is.na(technical_columns$Score1)
    
    data$y[,3] = rowMeans(data$y)
    
    predictions = do_cv(data,modelf,cv_num,parallel=cv.parallel,vars=1:3,essay_num=essay_num)
    
    final_predictions$score_1[essays_technical_columns$EssaySet == essay_num] = predictions$predictions[,1]
    final_predictions$score_2[essays_technical_columns$EssaySet == essay_num] = predictions$predictions[,2]
    final_predictions$score_1_2[essays_technical_columns$EssaySet == essay_num] = predictions$predictions[,3]
    
    real_grades_distribution = c(technical_columns$Score1[train_set],technical_columns$Score2[train_set])
    
    score_1_goc = grade_on_a_curve(real_grades_distribution, predictions$predictions[,1])
    score_2_goc = grade_on_a_curve(real_grades_distribution, predictions$predictions[,2])
    score_avg_goc = grade_on_a_curve(real_grades_distribution, (predictions$predictions[,1] + predictions$predictions[,2])/2)
    
    score_avg_goc_n = grade_on_a_curve(real_grades_distribution, (normalize01(predictions$predictions[,1]) + normalize01(predictions$predictions[,2]))/2)

    score_1_2_avg_goc = grade_on_a_curve(real_grades_distribution, (predictions$predictions[,1] + predictions$predictions[,2] + predictions$predictions[,3])/3)
    
    score_1_2_goc = grade_on_a_curve(real_grades_distribution, predictions$predictions[,3])
    
    final_predictions$score_1_goc[essays_technical_columns$EssaySet == essay_num] = score_1_goc
    final_predictions$score_2_goc[essays_technical_columns$EssaySet == essay_num] = score_2_goc
    final_predictions$score_avg_goc[essays_technical_columns$EssaySet == essay_num] = score_avg_goc
    
    final_predictions$score_1_2_goc[essays_technical_columns$EssaySet == essay_num] = score_1_2_goc
    
    final_predictions$score_1_2_avg_goc[essays_technical_columns$EssaySet == essay_num] = score_1_2_avg_goc

    final_predictions$type[essays_technical_columns$EssaySet == essay_num] = essays_technical_columns$type[essays_technical_columns$EssaySet == essay_num]
    final_predictions$real_score_1[essays_technical_columns$EssaySet == essay_num] = essays_technical_columns$Score1[essays_technical_columns$EssaySet == essay_num]
    final_predictions$real_score_2[essays_technical_columns$EssaySet == essay_num] = essays_technical_columns$Score2[essays_technical_columns$EssaySet == essay_num]
    final_predictions$essay_num[essays_technical_columns$EssaySet == essay_num] = essay_num
    
    print(0.5*ScoreQuadraticWeightedKappa(technical_columns$Score1[train_set],score_avg_goc[train_set]) + 0.5*ScoreQuadraticWeightedKappa(technical_columns$Score2[train_set],score_avg_goc[train_set]))
    print(0.5*ScoreQuadraticWeightedKappa(technical_columns$Score1[train_set],score_avg_goc_n[train_set]) + 0.5*ScoreQuadraticWeightedKappa(technical_columns$Score2[train_set],score_avg_goc_n[train_set]))
    
    print(0.5*ScoreQuadraticWeightedKappa(technical_columns$Score1[train_set],score_1_2_goc[train_set]) + 0.5*ScoreQuadraticWeightedKappa(technical_columns$Score2[train_set],score_1_2_goc[train_set]))
    print(0.5*ScoreQuadraticWeightedKappa(technical_columns$Score1[train_set],score_1_2_avg_goc[train_set]) + 0.5*ScoreQuadraticWeightedKappa(technical_columns$Score2[train_set],score_1_2_avg_goc[train_set]))
    
  }
  
  return(final_predictions)
}

# creating 1 vs others y
yconvert_to_1_vs_others <- function(y) {
  yraw = y
  y = data.frame(id=rep(NA,dim(y)[1]))
  for (yvalue in sort(unique(c(yraw[,1],yraw[,2])))) {
    if (!is.na(yvalue)) {
      y[,str_c("rater_1_eq_",yvalue)] = ifelse(yraw[,1]==yvalue, 1, 0)
      y[,str_c("rater_2_eq_",yvalue)] = ifelse(yraw[,2]==yvalue, 1, 0)
    }
  }
  y$id = NULL
  return(y)
}

model_essays_simple <- function(all.data, modelf, cv_num, essays = 1:10, cv.parallel=T) {
  essays_technical_columns = ESSAY_TECHNICAL_COLUMNS
  
  final_predictions = data.frame(id=essays_technical_columns$Id)
  final_predictions$type = essays_technical_columns$type
  final_predictions$real_score_1 = essays_technical_columns$Score1
  final_predictions$real_score_2 = essays_technical_columns$Score2
  final_predictions$essay_num = essays_technical_columns$EssaySet
  
  
  for (essay_num in essays) {
    print(Sys.time())
    
    train_set = essays_technical_columns$EssaySet == essay_num
    
    data = all.data[[essay_num]]
    
    technical_columns = essays_technical_columns[train_set,]
    technical_columns = essays_technical_columns[essays_technical_columns$Id %in% data$id,]
    
    vars = 1:dim(data$y)[2]
    
    # feature selection

    predictions = do_cv(data,modelf,cv_num,parallel=cv.parallel,vars=vars,essay_num=essay_num)
    
    for (var in vars) {
      final_predictions[train_set,str_c("score_",var)] = predictions$predictions[,var]
    }
    
    print(prediction_score(predictions$predictions[,1],predictions$predictions[,2],essay_num)$score_avg)
    
  }
  
  return(final_predictions)
}




# main cross validation process
do_cv <- function(data,modelf,essay_num, cv_num=5,vars=1:2,parallel=T) {
  TRAIN_SET = (1:length(data$y[,1]))[!is.na(data$y[,1])]
  TEST_SET = (1:length(data$y[,1]))[is.na(data$y[,1])]
  
  cv_sets <- generate_cv_sets(length(TRAIN_SET),cv_num)
  cv_sets[[cv_num+1]] <- list(trainset=TRAIN_SET,testset=TEST_SET)
  
  predictions = matrix(nrow=dim(data$x)[1],ncol=length(vars))
  prediction_object = vector(mode="list")
  
  models = list(length=length(vars))
  
  for (var in vars) {
    #print(var)
    #print(Sys.time())
    
    gc(reset=T)
    if (parallel==T) {
      lapply = mclapply
    }
    cv_set_run = lapply(1:length(cv_sets), FUN=function(cv_num){
      models = NULL
      gc(reset=T)
      
      cv_set = cv_sets[[cv_num]]
      xtrain = data$x[cv_set$trainset,]
      ytrain = data$y[cv_set$trainset,var]
      
      xtest = data$x[cv_set$testset,]
      ytest = data$y[cv_set$testset,var]
      
      if (any(is.na(ytrain))) {
        xtrain = xtrain[!is.na(ytrain),]
        ytrain = ytrain[!is.na(ytrain)]
      }
      
      model = modelf(xtrain,ytrain,xtest,ytest,var,essay_num)
      
      return(model)
    })
    
    gc(reset=T)
    
    models_cv = list(length=cv_num)
    if (cv_set_run[[1]]$predictions)
    for (cv in 1:length(cv_sets)) {
      models_cv[[cv]] = cv_set_run[[cv]]
      predictions[cv_sets[[cv]]$testset,var] = cv_set_run[[cv]]$predictions
    }
    
    models[[var]] = models_cv
  }
  
  predictions_df = as.data.frame(predictions)
  names(predictions_df) = names(data$y)
  
  test_error <- 0
  return(list(predictions=predictions,predictions_df=predictions_df,test_error=test_error,models=models,cv_sets=cv_sets))
}

# returns matrix of predictions
model_essays_pred_matrix <- function(all.data, modelf, cv_num, essays = 1:10, cv.parallel=T) {
  essays_technical_columns = ESSAY_TECHNICAL_COLUMNS
  
  #final_predictions = data.frame(id=essays_technical_columns$Id)
  #final_predictions$type = essays_technical_columns$type
  #final_predictions$real_score_1 = essays_technical_columns$Score1
  #final_predictions$real_score_2 = essays_technical_columns$Score2
  #final_predictions$essay_num = essays_technical_columns$EssaySet
  output = vector(mode="list")
  final_predictions = vector(mode="list")
  
  for (essay_num in essays) {
    print(Sys.time())
    
    train_set = essays_technical_columns$EssaySet == essay_num
    
    data = all.data[[essay_num]]
    
    technical_columns = essays_technical_columns[train_set,]
    technical_columns = essays_technical_columns[essays_technical_columns$Id %in% data$id,]
    
    vars = 1:dim(data$y)[2]

    predictions = do_cv_m(data,modelf,cv_num,parallel=cv.parallel,vars=vars,essay_num=essay_num)
    
    final_predictions[[essay_num]] = predictions$prediction_matrices
    print(choose_best_prediction_vec(predictions$prediction_matrices,essay_num)$best_score)
  }
  
  return(final_predictions)
}

do_cv_m <- function(data,modelf,essay_num,cv_num=5,vars=1:2,parallel=T) {
  TRAIN_SET = (1:length(data$y[,1]))[!is.na(data$y[,1])]
  TEST_SET = (1:length(data$y[,1]))[is.na(data$y[,1])]
  
  cv_sets <- generate_cv_sets(length(TRAIN_SET),cv_num)
  cv_sets[[cv_num+1]] <- list(trainset=TRAIN_SET,testset=TEST_SET)
  
  
  prediction_matrices = vector(mode="list")
  
  models = list(length=length(vars))
  
  for (var in vars) {
    #print(var)
    #print(Sys.time())
    
    gc(reset=T)
    if (parallel==T) {
      lapply = mclapply
    }
    cv_set_run = lapply(1:length(cv_sets), FUN=function(cv_num){
      models = NULL
      gc(reset=T)
      
      cv_set = cv_sets[[cv_num]]
      xtrain = data$x[cv_set$trainset,]
      ytrain = data$y[cv_set$trainset,var]
      
      xtest = data$x[cv_set$testset,]
      ytest = data$y[cv_set$testset,var]
      
      if (any(is.na(ytrain))) {
        xtrain = xtrain[!is.na(ytrain),]
        ytrain = ytrain[!is.na(ytrain)]
      }
      
      model = modelf(xtrain,ytrain,xtest,ytest,var,essay_num)
      
      return(model)
    })
    
    gc(reset=T)
    
    models_cv = list(length=cv_num)
    predictions_m = matrix(nrow=dim(data$x)[1],ncol=dim(cv_set_run[[1]]$predictions)[2])

    for (cv in 1:length(cv_sets)) {
      models_cv[[cv]] = cv_set_run[[cv]]
      predictions_m[cv_sets[[cv]]$testset,] = as.matrix(cv_set_run[[cv]]$predictions)
    }

    prediction_matrices[[var]] = predictions_m
    
    models[[var]] = models_cv
  }
  
  return(list(prediction_matrices=prediction_matrices,models=models,cv_sets=cv_sets))
}

create_gbm_model <- function(shrinkage=0.01,n.trees=2000,distribution="gaussian",interaction.depth=5,bag.fraction=0.5,n.minobsinnode = 10,save_models=F,simulate_ntrees=F) {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    if (is.na(var) | length(n.trees)==1) {
      trees = n.trees
    } else {
      trees = n.trees[var]
    }
    
    model = gbm.fit(xtrain,ytrain,
                    distribution=distribution,
                    shrinkage=shrinkage,
                    n.trees=trees,
                    interaction.depth=interaction.depth,
                    verbose=F,
                    n.minobsinnode=n.minobsinnode,
                    bag.fraction=bag.fraction)
    predictions = predict(model, xtest, trees)
    
    output = list(predictions=predictions)
    
    output$summary = summary(model)
    if (save_models) {
      output$model = model
      output$xtest = xtest
      output$ytest = ytest
    }
    
    if (simulate_ntrees) {
      ntrees_perf = c()
      for (ntreestry in seq(200,trees,200)) {
        ntrees_perf = c(ntrees_perf,predict(model, xtest, ntreestry))
      }
      output$ntrees_perf = ntrees_perf
    }
    return(output)  
  })
}


create_glmboost_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    data_train = cbind(ytrain,xtrain)
    colnames(data_train)[1] = "y"
    
    data_test = cbind(ytest,xtest)
    colnames(data_test)[1] = "y"
    
    model <- glmboost(y ~ ., data=data_train,...)
    predictions <- predict(model,newdata=xtest)
    
    output = list(predictions=predictions,model=model)
    return(output) 
  })
}


create_mean_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    return(list(predictions=apply(xtest,1,mean)))
  })
}


create_rf_model <- function(parallel=F,...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    xtrain = remove_na_nan_inf(xtrain,-1000)
    xtest = remove_na_nan_inf(xtest,-1000)
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain = xtrain[,numeric_columns]
    xtest = xtest[,numeric_columns]
    
    if (parallel) {
      model = randomForestPar(xtrain,ytrain,...) #,mtry=mtry,sampsize=sampsize
    } else {
      model = randomForest(xtrain,ytrain,...) #,mtry=mtry,sampsize=sampsize
    }
    
    predictions = predict(model, xtest)
    
    output = list(predictions=predictions)
    return(output)  
  })
}

randomForestPar <- function(x,y,ntree,importance=T,replace=T,nodesize=10,corr.bias=T,maxnodes=NULL) {
  cl <- makeCluster(4, type="SOCK")
  registerDoSNOW(cl)
  model <- foreach(ntree = rep(as.integer(ntree/4), 4), .combine = randomForest::combine, .packages = "randomForest") %dopar% {
    return(randomForest(x, y, ntree=ntree, importance=importance,do.trace=F,replace=replace,nodesize=nodesize,maxnodes=maxnodes,corr.bias=corr.bias)) #mtry=mtry,sampsize=sampsize
  }
  stopCluster(cl)
  return(model)
}


create_svm_model <- function(cost=5,nu=0.3) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain_scale <- scale(xtrain[,colnames(xtrain) %in% numeric_columns])
    xtest_scale <- scale(xtest[,colnames(xtest) %in% numeric_columns]
                         ,center=attr(xtrain_scale,"scaled:center")
                         ,scale =attr(xtrain_scale,"scaled:scale"))
    
    xtrain_scale  = remove_na_nan_inf(xtrain_scale,0)
    xtest_scale  = remove_na_nan_inf(xtest_scale,0)
    
    
    predictions_ensemble = NULL
    model <- svm(xtrain_scale,ytrain,type="nu-regression",cost=cost,nu=nu,seed=30)
    predictions = predict(model,xtest_scale)
    predictions = as.vector(predictions)
    
    return(list(predictions=predictions))
  })
}

create_nnet_model <- function(decay=1,size=10,maxit=100) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain_scale <- scale(xtrain[,colnames(xtrain) %in% numeric_columns])
    xtest_scale <- scale(xtest[,colnames(xtest) %in% numeric_columns]
                         ,center=attr(xtrain_scale,"scaled:center")
                         ,scale =attr(xtrain_scale,"scaled:scale"))
    
    xtrain_scale  = remove_na_nan_inf(xtrain_scale,0)
    xtest_scale  = remove_na_nan_inf(xtest_scale,0)
    
    
    model <- nnet(xtrain_scale, ytrain/4,size=size , MaxNWts = 100000, skip=T, decay=decay, maxit=maxit, trace=F)
    
    predictions = predict(model,xtest_scale)
    
    return(list(model=model,predictions=predictions))
  })
}

create_nnet_model.grid <- function(decay.v=c(0.5,1,2,3),size.v=c(1,2,3,4,5),maxit.v=c(50,100,200)) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num){
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain_scale <- scale(xtrain[,colnames(xtrain) %in% numeric_columns])
    xtest_scale <- scale(xtest[,colnames(xtest) %in% numeric_columns]
                         ,center=attr(xtrain_scale,"scaled:center")
                         ,scale =attr(xtrain_scale,"scaled:scale"))
    
    xtrain_scale  = remove_na_nan_inf(xtrain_scale,0)
    xtest_scale  = remove_na_nan_inf(xtest_scale,0)
    
    predictions = c()
    for (decay in decay.v) {
      for (size in size.v) {
        for (maxit in maxit.v) {
          model <- nnet(xtrain_scale, ytrain/4,size=size , MaxNWts = 100000, skip=T, decay=decay, maxit=maxit, trace=F)  
          predictions = cbind(predictions,predict(model,xtest_scale)*4)  
        }
      }
    }
    
    return(list(model=model,predictions=predictions))
  })
}


create_polymars_model <- function() {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    fit.mars <- polymars(ytrain, xtrain)
    predictions <- predict(fit.mars, x = xtest)
    
    output = list(predictions=predictions)
    return(output) 
    
  })
}

create_glmnet_model <- function(prediction.s=0.05, prediction.type="response",...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    model <- glmnet(as.matrix(xtrain), ytrain, ...)
    predictions <- predict(model,as.matrix(xtest),s=prediction.s,type=prediction.type)
    
    output = list(predictions=predictions)
    return(output) 
  })
}

create_glmnet_model.grid <- function(prediction.s=0.001, prediction.type="response", nlambda.v=c(5,10,20,50,75,100), alpha.v=c(0,0.1,0.25,0.50,0.75,1),...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num){

    predictions = c()
    for (nlambda in nlambda.v) {
      for (alpha in alpha.v) {
        model <- glmnet(as.matrix(xtrain), ytrain, nlambda=nlambda, alpha=alpha)
        predictions <- cbind(predictions,predict(model,as.matrix(xtest),s=prediction.s,type=prediction.type))
      }
    }
    
    output = list(predictions=predictions)
    return(output) 
  })
}

create_elasticnet_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    model <- enet(as.matrix(xtrain), ytrain)
    predictions <- predict(model,as.matrix(xtest))
    
    return(list(predictions=predictions$fit[,dim(predictions$fit)[2]]))
  })
}

create_lm_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    data_train = cbind(ytrain,xtrain)
    colnames(data_train)[1] = "y"
    
    data_test = cbind(ytest,xtest)
    colnames(data_test)[1] = "y"
    
    model <- lm(y ~ ., data=data_train)
    predictions <- predict(model,newdata=xtest)
    
    output = list(predictions=predictions,model=model)
    return(output) 
  })
}

create_sofia_model <- function(prediction_type="linear",...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain_scale <- scale(xtrain[,colnames(xtrain) %in% numeric_columns])
    xtest_scale <- scale(xtest[,colnames(xtest) %in% numeric_columns]
                         ,center=attr(xtrain_scale,"scaled:center")
                         ,scale =attr(xtrain_scale,"scaled:scale"))
    
    xtrain_scale  = (remove_na_nan_inf(xtrain_scale,0))
    xtest_scale  = (remove_na_nan_inf(xtest_scale,0))

    xtrain = data.frame(y=ytrain)
    xtrain = cbind(xtrain, xtrain_scale)
    
    xtest = data.frame(y=rep(0,dim(xtest_scale)[1]))
    xtest = cbind(xtest, xtest_scale)
    
    model <- sofia(y ~ ., data=xtrain, random_seed=30, ...)
    predictions = predict(model,xtest, prediction_type = prediction_type)
    
    predictions = as.vector(predictions)
    
    return(list(predictions=predictions))
  })
}


create_sklearn_model <- function(type="gbm",learn_rate,n_estimators,max_depth,subsample,min_samples_split=1,min_samples_leaf=1) {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    if (is.na(essay_num) | length(n_estimators)==1) {
      n_estimators = n_estimators
    } else {
      n_estimators = n_estimators[essay_num]
    }
    
    xtrain = convert_2_level_factors_to_numeric(xtrain)
    xtest = convert_2_level_factors_to_numeric(xtest)
    
    xtrain = convert_factors_to_bits(xtrain,T)
    xtest = convert_factors_to_bits(xtest,T)
    
    colnames_union = intersect(colnames(xtrain),colnames(xtest))
    
    xtrain = xtrain[,colnames_union]
    xtest = xtest[,colnames_union]
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain = xtrain[,numeric_columns]
    xtest = xtest[,numeric_columns]
    
    xtrain = remove_na_nan_inf(xtrain,-1000)
    xtest = remove_na_nan_inf(xtest,-1000)
    
    hash = digest(xtrain)
    
    write.csv(xtrain,file=sprintf("lib/sklearn/data/%s_xtrain.csv",hash),col.names=F,row.names=F)
    write.csv(ytrain,file=sprintf("lib/sklearn/data/%s_ytrain.csv",hash),col.names=F,row.names=F)
    write.csv(xtest,file=sprintf("lib/sklearn/data/%s_xtest.csv",hash),col.names=F,row.names=F)
    
    arguments = sprintf("-f %s --learn_rate %.4f --n_estimators %.0f --max_depth %.0f --subsample %.4f --min_samples_split %.0f --min_samples_leaf %.0f",
                        hash, learn_rate, n_estimators, max_depth, subsample, min_samples_split, min_samples_leaf)
    print(arguments)

    system(sprintf("python ~/Dropbox/asap-sas/v2/lib/sklearn/blend_%s.py %s",type,arguments), show.output.on.console=T, ignore.stdout=T)
    
    predictions = read.csv(sprintf("lib/sklearn/data/%s_ytest.csv",hash),header=F)
    
    unlink(sprintf("lib/sklearn/data/%s_xtrain.csv",hash))
    unlink(sprintf("lib/sklearn/data/%s_ytrain.csv",hash))
    unlink(sprintf("lib/sklearn/data/%s_xtest.csv",hash))
    unlink(sprintf("lib/sklearn/data/%s_ytest.csv",hash))
    
    return(list(predictions=predictions))
  })
}

create_knn_model.grid<- function(k.v=c(1,2,3,5,7,11)) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num){
    
    numeric_columns = colnames(xtrain)[sapply(xtrain,FUN=function(d){is.numeric(d)})]
    
    xtrain_scale <- scale(xtrain[,colnames(xtrain) %in% numeric_columns])
    xtest_scale <- scale(xtest[,colnames(xtest) %in% numeric_columns]
                         ,center=attr(xtrain_scale,"scaled:center")
                         ,scale =attr(xtrain_scale,"scaled:scale"))
    
    xtrain_scale  = (remove_na_nan_inf(xtrain_scale,0))
    xtest_scale  = (remove_na_nan_inf(xtest_scale,0))
    
    xtrain = data.frame(y=ytrain)
    xtrain = cbind(xtrain, xtrain_scale)
    
    xtest = data.frame(y=rep(0,dim(xtest_scale)[1]))
    xtest = cbind(xtest, xtest_scale)
    
    #model <- rknn(xtrain, xtest, ytrain, k = 3, mtr=400, r = 50)
    predictions = c()
    for (k in k.v) {
      model <- knn(xtrain, xtest, ytrain, k = k, prob=T)  
      nn.indices = attr(model,"nn.index")
      predictions = cbind(predictions,apply(nn.indices,1,FUN=function(d){mean(ytrain[d])}))
    }
    
    #predictions <- predict(fit.mars, x = xtest)

    output = list(predictions=predictions)
    return(output) 
    
  })
}

library(Cubist)
#install.packages("Cubist")
create_cubist_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num){
    
    xtrain <- as.matrix(xtrain)
    xtest <- as.matrix(xtest)
    
    Cubist.mod <- cubist(x = xtrain, y =ytrain, ...)
    
    predictions <- c()
    for (neighbors in 0:9) {
      predictions <- cbind(predictions,predict(Cubist.mod , newdata = xtest , neighbors = neighbors))
    }
    
    output = list(predictions=predictions)
    return(output)  
  })
}

create_combined_model <- function() {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    data_train = cbind(ytrain,xtrain)
    colnames(data_train)[1] = "y"
    
    data_test = cbind(ytest,xtest)
    colnames(data_test)[1] = "y"
    
    predictions_matrix = matrix(nrow=nrow(xtest),ncol=4)
    
    # lm model
    #model <- lm(y ~ ., data=data_train)
    #predictions_matrix[,1] <- predict(model,newdata=xtest)
    
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=0,nlambda=100)
    predictions_matrix[,2] <- predict(model,as.matrix(xtest),s=0.001)
    
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=0.5,nlambda=100)
    predictions_matrix[,3] <- predict(model,as.matrix(xtest),s=0.001)
    
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=1,nlambda=100)
    predictions_matrix[,4] <- predict(model,as.matrix(xtest),s=0.001)
    
    output = list(predictions=rowMeans(predictions_matrix,na.rm=T),model=model)
    return(output) 
  })
}


create_gbm_model_more_ntrees <- function(shrinkage=0.01,n.trees=2000,distribution="gaussian",interaction.depth=5,bag.fraction=0.5,n.minobsinnode = 10,save_models=F,more_ntrees=T,save.nth=25) {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    set.seed(30)
    
    if (is.na(essay_num) | length(n.trees)==1) {
      trees = n.trees
    } else {
      trees = n.trees[essay_num]
    }

    #if (use.weights) {
    #  freq.table = table(ytrain) / sum(ytrain)
    #  weights = 1/unlist(lapply(ytrain,FUN=function(d){ freq.table[as.character(d)==as.character(names(freq.table))] }))  
    #} else {
    #  weights = rep(1,length(ytrain))
    #}
    
    #feature_importance = abs(apply(xtrain,2,FUN=function(d){cor(ytrain,d)}))
    #selected_features = which(feature_importance > quantile(feature_importance,0.25))
    #xtrain = xtrain[,selected_features]
    #xtest = xtest[,selected_features]
    
    model = gbm.fit(xtrain,ytrain,
                    distribution=distribution,
                    shrinkage=shrinkage,
                    n.trees=trees,
                    interaction.depth=interaction.depth,
                    verbose=T,
                    n.minobsinnode=n.minobsinnode,
                    bag.fraction=bag.fraction)

    if (more_ntrees) {
  		predictions <- c()
  		for (ntreestry in seq(save.nth,trees,save.nth)) {
         Pred <- predict(model, xtest, ntreestry)
  			 predictions <- cbind(predictions, Pred)
  		}
  		colnames(predictions) = seq(save.nth,trees,save.nth)
    }
  
	
	else {
		predictions = predict(model, xtest, trees) 	
	}
	
    output = list(predictions=predictions)
    output$summary = summary(model)
    
	if (save_models) {
      output$model = model
      output$xtest = xtest
      output$ytest = ytest
    }
	
	
    return(output)  
  })
}

create_lars_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    model = lars(as.matrix(xtrain),as.vector(ytrain),...) 
    predictions = predict(model,as.vector(xtest))
    return(list(predictions=predictions$fit[,dim(predictions$fit)[2]]))
  })
}


create_penalized_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    model = penalized(as.vector(ytrain), as.matrix(xtrain), ...) 
    predictions = predict(model,as.vector(xtest))
    return(list(predictions=as.vector(predictions[,1])))
  })
}

create_blend_lm_model <- function() {
  return(function(xtrain,ytrain,xtest,ytest,var=NA,essay_num=NA){
    n.predictors = dim(xtrain)[2]
    coef.m = matrix(ncol=500,nrow=n.predictors)
    rownames(coef) = colnames(xtrain)
    
    data = xtrain
    #data = xtrain[,chosen.predictors]
    data = apply(data,2,normalize01)
    data = as.data.frame(data)
    data$y = as.vector(ytrain)
    
    for (n in 1:100) {
      # 50% of predictors - 50% observations
      print(n)
      chosen.predictors = ifelse(runif(n.predictors)>=-1,T,F)
      
      n.obs = dim(data)[1]
      
      chosen.obs = ifelse(runif(n.obs)>=0.75,T,F)
      model = lm(y ~ 0+.,data=data[chosen.obs,])
      
      model.summary = summary(model)
      coef.m[,n] = coef(model.summary)[,1]
    }
    
    coef.m[is.na(coef.m)] = 0
    coef.importance = apply(coef.m,1,mean)
    
    final_predictors = row.names(coef)[coef.importance>quantile(coef.importance,0.80)]
    
    final_data = data[,final_predictors]
    final_data$y = as.vector(ytrain)
    model = lm(y ~ ., data=final_data)
    
    predictions = predict(model,newdata=xtest)
    
    score_predictions(predictions,ytest,1)
    return(list(predictions=predictions))
  })
}


choose_best_prediction_vec <- function(prediction_matrix, essay_num) {
  #prediction_matrix = predictions[[1]]
  best_score = 0
  best_index = 0
  
  for (col in 1:dim(prediction_matrix[[1]])[2]) {
    current_score = prediction_score(prediction_matrix[[1]][,col], prediction_matrix[[2]][,col], essay_num)
    current_score = current_score$score_avg
    if (current_score > best_score) {
      best_score = current_score
      best_index = col
      best_vector = (prediction_matrix[[1]][,col] + prediction_matrix[[2]][,col]) / 2
      rater_1 = prediction_matrix[[1]][,col]
      rater_2 = prediction_matrix[[2]][,col]
    }
  }
  return(list(best_score=best_score,best_index=best_index,rater_1=rater_1,rater_2=rater_2))  
}



create_nnls_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
  xtrain <- as.matrix(xtrain)
  xtest <- as.matrix(xtest)
  ytrain <- as.vector(ytrain)
  
  
  fit.nnls <- nnls(xtrain, ytrain)
	
	initCoef <- coef(fit.nnls)
	initCoef[is.na(initCoef)] <- 0.0
	
	coef <- initCoef/sum(initCoef)
	
	predictions <- crossprod(t(xtest), coef)
	output = list(predictions=predictions,model=fit.nnls)
	return(output) 
  })
}


create_median_model <- function(...) {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    return(list(predictions=apply(xtest,1,median)))
  })
}


