setwd("~/Dropbox/asap-sas/submission.2")
source("config.r")

modelsToCreate  <- read.csv("models_to_create.txt")
modelsToCreate  <-as.character(paste("models/",modelsToCreate$x,sep=""))

optimalTrees <- read.csv("ntrees_optimal.txt")
optimalTrees$V1  <-as.character(paste("models/",optimalTrees$V1,sep=""))

modelsCreated = c()
DEBUG = F
TEST = F

if (!TEST) {
  CV_NUM = 7  
} else {
  CV_NUM = 2
}

for (data_version in c("data_v4","data_v5","data_v7","data_v8","data_v9","data_v10","data_v11","data_v12","data_v13","data_v14","data_v15")) {
  print(data_version)
  data = load_file(str_c("data/",data_version))
  
  for (interaction.depth in seq(from =1, to=23)) {
    for (minobs in c(5,10)) {
    for (shrinkage in c(0.01,0.05,0.1)) {
      modelname = str_c("models/",data_version,"_gbm_interaction_depth_",interaction.depth,"_shrinkage_",shrinkage,"_ntree_matrix_bag.fraction_0.5_minobs_",minobs)
      if (any(modelsToCreate == modelname )) {
        gbm_trees_vector <- as.vector(unlist(optimalTrees[optimalTrees$V1 == modelname,3:12]))
        print(modelname)
        if (!DEBUG) {
          if (TEST) { 
              minobs = 1
              interaction.depth = 1
              gbm_trees_vector = rep(200,10)
          }
          predictions = model_essays_pred_matrix(data, 
          									 create_gbm_model_more_ntrees(shrinkage=shrinkage,n.trees=gbm_trees_vector, distribution="gaussian", 
          																  n.minobsinnode=minobs,
          																  interaction.depth=interaction.depth, bag.fraction=0.5, save_models=F, save.nth=100),
          									 CV_NUM, essays=1:10,cv.parallel=T)
          class(predictions) = "matrix_predictions"
          save(predictions,file=modelname)
        }
        modelsCreated = c(modelsCreated,modelname)
        }
      }			
    }
  } 

  
 for (interaction.depth in seq(from =1, to=23)) {
  for (minobs in c(5,10)) {
  	for (shrinkage in c(0.01,0.05,0.1)) {
      modelname = str_c("models/",data_version,"_gbm_sklearn_interaction_depth_",interaction.depth,"_shrinkage_",shrinkage,"_ntree_matrix_bag.fraction_0.5_minobs_",minobs)
			if (any(modelsToCreate == modelname )) {
			  
			  gbm_trees_vector <- as.vector(unlist(optimalTrees[optimalTrees$V1 == modelname,3:12]))
  			print(modelname)
        if (!DEBUG) {
          if (TEST) { 
            minobs = 1
            interaction.depth = 1
            gbm_trees_vector = rep(200,10)
          }
  				predictions = model_essays_pred_matrix(data, create_sklearn_model(learn_rate=shrinkage,n_estimators=gbm_trees_vector,max_depth=interaction.depth,subsample=0.5,min_samples_split=minobs),
  				                                       CV_NUM, essays=1:10,cv.parallel=T)
  			  class(predictions) = "matrix_predictions"
  			  save(predictions,file=modelname)
			  }
        modelsCreated = c(modelsCreated,modelname)
			}
	  }			
  }
 }   

 if (F) {
  
       for (committees in c(10,25,50)) {
      			modelname = sprintf("models/%s_cubist_committees_%.0f",data_version,committees)
      			if (any(modelsToCreate == modelname )) {
      				print(modelname)
      				if (!DEBUG) {
                if (TEST) {
                  committees = 2
                }
      			    predictions = model_essays_pred_matrix(data,create_cubist_model(committees = committees,control=cubistControl(seed=30)), cv_num = CV_NUM, essays=1:10,cv.parallel=T)
      			    class(predictions) = "matrix_predictions"
      			    save(predictions, file=modelname)
      				}
      				modelsCreated = c(modelsCreated,modelname)
      			}
       }
      
        for (lambda in c(0.001,0.003,0.01,0.03,0.1,0.3,1,2)) {
          for (learner_type in c("sgd-svm","pegasos")) {
            for (loop_type in c("rank","combined-ranking")) {
          	    for (iterations in c(1000000, 2000000)) {
          	    modelname = sprintf("models/%s_sofia_lambda_%.4f_learner_%s_loop_type_%s_iterations_%.0f",data_version,lambda,learner_type,loop_type,iterations)
                		if (any(modelsToCreate == modelname )) {
                				hash_mask_bits = 0
                				print(modelname)
                				if (!DEBUG) {
                          if (TEST) {
                            iterations = 10000
                          }
                  				predictions = model_essays_simple(data, 
                  												  create_sofia_model(learner_type=learner_type, loop_type=loop_type, hash_mask_bits=hash_mask_bits, iterations=iterations), 
                  				                                  CV_NUM, cv.parallel=T,essays=1:10)
                  				save(predictions, file=modelname)
                				}
                				modelsCreated = c(modelsCreated,modelname)
                    }
          	    }
      	    }
          }
        }
      
      
        for (nu in c(0.01,0.03,0.1,0.3,0.7,1)) {
          for (cost in c(0.01,0.03,0.1,0.3,1,2,3,4,5,6)) {
              modelname = sprintf("models/%s_svm_cost_%.4f_nu_%.4f",data_version,cost,nu)
        		if (any(modelsToCreate == modelname )) {
              print(modelname)
              if (!DEBUG) {
                predictions = model_essays_simple(data,create_svm_model(cost=cost,nu=nu),
                                                  CV_NUM, essays=1:10,cv.parallel=T)
                save(predictions,file=modelname)
              }
              modelsCreated = c(modelsCreated,modelname)
        		}
          }
        }  
 }
}  

# checks
#setdiff(modelsCreated,modelsToCreate)
#setdiff(modelsToCreate,modelsCreated)

