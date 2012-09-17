setwd("~/Dropbox/asap-sas/submission.2")
source("config.r")

# various models
blend.data = list(
  list(name="gbm",path="models",pattern="gbm",quantile.min=0.70,quantile.max=1.00,model=create_glmnet_model(family="gaussian",alpha=0.0,nlambda=100)),
  list(name="others",path="models",pattern="cubist|sofia|svm",quantile.min=0.70,quantile.max=1.00,model=create_glmnet_model(family="gaussian",alpha=0.0,nlambda=100))
)

create_combined_model <- function() {
  return(function(xtrain,ytrain,xtest,ytest,var,essay_num=NA){
    data_train = cbind(ytrain,xtrain)
    colnames(data_train)[1] = "y"
    
    data_test = cbind(ytest,xtest)
    colnames(data_test)[1] = "y"
    
    predictions_matrix = matrix(nrow=nrow(xtest),ncol=4)
    
    # lm model
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=0,nlambda=100)
    predictions_matrix[,1] <- predict(model,as.matrix(xtest),s=0.001)
    
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=0.5,nlambda=100)
    predictions_matrix[,2] <- predict(model,as.matrix(xtest),s=0.001)
    
    model <- glmnet(as.matrix(xtrain),as.vector(ytrain),family="gaussian",alpha=1,nlambda=100)
    predictions_matrix[,3] <- predict(model,as.matrix(xtest),s=0.001)
    
    predictions_matrix[,4] <- rowMeans(xtest)
    output = list(predictions=rowMeans(predictions_matrix,na.rm=T))
    return(output) 
  })
}

for (models.to.blend in blend.data) {
  print(models.to.blend$name)
  models.data = get_models(models.to.blend$path,
                           models.to.blend$pattern,
                           quantile.min=models.to.blend$quantile.min,
                           quantile.max=models.to.blend$quantile.max)
  
  predictions = model_essays_simple(models.data$data, models.to.blend$model, cv_num = 7, cv.parallel=F)
  save(predictions, file=str_c("models_meta/", models.to.blend$name))
}


final.data.blend = get_models("models_meta","gbm",quantile.min=0.0,quantile.max=1)
for (essay_num in 1:10) {
  final.data.blend$data[[essay_num]]$x = apply(final.data.blend$data[[essay_num]]$x,2,normalize01)
}

blend_predictions_glmnet = model_essays_simple(final.data.blend$data, create_glmnet_model(family="gaussian",alpha=0.0,nlambda=100), cv_num = 7, cv.parallel=T)
summarize_predictions(blend_predictions_glmnet)
save_submission(blend_predictions_glmnet,"public")
save_submission(blend_predictions_glmnet,"private")
