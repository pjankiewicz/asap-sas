#get_optimal_ntrees

ntrees_caps = vector(mode="list")
model.num = 1
for (modelfile in list.files("models")) {
  predictions = load_file(str_c("models/",modelfile))
  if (class(predictions)=="matrix_predictions" & str_detect(modelfile,"ntree_matrix")) {
    print(modelfile)  
    optim_ntrees = (prediction_matrix_scores(predictions))
    ntrees_caps[[model.num]] = list(model=modelfile,optim_ntrees=optim_ntrees)
    model.num = model.num + 1
  }
}

optim.ntrees = do.call(rbind,lapply(ntrees_caps,FUN=function(d){t(c(d$model,d$optim_ntrees+500))}))

write.csv(optim.ntrees,file="ntrees_optimal.txt")

modelnames = list.files("models/")
write.csv(modelnames,file="models_to_create.txt",row.names=F)