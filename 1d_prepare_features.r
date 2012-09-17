#change to current directory
setwd("~/Dropbox/asap-sas/submission.2")
source("config.r")

# preparing data
essays_technical_columns = load_technical_columns()

# chunks clusters - bag of words
for (essay_num in 1:10) {
  print(essay_num)
  chunks = read.table(sprintf("features/essay_%d/chunks",essay_num),sep=",")
  
  technical_columns = load_technical_columns()
  
  technical_columns.this = technical_columns[technical_columns$EssaySet==essay_num,]
  clusters_final = data.frame(id=technical_columns.this$Id)
  
  clusters = kmeans(chunks[,2:dim(chunks)[2]], iter.max=1000,centers=30)
  clusters_df = data.frame(id=chunks[,1],cluster=clusters$cluster,one=1)
  clusters_trans = cast(clusters_df, id ~ cluster, sum)
  
  clusters_final = merge(clusters_final, clusters_trans, by="id", all.x=T, all.y=F)
  clusters_final[is.na(clusters_final)] = 0
  
  write.table(clusters_final,sprintf("features/essay_%d/chunks_clustered",essay_num),sep=",")
}
