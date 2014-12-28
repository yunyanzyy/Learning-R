complete <- function(directory,id=1:20){
  files_list<-list.files(directory,full.names=TRUE)
  dat<-data.frame()
  for (i in id){
    id=i
    nobs=sum(complete.cases(read.csv(files_list[i])))
    dat<-rbind(dat,data.frame(id,nobs))
  }
  dat #output result
}
