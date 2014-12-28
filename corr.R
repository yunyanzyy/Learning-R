corr <- function(directory, threshold = 0) {
    files_list<-list.files(directory,full.names=TRUE)
    cor<-numeric()
    dat<-data.frame()
    for (i in 1:332) {
      file <- read.csv(files_list[i])
      complete<-complete.cases(file)
      dat<-file[complete,]
        if (sum(complete)>threshold)
            {cor<-rbind(cor,cor(dat$sulfate,dat$nitrate))}
            else {0
            }
    }
    cor
}

