rankall <- function(outcome, num = "best") { 
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        col<-c(11,17,23)
        colname<-c("heart attack","heart failure","pneumonia")
        list<-cbind(col,colname)
        ## Check that state and outcome are valid
        if (!any(colname==outcome)){
                stop("invalid outcome")        }
        var<-as.numeric(list[colname==outcome,1])
        data_new<-data[,c(2,7,var)]
        data_new[,3]<-as.numeric(data_new[,3])
        data_complete<-data_new[complete.cases(data_new),]
        #get the complete one, then order it by state, value, hospital name
        #split out via state#
        data_split<-split(data_complete,data_complete$State)
        data_order<-lapply(data_split,function(x) x[order(x[,3],x[,1]),])
        #make a cycle, add rank into it (rank by state itself)
        #need to verify if rank correct
        r<-data.frame()
        for (i in 1:length(data_order)){
                if (nrow(data_order[[i]])==1) {
                        rank=c("best")
                } else {
                        if (nrow(data_order[[i]])==2){
                                rank=c("best","worst")
                        }else {
                                rank=c("best",2:(nrow(data_order[[i]])-1),"worst")}}
                new1<-cbind(data_order[[i]],rank)
                r<-rbind(r,new1)
        }
        ## For each state, find the hospital of the given rank
        final<-r[r$rank==num,]
        final<-final[,-c(3,4)]
        colnames(final)<-c("hospital","state")
        final
}
