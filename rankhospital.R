rankhospital <- function(state, outcome, num = "best") { 
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        col<-c(11,17,23)
        colname<-c("heart attack","heart failure","pneumonia")
        list<-cbind(col,colname)
        #Data Clean: get the var's number
        ## Check that state and outcome are valid
        if (!any(data$State==state)) {
                stop("invalid state")}
        if (!any(colname==outcome)){
                stop("invalid outcome")
        }
        var<-as.numeric(list[colname==outcome,1])
        data_new<-data[data$State==state,c(2,var)]
        data_new[,2]<-as.numeric(data_new[,2])
        data_complete<-data_new[complete.cases(data_new),]
        data_order<-data_complete[order(data_complete[,2],data_complete[,1]),]
        
        #define num
      
        rank<-c("best",2:(nrow(data_order)-1),"worst")
        data_order<-cbind(data_order,rank)
        if (!any(rank0==num)){
                print(NA)}
        else {data_order[rank==num,1]}
        ## Return hospital name in that state with the given rank ## 30-day death rate
}
