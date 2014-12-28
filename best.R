#Get the lowest death rate of a state via best function
#
best <- function(state, outcome) { 
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        col<-c(11,17,23)
        colname<-c("heart attack","heart failure","pneumonia")
        list<-cbind(col,colname)
        #Data Clean: get the var's number
        
        ## Check that state and outcome are valid,output error message
        if (!any(data$State==state)) {
                stop("invalid state")}
        if (!any(colname==outcome)){
                stop("invalid outcome")
        }
        #Subset:get the new dataset which by outcome/state
        var<-as.numeric(list[colname==outcome,1])
        data_new<-data[data$State==state,c(2,var)]
        data_new[,2]<-as.numeric(data_new[,2])
        data_complete<-data_new[complete.cases(data_new),]
        #Exclude NAs
        data_order<-data_complete[order(data_complete[,2],data_complete[,1]),]
        data_order[1,1]        
        #Sort by order
        ## Return hospital name in that state with lowest 30-day death ## rate
}
