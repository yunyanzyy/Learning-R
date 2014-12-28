#Simple copy the assignment2's example. 
#set:reset x and new matrix as the beginning
#get:equal to x (original data)
#setSolve: get solve
#getSolve:get m (already existing)
makeCacheMatri <- function(x=matrix()){
        m <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setSolve <- function(solve) m<<-solve
        getSolve <- function() m
        list(set=set, get=get,
             setSolve=setSolve,
             getSolve=getSolve)
}

cacheSolve <- function(x,...){
        m<-x$getSolve()
        ##First get m (the one already exisiting in memeory)
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ##if m not existing, use get to reset the datasource
        #Then use solve to reset the new matrix
        #Use setSolve to remember the result, output m
        else {
        data <- x$get()
        m<-solve(data,...)
        x$setSolve(m)
        m
        }
}

test<-matrix(1:25,nrow=5,ncol=5)
solveTest<-makeCacheMatri(te)
cacheSolve(solveTest)
solveTest$get

###test###
?solve
test
solve(test)
solve(test)
te<-matrix(rnorm(16),nrow=4,ncol=4)
solve(te)
te
te%*%solve(te)
m<-function(solve)
m
tt<-function(x=matrix()){
        m=NULL
        set=function(solve) m<<-solve
        set
}
tt(te)
a<-tt(te)
a
attributes(tt(te))
