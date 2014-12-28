## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#Simple copy the assignment2's example. 
#set:reset x and new matrix as the beginning
#get:equal to x (original data)
#setSolve: get solve
#getSolve:get m (already existing)
makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
