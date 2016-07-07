## This function cache the inverse of a matrix so if it is needed again it 
## can be searched at the cache and not calculated again.

## This function creates a list with functions that allows to set and get 
## the values of a matrix and the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function returns the inverse matrix. If it has been calculated 
## before it uses the cached data. If not, the function calculates the 
## inverse and stores it so it can be used without doing the calculation the 
## next time.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
